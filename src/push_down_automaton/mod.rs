extern crate num_traits;

use num_traits::{One, Zero};
use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{AddAssign, Mul, MulAssign};
use std::rc::Rc;
use std::slice::Iter;
use std::vec::Vec;

use integeriser::{HashIntegeriser, Integeriser};
use recognisable::{self, Configuration, Instruction, Item, Recognisable, Transition};
use recognisable::automaton::Automaton;
use util::integerisable::{Integerisable1, Integerisable2};
use util::push_down::Pushdown;

mod from_cfg;

pub use self::from_cfg::*;

type TransitionMap<A, T, W>
    = HashMap<A, BinaryHeap<Transition<PushDownInstruction<A>, T, W>>>;

/// Automaton with storage type `PushDown<A>`, terminals of type `T` and weights of type `W`.
#[derive(Debug, Clone)]
pub struct PushDownAutomaton<A, T, W>
    where A: Clone + Hash + Ord,
          T: Eq + Hash,
          W: Ord,
{
    a_integeriser: HashIntegeriser<A>,
    t_integeriser: HashIntegeriser<T>,
    transitions: Rc<TransitionMap<usize, usize, W>>,
    initial: PushDown<usize>,
}

/// Instruction on `PushDown<A>`s.
#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
pub enum PushDownInstruction<A> {
    Replace { current_val: Vec<A>, new_val: Vec<A> },
}

impl<A> PushDownInstruction<A> {
    fn map<F, B>(&self, f: &F) -> PushDownInstruction<B>
        where F: Fn(&A) -> B
    {
        match *self {
            PushDownInstruction::Replace { ref current_val, ref new_val } =>
                PushDownInstruction::Replace {
                    current_val: current_val.iter().map(f).collect(),
                    new_val: new_val.iter().map(f).collect(),
                },
        }
    }

    fn map_mut<F, B>(&self, f: &mut F) -> PushDownInstruction<B>
        where F: FnMut(&A) -> B
    {
        match *self {
            PushDownInstruction::Replace { ref current_val, ref new_val } => {
                PushDownInstruction::Replace {
                    current_val: map_vec_mut(current_val, f),
                    new_val: map_vec_mut(new_val, f),
                }
            },
        }
    }
}

fn map_vec_mut<A, B, F>(v: &Vec<A>, f: &mut F) -> Vec<B>
    where F: FnMut(&A) -> B
{
    let mut res = Vec::new();
    for a in v {
        res.push(f(a));
    }
    res
}

impl<A: Clone + Eq + Hash> Integerisable1 for PushDownInstruction<A> {
    type AInt = PushDownInstruction<usize>;
    type I = HashIntegeriser<A>;

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt {
        self.map_mut(&mut |a| integeriser.integerise(a.clone()))
    }

    fn un_integerise(v: &Self::AInt, integeriser: &Self::I) -> Self {
        v.map(&|i| integeriser.find_value(*i).unwrap().clone())
    }
}

/// Stack with Elements of type `A`
#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord)]
pub struct PushDown<A> {
    elements: Vec<A>,
    empty: A,
}

impl<A, T, W> PushDownAutomaton<A, T, W>
    where A: Clone + Hash + Ord + PartialEq,
          T: Clone + Eq + Hash + Ord,
          W: AddAssign + Clone + Eq + Ord + Zero,
{
    pub fn new<It>(transitions: It, initial: PushDown<A>) -> Self
        where It: IntoIterator<Item=Transition<PushDownInstruction<A>, T, W>>
    {
        let mut a_inter = HashIntegeriser::new();
        let mut t_inter = HashIntegeriser::new();
        let init = initial.integerise(&mut a_inter);
        let mut transition_map = HashMap::new();

        for t in transitions.into_iter().map(|t| t.integerise(&mut t_inter, &mut a_inter)) {
            match t.instruction {
                PushDownInstruction::Replace { ref current_val, .. } => {
                    let a = current_val.first().unwrap().clone();
                    *transition_map
                        .entry(a)
                        .or_insert(HashMap::new())
                        .entry((t.word, t.instruction.clone()))
                        .or_insert(W::zero()) += t.weight;
                },
            }
        }

        let f = |(k, hm): (_, HashMap<_, _>)| (k, hm.into_iter()
            .map(|((w, i), wt)| Transition {word: w, instruction: i, weight: wt}).collect());

        PushDownAutomaton {
            a_integeriser: a_inter,
            t_integeriser: t_inter,
            transitions: Rc::new(transition_map.into_iter().map(f).collect()),
            initial: init,
        }
    }
}

impl<A, T, W> PushDownAutomaton<A, T, W>
    where A: Clone + Hash + Ord,
          T: Clone + Hash + Eq + Ord,
          W: Clone + Ord,
{
    pub fn list_transitions<'a>(&'a self)
                                -> Box<Iterator<Item=Transition<PushDownInstruction<A>, T, W>> + 'a>
    {
        Box::new(
            self.transitions.values()
                .flat_map(
                    move |h| h.iter()
                        .map(move |t| Transition::un_integerise(t,
                                                                &self.t_integeriser,
                                                                &self.a_integeriser))
                )
        )
    }
}

impl<A> Instruction for PushDownInstruction<A>
    where A: Ord + PartialEq + Clone + Hash
{
    type Storage = PushDown<A>;

    fn apply(&self, p: PushDown<A>) -> Vec<PushDown<A>> {
        match *self {
            PushDownInstruction::Replace {ref current_val, ref new_val} =>
                p.replace(current_val, new_val).ok().into_iter().collect(),
        }
    }
}

impl<A, T, W> Automaton<T, W> for PushDownAutomaton<A, T, W>
    where A: Ord + PartialEq + Clone + Hash,
          T: Clone + Eq + Hash + Ord,
          W: AddAssign + Clone + MulAssign + One + Ord + Zero,
{
    type Key = usize;
    type I = PushDownInstruction<A>;
    type IInt = PushDownInstruction<usize>;
    type TInt = usize;

    fn from_transitions<It>(transitions: It, initial: PushDown<A>) -> Self
        where It: IntoIterator<Item=Transition<PushDownInstruction<A>, T, W>>
    {
        PushDownAutomaton::new(transitions, initial)
    }

    fn transitions<'a>(&'a self)
                       -> Box<Iterator<Item=Transition<PushDownInstruction<A>, T, W>> + 'a>
    {
        self.list_transitions()
    }

    fn initial(&self) -> PushDown<A> {
        Integerisable1::un_integerise(&self.initial, &self.a_integeriser)
    }

    fn item_map(&self, i: &Item<PushDown<usize>, PushDownInstruction<usize>, usize, W>)
                -> Item<PushDown<A>, PushDownInstruction<A>, T, W> {
        match *i {
            (Configuration { ref word, ref storage, ref weight }, ref pd) => {
                let pd_vec: Vec<_>
                    = pd.clone().into();
                let pd_unint: Vec<_>
                    = pd_vec.iter().map(
                        |t| Integerisable2::un_integerise(t,
                                                          &self.t_integeriser,
                                                          &self.a_integeriser))
                            .collect();
                (
                    Configuration {
                        word: word
                            .iter()
                            .map(|t| self.t_integeriser.find_value(*t).unwrap().clone())
                            .collect(),
                        storage: Integerisable1::un_integerise(storage, &self.a_integeriser),
                        weight: weight.clone(),
                    },
                    Pushdown::from(pd_unint.as_slice())
                )
            }
        }
    }

    fn terminal_to_int(&self, t: &T) -> usize {
        self.t_integeriser.find_key(t).unwrap()
    }

    fn extract_key(c: &Configuration<PushDown<usize>, usize, W>) -> &usize {
        if c.storage.is_bottom() {
            &c.storage.empty
        } else {
            c.storage.current_symbol()
        }
    }

    fn is_terminal(c: &Configuration<PushDown<usize>, usize, W>) -> bool {
        c.word.is_empty() && c.storage.is_bottom()
    }

    fn transition_map(&self) -> Rc<TransitionMap<usize, usize, W>> {
        self.transitions.clone()
    }


    fn initial_int(&self) -> PushDown<usize> {
        self.initial.clone()
    }
}

impl<A, T, W> Recognisable<T, W> for PushDownAutomaton<A, T, W>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          T: Clone + Debug + Eq + Hash + Ord,
          W: AddAssign + One + Mul<Output=W> + MulAssign + Clone + Copy + Debug + Eq + Ord + Zero,
{
    type Parse = Item<PushDown<A>, PushDownInstruction<A>, T, W>;

    fn recognise<'a>(&'a self, word: Vec<T>) -> Box<Iterator<Item=Self::Parse> + 'a> {
        Box::new(recognisable::automaton::recognise(self, word))
    }

    fn recognise_beam_search<'a>(&'a self, beam: usize, word: Vec<T>) -> Box<Iterator<Item=Self::Parse> + 'a> {
        Box::new(recognisable::automaton::recognise_beam(self, beam, word))
    }
}

impl<A> PushDown<A> {
    pub fn empty(&self) -> &A {
        &self.empty
    }
    pub fn current_symbol(&self) -> &A {
        self.elements.last().unwrap()
    }

    /// Checks whether stack is empty.
    pub fn is_bottom(&self) -> bool {
        self.elements.len() == 1
    }

    pub fn iter(&self) -> Iter<A> {
        self.elements.iter()
    }

    pub fn map<F, B>(&self, f: &F) -> PushDown<B>
        where F: Fn(&A) -> B,
    {
        PushDown {
            empty: f(&self.empty),
            elements: self.elements.iter().map(f).collect(),
        }
    }

    pub fn map_mut<F, B>(&self, f: &mut F) -> PushDown<B>
        where F: FnMut(&A) -> B,
    {
        PushDown {
            empty: f(&self.empty),
            elements: self.elements.iter().map(f).collect(),
        }
    }
}

impl<A: Clone + Eq + Hash> Integerisable1 for PushDown<A> {
    type AInt = PushDown<usize>;
    type I = HashIntegeriser<A>;

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt {
        self.map_mut(&mut |v| integeriser.integerise(v.clone()))
    }

    fn un_integerise(aint: &Self::AInt, integeriser: &Self::I) -> Self {
        aint.map(&|i| integeriser.find_value(*i).unwrap().clone())
    }
}

impl<A> PushDown<A>
    where A: Clone
{
    pub fn from_vec(vec: Vec<A>) -> PushDown<A> {
        PushDown {
            empty: vec[0].clone(),
            elements: vec,
        }
    }

    /// New `PushDown<A>` with empty-symbol of type `A` and initial symbol of type `A`
    pub fn new(a: A, empty: A) -> PushDown<A> {
        PushDown::from_vec(vec![empty, a])
    }
}

impl<A> PushDown<A>
    where A: Clone + Ord
{
    /// Operations for Instructions:
    /// Replaces the uppermost elements with the given elements.
    /// TODO cur_sym is given in reverse order.
    pub fn replace(mut self, cur_sym: &[A],  new_sym: &[A]) -> Result<Self, Self> {
        let mut new_cur_sym = cur_sym.to_vec(); //
        new_cur_sym.reverse();                  // TODO remove this

        if self.elements.ends_with(&new_cur_sym) {
            let n = self.elements.len();
            self.elements.truncate(n - cur_sym.len());
            self.elements.append(&mut new_sym.to_vec());
            Ok(self)
        } else {
            Err(self)
        }
    }
}


impl<A> Display for PushDown<A>
    where A: Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        let mut iter1 = self.elements.iter().peekable();

        while let Some(nt) = iter1.next() {
            buffer.push_str(format!("{}", nt).as_str());
            if iter1.peek().is_some() {
                buffer.push_str(" ");
            }
        }
        write!(f, "stack: [{}], empty:{}", buffer, self.empty)
    }
}


impl<A> Display for PushDownInstruction<A>
    where A: Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PushDownInstruction::Replace { ref current_val, ref new_val} => {
                let mut buffer1 = "".to_string();
                let mut buffer2 = "".to_string();

                let mut iter1 = current_val.iter().peekable();
                let mut iter2 = new_val.iter().peekable();

                while let Some(nt) = iter1.next() {
                    buffer1.push_str(format!("\"{}\"", nt).as_str());
                    if iter1.peek().is_some() {
                        buffer1.push_str(", ");
                    }
                }

                while let Some(nt) = iter2.next() {
                    buffer2.push_str(format!("\"{}\"", nt).as_str());
                    if iter2.peek().is_some() {
                        buffer2.push_str(", ");
                    }
                }
                write!(f, "(Replace {} // {})", buffer1, buffer2)
            }
        }
    }
}

impl<A, T, W> Display for PushDownAutomaton<A, T, W>
    where A: Clone + Display + Hash + Ord + PartialEq,
          T: Clone + Debug + Display + Eq + Hash + Ord,
          W: AddAssign + Clone + Display + MulAssign + One + Ord + Zero,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut formatted_transitions = String::new();
        for t in self.list_transitions() {
            formatted_transitions.push_str(&t.to_string());
            formatted_transitions.push_str("\n");
        }
        write!(f,
               "initial: {}\n\n{}",
               self.initial(),
               formatted_transitions
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pushdown_instruction_map_correctness() {
        let instruction = PushDownInstruction::Replace {
            current_val: vec![1, 2, 3], new_val: vec![1, 2, 3, 4]
        };
        let control_instruction = PushDownInstruction::Replace {
            current_val: vec![2, 4, 6], new_val: vec![2, 4, 6, 8]
        };

        assert_eq!(
            control_instruction,
            instruction.map(&|x| x * 2)
        );
    }

    #[test]
    fn test_pushdown_instruction_map_inverse() {
        let instruction = PushDownInstruction::Replace {
            current_val: vec![1, 2, 3], new_val: vec![1, 2, 3, 4]
        };
        let mapped_instruction = instruction.map(&|x| x * 2);

        assert_eq!(
            instruction,
            mapped_instruction.map(&|x| x / 2)
        );
    }

    #[test]
    fn test_map_vec_mut_correctness() {
        let vector = vec![1, 2, 3, 4];
        let mut modified = false;

        assert_eq!(
            vec![2, 4, 6, 8],
            map_vec_mut(&vector, &mut |x| { modified = true; x * 2 })
        );
        assert_eq!(
            true,
            modified
        );
    }

    #[test]
    fn test_map_mut_vec_inverse() {
        let vector = vec![1, 2, 3, 4];
        let mut modified = false;
        let mapped_vector = map_vec_mut(&vector, &mut |x| { modified = true; x * 2 });

        assert_eq!(
            vector,
            map_vec_mut(&mapped_vector, &mut |x| { modified = false; x / 2 })
        );
        assert_eq!(
            false,
            modified
        );
    }

    #[test]
    fn test_pushdown_instruction_map_mut_correctness() {
        let instruction = PushDownInstruction::Replace {
            current_val: vec![1, 2, 3], new_val: vec![1, 2, 3, 4]
        };
        let control_instruction = PushDownInstruction::Replace {
            current_val: vec![2, 4, 6], new_val: vec![2, 4, 6, 8]
        };
        let mut modified = false;

        assert_eq!(
            control_instruction,
            instruction.map_mut(&mut |x| { modified = true; x * 2 })
        );
        assert_eq!(
            true,
            modified
        );
    }

    #[test]
    fn test_pushdown_instruction_map_mut_inverse() {
        let instruction = PushDownInstruction::Replace {
            current_val: vec![1, 2, 3], new_val: vec![1, 2, 3, 4]
        };
        let mut modified = false;
        let mapped_instruction = instruction.map_mut(&mut |x| { modified = true; x * 2 });

        assert_eq!(
            instruction,
            mapped_instruction.map_mut(&mut |x| { modified = false; x / 2 })
        );
        assert_eq!(
            false,
            modified
        );
    }

    #[test]
    fn test_pushdown_instruction_apply_correctness() {
        let pushdown = PushDown::from_vec(vec![1, 2, 3, 4]);

        let pop_instruction = PushDownInstruction::Replace {
            current_val: vec![4, 3], new_val: vec![3]
        };
        assert_eq!(
            vec![PushDown::from_vec(vec![1, 2, 3])],
            pop_instruction.apply(pushdown.clone())
        );

        let push_instruction = PushDownInstruction::Replace {
            current_val: vec![4], new_val: vec![4, 5]
        };
        assert_eq!(
            vec![PushDown::from_vec(vec![1, 2, 3, 4, 5])],
            push_instruction.apply(pushdown.clone())
        );

        let identity_instruction = PushDownInstruction::Replace {
            current_val: vec![], new_val: vec![]
        };
        assert_eq!(
            vec![pushdown.clone()],
            identity_instruction.apply(pushdown.clone())
        );

        let invalid_instruction = PushDownInstruction::Replace {
            current_val: vec![5], new_val: vec![],
        };
        assert_eq!(
            Vec::<PushDown<_>>::new(),
            invalid_instruction.apply(pushdown)
        );
    }

    #[test]
    fn test_pushdown_instruction_inverse() {
        let pushdown = PushDown::from_vec(vec![1, 2, 3, 4]);
        let instruction = PushDownInstruction::Replace {
            current_val: vec![4, 3], new_val: vec![3]
        };
        let modified_pushdown = instruction.apply(pushdown.clone()).pop().unwrap();
        let inverse_instruction = PushDownInstruction::Replace {
            current_val: vec![3], new_val: vec![3, 4]
        };

        assert_eq!(
            vec![pushdown],
            inverse_instruction.apply(modified_pushdown)
        );
    }

    #[test]
    fn test_pushdown_instruction_integerise_inverse() {
        let instruction = PushDownInstruction::Replace {
            current_val: vec![1], new_val: vec![1, 2]
        };
        let mut integeriser = HashIntegeriser::new();
        let integerised_instruction = instruction.integerise(&mut integeriser);

        assert_eq!(
            instruction,
            PushDownInstruction::un_integerise(&integerised_instruction, &integeriser)
        );
    }
}
