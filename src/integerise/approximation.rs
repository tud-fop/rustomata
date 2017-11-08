use std::hash::Hash;
use std::fmt::Debug;

use integeriser::HashIntegeriser;

use automata::*;
use approximation::*;
use push_down::*;
use tree_stack::*;
use util::equivalence_classes::*;


///Integerised form of the `ApproximationStrategy` trait
pub trait IntApproximationStrategy<N1: Hash + Eq, N2: Hash + Eq, S> {
    fn integerise(&self, &HashIntegeriser<N1>)->(HashIntegeriser<N2>, S);
}


/// Integerised version of `Approximation`
pub trait IntApproximation<T1, T2, O> {
    fn approximation(&self, &T1) -> Result<(O, T2), String>;
}


//ptk
impl<N1: Ord + PartialEq + Debug + Clone + Hash, T: Ord, W: Ord> IntApproximationStrategy<N1, N1, PDTopKElement<usize, Transition<PushDown<usize>, PushDownInstruction<usize>, T, W>,
                                                                       TransitionKey<PushDown<usize>, PushDownInstruction<usize>, T, W>>>
    for PDTopKElement<N1, Transition<PushDown<N1>, PushDownInstruction<N1>, T, W>,
                          TransitionKey<PushDown<N1>, PushDownInstruction<N1>, T, W>>{

    fn integerise(&self, inter: &HashIntegeriser<N1>)-> (HashIntegeriser<N1>, PDTopKElement<usize, Transition<PushDown<usize>, PushDownInstruction<usize>, T, W>,
                                         TransitionKey<PushDown<usize>, PushDownInstruction<usize>, T, W>>){

         (inter.clone(), PDTopKElement::new(self.size))
    }
}

//relabel
impl<A1: Ord + PartialEq + Debug + Clone + Hash + Relabel<N1, N2, A2>,
     A2: Ord + PartialEq + Debug + Clone + Hash,
     N1: Clone + Eq + Hash,
     N2: Clone + Eq + Hash,
     T: Ord,
     W: Ord> IntApproximationStrategy<A1, A2, RlbElement<PushDown<usize>, usize, usize, Transition<PushDown<usize>, PushDownInstruction<usize>, T, W>, TransitionKey<PushDown<usize>, PushDownInstruction<usize>, T, W>>>
    for RlbElement<PushDown<A1>, N1, N2, Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>, TransitionKey<PushDown<A2>, PushDownInstruction<A2>, T, W>>{

    fn integerise(&self, inter: &HashIntegeriser<A1>)-> (HashIntegeriser<A2>, RlbElement<PushDown<usize>, usize, usize, Transition<PushDown<usize>, PushDownInstruction<usize>, T, W>, TransitionKey<PushDown<usize>, PushDownInstruction<usize>, T, W>>){
        let (ne, inter2) = in_fit(self.mapping.clone(), inter);
        (inter2, RlbElement::new(ne))
    }
}

//tts

impl<N1: Ord + PartialEq + Debug + Clone + Hash, T: Ord, W: Ord>
    IntApproximationStrategy<N1, N1, TTSElement<usize, Transition<TreeStack<usize>, TreeStackInstruction<usize>, T, W>,
                                                        TransitionKey<PushDown<usize>, PushDownInstruction<usize>, T, W>>>
    for TTSElement<N1, Transition<TreeStack<N1>, TreeStackInstruction<N1>, T, W>,
                          TransitionKey<PushDown<N1>, PushDownInstruction<N1>, T, W>>{

    fn integerise(&self, inter: &HashIntegeriser<N1>)-> (HashIntegeriser<N1>, TTSElement<usize, Transition<TreeStack<usize>, TreeStackInstruction<usize>, T, W>,
                                         TransitionKey<PushDown<usize>, PushDownInstruction<usize>, T, W>>){

         (inter.clone(), TTSElement::new())
    }
}
