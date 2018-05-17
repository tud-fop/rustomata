use grammars::pmcfg::{PMCFGRule, VarT};
use integeriser::Integeriser;
use super::BracketContent;
use super::bracket_fragment::BracketFragment;
use dyck::Bracket;
use util::factorizable::Factorizable;

/// Represents a part either
/// * before the first variable,
/// * between two variables, or
/// * after the last variable
/// for each component in the composition in a LCFRS rule.
#[derive(Debug)]
pub enum RuleFragment<'a, N, T, W>
where
    N: 'a,
    T: 'a,
    W: 'a,
{
    Start(&'a PMCFGRule<N, T, W>, usize, Vec<&'a T>, (usize, usize), W),
    Intermediate(&'a PMCFGRule<N, T, W>, usize, (usize, usize), Vec<&'a T>, (usize, usize)),
    End(&'a PMCFGRule<N, T, W>, usize, (usize, usize), Vec<&'a T>, W),
    Whole(&'a PMCFGRule<N, T, W>, usize, Vec<&'a T>, W),
}

/// Iterates over all `RuleFragment`s in a `PMCFGRule`.
pub struct FragmentIterator<'a, N: 'a, T: 'a, W: 'a>(&'a PMCFGRule<N, T, W>, usize, i64, Vec<W>);

/// Constructs a `FragmentIterator` for each `PMCFGRule`
pub fn fragments<'a, N: 'a, T: 'a, W: 'a + Factorizable>(
    rule: &'a PMCFGRule<N, T, W>,
) -> FragmentIterator<'a, N, T, W>
where
    W: Copy
{
    FragmentIterator(rule, 0, -1, rule.weight.factorize(2 * rule.composition.composition.len()))
}

impl<'a, N, T, W> Iterator for FragmentIterator<'a, N, T, W>
where
    W: Mul<Output=W>
{
    type Item = RuleFragment<'a, N, T, W>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 >= self.0.composition.composition.len() {
            return None;
        }

        let component = &self.0.composition.composition[self.1];
        let mut terminals = Vec::new();

        let start_var = if self.2 == -1 {
            None
        } else {
            match component[self.2 as usize] {
                VarT::Var(i, j) => Some((i, j)),
                _ => None,
            }
        };
        self.2 += 1;

        for index in (self.2 as usize)..component.len() {
            match component[index] {
                VarT::T(ref t) => terminals.push(t),
                VarT::Var(i, j) => {
                    if let Some((i_, j_)) = start_var {
                        self.2 = index as i64;
                        return Some(Intermediate(self.0, self.1, (i_, j_), terminals, (i, j)));
                    } else {
                        self.2 = index as i64;
                        return Some(Start(self.0, self.1, terminals, (i, j), self.3.remove(0)));
                    }
                }
            }
        }
        let comp = self.1;
        self.1 += 1;
        self.2 = -1;
        if let Some((i, j)) = start_var {
            return Some(End(self.0, comp, (i, j), terminals, self.3.remove(0)));
        } else {
            return Some(Whole(self.0, comp, terminals, self.3.remove(0) * self.3.remove(0)));
        }
    }
}

use self::RuleFragment::*;

impl<'a, N, T, W> RuleFragment<'a, N, T, W>
where
    T: Clone + PartialEq,
    N: Clone + PartialEq,
{
    fn rule(&self) -> &'a PMCFGRule<N, T, W> {
        match *self {
            Start(r, _, _, _, _) |
            Intermediate(r, _, _, _, _) |
            End(r, _, _, _, _) |
            Whole(r, _, _, _) => r,
        }
    }

    /// Extracts a `BracketFragment` for the construction of the `GeneratorAutomaton` and ´FilterAutomaton` in a CS characterization.
    pub fn bracket_word(
        &self,
        integerizer: &Integeriser<Item = PMCFGRule<N, T, W>>,
    ) -> BracketFragment<T> {
        let mut bracks = Vec::new();
        let r = integerizer.find_key(self.rule()).unwrap();

        match *self {
            Start(_, j, _, _, _) => bracks.push(Bracket::Open(BracketContent::Component(r, j))),
            Intermediate(_, _, (i, j), _, _) => {
                bracks.push(Bracket::Close(BracketContent::Variable(r, i, j)))
            }
            End(_, _, (i, j), _, _) => bracks.push(Bracket::Close(BracketContent::Variable(r, i, j))),
            Whole(_, j, _, _) => bracks.push(Bracket::Open(BracketContent::Component(r, j))),
        };

        for symbol in self.terminals() {
            bracks.push(Bracket::Open(BracketContent::Terminal((*symbol).clone())));
            bracks.push(Bracket::Close(BracketContent::Terminal((*symbol).clone())));
        }

        match *self {
            Start(_, _, _, (i, j), _) => bracks.push(Bracket::Open(BracketContent::Variable(r, i, j))),
            Intermediate(_, _, _, _, (i, j)) => {
                bracks.push(Bracket::Open(BracketContent::Variable(r, i, j)))
            }
            End(_, j, _, _, _) => bracks.push(Bracket::Close(BracketContent::Component(r, j))),
            Whole(_, j, _, _) => bracks.push(Bracket::Close(BracketContent::Component(r, j))),
        };

        BracketFragment(bracks)
    }

    /// Lists the terminals in a `RuleFragment`.
    pub fn terminals(&self) -> &[&'a T] {
        match *self {
            Start(_, _, ref ts, _, _) |
            Intermediate(_, _, _, ref ts, _) |
            End(_, _, _, ref ts, _) |
            Whole(_, _, ref ts, _) => ts,
        }
    }

    fn from(&self) -> Bracket<(N, usize)> {
        match *self {
            Start(r, j, _, _, _) => Bracket::Open((r.head.clone(), j)),
            Intermediate(r, _, (i, j), _, _) => Bracket::Close((r.tail[i].clone(), j)),
            End(r, _, (i, j), _, _) => Bracket::Close((r.tail[i].clone(), j)),
            Whole(r, j, _, _) => Bracket::Open((r.head.clone(), j)),
        }
    }

    fn to(&self) -> Bracket<(N, usize)> {
        match *self {
            Start(r, _, _, (i, j), _) => Bracket::Open((r.tail[i].clone(), j)),
            Intermediate(r, _, _, _, (i, j)) => Bracket::Open((r.tail[i].clone(), j)),
            End(r, j, _, _, _) => Bracket::Close((r.head.clone(), j)),
            Whole(r, j, _, _) => Bracket::Close((r.head.clone(), j)),
        }
    }

    fn pdi(
        &self,
        integeriser: &Integeriser<Item = PMCFGRule<N, T, W>>,
    ) -> PushDownInstruction<(usize, usize, usize)> {
        match *self {
            Start(r, _, _, (i, j), _) => {
                PushDownInstruction::Add((integeriser.find_key(r).unwrap(), i, j))
            }
            Intermediate(r, _, (i1, j1), _, (i2, j2)) => {
                let i = integeriser.find_key(r).unwrap();
                PushDownInstruction::Replace((i, i1, j1), (i, i2, j2))
            }
            End(r, _, (i, j), _, _) => {
                PushDownInstruction::Remove((integeriser.find_key(r).unwrap(), i, j))
            }
            _ => PushDownInstruction::Nothing,
        }
    }
}

use super::automata::{PushDownInstruction, StateInstruction};
use recognisable::Transition;
use num_traits::One;
use std::ops::Mul;

type Trans<N, T, W> = Transition<
    (StateInstruction<Bracket<(N, usize)>>,
     PushDownInstruction<(usize, usize, usize)>),
    BracketFragment<T>,
    W,
>;

impl<'a, N, T, W> RuleFragment<'a, N, T, W>
where
    N: Clone + PartialEq,
    T: Clone + PartialEq,
    W: One
{
    /// Extracts the transition of the push-down automaton for the construction of the `PushDownGenerator`.
    pub fn pds(
        &self,
        integeriser: &Integeriser<Item = PMCFGRule<N, T, W>>,
    ) -> Trans<N, T, W>
    where
        W: Copy
    {
        let weight = match *self {
            Start(_, _, _, _, weight)
            | End(_, _, _, _, weight)
            | Whole(_, _, _, weight) => weight,
            Intermediate(_, _, _, _, _) => W::one()
        };

        Transition {
            word: vec![self.bracket_word(integeriser)],
            weight,
            instruction: (
                StateInstruction(self.from(), self.to()),
                self.pdi(integeriser),
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use grammars::pmcfg::Composition;
    use log_domain::LogDomain;
    use num_traits::One;
    use integeriser::{HashIntegeriser, Integeriser};

    #[test]
    fn fragments() {
        let rule: PMCFGRule<usize, usize, LogDomain<f64>> = PMCFGRule {
            head: 1,
            tail: vec![1],
            composition: Composition {
                composition: vec![vec![VarT::T(1), VarT::Var(0, 0), VarT::T(2)]],
            },
            weight: LogDomain::one(),
        };

        let mut int = HashIntegeriser::new();
        int.integerise(rule.clone());

        assert_eq!(
            super::fragments(&rule)
                .map(|f| f.bracket_word(&int))
                .collect::<Vec<_>>(),
            vec![
                BracketFragment(
                    vec![
                        Bracket::Open(BracketContent::Component(0, 0)),
                        Bracket::Open(BracketContent::Terminal(1)),
                        Bracket::Close(BracketContent::Terminal(1)),
                        Bracket::Open(BracketContent::Variable(0, 0, 0))
                    ]
                ),
                BracketFragment(
                    vec![
                        Bracket::Close(BracketContent::Variable(0, 0, 0)),
                        Bracket::Open(BracketContent::Terminal(2)),
                        Bracket::Close(BracketContent::Terminal(2)),
                        Bracket::Close(BracketContent::Component(0, 0)),
                    ]
                ),
            ]
        )
    }
}
