use super::BracketContent;
use dyck::Bracket;
use grammars::pmcfg::{PMCFGRule, VarT};
use util::factorizable::Factorizable;

use integeriser::Integeriser;
use num_traits::One;
use std::ops::Mul;

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
    End(&'a PMCFGRule<N, T, W>, usize, (usize, usize), Vec<&'a T>),
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
    FragmentIterator(rule, 0, -1, rule.weight.factorize(rule.composition.composition.len()))
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
            return Some(End(self.0, comp, (i, j), terminals));
        } else {
            return Some(Whole(self.0, comp, terminals, self.3.remove(0)));
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
            End(r, _, _, _) |
            Whole(r, _, _, _) => r,
        }
    }

    /// returns the first bracket
    fn first<I>(&self, integeriser: &I) -> Bracket<BracketContent<T>>
    where
        I: Integeriser<Item = PMCFGRule<N, T, W>>
    {
        let r = integeriser.find_key(self.rule()).unwrap();
        match *self {
            Start(_, j, _, _, _) => Bracket::Open(BracketContent::Component(r, j)),
            Intermediate(_, _, (i, j), _, _) => Bracket::Close(BracketContent::Variable(r, i, j)),
            End(_, _, (i, j), _) => Bracket::Close(BracketContent::Variable(r, i, j)),
            Whole(_, j, _, _) => Bracket::Open(BracketContent::Component(r, j)),
        }
    }

    /// returns the last bracket
    fn last<I>(&self, integeriser: &I) -> Bracket<BracketContent<T>>
    where
        I: Integeriser<Item = PMCFGRule<N, T, W>>
    {
        let r = integeriser.find_key(self.rule()).unwrap();
        match *self {
            Start(_, _, _, (i, j), _) => Bracket::Open(BracketContent::Variable(r, i, j)),
            Intermediate(_, _, _, _, (i, j)) => Bracket::Open(BracketContent::Variable(r, i, j)),
            End(_, j, _, _) => Bracket::Close(BracketContent::Component(r, j)),
            Whole(_, j, _, _) => Bracket::Close(BracketContent::Component(r, j)),
        }
    }

    /// Lists the terminals in a `RuleFragment`.
    pub fn terminals(&self) -> &[&'a T] {
        match *self {
            Start(_, _, ref ts, _, _) |
            Intermediate(_, _, _, ref ts, _) |
            End(_, _, _, ref ts) |
            Whole(_, _, ref ts, _) => ts,
        }
    }

    /// left state
    fn from(&self) -> Bracket<(N, usize)> {
        match *self {
            Start(r, j, _, _, _) => Bracket::Open((r.head.clone(), j)),
            Intermediate(r, _, (i, j), _, _) => Bracket::Close((r.tail[i].clone(), j)),
            End(r, _, (i, j), _) => Bracket::Close((r.tail[i].clone(), j)),
            Whole(r, j, _, _) => Bracket::Open((r.head.clone(), j)),
        }
    }

    /// target state
    fn to(&self) -> Bracket<(N, usize)> {
        match *self {
            Start(r, _, _, (i, j), _) => Bracket::Open((r.tail[i].clone(), j)),
            Intermediate(r, _, _, _, (i, j)) => Bracket::Open((r.tail[i].clone(), j)),
            End(r, j, _, _) => Bracket::Close((r.head.clone(), j)),
            Whole(r, j, _, _) => Bracket::Close((r.head.clone(), j)),
        }
    }
}

type Singleton<N, T> = (Bracket<(N, usize)>, Bracket<BracketContent<T>>);

impl<'a, N, T, W> RuleFragment<'a, N, T, W>
where
    N: Clone + PartialEq,
    T: Clone + PartialEq,
    W: One
{
    /// Splits the `RuleFragment` into three parts:
    /// * the left state and the first bracket
    /// * the terminal symbols between the brackets
    /// * the target state and the last bracket
    pub fn singletons<I>(&self, integeriser: &I) -> (Singleton<N, T>, &[&'a T], Singleton<N, T>)
    where
        I: Integeriser<Item = PMCFGRule<N, T, W>>,
        W: Copy
    {
        ( (self.from(), self.first(integeriser))
        ,  self.terminals()
        , (self.to(), self.last(integeriser))
        )
    }

    /// Returns the weight of the `RuleFragment`
    pub fn weight(&self) -> W
    where
        W: Copy + Mul<Output=W>
    {
        match self {
            &Start(_, _, _, _, weight) => weight,
            &Whole(_, _, _, weight) => weight,
            &Intermediate(_, _, _, _, _)
            | &End(_, _, _, _) => W::one()
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

        use self::Bracket::*;
        use self::BracketContent::*;

        let singletons: Vec<(Singleton<usize, usize>, Vec<usize>, Singleton<usize, usize>)>
            = vec![
                ( (Open((1, 0)), Open(Component(0, 0)))
                , vec![1]
                , (Open((1, 0)), Open(Variable(0, 0, 0)))
                ),
                ( (Close((1, 0)), Close(Variable(0, 0, 0)))
                , vec![2]
                , (Close((1, 0)), Close(Component(0, 0)))
                )
            ];

        assert_eq!(
            super::fragments(&rule)
                .map(|f| { let (p, ts, q) = f.singletons(&int); (p, ts.iter().map(|p| *(*p)).collect::<Vec<_>>(), q) } )
                .collect::<Vec<_>>(),
            singletons
        )
    }
}
