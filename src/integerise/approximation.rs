use std::hash::Hash;
use std::fmt::Debug;

use automata::*;
use approximation::*;
use util::*;
use push_down::*;
use tree_stack::*;
use util::equivalence_classes::*;


///Integerised form of the `ApproximationStrategy` trait
pub trait IntApproximationStrategy<N1: Hash + Eq, N2: Hash + Eq, S> {
    fn integerise(&self, &Integeriser<N1>)->(Integeriser<N2>, S);
}


/// Integerised version of `Approximation`
pub trait IntApproximation<T1, T2, O> {
    fn approximation(&self, &T1) -> Result<(O, T2), String>;
}


//ptk
impl<N1: Ord + PartialEq + Debug + Clone + Hash, T: Ord, W: Ord> IntApproximationStrategy<N1, N1, PDTopKElement<u64, Transition<PushDown<u64>, PushDownInstruction<u64>, T, W>,
                                                                       TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>>
    for PDTopKElement<N1, Transition<PushDown<N1>, PushDownInstruction<N1>, T, W>,
                          TransitionKey<PushDown<N1>, PushDownInstruction<N1>, T, W>>{

    fn integerise(&self, inter: &Integeriser<N1>)-> (Integeriser<N1>, PDTopKElement<u64, Transition<PushDown<u64>, PushDownInstruction<u64>, T, W>,
                                         TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>){

         (inter.clone(), PDTopKElement::new(self.size))
    }
}

//relabel
impl<A1: Ord + PartialEq + Debug + Clone + Hash + Relabel<N1, N2, A2>,
     A2: Ord + PartialEq + Debug + Clone + Hash,
     N1: Clone + Eq + Hash,
     N2: Clone + Eq + Hash,
     T: Ord,
     W: Ord> IntApproximationStrategy<A1, A2, RlbElement<PushDown<u64>, u64, u64, Transition<PushDown<u64>, PushDownInstruction<u64>, T, W>, TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>>
    for RlbElement<PushDown<A1>, N1, N2, Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>, TransitionKey<PushDown<A2>, PushDownInstruction<A2>, T, W>>{

    fn integerise(&self, inter: &Integeriser<A1>)-> (Integeriser<A2>, RlbElement<PushDown<u64>, u64, u64, Transition<PushDown<u64>, PushDownInstruction<u64>, T, W>, TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>){
        let (ne, inter2) = in_fit(self.mapping.clone(), inter);
        (inter2, RlbElement::new(ne))
    }
}

//tts

impl<N1: Ord + PartialEq + Debug + Clone + Hash, T: Ord, W: Ord>
    IntApproximationStrategy<N1, N1, TTSElement<u64, Transition<TreeStack<u64>, TreeStackInstruction<u64>, T, W>,
                                                        TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>>
    for TTSElement<N1, Transition<TreeStack<N1>, TreeStackInstruction<N1>, T, W>,
                          TransitionKey<PushDown<N1>, PushDownInstruction<N1>, T, W>>{

    fn integerise(&self, inter: &Integeriser<N1>)-> (Integeriser<N1>, TTSElement<u64, Transition<TreeStack<u64>, TreeStackInstruction<u64>, T, W>,
                                         TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>){

         (inter.clone(), TTSElement::new())
    }
}
