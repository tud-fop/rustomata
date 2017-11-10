cognize_with_tsa<A: PartialEq + Eq + Clone + Hash + Ord + Debug, 
                          F: Fn(&A) -> usize>
       (alphabet: Vec<A>, sort: F, partition: HashMap<A, &[A]>, word: Vec<Bracket<A>>) -> bool {
    
    use TreeStackAutomaton;
    use TreeStackInstruction;
    use Transition;
    use std::marker::PhantomData;
    use std::collections::BTreeSet; // HashSet does not implement Hash
    use Automaton;
    use TreeStack;
    use Configuration;

    let mut transitions = Vec::new();

    
    for sigma in alphabet.clone().into_iter() {

        let mut next_subset: BTreeSet<A> = partition.get(&sigma).unwrap().iter().map(|e| e.clone()).collect();
        next_subset.remove(&sigma);
        transitions.push(Transition{
            _dummy: PhantomData,
            word: vec![Bracket::Open(sigma.clone())],
            weight: 1,
            instruction: TreeStackInstruction::Push{ 
                n: sort(&sigma), 
                current_val: MDTreeElem::Root,
                new_val: MDTreeElem::Node(Some(sigma.clone()), next_subset)
            }
        });
        for subset in power(partition.get(&sigma).unwrap()).into_iter() {
            if subset.contains(&sigma) {
                let mut next_subset = subset.clone();
                next_subset.remove(&sigma);
                transitions.push(Transition{
                    _dummy: PhantomData,
                    word: vec![Bracket::Open(sigma.clone())],
                    weight: 1,
                    instruction: TreeStackInstruction::Up{ 
                        n: sort(&sigma), 
                        current_val: MDTreeElem::Root, 
                        old_val: MDTreeElem::Node(None, subset.clone()),
                        new_val: MDTreeElem::Node(Some(sigma.clone()), next_subset)
                    }
                });
            } else {
                transitions.push(Transition{
                    _dummy: PhantomData,
                    word: vec![Bracket::Close(sigma.clone())],
                    weight: 1,
                    instruction: TreeStackInstruction::Down{
                        current_val: MDTreeElem::Node(Some(sigma.clone()), subset.clone()),
                        old_val: MDTreeElem::Root,
                        new_val: MDTreeElem::Root
                    }
                });
            }
        }

        for predecessor_sigma in alphabet.clone().into_iter() {
            for p_subset in power(partition.get(&predecessor_sigma).unwrap()).into_iter() {
                
                if !p_subset.contains(&predecessor_sigma){
                    let mut next_subset: BTreeSet<A> = partition.get(&sigma).unwrap().iter().map(|e| e.clone()).collect();
                    next_subset.remove(&sigma);
                    transitions.push(Transition{
                        _dummy: PhantomData,
                        word: vec![Bracket::Open(sigma.clone())],
                        weight: 1,
                        instruction: TreeStackInstruction::Push{ 
                            n: sort(&sigma), 
                            current_val: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone()),
                            new_val: MDTreeElem::Node(Some(sigma.clone()), next_subset.clone())
                        }
                    });
                
                    for subset in power(partition.get(&sigma).unwrap()).into_iter(){
                        if subset.contains(&sigma) {
                            let mut next_subset = subset.clone();
                            next_subset.remove(&sigma);
                            transitions.push(Transition{
                                _dummy: PhantomData,
                                word: vec![Bracket::Open(sigma.clone())],
                                weight: 1,
                                instruction: TreeStackInstruction::Up{ 
                                    n: sort(&sigma), 
                                    current_val: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone()), 
                                    old_val: MDTreeElem::Node(None, subset.clone()),
                                    new_val: MDTreeElem::Node(Some(sigma.clone()), next_subset.clone())
                                }
                            });
                        } else {
                            transitions.push(Transition{
                                _dummy: PhantomData,
                                word: vec![Bracket::Close(sigma.clone())],
                                weight: 1,
                                instruction: TreeStackInstruction::Down{
                                    current_val: MDTreeElem::Node(Some(sigma.clone()), subset.clone()),
                                    old_val: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone()),
                                    new_val: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone())
                                }
                            });
                        }
                    }
                }

                
            }
        }
    }

    let automaton = TreeStackAutomaton::new(transitions, TreeStack::new(MDTreeElem::Root));

    for (Configuration{ storage, .. }, _) in automaton.recognise(word) {
        // check if all nodes are of form (None, ∅)
        let mut empty = true;
        let mut chk = | node: &MDTreeElem<A> | -> bool {
            match node {
                &MDTreeElem::Root => { true },
                &MDTreeElem::Node(None, ref s) => {
                    empty = empty && s.is_empty();
                    s.is_empty()
                },
                &MDTreeElem::Node(Some(_), _) => {
                    empty = false;
                    false
                }
            }
        };
        println!("{:?}", storage);//.map_mut(&mut chk));
        return true;
    }

    false
}