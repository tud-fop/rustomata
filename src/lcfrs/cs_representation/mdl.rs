
use util::partition::Partition;
use dyck::multiple::MultipleDyckLanguage;
use pmcfg::PMCFGRule;
use integeriser::Integeriser;
use super::BracketContent;
use VarT;

/// Construction of a sorted `MultipleDycklanguage` for the CS characterization of an LCFRS. 
pub fn mdl<'a, R, I, N, T, W>(lcfrs: R, integeriser: &I) -> MultipleDyckLanguage<BracketContent<T>>
where
    R: IntoIterator<Item = &'a PMCFGRule<N, T, W>>,
    I: Integeriser<Item = PMCFGRule<N, T, W>>,
    T: Ord + Clone + 'a,
    W: 'a,
    N: 'a
{
    let mut partition = Vec::new();
    for rule in lcfrs {
        let rule_id = integeriser.find_key(rule).unwrap();
        partition.push(
            (0..rule.composition.composition.len())
                .map(|component| BracketContent::Component(rule_id, component))
                .collect(),
        );
        partition.extend(
            (0..rule.tail.len())
                .map(
                    |successor|
                    rule.composition.composition.iter().flat_map(
                        |component|
                        component.iter().filter_map(
                            |vart| 
                            if let &VarT::Var(i, j) = vart {
                                if i == successor { Some(BracketContent::Variable(rule_id, i, j)) }
                                else { None }
                            } else {
                                None
                            }
                        )
                    ).collect()
                )
        );
    }

    MultipleDyckLanguage::sorted(
        Partition::new(partition).unwrap(),
        |symbol|
        if let &BracketContent::Variable(_, i, _) = symbol {
            i
        } else {
            1
        }
    )
}