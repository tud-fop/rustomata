use std::marker::PhantomData;
use std::str::FromStr;

use cfg::{VarT, Composition, CFGRule, CFG};

impl<N: FromStr, T: FromStr + Clone, W: FromStr> FromStr for CFG<N, T, W> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let initial: N;
        let mut rules: Vec<CFGRule<N, T, W>> = Vec::new();

        let mut it = s.lines();

        match it.next() {
            Some(l) if l.starts_with("initial: ") => {
                initial = try!(l[8..]
                    .trim()
                    .parse()
                    .map_err(|_| format!("Substring {} is not a nonterminal.", l[8..].trim())))
            }
            _ => return Err("No initial nonterminal supplied on line 1.".to_string()),
        }

        for l in s.lines() {
            if !l.is_empty() && !l.starts_with("initial: ") {
                rules.push(try!(l.trim().parse()));
            }
        }

        Ok(CFG {
            _dummy: PhantomData,
            initial: initial,
            rules: rules,
        })

    }
}

impl<N: FromStr, T: FromStr + Clone, W: FromStr> FromStr for CFGRule<N, T, W> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let head: N;

        match (s.contains('→'), s.contains("->"), s.split('→').nth(0), s.split("->").nth(0)) {
            (true, false, Some(v), _) => {
                head = try!(v.trim().parse().map_err(|_| format!("Can not parse \"{}\" as N.", v)))
            }
            (false, true, _, Some(v)) => {
                head = try!(v.trim().parse().map_err(|_| format!("Can not parse \"{}\" as N.", v)))
            }
            _ => return Err(format!("No unique arrow found in \"{}\".", s)),
        }

        let composition: Composition<N,T>;

        match (s.find('['), s.rfind(']')) {
            (Some(l), Some(r)) => composition = try!(s[l..r + 1].parse()),
            _ => return Err(format!("No composition function found in \"{}\".", s)),
        }

        let weight: W;

        match s.rfind('#') {
            Some(pos) => {
                weight = try!(s[pos + 1..]
                    .trim()
                    .parse()
                    .map_err(|_| format!("Can not parse \"{}\" as W.", &s[pos + 1..])))
            }
            _ => return Err(format!("No weight found in \"{}\".", s)),
        }

        Ok(CFGRule {
            head: head,
            composition : composition,
            weight: weight,
        })
    }
}

impl<N: FromStr, T: FromStr + Clone> FromStr for Composition<N,T> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut buffer: String= String::new();
        let mut composition: Vec<VarT<N,T>> = Vec::new();

        let mut active: bool = false;

        for c in s.chars() {
            match (c, active) {
                ('[', false) => active = true,
                (']', true) => {
                    return Ok(Composition { composition: composition });
                },
                ('\t',_) => (),
                (' ',_) => (),

                (_ ,true) => {
                    buffer.push(c);
                    composition.push(try!(buffer.trim().parse()));
                    buffer.clear();
                }
                _ => (),
            }
        }
        Err(format!("Can not parse \"{}\" as Composition<T>.", s))
    }
}

impl<N: FromStr, T: FromStr> FromStr for VarT<N,T> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let c= s.chars().nth(0);
        match c {
             Some(x) if !x.is_lowercase() => {
                Ok(VarT::Label(try!(s.parse()
                                 .map_err(|_| format!("Could not parse \"{}\" as T.", x)))))
            }
            Some(x) if x.is_lowercase() => {
                Ok(VarT::Value(try!(s.parse()
                    .map_err(|_| format!("Could not parse \"{}\" as T.", x)))))
            }
            Some(_) => Err(format!("Could not parse \"{}\" as VarT<T>.", s)),

            None => Err(format!("Could not parse \"{}\" as VarT<T>.", s)),
        }
    }
}
