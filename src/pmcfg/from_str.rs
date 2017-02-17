use std::marker::PhantomData;
use std::str::FromStr;

use pmcfg::{VarT, Composition, PMCFGRule, PMCFG};

impl<N: FromStr, T: FromStr + Clone, W: FromStr> FromStr for PMCFG<N, T, W> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let initial: N;
        let mut rules: Vec<PMCFGRule<N, T, W>> = Vec::new();

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
                rules.push(try!(l.trim().parse().map_err(|_| format!("Substring {} is not a rule.", l.trim()))));
            }
        }

        Ok(PMCFG {
            _dummy: PhantomData,
            initial: initial,
            rules: rules,
        })

    }
}

impl<N: FromStr, T: FromStr + Clone, W: FromStr> FromStr for PMCFGRule<N, T, W> {
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

        let mut tail: Vec<N>;

        match (s.rfind(']'), s.rfind('#')) {
            (Some(l), Some(r)) => {
                let mut rhs = s[l + 1..r].trim()[1..].to_string();
                rhs.pop();

                tail = Vec::new();
                for x in rhs.split(',').map(|x1| x1.trim()).filter(|y| !y.is_empty()) {
                    tail.push(try!(x
                        .parse()
                        .map_err(|_| format!("Can not parse \"{}\" as N.", x))));
                }
            }
            _ => return Err(format!("No successor nonterminals found in \"{}\".", s)),
        }

        let composition: Composition<T>;

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

        Ok(PMCFGRule {
            head: head,
            tail: tail,
            composition: composition,
            weight: weight,
        })
    }
}

impl<T: FromStr + Clone> FromStr for Composition<T> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut buffer: String = String::new();
        let mut vart_buffer: Vec<VarT<T>> = Vec::new();
        let mut composition: Vec<Vec<VarT<T>>> = Vec::new();

        let mut active: bool = false;

        let mut inner_mode: bool = false;
        let mut literal_mode: bool = false;
        let mut escaped_mode: bool = false;

        for c in s.chars() {
            match (c, active, inner_mode, literal_mode, escaped_mode) {
                ('[', false, false, false, false) => active = true,
                (']', true, false, false, false) => return Ok(Composition { composition: composition }),
                ('[', true, false, false, false) => {
                    inner_mode = true;
                    vart_buffer.clear();
                    buffer.clear();
                }
                (']', true, true, false, false) => {
                    inner_mode = false;
                    if !buffer.is_empty() {
                        vart_buffer.push(try!(buffer.trim().parse()));
                        buffer.clear();
                    }
                    composition.push(vart_buffer.clone());
                }
                ('"', true, true, false, false) => literal_mode = true,
                ('"', true, true, true , false) => literal_mode = false,
                (_  , true, true, true , true ) => {
                    buffer.push(c);
                    escaped_mode = false;
                }
                ('\\', true, true, _    , false) => escaped_mode = true,
                (',', true, true, false, false) => {
                    vart_buffer.push(try!(buffer.trim().parse()));
                    buffer.clear();
                }
                (',', true, true, true, false) => buffer.push(c),
                (_, true, true, _, false) => buffer.push(c),
                _ => (),
            }
        }

        Err(format!("Can not parse \"{}\" as Composition<T>.", s))
    }
}

impl<T: FromStr> FromStr for VarT<T> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v: Vec<&str> = s.split_whitespace().collect();
        match v[0] {
            "Var" if v.len() == 3 => {
                Ok(VarT::Var(try!(v[1].parse()
                                 .map_err(|_| format!("Could not parse \"{}\" as u8.", v[1]))),
                             try!(v[2].parse()
                                 .map_err(|_| format!("Could not parse \"{}\" as u8.", v[2])))))
            }
            "T" if v.len() == 2 => {
                Ok(VarT::T(try!(v[1].parse()
                    .map_err(|_| format!("Could not parse \"{}\" as T.", v[1])))))
            }
            _ => Err(format!("Could not parse \"{}\" as VarT<T>.", s)),
        }
    }
}
