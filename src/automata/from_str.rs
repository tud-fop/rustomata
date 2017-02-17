use std::vec::Vec;
use std::marker::PhantomData;
use std::str::FromStr;

use automata::{Instruction, Transition};

/// Each item must be enclosed in quotation marks (i.e. `"⟨item⟩"`), `"` inside the `⟨item⟩` as well as `\` need to be escaped with `\`.
fn vec_from_str<T: FromStr>(s: &str) -> Result<Vec<T>, String> {
    let mut result: Vec<T> = Vec::new();
    let mut buffer: String = String::new();
    let mut is_escaped: bool = false;
    let mut is_inside: bool = false;

    for c in s.chars() {
        match (c, is_inside, is_escaped) {
            (']' , false, _    )  // read closing bracket in outer mode
                => break,
            ('"' , false, _    )  // read '"' in outer mode
                => {
                    is_inside = true;
                    is_escaped = false;
                },
            ('"' , true , true )  // read escaped '"' in inner mode
                => {
                    buffer.push('"');
                    is_escaped = false;
                },
            ('"' , true , false)  // read unescaped '"' in inner mode
                => {
                    result.push(try!(buffer.parse().map_err(|_| format!("Substring {} could not be parsed.", buffer))));
                    is_inside = false;
                    buffer.clear();
                }
            ('\\', true , false)  // read escape char in inner mode
                => is_escaped = true,
            ('\\', true , true )  // read escaped '\' in inner mode
                => {
                    buffer.push('\\');
                    is_escaped = false;
                },
            (_   , true , false)  // read a normal character in inner mode
                => buffer.push(c),
            (_   , true , true )  // read escaped character in inner mode
                => return Err(format!("Character {} should not be escaped.", c)),
            _
                => ()
        }
        // println!("character: {},  buffer: {},  is_inside: {},  is_escaped: {}", c, buffer, is_inside, is_escaped);
    }

    Ok(result)
}

impl<A, I: Instruction<A> + FromStr, T: FromStr, W: FromStr> FromStr for Transition<A, I, T, W> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("Transition ") {
            let word: Vec<T> = try!(match (s.find('['), s.rfind(']')) {
                (Some(bl), Some(br)) => vec_from_str(&s[bl..br + 1]),
                _ => return Err("Substring \"[⟨word⟩]\" not found.".to_string()),
            });

            let weight: W = try!(match s.find('#') {
                Some(rl) => {
                    s[rl + 1..]
                        .trim()
                        .parse()
                        .map_err(|_| format!("Substring {} is no weight.", &s[rl + 1..]))
                }
                _ => return Err("Substring \"#⟨weight⟩\" not found.".to_string()),
            });

            let instruction: I = try!(match (s.find('('), s.rfind(')')) {
                (Some(pl), Some(pr)) => {
                    s[pl + 1..pr]
                        .parse()
                        .map_err(|_| format!("Substring {} is not an instruction.", &s[pl + 1..pr]))
                }
                _ => Err("Substring \"(⟨instr⟩)\" not found.".to_string()),
            });

            Ok(Transition {
                _dummy: PhantomData,
                word: word,
                weight: weight,
                instruction: instruction,
            })
        } else {
            Err("Not a transition.".to_string())
        }
    }
}
