extern crate bincode;
use clap::{App, Arg, ArgMatches, SubCommand};
use flate2::{read, write, Compression};
use log_domain::LogDomain;
use rustomata::grammars::lcfrs::from_discodop::DiscoDopGrammar;
use rustomata::grammars::{
    lcfrs::{
        csparsing::{CSRepresentation, DebugResult},
        Lcfrs,
    },
    pmcfg::negra::{noparse, to_negra, DumpMode},
};
use std::{
    fs::File,
    io::{stdin, stdout, Read},
};

pub fn get_sub_command(name: &str) -> App {
    SubCommand::with_name(name)
        .about("Chomsky-Schützenberger representation of LCFRS")
        .subcommand(
            SubCommand::with_name("extract")
                .about(
                    "Reads a grammar from stdin and prints an object
                                       that Chomsky-Schützenberger-represents the grammar.",
                )
                .arg(
                    Arg::with_name("sxlen")
                        .short("s")
                        .long("sxlen")
                        .takes_value(true)
                        .help("maximum length to compute the sx estimate for")
                )
                .arg(
                    Arg::with_name("disco-grammar")
                        .short("d")
                        .long("disco")
                        .takes_value(false)
                        .help("Use a grammar extracted by disco-dop.")
                ).arg(
                    Arg::with_name("disco-lexer")
                        .short("l")
                        .long("lexer")
                        .takes_value(true)
                        .help("Provide the lexer file of a disco-dop grammar.")
                ).arg(
                    Arg::with_name("gzipped")
                        .short("z")
                        .long("zipped")
                        .takes_value(false)
                        .help("if the provided grammar file is Gzipped (default for disco-dop grammars)")
                        .conflicts_with("ungzipped")
                ).arg(
                    Arg::with_name("ungzipped")
                        .long("unzipped")
                        .takes_value(false)
                        .help("if the provided grammar file is not Gzipped (default for raw grammars)")
                        .conflicts_with("gzipped")
                ).arg(
                    Arg::with_name("grammar")
                        .index(1)
                        .required(false)
                        .help("Grammar file. Reads from stdin if not provided.")
                )
        )
        .subcommand(
            SubCommand::with_name("parse")
                .about(
                    "Reads a list of words from stdin and parses them
                                       using the CS representation of a grammar.",
                )
                .arg(
                    Arg::with_name("csfile")
                        .required(true)
                        .index(1)
                        .help("The file that contains the CS representation of a grammar."),
                )
                .arg(
                    Arg::with_name("k")
                        .short("k")
                        .takes_value(true)
                        .help("Puts the `k` in k-best parsing."),
                )
                .arg(
                    Arg::with_name("beam")
                        .short("b")
                        .long("beam")
                        .takes_value(true)
                        .help("limits the number of constituents for each span during parsing."),
                )
                .arg(
                    Arg::with_name("threshold")
                        .short("t")
                        .long("threshold")
                        .takes_value(true)
                        .help("Limits the constituents to a certain weight range during parsing."),
                )
                .arg(
                    Arg::with_name("candidates")
                        .short("c")
                        .long("candidates")
                        .takes_value(true)
                        .help("Maximum number of candidates to enumerate."),
                )
                .arg(
                    Arg::with_name("with-lines")
                        .short("l")
                        .long("with-lines")
                        .help("The first symbol denotes the id of the parse tree."),
                )
                .arg(
                    Arg::with_name("with-pos")
                        .short("p")
                        .long("with-pos")
                        .help("Parses the sentence with already annotated POS tags."),
                )
                .arg(
                    Arg::with_name("debugmode")
                        .short("d")
                        .long("debug")
                        .help("Prints debug information instead of parse trees."),
                )
                .arg(
                    Arg::with_name("fallback")
                        .short("f")
                        .long("with-fallback")
                        .help("Will output an incorrect parse tree if the parse does not find a correct one."),
                ),
        )
}

enum FileReader<R: Read> {
    Plain(R),
    GZipped(R),
}

impl<R: Read> FileReader<R> {
    fn new(file: R, zipped: bool) -> Self {
        if zipped {
            FileReader::GZipped(file)
        } else {
            FileReader::Plain(file)
        }
    }

    fn read(self) -> Result<String, ::std::io::Error> {
        let mut s = String::new();
        match self {
            FileReader::Plain(mut f) => {
                f.read_to_string(&mut s)?;
            }
            FileReader::GZipped(f) => {
                read::GzDecoder::new(f).read_to_string(&mut s)?;
            }
        }
        Ok(s)
    }
}

pub fn handle_sub_matches(submatches: &ArgMatches) {
    match submatches.subcommand() {
        ("extract", Some(params)) => {
            let grammar_is_gzipped = (params.is_present("gzipped")
                || params.is_present("disco-grammar"))
                && !params.is_present("ungzipped");
            let grammar_string = if let Some(path) = params.value_of("grammar") {
                FileReader::new(
                    File::open(path).expect("could not open grammar file"),
                    grammar_is_gzipped,
                )
                .read()
                .expect("could not read grammar file")
            } else {
                FileReader::new(stdin(), grammar_is_gzipped)
                    .read()
                    .expect("could not read grammar file")
            };
            let sxlen = params
                .value_of("sxlen")
                .map_or(0usize, |s| s.parse().unwrap());

            let gmr: Lcfrs<String, String, LogDomain<f64>> = if params.is_present("disco-grammar") {
                let dgmr: DiscoDopGrammar<_, _, _> =
                    grammar_string.parse().expect("Could not parse grammar.");
                if params.is_present("disco-lexer") {
                    let lexer = File::open(
                        params
                            .value_of("disco-lexer")
                            .expect("Missing lexer file as argument"),
                    )
                    .expect("could not open lexer file");
                    let lexer_string = FileReader::new(lexer, grammar_is_gzipped)
                        .read()
                        .expect("could not read lexer file");
                    dgmr.with_lexer(lexer_string.parse().expect("Could not parse lexer file."))
                        .into()
                } else {
                    dgmr.with_default_lexer().into()
                }
            } else {
                grammar_string
                    .parse()
                    .expect("Could not decode the grammar provided via stdin.")
            };

            bincode::serialize_into(
                &mut write::GzEncoder::new(stdout(), Compression::best()),
                &CSRepresentation::new(gmr, sxlen),
                bincode::Infinite,
            )
            .unwrap()
        }

        ("parse", Some(params)) => {
            let mut word_strings = String::new();
            stdin()
                .read_to_string(&mut word_strings)
                .expect("Please pass a string to parse using stdin or using the `--word` option.");
            let k: usize = match params.value_of("k") {
                Some(n) => n.parse::<usize>().unwrap(),
                None => 1usize,
            };

            let beam_width: Option<usize> = params.value_of("beam").map(|s| s.parse().unwrap());
            let beam_threshold: Option<LogDomain<f64>> =
                params.value_of("threshold").map(|s| s.parse().unwrap());
            let candidates: Option<usize> =
                params.value_of("candidates").map(|s| s.parse().unwrap());

            let csfile = File::open(params.value_of("csfile").unwrap()).unwrap();

            let csrep: CSRepresentation<String, String, LogDomain<f64>> =
                bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite)
                    .unwrap();
            let mut parser = csrep.build_generator();
            if let Some(beam) = beam_width {
                parser.set_beam(beam)
            };
            if let Some(delta) = beam_threshold {
                parser.set_delta(delta)
            };
            if let Some(candidates) = candidates {
                parser.set_candidates(candidates)
            };

            for (i, sentence) in word_strings.lines().enumerate() {
                let (i, words) = split_line(sentence, params.is_present("with-lines"), i);
                let (words, negra_mode) = split_pos(words, params.is_present("with-pos"));

                if params.is_present("debugmode") {
                    let tuple = parser.debug(words.as_slice());
                    eprint!("{} {} {:?} ", tuple.0, tuple.1, tuple.2);
                    match tuple.3 {
                        DebugResult::Parse(t, n) => {
                            eprintln!("parse {}", n);
                            println!("{}", to_negra(&t, i, negra_mode));
                        }
                        DebugResult::Fallback(t, n) => {
                            eprintln!("fallback {}", n);
                            println!("{}", to_negra(&t, i, negra_mode));
                        }
                        DebugResult::Noparse => {
                            eprintln!("noparse 0");
                            println!("{}", noparse(&words, i, negra_mode));
                        }
                    }
                } else {
                    let mut found_trees = false;
                    let (iterator, fallback) = parser.with_fallback(words.as_slice());
                    for derivation in iterator.take(k) {
                        found_trees = true;
                        println!(
                            "{}",
                            to_negra(
                                &derivation
                                    .into_iter()
                                    .map(|(k, v)| (k, v.clone()))
                                    .collect(),
                                i,
                                negra_mode.clone()
                            )
                        );
                    }
                    if !found_trees && params.is_present("fallback") {
                        if let Some(tree) = fallback {
                            println!("{}", to_negra(&tree, i, negra_mode));
                        } else {
                            println!("{}", noparse(&words, i, negra_mode));
                        }
                    } else if !found_trees {
                        println!("{}", noparse(&words, i, negra_mode));
                    }
                }
            }
        }
        _ => (),
    }
}

fn split_line<'a>(
    line: &'a str,
    with_line_number: bool,
    default_line_number: usize,
) -> (usize, impl Iterator<Item = &'a str> + 'a) {
    let mut word_iterator = line.split_whitespace();

    let line_number = if with_line_number {
        word_iterator
            .next()
            .expect("missing line number")
            .parse()
            .expect("invalid line numer")
    } else {
        default_line_number
    };

    (line_number, word_iterator)
}

fn split_pos<'a>(
    words: impl Iterator<Item = &'a str> + 'a,
    with_pos: bool,
) -> (Vec<String>, DumpMode<String>) {
    if with_pos {
        let (pos, ws) = words
            .map(|wp| {
                let mut it = wp.rsplitn(2, '/');
                (
                    it.next().unwrap().to_string(),
                    it.next().expect("missing pos annotation").to_string(),
                )
            })
            .unzip();
        (pos, DumpMode::FromPos(ws))
    } else {
        (words.map(|s| s.to_string()).collect(), DumpMode::Default)
    }
}
