use clap::{Arg, ArgMatches, App, SubCommand};

use std::io::{self, Read};
use std::fs::File;

use integerise::{IntPushDownAutomaton, IntTreeStackAutomaton};
use integerise::{IntApproximation, IntegerisedAutomaton};
use approximation::ptk::PDTopKElement;
use approximation::relabel::RlbElement;
use approximation::tts::TTSElement;
use coarse_to_fine::{self, benchmark, equivalence_classes};
use coarse_to_fine::equivalence_classes::EquivalenceClass;
use cfg::CFG;
use pmcfg::PMCFG;
use log_domain::LogDomain;

pub fn get_sub_command() -> App<'static, 'static> {
    SubCommand::with_name("coarse-to-fine")
        .about("functions related to the coarse-to-fine recognise approach")
        .subcommand(SubCommand::with_name("cfg")
                    .about("coarse-to-fine recognising using push-down automata")
                    .subcommand(SubCommand::with_name("parse")
                                .about("parses a word given a context-free grammar")
                                .arg(Arg::with_name("grammar")
                                     .help("grammar file to use")
                                     .index(1)
                                     .required(true))
                                .arg(Arg::with_name("classes")
                                     .help("classes file to use")
                                     .index(2)
                                     .required(true))
                                .arg(Arg::with_name("topk-size")
                                     .help("size of the limited push-down")
                                     .index(3)
                                     .required(true))
                                .arg(Arg::with_name("number-of-parses")
                                     .help("number of parses that should be returned")
                                     .short("n")
                                     .long("number")
                                     .default_value("1")
                                     .required(false))
                                .arg(Arg::with_name("limit-RLB")
                                     .help("number of parses RLB needs to filter")
                                     .short("k")
                                     .long("limit1")
                                     .default_value("100")
                                     .required(false))
                                .arg(Arg::with_name("limit-PTK")
                                     .help("number of parses PTK needs to filter")
                                     .short("r")
                                     .long("limit2")
                                     .default_value("1000")
                                     .required(false)))
                    .subcommand(SubCommand::with_name("automaton")
                                .about("constructs a number of push-down automata from the given context-free grammar")
                                .arg(Arg::with_name("grammar")
                                     .help("grammar file to use")
                                     .index(1)
                                     .required(true))
                                .arg(Arg::with_name("classes")
                                     .help("classes file to use")
                                     .index(2)
                                     .required(true))
                                .arg(Arg::with_name("topk-size")
                                     .help("size of the limited push-down")
                                     .index(3)
                                     .required(true))))
        .subcommand(SubCommand::with_name("benchmark")
                    .about("benchmarks different coarse-to-fine schemes")
                    .arg(Arg::with_name("grammar")
                         .help("grammar file to use")
                         .index(1)
                         .required(true))
                    .arg(Arg::with_name("classes")
                         .help("classes file to use")
                         .index(2)
                         .required(true))
                    .arg(Arg::with_name("words")
                         .help("file containing the words to check")
                         .index(3)
                         .required(true))
                    .arg(Arg::with_name("topk-size")
                         .help("size of the limited push-down")
                         .index(4)
                         .required(true))
                    .arg(Arg::with_name("number-of-parses")
                         .help("number of parses that should be returned")
                         .short("n")
                         .long("number")
                         .default_value("1")
                         .required(false))
                    .arg(Arg::with_name("limit-TTS")
                         .help("number of parses TTS needs to filter")
                         .short("l")
                         .long("limit1")
                         .default_value("100")
                         .required(false))
                    .arg(Arg::with_name("limit-RLB")
                         .help("number of parses RLB needs to filter")
                         .short("k")
                         .long("limit2")
                         .default_value("1000")
                         .required(false))
                    .arg(Arg::with_name("limit-PTK")
                         .help("number of parses PTK needs to filter")
                         .short("r")
                         .long("limit3")
                         .default_value("10000")
                         .required(false))
                    .arg(Arg::with_name("number-words")
                         .help("number of words that are filtered")
                         .short("w")
                         .long("wordlimit")
                         .default_value("1")
                         .required(false))
                    .arg(Arg::with_name("nfa")
                         .help("are we converting into nfa")
                         .short("b")
                         .long("nfabool")
                         .default_value("false")
                         .required(false)))
        .subcommand(SubCommand::with_name("mcfg")
                    .about("coarse-to-fine recognising using push-down and tree-stack automata")
                    .subcommand(SubCommand::with_name("parse")
                                .about("parses a word given a context-free grammar")
                                .arg(Arg::with_name("grammar")
                                     .help("grammar file to use")
                                     .index(1)
                                     .required(true))
                                .arg(Arg::with_name("classes")
                                     .help("classes file to use")
                                     .index(2)
                                     .required(true))
                                .arg(Arg::with_name("topk-size")
                                     .help("size of the limited push-down")
                                     .index(3)
                                     .required(true))
                                .arg(Arg::with_name("number-of-parses")
                                     .help("number of parses that should be returned")
                                     .short("n")
                                     .long("number")
                                     .default_value("1")
                                     .required(false))
                                .arg(Arg::with_name("limit-TTS")
                                     .help("number of parses TTS needs to filter")
                                     .short("l")
                                     .long("limit1")
                                     .default_value("100")
                                     .required(false))
                                .arg(Arg::with_name("limit-RLB")
                                     .help("number of parses RLB needs to filter")
                                     .short("k")
                                     .long("limit2")
                                     .default_value("1000")
                                     .required(false))
                                .arg(Arg::with_name("limit-PTK")
                                     .help("number of parses PTK needs to filter")
                                     .short("r")
                                     .long("limit3")
                                     .default_value("10000")
                                     .required(false)))
                    .subcommand(SubCommand::with_name("automaton")
                                .about("constructs a number of push-down automata from the given context-free grammar")
                                .arg(Arg::with_name("grammar")
                                     .help("grammar file to use")
                                     .index(1)
                                     .required(true))
                                .arg(Arg::with_name("classes")
                                     .help("classes file to use")
                                     .index(2)
                                     .required(true))
                                .arg(Arg::with_name("topk-size")
                                     .help("size of the limited push-down")
                                     .index(3)
                                     .required(true))))
}

pub fn handle_sub_matches(ctf_matches: &ArgMatches) {
    match ctf_matches.subcommand() {
        ("cfg", Some(cfg_matches)) => {
            match cfg_matches.subcommand() {
                ("parse", Some(cfg_parse_matches)) => {
                    let grammar_file_name = cfg_parse_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: CFG<String, String, LogDomain<f64>> =
                        grammar_string.parse().unwrap();

                    let a = IntPushDownAutomaton::from(grammar);

                    let classes_file_name = cfg_parse_matches.value_of("classes").unwrap();
                    let mut classes_file = File::open(classes_file_name).unwrap();
                    let mut classes_string = String::new();
                    let _ = classes_file.read_to_string(&mut classes_string);
                    let e: EquivalenceClass<String, String> = classes_string.parse().unwrap();

                    let rlb = RlbElement::new(e);

                    let (b, nrlb) = a.approximation(&rlb).unwrap();

                    let size = cfg_parse_matches
                        .value_of("topk-size")
                        .unwrap()
                        .parse::<usize>()
                        .unwrap();

                    let ptk = PDTopKElement::new(size);

                    let (c, nptk) = b.approximation(&ptk).unwrap();

                    let mut corpus = String::new();
                    let _ = io::stdin().read_to_string(&mut corpus);

                    let n1 = cfg_parse_matches
                        .value_of("limit-RLB")
                        .unwrap()
                        .parse()
                        .unwrap();
                    let n2 = cfg_parse_matches
                        .value_of("limit-PTK")
                        .unwrap()
                        .parse()
                        .unwrap();
                    let n3 = cfg_parse_matches
                        .value_of("number-of-parses")
                        .unwrap()
                        .parse()
                        .unwrap();
                    let mut c2 = 0;
                    let mut c3 = 0;

                    for sentence in corpus.lines() {
                        for parse1 in c.recognise(
                            sentence
                                .split_whitespace()
                                .map(|x| x.to_string())
                                .collect(),
                        ).take(n1)
                        {
                            let s1 = coarse_to_fine::ctf_level_i(parse1.give_up().1, &nptk, &b);
                            for parse2 in s1 {
                                let s2 = coarse_to_fine::ctf_level_i(parse2.give_up().1, &nrlb, &a);
                                for parse3 in s2 {
                                    println!("{}", coarse_to_fine::Run::new(parse3.translate().1));
                                    c3 += 1;
                                    if c3 >= n3 {
                                        break;
                                    }
                                }
                                c2 += 1;
                                if c2 >= n2 || c3 >= n3 {
                                    break;
                                }
                            }
                            if c2 >= n2 || c3 >= n3 {
                                break;
                            }
                        }
                    }
                }
                ("automaton", Some(cfg_automaton_matches)) => {
                    let grammar_file_name = cfg_automaton_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: CFG<String, String, LogDomain<f64>> =
                        grammar_string.parse().unwrap();

                    let a = IntPushDownAutomaton::from(grammar);

                    let classes_file_name = cfg_automaton_matches.value_of("classes").unwrap();
                    let mut classes_file = File::open(classes_file_name).unwrap();
                    let mut classes_string = String::new();
                    let _ = classes_file.read_to_string(&mut classes_string);
                    let e: EquivalenceClass<String, String> = classes_string.parse().unwrap();

                    let rlb = RlbElement::new(e);

                    let (b, _) = a.approximation(&rlb).unwrap();

                    println!("Step 1 (relabel): \n\n{}", b);

                    let size = cfg_automaton_matches
                        .value_of("topk-size")
                        .unwrap()
                        .parse::<usize>()
                        .unwrap();

                    let ptk = PDTopKElement::new(size);

                    let (c, _) = b.approximation(&ptk).unwrap();

                    println!("Step 2 (restrict to size): \n\n{}", c);
                }
                _ => (),
            }
        }
        ("mcfg", Some(mcfg_matches)) => {
            match mcfg_matches.subcommand() {
                ("parse", Some(mcfg_parse_matches)) => {
                    let grammar_file_name = mcfg_parse_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: PMCFG<String, String, LogDomain<f64>> =
                        grammar_string.parse().unwrap();

                    let automaton = IntTreeStackAutomaton::from(grammar);

                    let tts = TTSElement::new();

                    let (a, ntts) = automaton.approximation(&tts).unwrap();

                    let classes_file_name = mcfg_parse_matches.value_of("classes").unwrap();
                    let mut classes_file = File::open(classes_file_name).unwrap();
                    let mut classes_string = String::new();
                    let _ = classes_file.read_to_string(&mut classes_string);
                    let e: equivalence_classes::EquivalenceClass<
                        String,
                        String,
                    > = classes_string.parse().unwrap();

                    let rlb = RlbElement::new(e);

                    let (b, nrlb) = a.approximation(&rlb).unwrap();

                    let size = mcfg_parse_matches
                        .value_of("topk-size")
                        .unwrap()
                        .parse::<usize>()
                        .unwrap();

                    let ptk = PDTopKElement::new(size);

                    let (c, nptk) = b.approximation(&ptk).unwrap();

                    let mut corpus = String::new();
                    let _ = io::stdin().read_to_string(&mut corpus);

                    let n1 = mcfg_parse_matches
                        .value_of("limit-TTS")
                        .unwrap()
                        .parse()
                        .unwrap();
                    let n2 = mcfg_parse_matches
                        .value_of("limit-RLB")
                        .unwrap()
                        .parse()
                        .unwrap();
                    let n3 = mcfg_parse_matches
                        .value_of("limit-PTK")
                        .unwrap()
                        .parse()
                        .unwrap();
                    let n4 = mcfg_parse_matches
                        .value_of("number-of-parses")
                        .unwrap()
                        .parse()
                        .unwrap();

                    for sentence in corpus.lines() {
                        let mut c2 = 0;
                        let mut c3 = 0;
                        let mut c4 = 0;
                        for parse1 in c.recognise(
                            sentence
                                .split_whitespace()
                                .map(|x| x.to_string())
                                .collect(),
                        ).take(n1)
                        {
                            let s1 = coarse_to_fine::ctf_level_i(parse1.give_up().1, &nptk, &b);
                            for parse2 in s1 {
                                let s2 = coarse_to_fine::ctf_level_i(parse2.give_up().1, &nrlb, &a);
                                for parse3 in s2 {
                                    let s3 = coarse_to_fine::ctf_level_i(
                                        parse3.give_up().1,
                                        &ntts,
                                        &automaton,
                                    );
                                    for parse4 in s3 {
                                        println!(
                                            "{}",
                                            coarse_to_fine::Run::new(parse4.translate().1)
                                        );
                                        c4 += 1;
                                        if c4 >= n4 {
                                            break;
                                        }
                                    }
                                    c3 += 1;
                                    if c4 >= n4 || c3 >= n3 {
                                        break;
                                    }
                                }
                                c2 += 1;
                                if c2 >= n2 || c3 >= n3 || c4 >= n4 {
                                    break;
                                }
                            }
                            if c2 >= n2 || c3 >= n3 || c4 >= n4 {
                                break;
                            }
                        }
                    }
                }
                ("automaton", Some(mcfg_automaton_matches)) => {
                    let grammar_file_name = mcfg_automaton_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: PMCFG<String, String, LogDomain<f64>> =
                        grammar_string.parse().unwrap();

                    let automaton = IntTreeStackAutomaton::from(grammar);

                    let tts = TTSElement::new();

                    let (a, _) = automaton.approximation(&tts).unwrap();

                    println!("Step 1 (transform to push-down): \n\n{}", a);

                    let classes_file_name = mcfg_automaton_matches.value_of("classes").unwrap();
                    let mut classes_file = File::open(classes_file_name).unwrap();
                    let mut classes_string = String::new();
                    let _ = classes_file.read_to_string(&mut classes_string);
                    let e: equivalence_classes::EquivalenceClass<
                        String,
                        String,
                    > = classes_string.parse().unwrap();

                    let rlb = RlbElement::new(e);

                    let (b, _) = a.approximation(&rlb).unwrap();

                    println!("Step 2 (relabel): \n\n{}", b);

                    let size = mcfg_automaton_matches
                        .value_of("topk-size")
                        .unwrap()
                        .parse::<usize>()
                        .unwrap();

                    let ptk = PDTopKElement::new(size);


                    let (c, _) = b.approximation(&ptk).unwrap();

                    println!("Step 3 (restrict to size): \n\n{}", c);
                }
                _ => (),
            }
        }
        ("benchmark", Some(benchmark_matches)) => {
            let grammar_file_name = benchmark_matches.value_of("grammar").unwrap();
            let mut grammar_file = File::open(grammar_file_name).unwrap();
            let mut grammar_string = String::new();
            let _ = grammar_file.read_to_string(&mut grammar_string);
            let grammar: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();

            let classes_file_name = benchmark_matches.value_of("classes").unwrap();
            let mut classes_file = File::open(classes_file_name).unwrap();
            let mut classes_string = String::new();
            let _ = classes_file.read_to_string(&mut classes_string);
            let classes: equivalence_classes::EquivalenceClass<String, String> =
                classes_string.parse().unwrap();

            let size = benchmark_matches
                .value_of("topk-size")
                .unwrap()
                .parse::<usize>()
                .unwrap();

            let limit = benchmark_matches
                .value_of("number-of-parses")
                .unwrap()
                .parse()
                .unwrap();
            let limit1 = benchmark_matches
                .value_of("limit-TTS")
                .unwrap()
                .parse()
                .unwrap();
            let limit2 = benchmark_matches
                .value_of("limit-RLB")
                .unwrap()
                .parse()
                .unwrap();
            let limit3 = benchmark_matches
                .value_of("limit-PTK")
                .unwrap()
                .parse()
                .unwrap();
            let check = benchmark_matches
                .value_of("number-words")
                .unwrap()
                .parse()
                .unwrap();
            let nfa: bool = benchmark_matches.value_of("nfa").unwrap().parse().unwrap();

            let corpus_file_name = benchmark_matches.value_of("words").unwrap();
            let mut corpus_file = File::open(corpus_file_name).unwrap();
            let mut corpus = String::new();
            let _ = corpus_file.read_to_string(&mut corpus);

            benchmark::benchmark(
                grammar,
                classes,
                size,
                limit,
                limit1,
                limit2,
                limit3,
                &corpus,
                check,
                nfa,
            )
        }
        _ => (),
    }
}
