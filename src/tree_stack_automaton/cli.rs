use clap::{Arg, ArgMatches, App, SubCommand};
use log_domain::LogDomain;
use recognisable::{Item, Recognisable};
use std::fmt::Debug;
use tree_stack_automaton::{TreeStackAutomaton, TreeStack, TreeStackInstruction};

use std::io::{self, Read};
use std::fs::File;

pub fn get_sub_command() -> App<'static, 'static> {
    SubCommand::with_name("tsa")
        .about("functions related to tree-stack automata")
        .subcommand(
            SubCommand::with_name("recognise")
                .about("recognises from stdin with a tree-stack automaton")
                .arg(
                    Arg::with_name("automaton")
                        .help("automaton file to use")
                        .index(1)
                        .required(true),
                )
                .arg(
                    Arg::with_name("beam-width")
                        .help("maximum number of frontier nodes in the search space")
                        .short("b")
                        .long("beam")
                        .value_name("beam-width")
                        .required(false),
                )
                .arg(
                    Arg::with_name("number-of-runs")
                        .help("number of runs that should be returned")
                        .short("n")
                        .long("number")
                        .default_value("1")
                        .required(false),
                )
                .arg(
                    Arg::with_name("coarse-to-fine")
                        .help("use coarse-to-parsing")
                        .short("ctf")
                        .long("coarse-to-fine")
                        .value_name("strategies")
                        .default_value("")
                        .required(false),
                ),
        )
}

pub fn handle_sub_matches(tsa_matches: &ArgMatches) {
    match tsa_matches.subcommand() {
        ("recognise", Some(tsa_recognise_matches)) => {
            let automaton_file_name = tsa_recognise_matches.value_of("automaton").unwrap();
            let mut automaton_file = File::open(automaton_file_name).unwrap();
            let mut automaton_string = String::new();
            let _ = automaton_file.read_to_string(&mut automaton_string);
            let automaton: TreeStackAutomaton<String, String, LogDomain<f64>> =
                automaton_string.parse().unwrap();

            let mut corpus_raw = String::new();
            let _ = io::stdin().read_to_string(&mut corpus_raw);
            let corpus =
                corpus_raw
                .lines()
                .map(|s| s.split_whitespace().map(|x| x.to_string()).collect())
                .collect();

            let n = tsa_recognise_matches
                .value_of("number-of-runs")
                .unwrap()
                .parse()
                .unwrap();

            let beam =
                match tsa_recognise_matches.value_of("beam-width") {
                    Some(b) => Some(b.parse().unwrap()),
                    None => None,
                };

            match tsa_recognise_matches.value_of("strategies") {
                Some("tts") => {
                    use approximation::ApproximationStrategy;
                    use approximation::tts::TTSElement;
                    use recognisable::coarse_to_fine::CoarseToFineRecogniser;
                    use std::rc::Rc;

                    let rec = coarse_to_fine_recogniser!(automaton; TTSElement::new());

                    recognise_corpus(rec, n, beam, corpus)
                },
                Some(e) => panic!("[ERR] Strategy \"{}\" unknown.", e),
                None => recognise_corpus(automaton, n, beam, corpus)
            }
        }
        _ => (),
    }
}

fn recognise_corpus<A, Rec, T, W>(rec: Rec, n: usize, beam: Option<usize>, corpus: Vec<Vec<T>>)
    where Rec: Recognisable<T, W, Parse = Item<TreeStack<A>, TreeStackInstruction<A>, T, W>>,
          A: Debug,
          W: Debug,
          T: Debug,
{
    for sentence in corpus {
        println!("{:?}:", sentence);
        match beam {
            Some(b) =>
                for run in rec.recognise_beam_search(b, sentence).take(n) {
                    println!("  {:?}", run.1);
                },
            None =>
                for run in rec.recognise(sentence).take(n) {
                    println!("  {:?}", run.1)
                }
        }
        println!();
    }
}
