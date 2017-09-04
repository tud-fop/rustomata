use time::PreciseTime;
use std::io::prelude::*;
use std::fs::File;

pub use tree_stack::*;
pub use automata::*;
pub use pmcfg::*;
pub use util::*;
use util::equivalence_classes::*;
pub use util::ctf::*;

pub use push_down::*;
pub use cfg::*;
pub use approximation::*;
pub use integerise::*;

//Test a multitude of combinations for coarse-to-fine parsing and takes their times. Results in extra file
pub fn benchmark(grammar_string: String, classes_string: String, ptk_size: usize, limit: usize, limit1: usize, limit2: usize, limit3: usize, corpus: String){
    //File that contains the results
    let mut f = File::create("benchmark-results.txt").unwrap();
    write!(&mut f, "Benchmarking results \n\n");
    let w = 30;

    //Create initial PMCFG
    println!("Start Initialisation");
    let grammar_start = PreciseTime::now();
    let grammar: PMCFG<String, String, log_prob::LogProb> = grammar_string.parse().unwrap();
    let grammar_end = PreciseTime::now();

    //create initial EquivalenceClass
    let eq_start = PreciseTime::now();
    let eq: EquivalenceClass<String, String> = classes_string.parse().unwrap();
    let eq_end = PreciseTime::now();

    //creates the initial approximation strategies
    let ap_start = PreciseTime::now();
    let tts = TTSElement::new();
    let ap_1 = PreciseTime::now();
    let rlb = RlbElement::new(eq);
    let ap_2 = PreciseTime::now();
    let ptk = PDTopKElement::new(ptk_size);
    let ap_end = PreciseTime::now();

    //creates all automata that are to be used
    println!("Automaton");
    let at_start = PreciseTime::now();
    let automaton = IntTreeStackAutomaton::from(grammar);
    let at_1 = PreciseTime::now();
    println!("TTS");
    let (app1, ntts) = automaton.approximation(&tts).unwrap();
    let at_2 = PreciseTime::now();
    println!("RLB");
    let (app2, nrlb) = app1.approximation(&rlb).unwrap();
    let at_3 = PreciseTime::now();
    println!("PTK");
    let (app3, nptk) = app2.approximation(&ptk).unwrap();
    let at_end = PreciseTime::now();

    //save times for initial startup
    write!(&mut f, "Construction grammar: {}\nConstruction equivalence-class: {}\n\nConstruction TTS: {}\nConstruction RLB: {}\nConstruction PTK: {}\n\nGeneration Automata: {}\nApproximation TTS: {}\nApproximation RLB: {}\nApproximation PTK: {}\n\nRecognition times:\n", grammar_start.to(grammar_end),
                        eq_start.to(eq_end),
                        ap_start.to(ap_1),
                        ap_1.to(ap_2),
                        ap_2.to(ap_end),
                        at_start.to(at_1),
                        at_1.to(at_2),
                        at_2.to(at_3),
                        at_3.to(at_end)
                    );
    write!(&mut f, "\n{0: <width$} | {1: <width$} | {2: <width$} | {3: <width$} | {4: <width$} \n",
    "Word", "3-Layers", "2-Layers", "1-Layer", "Normal", width = w);

    println!("Start Test");
    //tests different combinations and takes individual times
    for sentence in corpus.lines() {

        //creates the word to be recognised
        println!("{}:\n", sentence);
        let sentence2 = sentence.clone();
        let word = sentence.split_whitespace().map(|x| x.to_string()).collect();
        println!("{:?}", word);

        //No approximation
        println!("no Approximation");
        let p4_start = PreciseTime::now();
        for parse in automaton.recognise(sentence2.split_whitespace().map(|x| x.to_string()).collect()).take(limit) {
            println!("{}", Run::new(parse.translate().1));
        }
        let p4_end = PreciseTime::now();

        println!("1-Layer");
        //TTS
        let p3_start = PreciseTime::now();
        let mut c = 0;
        for parse3 in app1.recognise(sentence2.split_whitespace().map(|x| x.to_string()).collect()).take(limit1) {
            let s3 = ctf_level_i(&word, parse3.give_up().1, &ntts, &automaton);
            for parse4 in s3{
                println!("{}", Run::new(parse4.translate().1));
                c=c+1;
                if c>=limit{
                    break
                }
            }
            if c>=limit{
                break;
            }
        }
        let p3_end = PreciseTime::now();

        println!("2-Layers");
        //TTS -> RLB
        let p2_start = PreciseTime::now();
        let mut c = 0;
        let mut c1 = 0;
        for parse2 in app2.recognise(sentence2.split_whitespace().map(|x| x.to_string()).collect()).take(limit2) {
            let s2 = ctf_level_i(&word, parse2.give_up().1, &nrlb, &app1);
            for parse3 in s2{
                let s3 = ctf_level_i(&word, parse3.give_up().1, &ntts, &automaton);
                for parse4 in s3{
                    println!("{}", Run::new(parse4.translate().1));
                    c=c+1;
                    if c>=limit{
                        break
                    }
                }
                c1=c1+1;
                if c>=limit||c1>=limit1{
                    break;
                }
            }
            if c1>=limit1||c>=limit{
                break;
            }
        }
        let p2_end = PreciseTime::now();

        println!("3-Layers");
        //TTS -> RLB -> PTK
        let p1_start = PreciseTime::now();
        let mut c = 0;
        let mut c1 = 0;
        let mut c2 = 0;
        for parse1 in app3.recognise(sentence2.split_whitespace().map(|x| x.to_string()).collect()).take(limit3) {
            let s1 = ctf_level_i(&word, parse1.give_up().1, &nptk, &app2);
            for parse2 in s1{
                let s2 = ctf_level_i(&word, parse2.give_up().1, &nrlb, &app1);
                for parse3 in s2{
                    let s3 = ctf_level_i(&word, parse3.give_up().1, &ntts, &automaton);
                    for parse4 in s3{
                        println!("{}", Run::new(parse4.translate().1));
                        c=c+1;
                        if c>=limit{
                            break
                        }
                    }
                    c1=c1+1;
                    if c>=limit||c1>=limit1{
                        break;
                    }
                }
                c2=c2+1;
                if c2>=limit2||c1>=limit1||c>=limit{
                    break;
                }
            }
            if c2>=limit2||c1>=limit1||c>=limit{
                break;
            }
        }
        let p1_end = PreciseTime::now();

        //save results and times for this sentence
        write!(&mut f, "\n{0: <width$} | {1: <width$} | {2: <width$} | {3: <width$} | {4: <width$} \n",
        sentence, p1_start.to(p1_end), p2_start.to(p2_end), p3_start.to(p3_end), p4_start.to(p4_end), width = w);


    }
}
