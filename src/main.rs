extern crate clap;
extern crate integeriser;
extern crate log_domain;
#[macro_use]
extern crate nom;
extern crate num_traits;
extern crate time;
extern crate rand;
extern crate openfsa;
#[macro_use]
extern crate serde_derive;
extern crate serde;


mod approximation;
mod automata;
mod cfg;
mod coarse_to_fine;
mod integerise;
mod nfa;
mod dyck;
mod cs_representation;
mod pmcfg;
mod push_down_automaton;
mod tree_stack_automaton;
mod util;

#[cfg(test)]
mod tests;

use clap::App;

pub use approximation::*;
pub use automata::*;
pub use cfg::*;
pub use coarse_to_fine::*;
pub use pmcfg::*;
pub use push_down_automaton::*;
pub use tree_stack_automaton::*;

fn main() {
    let matches
        = App::new("rustomata")
        .version("0.1")
        .author("Tobias Denkinger <tobias.denkinger@tu-dresden.de>")
        .about("Framework for (weighted) automata with storage")
        .subcommand(pmcfg::cli::get_sub_command())
        .subcommand(cfg::cli::get_sub_command())
        .subcommand(tree_stack_automaton::cli::get_sub_command())
        .subcommand(coarse_to_fine::cli::get_sub_command())
        .subcommand(approximation::cli::get_sub_command())
        .subcommand(cs_representation::cli::get_sub_command("cs"))
        .get_matches();

    match matches.subcommand() {
        ("mcfg", Some(mcfg_matches)) =>
            pmcfg::cli::handle_sub_matches(mcfg_matches),
        ("cfg", Some(cfg_matches)) =>
            cfg::cli::handle_sub_matches(cfg_matches),
        ("coarse-to-fine", Some(ctf_matches)) =>
            coarse_to_fine::cli::handle_sub_matches(ctf_matches),
        ("tsa", Some(tsa_matches)) =>
            tree_stack_automaton::cli::handle_sub_matches(tsa_matches),
        ("approximation", Some(r_matches)) =>
            approximation::cli::handle_sub_matches(r_matches),
        ("cs", Some(cs_matches)) =>
            cs_representation::cli::handle_sub_matches(cs_matches),
        _ => (),
    }

}
