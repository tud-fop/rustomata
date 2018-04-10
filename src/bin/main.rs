extern crate clap;
extern crate log_domain;
extern crate flate2;

#[macro_use]
extern crate rustomata;

use clap::App;

mod approximation;
mod cfg;
mod pmcfg;
mod tree_stack_automata;
mod csparsing;

fn main() {
    let matches = App::new("rustomata")
        .version("0.1")
        .author("Tobias Denkinger <tobias.denkinger@tu-dresden.de>")
        .about("Framework for (weighted) automata with storage")
        .subcommand(pmcfg::get_sub_command())
        .subcommand(cfg::get_sub_command())
        .subcommand(tree_stack_automata::get_sub_command())
        .subcommand(approximation::get_sub_command())
        .subcommand(csparsing::get_sub_command("csparsing"))
        .get_matches();

    match matches.subcommand() {
        ("mcfg", Some(mcfg_matches)) => pmcfg::handle_sub_matches(mcfg_matches),
        ("cfg", Some(cfg_matches)) => cfg::handle_sub_matches(cfg_matches),
        ("tsa", Some(tsa_matches)) => tree_stack_automata::handle_sub_matches(tsa_matches),
        ("approximation", Some(r_matches)) => approximation::handle_sub_matches(r_matches),
        ("csparsing", Some(r_matches)) => csparsing::handle_sub_matches(r_matches),
        _ => (),
    }

}
