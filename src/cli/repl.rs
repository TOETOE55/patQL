use crate::cli::args::Opt;
use crate::cli::preload::pre;
use pat_ql::ast::Decl;
use pat_ql::evaluation::Driver;
use pat_ql::parse::grammar::decl::decls;
use pat_ql::parse::grammar::term_phase::term;
use psc::{ParseState, Parser};
use std::convert::identity;
use std::io::{stdin, stdout, BufRead, Read, Write};
use std::str;
use std::{fs, io};
use structopt::StructOpt;

pub fn app() {
    let opt: Opt = Opt::from_args();
    let decls = opt
        .files
        .iter()
        .map(String::as_str)
        .flat_map(parse_file)
        .flatten()
        .chain(pre())
        .collect::<Vec<Decl>>();
    //
    //    for decl in decls.iter() {
    //        println!("{}", decl);
    //    }

    repl(decls)
}

fn repl(pre: Vec<Decl>) {
    let drive = Driver::new(pre);
    let read = stdin();
    let mut write = stdout();
    loop {
        write.write(">".as_bytes()).unwrap();
        write.flush().unwrap();
        let mut line = String::new();
        read.read_line(&mut line).unwrap();

        let mut state = ParseState::new(&line);
        let term = term().parse(&mut state);
        let term = match term {
            Ok(t) => t,
            Err(err) => {
                println!("{:?}", err);
                continue;
            }
        };
        write.write("press enter:".as_bytes()).unwrap();
        write.flush().unwrap();
        let iter = drive.query(&term);
        for (_, result) in read.lock().lines().zip(iter) {
            print!("{}", result);
            write.flush().unwrap();
        }
        println!("finished");
    }
}

fn read_file_impl(file_arg: &str) -> io::Result<String> {
    let mut content = String::new();
    let mut file = fs::File::open(file_arg)?;
    file.read_to_string(&mut content)?;
    Ok(content)
}

fn read_file(file_arg: &str) -> Option<String> {
    read_file_impl(file_arg)
        .map_err(|io_err| eprintln!("Cannot read `{}`: {}", file_arg, io_err))
        .ok()
}

fn parse_file(file_arg: &str) -> Option<Vec<Decl>> {
    let file_content = read_file(file_arg)?;
    let mut state = ParseState::new(&file_content);
    let result = decls().parse(&mut state);
    match result {
        Ok(decls) => Some(decls),
        Err(err) => {
            println!("{:?}", err);
            None
        }
    }
}
