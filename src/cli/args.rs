use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "patql")]
pub struct Opt {
    #[structopt(name = "FILE")]
    pub files: Vec<String>,
}
