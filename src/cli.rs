use structopt::StructOpt;

use saunf::web;

#[derive(StructOpt, Debug)]
pub enum WebCmd {
    /// Run development server
    Dev,
}

#[derive(StructOpt, Debug)]
pub enum CliCmd {
    /// Manage saunf web apps
    Web(WebCmd),
}

#[derive(StructOpt, Debug)]
#[structopt(name = "saunf", about = "Personal Project Management")]
pub struct CliOpts {
    #[structopt(short, global = true, parse(from_occurrences))]
    pub verbosity: i32,

    #[structopt(subcommand)]
    pub cmd: CliCmd,
}

pub async fn run(cmd: CliCmd) -> anyhow::Result<()> {
    match cmd {
        CliCmd::Web(web_cmd) => match web_cmd {
            WebCmd::Dev => web::run().await,
        },
    };

    Ok(())
}
