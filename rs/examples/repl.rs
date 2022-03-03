use callback_rs::dynamic::Typeable;
use callback_rs::ffi;
use callback_rs::list::HsList;
use callback_rs::session;
use color_eyre::eyre::Result;

use std::path::PathBuf;
use structopt::StructOpt;

/// A basic example
#[derive(StructOpt, Debug)]
#[structopt(name = "basic")]
struct Opt {
    libdir: Option<PathBuf>,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    unsafe {
        ffi::potatoInit();
    }

    let opt = Opt::from_args();

    let session = session::Session::new(opt.libdir.as_ref());

    session.import_modules(&["Prelude"]);
    session.import_modules(&["Prelude", "Data.Word"]);
    let dynamic = session.run_expr_dyn("head [4,1,2,3] :: Word64")?;
    eprintln!("the value is {:?}", u64::from_dynamic(&dynamic));

    let config = rustyline::Config::builder()
        .history_ignore_space(true)
        .edit_mode(rustyline::EditMode::Emacs)
        .build();
    let mut rl = rustyline::Editor::<()>::with_config(config);
    let xdg_dirs = xdg::BaseDirectories::with_prefix("rhc")?;
    let hist_path = xdg_dirs
        .find_data_files("history.txt")
        .next()
        .unwrap_or_else(|| xdg_dirs.place_data_file("history.txt").unwrap());
    if rl.load_history(&hist_path).is_err() {
        eprintln!("No previous history.");
    }
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if !line.starts_with(':') {
                    if let Err(err) = session.print_expr(&line) {
                        eprintln!("{}", err);
                    }
                    continue;
                }
                let name = match line[1..].split_whitespace().next() {
                    Some(name) => name,
                    None => {
                        eprintln!("NO COMMAND!");
                        continue;
                    }
                };
                let expr = line.get(name.len() + 2..).unwrap_or("").trim();
                match name {
                    "include" => {
                        let includes: Vec<&str> = expr.split(':').collect();
                        if !session.set_import_paths(&includes) {
                            eprintln!("imports FAILED!");
                        }
                    }
                    "load" => {
                        let loads: Vec<&str> = expr.split(':').collect();
                        if !session.set_load_paths(&loads) {
                            eprintln!("LOAD FAILED!");
                        }
                    }
                    "import" => {
                        let imports: Vec<&str> = expr.split(':').collect();
                        session.import_modules(&imports);
                    }
                    "decl" => {
                        if let Err(err) = session.run_decl(expr) {
                            eprintln!("{}", err);
                        }
                    }
                    cmd => eprintln!("unknown command '{}'", cmd),
                }
            }
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("{}", err);
            }
        }
    }
    rl.save_history(&hist_path)?;
    Ok(())
}
