use std::fs;

use ruth::Engine;

mod repl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = pico_args::Arguments::from_env();

    let root = std::env::current_dir()?;

    let mut engine = Engine::new();

    let do_repl = args.contains("--repl");
    let mut had_any_files = false;
    while let Some(path_stub) = args.opt_free_from_str::<String>()? {
        let path = root.join(path_stub);
        let source = fs::read_to_string(&path)?;

        if let Err(ono) = engine.read_eval(&source, path.to_string_lossy().into_owned()) {
            ono.report().eprint(ariadne::sources(std::iter::once((
                path.to_string_lossy().into_owned(),
                &source,
            ))))?;
            break;
        }

        had_any_files = true;
    }

    if do_repl || !had_any_files {
        repl::repl(engine)?;
    }

    Ok(())
}
