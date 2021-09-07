use std::{fs, thread};

use ruth::Engine;

mod repl;

fn main() -> anyhow::Result<()> {
    thread::Builder::new()
        .stack_size(32 * 1024 * 1024)
        .spawn(|| -> anyhow::Result<()> {
            let mut args = pico_args::Arguments::from_env();

            let root = std::env::current_dir()?;

            let mut engine = Engine::new();

            let do_repl = args.contains("--repl");
            let mut had_any_files = false;
            while let Some(path_stub) = args.opt_free_from_str::<String>()? {
                had_any_files = true;

                let path = root.join(path_stub);
                let source = fs::read_to_string(&path)?;

                match engine.read_eval(&source, path.to_string_lossy().into_owned()) {
                    Err(ono) => {
                        ono.report().eprint(ariadne::sources(std::iter::once((
                            path.to_string_lossy().into_owned(),
                            &source,
                        ))))?;
                        break;
                    }
                    Ok(Err(err)) => {
                        let expr = err.into_expr(&mut engine);
                        eprintln!("{}", engine.write_expr(expr).unwrap());
                        break;
                    }
                    Ok(Ok(_)) => {}
                }
            }

            if do_repl || !had_any_files {
                repl::repl(engine)?;
            }

            Ok(())
        })?
        // Unwrap the first one because if we panic inside we panic outside
        .join()
        .unwrap()?;
    Ok(())
}
