use std::{ffi::OsString, fs, path::PathBuf};

use please::Engine;

#[test]
fn suite() {
    let root = concat!(env!("CARGO_MANIFEST_DIR"), "/tests");

    let mut engine = Engine::new();

    let mut paths = Vec::new();

    let mut todo = vec![PathBuf::from(root)];
    while let Some(path) = todo.pop() {
        if path.is_dir() {
            for entry in fs::read_dir(path).unwrap() {
                let entry = entry.unwrap();
                let path = entry.path();
                todo.push(path)
            }
        } else if path.extension() == Some(&OsString::from("please")) {
            paths.push(path);
        }
    }

    paths.sort_unstable();

    for path in paths {
        // Run the callback
        let name = path.to_string_lossy().into_owned();
        let source = fs::read_to_string(&name).unwrap();
        let res = engine.read_eval(&source, name.to_owned());
        match res {
            Ok(Err(ono)) => {
                let ono = ono.into_expr(&mut engine);
                panic!("{}", engine.write_expr(ono).unwrap());
            }
            Ok(Ok(_)) => {}
            Err(e) => {
                e.report()
                    .eprint(ariadne::sources(std::iter::once((name, source))))
                    .unwrap();
                panic!();
            }
        }
    }
}
