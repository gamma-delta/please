use gc::Gc;
use ruth::{Engine, Expr, ExprParseErrorInfo};
use termwiz::lineedit::{line_editor_terminal, LineEditor, LineEditorHost, NopLineEditorHost};

pub fn repl(mut engine: Engine) -> termwiz::Result<()> {
    let mut in_parens = false;

    let ps1 = engine.intern_symbol("ps1");
    let ps2 = engine.intern_symbol("ps2");

    let mut terminal = line_editor_terminal()?;
    let mut editor = LineEditor::new(&mut terminal);
    let mut host = NopLineEditorHost::default();

    let mut input = String::new();

    loop {
        let ps = if in_parens { ps2 } else { ps1 };
        let ps = engine.eval(engine.thtdlib(), Gc::new(Expr::Symbol(ps)));
        let ps = engine.print_expr(ps);
        editor.set_prompt(&ps);

        let line = editor.read_line(&mut host)?.unwrap_or_default();
        input.push_str(&line);
        host.history().add(&line);

        // we try to be done
        let res = engine.read_one(&input, "<repl>".to_owned());
        let expr = match res {
            Ok(it) => Ok(it),
            Err(ohno) => {
                match &ohno.source() {
                    // we might supply the closer later; do nothing
                    ExprParseErrorInfo::ExpectedCloseParen { .. }
                    | ExprParseErrorInfo::ExpectedCloseQuote
                    | ExprParseErrorInfo::ExpectedCloseBlockComment => {
                        in_parens = true;
                        continue;
                    }
                    ExprParseErrorInfo::Eof => {
                        continue;
                    }
                    _ => Err(ohno),
                }
            }
        };
        in_parens = false;

        match expr {
            Ok(expr) => {
                let result = engine.eval(engine.thtdlib(), Gc::new(expr));
                println!("{}\n", engine.write_expr(result));
            }
            Err(ono) => {
                ono.report()
                    .eprint(ariadne::sources(std::iter::once((
                        "<repl>".to_owned(),
                        input.to_owned(),
                    ))))
                    .unwrap();
            }
        }

        input.clear();
    }
}
