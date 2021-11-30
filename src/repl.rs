use super::{Engine, Expr, ExprParseErrorInfo};
use gc::Gc;
use termwiz::lineedit::{line_editor_terminal, LineEditor, LineEditorHost, NopLineEditorHost};

impl Engine {
    pub fn repl(&mut self) -> termwiz::Result<()> {
        let mut in_parens = false;

        let ps1 = self.intern_symbol("ps1");
        let ps2 = self.intern_symbol("ps2");

        let mut terminal = line_editor_terminal()?;
        let mut editor = LineEditor::new(&mut terminal);
        let mut host = NopLineEditorHost::default();

        let mut input = String::new();

        loop {
            let ps = if in_parens { ps2 } else { ps1 };
            let ps = self.eval(self.thtdlib(), Gc::new(Expr::Symbol(ps)));
            let ps = self.print_expr(ps).unwrap();
            editor.set_prompt(&ps);

            let line = editor.read_line(&mut host)?.unwrap_or_default();
            input.push_str(&line);
            input.push('\n');
            host.history().add(&line);

            // we try to be done
            let res = self.read_one(&input, "<repl>".to_owned());
            let expr = match res {
                Ok(it) => Ok(it),
                Err(ohno) => {
                    match &ohno.source() {
                        // we might supply the closer later; do nothing
                        ExprParseErrorInfo::ExpectedCloseParen { .. }
                        | ExprParseErrorInfo::ExpectedCloseQuote
                        | ExprParseErrorInfo::ExpectedCloseBlockComment
                        | ExprParseErrorInfo::HerestringNoEnding(..)
                        | ExprParseErrorInfo::HerestringNoContents => {
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
                    let result = self.eval(self.thtdlib(), Gc::new(expr));
                    let string = match self.write_expr(result) {
                        Ok(s) => s,
                        Err(e) => {
                            let expr = e.into_expr(self);
                            self.write_expr(expr).unwrap()
                        }
                    };
                    println!("{}\n", string);
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
}
