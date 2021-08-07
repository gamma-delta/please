use std::io::{self, Write};

use ariadne::Source;
use gc::Gc;
use ruth::{Engine, Expr, ExprParseErrorInfo};

fn main() {
    let mut engine = Engine::new();

    let mut in_parens = false;

    let ps1 = engine.intern_symbol("ps1");
    let ps2 = engine.intern_symbol("ps2");

    let mut input = String::new();

    loop {
        let ps = if in_parens { ps2 } else { ps1 };
        let ps = engine.eval(engine.thtdlib(), Gc::new(Expr::Symbol(ps)));
        let ps = match &*ps {
            Expr::String(s) => s.as_str(),
            _ => "",
        };
        print!("{}", ps);
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut input).unwrap();
        // we try to be done
        let res = engine.read_one(&input);
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
                ono.report().eprint(Source::from(&input)).unwrap();
            }
        }

        input.clear();
    }
}
