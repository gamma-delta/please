use std::io::{self, Write};

use ariadne::Source;
use ruth::{Engine, Expr, ExprParseErrorInfo, Namespace};

fn main() {
    let mut engine = Engine::new();
    let mut namespace = Namespace::new();

    let mut in_parens = false;

    let ps1 = engine.get_symbol_index("ps1");
    let ps2 = engine.get_symbol_index("ps2");

    let mut input = String::new();

    loop {
        let ps = if in_parens { ps2 } else { ps1 };
        let ps = engine.eval(&mut namespace, ps);
        let ps = match engine.get(ps) {
            Expr::String(s) => s.as_str(),
            _ => "",
        };
        print!("{}", ps);
        io::stdout().flush().unwrap();
        in_parens = false;

        io::stdin().read_line(&mut input).unwrap();
        // we try to be done
        let res = engine.read_source(&input);
        let expr = match res {
            Ok(it) => Ok(it),
            Err(ohno) => {
                match &ohno.source() {
                    // we might supply the closer later; do nothing
                    ExprParseErrorInfo::ExpectedCloseParen { .. }
                    | ExprParseErrorInfo::ExpectedCloseQuote => {
                        in_parens = true;
                        continue;
                    }
                    _ => Err(ohno),
                }
            }
        };

        match expr {
            Ok(expr) => {
                let idx = engine.insert(expr);
                let result = engine.eval(&mut namespace, idx);
                println!("{}\n", engine.print_expr(result));
            }
            Err(ono) => {
                ono.report().eprint(Source::from(&input)).unwrap();
            }
        }

        input.clear();
    }
}
