use std::collections::HashMap;

use gc::Gc;

use crate::{Engine, Exception, Expr, Symbol};

/// Why this doesn't exist in the stdlib I'll never know.
macro_rules! hashmap {
    ($($key:expr => $val:expr),* $(,)*) => {{
        #[allow(unused_mut)]
        let mut map = HashMap::new();
        $(
            map.insert($key, $val);
        )*
        map
    }};
}

impl Engine {
    /// Given an argument spec and values to fill it, return a shiny new namespace for the arguments.
    pub fn destructure_assign(
        &mut self,
        spec: Gc<Expr>,
        val: Gc<Expr>,
    ) -> Result<HashMap<Symbol, Gc<Expr>>, Exception> {
        let underscore = self.intern_symbol("_");
        let default_sym = self.intern_symbol("default");

        println!(
            "{} <- {}",
            self.print_expr(spec.to_owned()).unwrap(),
            self.print_expr(val.to_owned()).unwrap()
        );

        // (default spec otherwise) tries to match to the spec, and if not fills it with the default and tries again with the next.
        // this is some Good Code :tm:
        if let Expr::Pair(car, cdr) = &*spec {
            if let Some(list) = self.sexp_to_list(car.to_owned())? {
                match list.as_slice() {
                    [default_checker, ..] if **default_checker == Expr::Symbol(default_sym) => {
                        return if let [_, spec, default] = list.as_slice() {
                            match self.destructure_assign(spec.to_owned(), val.to_owned()) {
                                Ok(it) => {
                                    // No need for the default argument
                                    Ok(it)
                                }
                                Err(_) => {
                                    // Something went wrong; try to use the default argument...
                                    let mut defaulted = self
                                        .destructure_assign(spec.to_owned(), default.to_owned())?;
                                    // ... and pass the troublemaker value off to the next spec
                                    defaulted.extend(self.destructure_assign(cdr.to_owned(), val)?);
                                    Ok(defaulted)
                                }
                            }
                        } else {
                            Err(self.make_err(
                                "assignment/default",
                                "default must be of the form ('default spec default)".to_string(),
                                Some(spec),
                            ))
                        };
                    }
                    _ => {}
                }
            }
        }

        match (&*spec, &*val) {
            // ignore _
            (Expr::Symbol(sym), _) if *sym == underscore => Ok(HashMap::new()),
            // plain ol variable binding
            (Expr::Symbol(sym), _) => Ok(hashmap! {*sym => val}),

            (Expr::Pair(spec_car, spec_cdr), Expr::Pair(val_car, val_cdr)) => {
                let mut out = HashMap::new();

                out.extend(self.destructure_assign(spec_car.to_owned(), val_car.to_owned())?);
                out.extend(self.destructure_assign(spec_cdr.to_owned(), val_cdr.to_owned())?);

                Ok(out)
            }
            (Expr::Map(spec_map), Expr::Map(val_map)) => {
                let mut out = HashMap::with_capacity(spec_map.capacity());

                for (spec_k, spec_v) in spec_map.iter() {
                    if let Some(val_v) = val_map.get(spec_k) {
                        out.extend(self.destructure_assign(spec_v.to_owned(), val_v.to_owned())?);
                    } else {
                        let sk = self.print_expr(spec_k.to_owned())?;
                        return Err(self.make_err(
                            "assignment/map/missing-key",
                            format!("the key {} was not present in the value map", sk),
                            Some(spec_k.to_owned()),
                        ));
                    }
                }

                Ok(out)
            }

            _ if spec == val => {
                // Well, it matches ... just return an empty namespace
                // This is probably not super helpful for normal assignment but might be cool for match or if-let
                Ok(HashMap::new())
            }
            _ => {
                let s = self.print_expr(spec.to_owned())?;
                let v = self.print_expr(val.to_owned())?;
                Err(self.make_err(
                    "assignment/invalid",
                    format!("cannot bind {} to {}", v, s,),
                    Some(Engine::list_to_sexp(&[spec, val])),
                ))
            }
        }
    }
}
