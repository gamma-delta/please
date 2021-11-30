# PLEASE

> Petra Left Everything Advanced to Someone Else

This is a pretty garbage lisp I wrote.

## Times I Messed Up

- The `c[ad]{2,}r` family is backwards. `(cadr (cons (cons 'a 'b) (cons 'c 'd)))` is `'b`, not `'c`.
- Macros are literally just functions with a special flag telling the interpreter to not eval their bodies.
  (To be fair, this is how I was explained what a macro is by Alwinfy)
- String processing is a nightmare

## Special Thanks To

Alwinfy, the "someone else" in PLEASE. They implemented tail recursion and lazy lists.
