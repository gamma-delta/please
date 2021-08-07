use std::{fmt::Debug, num::ParseIntError};

use ariadne::{CharSet, Config, Label, Report, ReportKind, Source};
use gc::Gc;
use thiserror::Error;

use crate::{Engine, Expr};

/// Error when lexing or parsing an expression
#[derive(Error)]
#[error("{source:}")]
pub struct ExprParseError {
    source: ExprParseErrorInfo,
    report: Report,
}

impl ExprParseError {
    fn new<'a>(s: &'a str, err: ExprParseErrorLimited<'a>) -> ExprParseError {
        let (start, end) = string_pos(s, err.offender);

        let mut report = Report::build(ReportKind::Error, (), start)
            .with_config(ariadne::Config::default().with_char_set(CharSet::Ascii))
            .with_message(err.data.to_string());

        match &err.data {
            ExprParseErrorInfo::ParseInt {
                radix,
                radix_prefix,
                source,
            } => {
                report = report
                    .with_label(Label::new(start..end).with_message(source.to_string()))
                    .with_note(if let Some(prefix) = radix_prefix {
                        format!("the prefix {} indicates a base-{} literal", prefix, radix)
                    } else {
                        "no prefix indicates a base-10 literal".to_string()
                    });
            }
            &ExprParseErrorInfo::BadIntRadix(radix) => {
                report = report
                    .with_label(Label::new(start..end).with_message(format!("this {:?} is not valid", radix)))
                    .with_note("the valid indicators are 'x' for base-16, 'o' for base-8, and 'b' for base-2");
            }
            ExprParseErrorInfo::IndeterminableToken => {
                report = report
                    .with_label(Label::new(start..end).with_message("this is unintelligible"))
                    .with_note(format!("the problem is {:?}", err.offender));
            }
            ExprParseErrorInfo::ExpectedCloseParen { opener, closer } => {
                report = report
                    .with_label(Label::new(start..end).with_message(format!(
                        "this {:?} expects a {:?} to close it, but there wasn't one",
                        opener, closer
                    )))
                    .with_note(format!("try putting a {:?} at the end", closer));
            }
            ExprParseErrorInfo::WrongCloseParen {
                opener,
                expected_closer,
                got_closer,
            } => {
                println!("span {:?} with content {:?}", start..end, &s[start..end]);
                report = report
                    .with_label(Label::new(start..start + 1).with_message(format!(
                        "this {:?} expects a {:?} to close it...",
                        opener, expected_closer
                    )))
                    .with_label(
                        Label::new(end - 1..end)
                            .with_message(format!("but there was a {:?} instead", got_closer)),
                    )
                    .with_note(format!(
                        "try replacing the {:?} with a {:?}",
                        got_closer, expected_closer
                    ));
            }
            ExprParseErrorInfo::ExpectedCloseQuote => {
                report =
                    report
                        .with_label(Label::new(start..start + 1).with_message(
                            "this double-quote expects a double-quote to close it...",
                        ))
                        .with_label(
                            Label::new(end - 1..end)
                                .with_message("but none was found here (or it was escaped)"),
                        )
                        .with_note("try putting a '\"' at the end");
            }
            ExprParseErrorInfo::InvalidEscape(pos, problem) => {
                report = report.with_label(
                    Label::new(start + *pos..start + *pos + 1).with_message(problem.to_string()),
                );
            }
            ExprParseErrorInfo::InvalidRemainder => {
                report = report
                    .with_label(
                        Label::new(start..end)
                            .with_message("this was left over after reading a well-formed expr"),
                    )
                    .with_note(
                        "try deleting this, or surrounding everything with parens to make a sexpr",
                    );
            }
            ExprParseErrorInfo::ExpectedCloseBlockComment => {
                report = report
                    .with_label(Label::new(start..start + 1).with_message(
                        "this block comment start expects a double-quote to close it...",
                    ))
                    .with_label(Label::new(end - 1..end).with_message("but none was found here"))
                    .with_note("try putting a \"*;\" at the end");
            }
            ExprParseErrorInfo::QuoteNothing => {
                report =
                    report.with_label(Label::new(start..end).with_message("you can't quote that"))
            }
            ExprParseErrorInfo::Eof => {
                report =
                    report.with_label(Label::new(start..end).with_message("found nothing here"))
            }
        }

        ExprParseError {
            report: report.finish(),
            source: err.data,
        }
    }

    pub fn report(&self) -> &Report {
        &self.report
    }

    /// Get a reference to the expr parse error's source.
    pub fn source(&self) -> &ExprParseErrorInfo {
        &self.source
    }
}

impl Debug for ExprParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ExprParseError").field(&self.source).finish()
    }
}

#[derive(Error, Debug)]
pub enum ExprParseErrorInfo {
    #[error("could not parse base-{radix:} number literal")]
    ParseInt {
        radix: u32,
        radix_prefix: Option<String>,
        source: ParseIntError,
    },
    #[error("{0:?} is not a valid radix selector")]
    BadIntRadix(char),
    #[error("could not figure out what kind of token this was meant to be")]
    IndeterminableToken,
    /// This error should span all the way from the opening paren to the closer
    #[error("expected a closing {closer:?} to this sexpr`")]
    ExpectedCloseParen { opener: char, closer: char },
    #[error("wrong closing paren to close {opener:?}")]
    WrongCloseParen {
        opener: char,
        expected_closer: char,
        got_closer: char,
    },
    #[error("expected a closing quote")]
    ExpectedCloseQuote,
    /// The usize is the position from the start of the string including the quote
    /// where the bad is
    #[error("bad escape sequence")]
    InvalidEscape(usize, InvalidEscape),
    #[error("after reading one datum, there was leftover")]
    InvalidRemainder,
    #[error("expected a closing \"*;\" to this block comment")]
    ExpectedCloseBlockComment,
    #[error("found nothing after a quote")]
    QuoteNothing,
    #[error("expected a datum but found nothing")]
    Eof,
}

struct ExprParseErrorLimited<'a> {
    data: ExprParseErrorInfo,
    offender: &'a str,
}

/// Read as many datums as possible from the source string until it is exhausted.
pub fn read_many(whole: &str, engine: &mut Engine) -> Result<Vec<Expr>, ExprParseError> {
    let mut out = Vec::new();

    let mut s = whole;
    while !s.is_empty() {
        let (expr, rest) = read_expr(s, engine).map_err(|err| ExprParseError::new(whole, err))?;
        if let Some(expr) = expr {
            out.push(expr);
            s = rest;
        }
    }

    Ok(out)
}

/// Read exactly one datum and return it.
pub fn read_one(s: &str, engine: &mut Engine) -> Result<Expr, ExprParseError> {
    let exprs = read_many(s, engine)?;
    if exprs.is_empty() {
        Err(ExprParseError::new(
            s,
            ExprParseErrorLimited {
                data: ExprParseErrorInfo::Eof,
                offender: s,
            },
        ))
    } else if exprs.len() >= 2 {
        Err(ExprParseError::new(
            s,
            ExprParseErrorLimited {
                data: ExprParseErrorInfo::InvalidRemainder,
                offender: s,
            },
        ))
    } else {
        // god this is so stupid just let me [0]
        let mut rust_bad = exprs;
        Ok(rust_bad.remove(0))
    }
}

/// Ok case means "we read this and had this string leftover";
/// Err case means "oh no we couldn't read the thing"
type ReadResult<'a, T> = Result<(T, &'a str), ExprParseErrorLimited<'a>>;

#[derive(Debug, Error)]
pub enum InvalidEscape {
    #[error("{0:?} cannot be escaped")]
    BadChar(char),
    #[error("cannot escape the end of file")]
    Eof,
}

/// also trim whitespace from start
fn read_until_delim(s: &str) -> (&str, &str) {
    let s = s.trim_start();
    let idx = s.find(is_delim);
    match idx {
        // there is no remainder, it is all token
        // make the "rest" string still part of s by splitting it at the end
        None => s.split_at(s.len()),
        Some(idx) => s.split_at(idx),
    }
}

fn is_delim(c: char) -> bool {
    match c {
        '(' | ')' | '[' | ']' | '{' | '}' => true,
        ',' | ';' => true,
        _ if c.is_whitespace() => true,
        _ => false,
    }
}

fn read_expr<'a>(whole: &'a str, state: &mut Engine) -> ReadResult<'a, Option<Expr>> {
    let s = whole.trim_start();
    if s.starts_with(";*") {
        // block comment
        // find the next ;* and recur, or the next *; and return the rest.
        fn recur(s: &str) -> Result<&str, ()> {
            if let Some(idx) = s.find(";*") {
                // go down the call stack and find its closer
                // skip the 2 bytes
                recur(&s[idx + 2..])
            } else if let Some(idx) = s.find("*;") {
                // we're done!
                Ok(&s[idx + 2..])
            } else {
                // eof :(
                Err(())
            }
        }

        if let Ok(rest) = recur(s) {
            return read_expr(rest, state);
        } else {
            return Err(ExprParseErrorLimited {
                data: ExprParseErrorInfo::ExpectedCloseBlockComment,
                offender: s,
            });
        }
    } else if s.starts_with(';') {
        // line comment
        let rest = if let Some(idx) = s.find('\n') {
            // one byte to skip the line feed
            &s[idx + 1..]
        } else {
            // get the end of the string
            s.split_at(s.len()).1
        };
        return read_expr(rest, state);
    }

    if let Some(quote) = try_read_quote(s, state) {
        let (quoted, rest) = quote?;

        let quote = state.intern_symbol("quote");
        let quote_idx = Gc::new(Expr::Symbol(quote));
        let quoted_idx = Gc::new(quoted);
        let null_idx = Gc::new(Expr::Nil);
        let quoted_pair = Gc::new(Expr::Pair(quoted_idx, null_idx));
        Ok((Some(Expr::Pair(quote_idx, quoted_pair)), rest))
    } else if let Some(int) = try_read_int(s, state) {
        let (int, rest) = int?;
        Ok((Some(Expr::Integer(int)), rest))
    } else if let Some(string) = try_read_string(s, state) {
        let (string, rest) = string?;
        Ok((Some(Expr::String(string)), rest))
    } else if let Some(ur_mom) = try_read_sexpr(s, state) {
        let (sexhaha, rest) = ur_mom?;
        let expr = match sexhaha {
            Some((car, cdr)) => Expr::Pair(car, cdr),
            None => Expr::Nil,
        };
        Ok((Some(expr), rest))
    } else if let Some(symbol) = try_read_symbol(s, state) {
        // Do symbols last so we need to specially omit as little as possible
        let (id, rest) = symbol?;
        Ok((Some(Expr::Symbol(id)), rest))
    } else if s.contains(|c: char| !c.is_whitespace()) {
        Err(ExprParseErrorLimited {
            data: ExprParseErrorInfo::IndeterminableToken,
            offender: s,
        })
    } else {
        Ok((None, s))
    }
}

fn try_read_int<'a>(s: &'a str, _state: &mut Engine) -> Option<ReadResult<'a, i64>> {
    let (whole, rest) = read_until_delim(s);
    if whole.starts_with(|c: char| c.is_numeric() || c == '-' || c == '+') {
        let (radix, prefix, s) = if whole.starts_with(&['+', '-'][..]) {
            (10, None, whole)
        } else {
            let base = whole.char_indices().nth(1);
            match base {
                None => (10, None, whole),
                Some((_, c)) if c.is_numeric() => (10, None, whole),
                Some((idx, c)) => {
                    let radix = match c {
                        'x' => 16,
                        'o' => 8,
                        'b' => 2,
                        _ => {
                            return Some(Err(ExprParseErrorLimited {
                                data: ExprParseErrorInfo::BadIntRadix(c),
                                offender: &whole[idx..idx + c.len_utf8()],
                            }));
                        }
                    };
                    (radix, Some(&whole[..=idx]), &whole[idx + c.len_utf8()..])
                }
            }
        };

        match i64::from_str_radix(s, radix) {
            Ok(num) => Some(Ok((num, rest))),
            Err(ono) => {
                if whole.starts_with(&['+', '-'][..]) {
                    // Special-case allow symbols starting with `+` and `-`
                    // by passing thru here
                    None
                } else {
                    Some(Err(ExprParseErrorLimited {
                        data: ExprParseErrorInfo::ParseInt {
                            radix,
                            radix_prefix: prefix.map(|s| s.to_owned()),
                            source: ono,
                        },
                        offender: whole,
                    }))
                }
            }
        }
    } else {
        None
    }
}

fn try_read_symbol<'a>(s: &'a str, state: &mut Engine) -> Option<ReadResult<'a, u64>> {
    let (s, rest) = read_until_delim(s);

    if is_valid_symbol(s) {
        let id = state.intern_symbol(s);
        Some(Ok((id, rest)))
    } else {
        None
    }
}

fn is_valid_symbol(s: &str) -> bool {
    !s.is_empty() && !s.starts_with(char::is_numeric)
}

/// May return a pair or null
#[allow(clippy::type_complexity)]
fn try_read_sexpr<'a>(
    s: &'a str,
    state: &mut Engine,
) -> Option<ReadResult<'a, Option<(Gc<Expr>, Gc<Expr>)>>> {
    #[allow(clippy::type_complexity)]
    fn recurse<'b>(
        s: &'b str,
        opener: char,
        closer: char,
        state: &mut Engine,
        original_rest: &'b str,
    ) -> Result<(Option<(Gc<Expr>, Gc<Expr>)>, &'b str), ExprParseErrorLimited<'b>> {
        let s = s.trim();

        let (car, rest) = read_expr(s, state)?;
        match car {
            Some(car) => {
                let (cdr, rest) = recurse(rest, opener, closer, state, original_rest)?;
                let cdr = if let Some((cdr0, cdr1)) = cdr {
                    Gc::new(Expr::Pair(cdr0, cdr1))
                } else {
                    // that's the null, point to null
                    Gc::new(Expr::Nil)
                };
                Ok((Some((Gc::new(car), cdr)), rest))
            }
            None => {
                if let Some(rest) = s.strip_prefix(closer) {
                    // we can finally rest
                    Ok((None, rest))
                } else if s.starts_with(is_closer) {
                    let (start, _) = string_pos(original_rest, s);
                    Err(ExprParseErrorLimited {
                        data: ExprParseErrorInfo::WrongCloseParen {
                            opener,
                            expected_closer: closer,
                            got_closer: s.chars().next().unwrap(),
                        },
                        offender: &original_rest[..=start],
                    })
                } else {
                    Err(ExprParseErrorLimited {
                        data: ExprParseErrorInfo::ExpectedCloseParen { opener, closer },
                        offender: original_rest,
                    })
                }
            }
        }
    }
    let (s, rest) = read_until_delim(s);
    // The paren is always put in `rest`, this means we have only WS until a (
    if s.is_empty() {
        let opener = rest.chars().next()?;
        if let Some(closer) = match_paren(opener) {
            let s = rest[opener.len_utf8()..].trim_start();

            return match recurse(s, opener, closer, state, rest) {
                Ok((Some((car, cdr)), rest)) => Some(Ok((Some((car, cdr)), rest))),
                Ok((None, rest)) => {
                    // we just read a `()`
                    // now we don't need to special case it!
                    Some(Ok((None, rest)))
                }
                Err(ono) => Some(Err(ono)),
            };
        }
    }

    None
}

/// Get the character matching with the given character
fn match_paren(c: char) -> Option<char> {
    Some(match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _ => return None,
    })
}

fn is_closer(c: char) -> bool {
    c == ')' || c == ']' || c == '}'
}

fn try_read_string<'a>(s: &'a str, _state: &mut Engine) -> Option<ReadResult<'a, String>> {
    let whole = s.trim_start();
    if let Some(mut s) = whole.strip_prefix('"') {
        let mut accumulated = String::new();
        loop {
            if let Some(esc_pos) = s.find('\\') {
                // add 1 to skip the backslash
                let rest = &s[esc_pos + 1..];
                let (esc, rest) = match escape(rest) {
                    Ok(it) => it,
                    Err(ono) => {
                        let (badpos, _) = string_pos(whole, rest);
                        return Some(Err(ExprParseErrorLimited {
                            data: ExprParseErrorInfo::InvalidEscape(badpos, ono),
                            offender: &whole[..=badpos],
                        }));
                    }
                };
                accumulated.push_str(&s[..esc_pos]);
                accumulated.push_str(&esc);
                s = rest;
            } else if let Some(quote_pos) = s.find('"') {
                accumulated.push_str(&s[..quote_pos]);
                return Some(Ok((accumulated, &s[quote_pos + 1..])));
            } else {
                // bruh
                let remove_newline = if let Some(idx) = whole.find(&['\n', '\r'][..]) {
                    &whole[..idx]
                } else {
                    whole
                };
                return Some(Err(ExprParseErrorLimited {
                    data: ExprParseErrorInfo::ExpectedCloseQuote,
                    offender: remove_newline,
                }));
            }
        }
    } else {
        None
    }
}

/// Consume an escape sequence with the backslash already gone.
fn escape(s: &str) -> Result<(String, &str), InvalidEscape> {
    match s.chars().next() {
        None => Err(InvalidEscape::Eof),
        Some(sentinel) => {
            // the rest, assuming only 1 char escape
            let naive_rest = &s[sentinel.len_utf8()..];
            match sentinel {
                '\\' => Ok(("\\".to_string(), naive_rest)),
                '"' => Ok(("\"".to_string(), naive_rest)),
                '\'' => Ok(("'".to_string(), naive_rest)),
                'n' => Ok(("\n".to_string(), naive_rest)),
                't' => Ok(("\t".to_string(), naive_rest)),

                _ => Err(InvalidEscape::BadChar(sentinel)),
            }
        }
    }
}

fn try_read_quote<'a>(whole: &'a str, state: &mut Engine) -> Option<ReadResult<'a, Expr>> {
    let s = whole.trim_start();
    s.strip_prefix('\'').map(|s| {
        read_expr(s, state).and_then(|(expr, rest)| {
            if let Some(expr) = expr {
                Ok((expr, rest))
            } else {
                Err(ExprParseErrorLimited {
                    data: ExprParseErrorInfo::QuoteNothing,
                    // include quote
                    offender: &whole[..=1],
                })
            }
        })
    })
}

/// Find the byte positions of the child string's start and end in the parent string.
/// Start is inclusive, end is exclusive.
///
/// Panics if any part of the child string is outside the parent.
fn string_pos<'a>(parent: &'a str, child: &'a str) -> (usize, usize) {
    // this should go down with no unsafe code, even though it involves pointers.
    let pparent = parent.as_ptr() as usize;
    let pchild = child.as_ptr() as usize;

    debug_assert!(
        pparent <= pchild,
        "the child string {:?} started before the parent string {:?} ({:#x} < {:#x})",
        child,
        parent,
        pchild,
        pparent
    );
    let start = pchild - pparent;

    let end = start + child.len();
    debug_assert!(
        end <= parent.len(),
        "the end of the child string {:?} ended after the parent string {:?} ({:#x} > {:#x})",
        child,
        parent,
        pchild + child.len(),
        pparent + parent.len()
    );

    (start, end)
}

#[test]
fn test_string_pos() {
    let s = "True, nervous, dreadfully nervous I have been, and am.";

    let child = &s[3..13];
    let bounds = string_pos(s, child);
    assert_eq!(bounds, (3, 13));

    let bounds = string_pos(s, s);
    assert_eq!(bounds, (0, s.len()));
}

#[test]
fn ariadne() {
    let source = "(r\nr\n}";

    Report::build(ReportKind::Error, (), 0)
        .with_config(Config::default().with_char_set(CharSet::Ascii))
        .with_label(Label::new(0..1).with_message("opens here"))
        .with_label(Label::new(5..6).with_message("closes here"))
        .finish()
        .eprint(Source::from(source))
        .unwrap();
}
