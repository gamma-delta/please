use std::{fmt::Debug, num::ParseIntError, ops::Range};

use ariadne::{CharSet, Label, Report, ReportKind};
use gc::Gc;
use itertools::{Either, Itertools};
use thiserror::Error;

use crate::{display::BstrFmt, hash::GcMap, Engine, Expr, Symbol};

/// Error when lexing or parsing an expression
#[derive(Error)]
#[error("{source:}")]
pub struct ExprParseError {
    source: ExprParseErrorInfo,
    report: Report<(String, Range<usize>)>,
}

impl ExprParseError {
    fn new<'a>(s: &'a [u8], source: String, err: ExprParseErrorLimited<'a>) -> ExprParseError {
        let (start, end) = string_pos(s, err.offender);

        let mut report = Report::build(ReportKind::Error, &source, start)
            .with_config(ariadne::Config::default().with_char_set(CharSet::Ascii))
            .with_message(err.data.to_string());

        let all = (source.to_owned(), start..end);

        match &err.data {
            ExprParseErrorInfo::ParseInt {
                radix,
                radix_prefix,
                source,
            } => {
                report = report
                    .with_label(Label::new(all).with_message(source.to_string()))
                    .with_note(if let Some(prefix) = radix_prefix {
                        format!("the prefix {} indicates a base-{} literal", prefix, radix)
                    } else {
                        "no prefix indicates a base-10 literal".to_string()
                    });
            }
            &ExprParseErrorInfo::BadIntRadix(radix) => {
                report = report
                    .with_label(Label::new(all).with_message(format!("this {:?} is not valid", radix)))
                    .with_note("the valid indicators are 'x' for base-16, 'o' for base-8, and 'b' for base-2");
            }
            ExprParseErrorInfo::IndeterminableToken => {
                report = report
                    .with_label(Label::new(all).with_message("this is unintelligible"))
                    .with_note(format!("the problem is {:?}", BstrFmt(err.offender)));
            }
            ExprParseErrorInfo::ExpectedCloseParen { opener, closer } => {
                report = report
                    .with_label(Label::new(all).with_message(format!(
                        "this {:?} expects a {:?} to close it, but there wasn't one",
                        opener, closer
                    )))
                    .with_note(format!("try putting a {:?} at the end", closer));
            }
            ExprParseErrorInfo::UnexpectedCloseParen { closer } => {
                report = report
                    .with_label(Label::new(all).with_message(format!(
                        "there was no opening paren for this {:?} to close",
                        closer
                    )))
                    .with_note(format!("try removing the {:?}", closer));
            }
            ExprParseErrorInfo::WrongCloseParen {
                opener,
                expected_closer,
                got_closer,
            } => {
                report = report
                    .with_label(Label::new(all.clone()).with_message(format!(
                        "this {:?} expects a {:?} to close it...",
                        opener, expected_closer
                    )))
                    .with_label(
                        Label::new(all)
                            .with_message(format!("but there was a {:?} instead", got_closer)),
                    )
                    .with_note(format!(
                        "try replacing the {:?} with a {:?}",
                        got_closer, expected_closer
                    ));
            }
            ExprParseErrorInfo::WrongDotTrailCount => {
                report = report.with_label(
                    Label::new(all)
                        .with_message("exactly one expr is required after a dot to make a pair"),
                )
            }
            ExprParseErrorInfo::ExpectedCloseQuote => {
                report = report
                    .with_label(
                        Label::new(all.clone()).with_message(
                            "this double-quote expects a double-quote to close it...",
                        ),
                    )
                    .with_label(
                        Label::new(all).with_message("but none was found here (or it was escaped)"),
                    )
                    .with_note("try putting a '\"' at the end");
            }
            ExprParseErrorInfo::HerestringNoName => {
                report = report.with_label(
                    Label::new(all).with_message("an indicator string is required after this <<"),
                );
            }
            ExprParseErrorInfo::HerestringNoEnding(name) => {
                report = report
                    .with_label(Label::new(all)
                        .with_message(format!("the ending to the herestring {:?} wasn't found", name)))
                    .with_note(format!("make sure that {:?} is on a line all by itself with no leading or trailing spaces or characters", name));
            }
            ExprParseErrorInfo::HerestringNoContents => {
                report = report.with_label(
                    Label::new(all).with_message(
                        "a newline is required after this so the herestring can have contents"
                            .to_owned(),
                    ),
                );
            }
            ExprParseErrorInfo::InvalidEscape(pos, problem) => {
                report = report
                    .with_label(
                        Label::new((source, start + pos..start + pos + 1))
                            .with_message(problem.to_string()),
                    )
                    .with_label(Label::new(all).with_message("the string is here".to_owned()));
            }
            ExprParseErrorInfo::InvalidRemainder => {
                report = report
                    .with_label(
                        Label::new(all)
                            .with_message("this was left over after reading a well-formed expr"),
                    )
                    .with_note(
                        "try deleting this, or surrounding everything with parens to make a sexpr",
                    );
            }
            ExprParseErrorInfo::ExpectedCloseBlockComment => {
                report = report
                    .with_label(Label::new(all.clone()).with_message(
                        "this block comment start expects a double-quote to close it...",
                    ))
                    .with_label(Label::new(all).with_message("but none was found here"))
                    .with_note("try putting a \"*;\" at the end");
            }
            ExprParseErrorInfo::PrefixNothing => {
                report = report.with_label(Label::new(all).with_message("you can't quote that"))
            }
            ExprParseErrorInfo::Eof => {
                report = report.with_label(Label::new(all).with_message("found nothing here"))
            }
            ExprParseErrorInfo::MapNeedsEven(count) => {
                report = report.with_label(
                    Label::new(all).with_message(format!("the sexpr had length {}", *count)),
                )
            }
            ExprParseErrorInfo::MapNeedsSexpr => {
                report = report.with_label(Label::new(all).with_message("should be a sexpr"))
            }
        }

        ExprParseError {
            report: report.finish(),
            source: err.data,
        }
    }

    pub fn report(&self) -> &Report<(String, Range<usize>)> {
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
    #[error("expected a closing {closer:?} to this sexpr")]
    ExpectedCloseParen { opener: char, closer: char },
    #[error("did not expect a closing {closer:?} here")]
    UnexpectedCloseParen { closer: char },
    #[error("wrong closing paren to close {opener:?}")]
    WrongCloseParen {
        opener: char,
        expected_closer: char,
        got_closer: char,
    },
    #[error("expected exactly one expr after a dot")]
    WrongDotTrailCount,
    #[error("expected a closing quote")]
    ExpectedCloseQuote,
    #[error("herestring requires a newline")]
    HerestringNoContents,
    #[error("herestring requires a name after the <<")]
    HerestringNoName,
    /// contains the ending we expect
    #[error("herestring requires an ending {0:?}")]
    HerestringNoEnding(String),
    /// The usize is the position from the start of the string including the quote
    /// where the bad is
    #[error("bad escape sequence")]
    InvalidEscape(usize, InvalidEscape),
    #[error("after reading one datum, there was leftover")]
    InvalidRemainder,
    #[error("expected a closing \"*;\" to this block comment")]
    ExpectedCloseBlockComment,
    #[error("found nothing after a special prefix")]
    PrefixNothing,
    #[error("expected a datum but found nothing")]
    Eof,
    #[error("expected a sexpr after a # for a map literal")]
    MapNeedsSexpr,
    /// Number is how many exprs ended up being there
    #[error("map literal requires an even number of exprs")]
    MapNeedsEven(usize),
}

struct ExprParseErrorLimited<'a> {
    data: ExprParseErrorInfo,
    offender: &'a [u8],
}

/// Read as many datums as possible from the source string until it is exhausted.
pub fn read_many(
    whole: &[u8],
    source: String,
    engine: &mut Engine,
) -> Result<Vec<Expr>, ExprParseError> {
    // ariadne doesn't like carriage returns, so we strip them
    // until zesterer gets their act together
    let whole = whole.iter().copied().filter(|b| *b != b'\r').collect_vec();
    let whole = whole.as_slice();

    let mut out = Vec::new();

    let mut s = whole;
    while !s.is_empty() {
        let (expr, rest) = try_read_expr(s, engine)
            .map_err(|err| ExprParseError::new(whole, source.to_owned(), err))?;
        if let Some(expr) = expr {
            out.push(expr);
        }
        s = rest;
    }

    Ok(out)
}

/// Read exactly one datum and return it.
pub fn read_one(s: &[u8], source: String, engine: &mut Engine) -> Result<Expr, ExprParseError> {
    let exprs = read_many(s, source.clone(), engine)?;
    if exprs.is_empty() {
        Err(ExprParseError::new(
            s,
            source,
            ExprParseErrorLimited {
                data: ExprParseErrorInfo::Eof,
                offender: s,
            },
        ))
    } else if exprs.len() >= 2 {
        Err(ExprParseError::new(
            s,
            source,
            ExprParseErrorLimited {
                data: ExprParseErrorInfo::InvalidRemainder,
                offender: s,
            },
        ))
    } else {
        // god this is so stupid just let me [0]
        let mut rust_bad = exprs;
        Ok(rust_bad.swap_remove(0))
    }
}

/// Ok case means "we read this and had this string leftover";
/// Err case means "oh no we couldn't read the thing"
type ReadResult<'a, T> = Result<(T, &'a [u8]), ExprParseErrorLimited<'a>>;

#[derive(Debug, Error)]
pub enum InvalidEscape {
    #[error("{0:?} cannot be escaped")]
    BadChar(char),
    #[error("cannot escape the end of file")]
    Eof,
}

/// also trim whitespace from start
fn read_until_delim(s: &[u8]) -> (&[u8], &[u8]) {
    let s = s.trim_start();
    let idx = s.find_by(is_delim);
    match idx {
        // there is no remainder, it is all token
        // make the "rest" string still part of s by splitting it at the end
        None => s.split_at(s.len()),
        Some(idx) => s.split_at(idx),
    }
}

fn is_delim(b: u8) -> bool {
    match b {
        b'(' | b')' | b'[' | b']' | b'{' | b'}' => true,
        _ if b.is_ascii_whitespace() => true,
        _ => false,
    }
}

fn try_read_expr<'a>(whole: &'a [u8], state: &mut Engine) -> ReadResult<'a, Option<Expr>> {
    let s = whole.trim_start();
    if s.starts_with(b";*") {
        // block comment
        // find the next ;* and recur, or the next *; and return the rest.
        fn recur(s: &[u8]) -> Result<&[u8], ()> {
            if let Some(idx) = s.find(b";*") {
                // go down the call stack and find its closer
                // skip the 2 bytes
                recur(&s[idx + 2..])
            } else if let Some(idx) = s.find(b"*;") {
                // we're done!
                Ok(&s[idx + 2..])
            } else {
                // eof :(
                Err(())
            }
        }

        if let Ok(rest) = recur(s) {
            return try_read_expr(rest, state);
        } else {
            return Err(ExprParseErrorLimited {
                data: ExprParseErrorInfo::ExpectedCloseBlockComment,
                offender: s,
            });
        }
    } else if s.starts_with(b";") {
        // line comment
        let rest = if let Some(idx) = s.find(b"\n") {
            &s[idx..]
        } else {
            // get the end of the string
            s.split_at(s.len()).1
        };
        return try_read_expr(rest, state);
    }

    if let Some(quote) = try_read_prefix_family(s, state) {
        let ((quotefunc, quoted), rest) = quote?;

        let quotesym = state.intern_symbol(quotefunc);
        let quote = Gc::new(Expr::Symbol(quotesym));
        let quoted = Gc::new(quoted);

        let list = Expr::Pair(quote, Expr::pair(quoted, Expr::nil()));
        Ok((Some(list), rest))
    } else if let Some(b) = try_read_bool(s, state) {
        let (b, rest) = b?;
        Ok((Some(Expr::Bool(b)), rest))
    } else if let Some(int) = try_read_int(s, state) {
        let (int, rest) = int?;
        Ok((Some(Expr::Integer(int)), rest))
    } else if let Some(float) = try_read_float(s, state) {
        let (float, rest) = float?;
        Ok((Some(Expr::Float(float)), rest))
    } else if let Some(string) = try_read_string(s, state) {
        let (string, rest) = string?;
        Ok((Some(Expr::String(string)), rest))
    } else if let Some(ur_mom) = try_read_sexpr(s, state) {
        let (sexhaha, rest) = ur_mom?;
        Ok((Some(sexhaha), rest))
    } else if let Some(map) = try_read_map(s, state) {
        let (map, rest) = map?;
        Ok((Some(Expr::Map(map)), rest))
    } else if let Some(symbol) = try_read_symbol(s, state) {
        // Do symbols last so we need to specially omit as little as possible
        let (id, rest) = symbol?;
        Ok((Some(Expr::Symbol(id)), rest))
    } else if s.starts_by(is_closer) {
        let problem = s.first().unwrap();
        Err(ExprParseErrorLimited {
            data: ExprParseErrorInfo::UnexpectedCloseParen {
                closer: *problem as char,
            },
            offender: &s[..1],
        })
    } else if s.contains_by(|b| !b.is_ascii_whitespace()) {
        Err(ExprParseErrorLimited {
            data: ExprParseErrorInfo::IndeterminableToken,
            offender: s,
        })
    } else {
        Ok((None, s))
    }
}

fn try_read_float<'a>(s: &'a [u8], _state: &mut Engine) -> Option<ReadResult<'a, f64>> {
    let (s, rest) = read_until_delim(s);
    if s.starts_by(|b| b.is_ascii_digit() || b"+-.".contains(&b)) {
        let f = (|| {
            let stred = std::str::from_utf8(s).ok()?;
            let f = stred.parse::<f64>().ok()?;
            Some(f)
        })();
        f.map(|f| Ok((f, rest)))
    } else {
        None
    }
}

fn try_read_int<'a>(s: &'a [u8], _state: &mut Engine) -> Option<ReadResult<'a, i64>> {
    let (whole, rest) = read_until_delim(s);
    if whole.contains(&b'.') {
        // maybe a float, who knows? it's sure not an int
        return None;
    }

    if whole.starts_by(|b| b.is_ascii_digit() || b"+-".contains(&b)) {
        let (radix, prefix, s) = if whole.starts_by(|b| b == b'-' || b == b'+') {
            (10, None, whole)
        } else {
            let base = whole.get(1);
            match base {
                None => (10, None, whole),
                Some(b) => {
                    if b.is_ascii_digit() {
                        (10, None, whole)
                    } else {
                        let radix = match b {
                            b'x' => 16,
                            b'o' => 8,
                            b'b' => 2,
                            _ => {
                                return Some(Err(ExprParseErrorLimited {
                                    data: ExprParseErrorInfo::BadIntRadix(*b as char),
                                    offender: &whole[1..2],
                                }));
                            }
                        };
                        (radix, Some(&whole[..2]), &whole[2..])
                    }
                }
            }
        };

        let stred = std::str::from_utf8(s).ok()?;
        match i64::from_str_radix(stred, radix) {
            Ok(num) => Some(Ok((num, rest))),
            Err(ono) => {
                if whole.starts_by(|b| b == b'-' || b == b'+') {
                    // Special-case allow symbols starting with `+` and `-`
                    // by passing thru here
                    None
                } else {
                    Some(Err(ExprParseErrorLimited {
                        data: ExprParseErrorInfo::ParseInt {
                            radix,
                            radix_prefix: prefix.map(|s| String::from_utf8_lossy(s).into_owned()),
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

fn try_read_bool<'a>(s: &'a [u8], _: &mut Engine) -> Option<ReadResult<'a, bool>> {
    let (s, rest) = read_until_delim(s);

    match s {
        b"true" => Some(Ok((true, rest))),
        b"false" => Some(Ok((false, rest))),
        _ => None,
    }
}

fn try_read_symbol<'a>(s: &'a [u8], state: &mut Engine) -> Option<ReadResult<'a, Symbol>> {
    let (s, rest) = read_until_delim(s);

    if is_valid_symbol(s) {
        let id = state.intern_symbol(s);
        Some(Ok((id, rest)))
    } else {
        None
    }
}

fn is_valid_symbol(s: &[u8]) -> bool {
    !s.is_empty() && !s.starts_by(|b| b.is_ascii_digit())
}

/// May return a pair or null
#[allow(clippy::type_complexity)]
fn try_read_sexpr<'a>(s: &'a [u8], state: &mut Engine) -> Option<ReadResult<'a, Expr>> {
    #[allow(clippy::type_complexity)]
    fn recurse<'b>(
        s: &'b [u8],
        opener: u8,
        closer: u8,
        state: &mut Engine,
        original_rest: &'b [u8],
    ) -> Result<(Expr, &'b [u8]), ExprParseErrorLimited<'b>> {
        let s = s.trim_start();

        if let Some(rest) = s.strip_prefix(&[closer]) {
            // we can finally rest
            Ok((Expr::Nil, rest))
        } else if s.starts_by(is_closer) {
            let (start, _) = string_pos(original_rest, s);
            Err(ExprParseErrorLimited {
                data: ExprParseErrorInfo::WrongCloseParen {
                    opener: opener as char,
                    expected_closer: closer as char,
                    got_closer: s[0] as char,
                },
                offender: &original_rest[..=start],
            })
        } else {
            let (car, rest) = try_read_expr(s, state)?;
            match car {
                Some(car) => {
                    let (rest_first, rest_last) = read_until_delim(rest);
                    if rest_first == b"." {
                        // ok we expect one more expr then leave
                        let (expr, rest) = try_read_expr(rest_last, state)?;
                        let cdr = match expr {
                            Some(it) => it,
                            None => {
                                return Err(ExprParseErrorLimited {
                                    data: ExprParseErrorInfo::WrongDotTrailCount,
                                    offender: s,
                                })
                            }
                        };

                        return if let Some(rest) = rest.strip_prefix(&[closer]) {
                            let pair = Expr::Pair(Gc::new(car), Gc::new(cdr));
                            Ok((pair, rest))
                        } else if let Some(problem) = rest.strip_prefix_by(is_closer) {
                            Err(ExprParseErrorLimited {
                                data: ExprParseErrorInfo::WrongCloseParen {
                                    opener: opener as char,
                                    expected_closer: closer as char,
                                    got_closer: problem[0] as char,
                                },
                                offender: &s[..=problem.len()],
                            })
                        } else {
                            Err(ExprParseErrorLimited {
                                data: ExprParseErrorInfo::WrongDotTrailCount,
                                offender: rest,
                            })
                        };
                    }

                    let (cdr, rest) = recurse(rest, opener, closer, state, original_rest)?;
                    let pair = Expr::Pair(Gc::new(car), Gc::new(cdr));
                    Ok((pair, rest))
                }
                None => Err(ExprParseErrorLimited {
                    data: ExprParseErrorInfo::ExpectedCloseParen {
                        opener: opener as char,
                        closer: closer as char,
                    },
                    offender: original_rest,
                }),
            }
        }
    }
    let (s, rest) = read_until_delim(s);
    // The paren is always put in `rest`, this means we have only WS until a (
    if s.is_empty() {
        let opener = *rest.first()?;
        if let Some(closer) = match_paren(opener) {
            let s = &rest[1..];

            let (list, rest) = match recurse(s, opener, closer, state, rest) {
                Ok(it) => it,
                Err(err) => return Some(Err(err)),
            };
            return Some(Ok((list, rest)));
        }
    }

    None
}

/// Get the character matching with the given character
fn match_paren(b: u8) -> Option<u8> {
    Some(match b {
        b'(' => b')',
        b'[' => b']',
        b'{' => b'}',
        _ => return None,
    })
}

fn is_closer(b: u8) -> bool {
    b == b')' || b == b']' || b == b'}'
}

fn try_read_string<'a>(s: &'a [u8], state: &mut Engine) -> Option<ReadResult<'a, Vec<u8>>> {
    let whole = s.trim_start();
    if let Some(rest) = whole.strip_prefix(b"<<") {
        let newline_pos = match rest.find(b"\n") {
            Some(it) => it,
            None => {
                return Some(Err(ExprParseErrorLimited {
                    data: ExprParseErrorInfo::HerestringNoContents,
                    offender: rest,
                }))
            }
        };
        let (here_starter, rest) = rest.split_at(newline_pos);
        if here_starter.is_empty() {
            return Some(Err(ExprParseErrorLimited {
                data: ExprParseErrorInfo::HerestringNoContents,
                offender: here_starter,
            }));
        }
        // now find it with the newline to force it to be
        // alone on the line
        let here_ender = [b"\n", here_starter, b"\n"].concat();
        let herestring_end = match rest[1..].find(&here_ender) {
            Some(it) => it,
            None => {
                return Some(Err(ExprParseErrorLimited {
                    data: ExprParseErrorInfo::HerestringNoEnding(
                        String::from_utf8_lossy(here_starter).into_owned(),
                    ),
                    offender: here_starter,
                }))
            }
        };
        let (herestring, rest) = rest[1..].split_at(herestring_end);
        Some(Ok((herestring.to_vec(), &rest[here_ender.len()..])))
    } else {
        try_read_normal_string(s, state)
    }
}

fn try_read_normal_string<'a>(s: &'a [u8], _state: &mut Engine) -> Option<ReadResult<'a, Vec<u8>>> {
    let whole = s.trim_start();
    if let Some(mut s) = whole.strip_prefix(b"\"") {
        // doesn't make sure \ is before "
        // see which index is first, use Either to figure which to consume to.
        let mut accumulated = Vec::new();
        loop {
            let bs_pos = s.find(b"\\");
            let quote_pos = s.find(b"\"");
            // See which index is first
            // Left = backslash, Right = quote
            let point_of_interest = match (bs_pos, quote_pos) {
                (Some(bs), Some(quote)) => {
                    // Worry about whichever is first
                    if bs < quote {
                        Either::Left(bs)
                    } else {
                        Either::Right(quote)
                    }
                }
                (None, Some(quote)) => {
                    // There's no backslashes in the entire rest of source so use the quote
                    Either::Right(quote)
                }
                // There's no quote left so bail!
                _ => {
                    // fix bug with ariadne
                    let remove_newline = if let Some(idx) = whole.find(b"\r\n") {
                        &whole[..idx]
                    } else {
                        whole
                    };
                    return Some(Err(ExprParseErrorLimited {
                        data: ExprParseErrorInfo::ExpectedCloseQuote,
                        offender: remove_newline,
                    }));
                }
            };

            match point_of_interest {
                Either::Left(esc_pos) => {
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
                    accumulated.extend_from_slice(&s[..esc_pos]);
                    accumulated.extend_from_slice(&esc);
                    s = rest;
                }
                Either::Right(quote_pos) => {
                    accumulated.extend_from_slice(&s[..quote_pos]);
                    return Some(Ok((accumulated, &s[quote_pos + 1..])));
                }
            }
        }
    } else {
        None
    }
}

/// Consume an escape sequence with the backslash already gone.
fn escape(s: &[u8]) -> Result<(Vec<u8>, &[u8]), InvalidEscape> {
    match s.first() {
        None => Err(InvalidEscape::Eof),
        Some(&sentinel) => {
            // the rest, assuming only 1 char escape
            let naive_rest = &s[1..];
            match sentinel {
                b'\\' => Ok((b"\\".to_vec(), naive_rest)),
                b'"' => Ok((b"\"".to_vec(), naive_rest)),
                b'\'' => Ok((b"'".to_vec(), naive_rest)),
                b'n' => Ok((b"\n".to_vec(), naive_rest)),
                b'r' => Ok((b"\r".to_vec(), naive_rest)),
                b't' => Ok((b"\t".to_vec(), naive_rest)),
                b'0' => Ok((b"\0".to_vec(), naive_rest)),
                // "Formfeed Page Break"
                b'f' => Ok((b"\x0c".to_vec(), naive_rest)),
                // "Vertical Tab"
                b'v' => Ok((b"\x0b".to_vec(), naive_rest)),
                // Bell
                b'a' => Ok((b"\x07".to_vec(), naive_rest)),

                _ => Err(InvalidEscape::BadChar(sentinel as char)),
            }
        }
    }
}

/// For prefixes that insert their own sexprs, like the quote family and #.
///
/// `true` means the substitution goes on the outside, `false` means on the inside.
fn try_read_prefix_family<'a>(
    whole: &'a [u8],
    state: &mut Engine,
) -> Option<ReadResult<'a, (&'static [u8], Expr)>> {
    let whole = whole.trim_start();
    special_prefixes(whole).map(|(quote, rest)| {
        try_read_expr(rest, state).and_then(|(expr, rest)| {
            if let Some(expr) = expr {
                Ok(((quote, expr), rest))
            } else {
                Err(ExprParseErrorLimited {
                    data: ExprParseErrorInfo::PrefixNothing,
                    // include quote
                    offender: whole,
                })
            }
        })
    })
}

/// Return the name of the form to put inside the parensm and the rest of the string.
fn special_prefixes<'a>(s: &'a [u8]) -> Option<(&'static [u8], &'a [u8])> {
    [
        (&b"'"[..], &b"quote"[..]),
        (b"`", b"quasiquote"),
        // put this first so it looks for it first
        (b",@", b"unquote-splicing"),
        (b",", b"unquote"),
    ]
    .iter()
    .find_map(|(header, quote)| s.strip_prefix(*header).map(|rest| (*quote, rest)))
}

fn try_read_map<'a>(s: &'a [u8], state: &mut Engine) -> Option<ReadResult<'a, GcMap>> {
    let (s, sexp_str) = read_until_delim(s);
    if s == b"#" {
        match try_read_sexpr(sexp_str, state) {
            Some(Ok((expr, rest))) => {
                // safe to unwrap because it's not using any lazy pairs and because it will always be a sexpr
                let kvs = state.sexp_to_list(Gc::new(expr)).unwrap().unwrap();
                if kvs.len() % 2 != 0 {
                    let (start, _) = string_pos(sexp_str, rest);
                    let offender = &sexp_str[..start];
                    return Some(Err(ExprParseErrorLimited {
                        data: ExprParseErrorInfo::MapNeedsEven(kvs.len()),
                        offender,
                    }));
                }

                let mut map = GcMap::new();
                for kv in kvs.chunks_exact(2) {
                    map.insert(kv[0].to_owned(), kv[1].to_owned());
                }

                Some(Ok((map, rest)))
            }
            Some(Err(ono)) => Some(Err(ono)),
            None => Some(Err(ExprParseErrorLimited {
                data: ExprParseErrorInfo::MapNeedsSexpr,
                offender: sexp_str,
            })),
        }
    } else {
        None
    }
}

/// Find the byte positions of the child string's start and end in the parent string.
/// Start is inclusive, end is exclusive.
///
/// Panics if any part of the child string is outside the parent.
fn string_pos<'a>(parent: &'a [u8], child: &'a [u8]) -> (usize, usize) {
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
    let s = b"True, nervous, dreadfully nervous I have been, and am.";

    let child = &s[3..13];
    let bounds = string_pos(s, child);
    assert_eq!(bounds, (3, 13));

    let bounds = string_pos(s, s);
    assert_eq!(bounds, (0, s.len()));
}

trait BytestrExt {
    fn trim_start(&self) -> &Self;
    fn find(&self, needle: &Self) -> Option<usize>;
    fn find_by(&self, searcher: impl FnMut(u8) -> bool) -> Option<usize>;
    fn starts_by(&self, searcher: impl FnMut(u8) -> bool) -> bool;
    fn contains_by(&self, searcher: impl FnMut(u8) -> bool) -> bool;
    fn strip_prefix_by(&self, searcher: impl FnMut(u8) -> bool) -> Option<&Self>;
}

impl BytestrExt for [u8] {
    fn trim_start(&self) -> &Self {
        // find the first position that is *not* whitespace and cut before it
        let idx = self
            .iter()
            .position(|b| !b" \t\r\n\0\x0b\x0c".contains(b))
            .unwrap_or_else(|| self.len());
        &self[idx..]
    }

    fn find(&self, needle: &Self) -> Option<usize> {
        self.windows(needle.len()).position(|slice| slice == needle)
    }

    fn find_by(&self, searcher: impl FnMut(u8) -> bool) -> Option<usize> {
        self.iter().copied().position(searcher)
    }

    fn starts_by(&self, mut searcher: impl FnMut(u8) -> bool) -> bool {
        match self.first() {
            Some(b) => searcher(*b),
            None => false,
        }
    }

    fn contains_by(&self, searcher: impl FnMut(u8) -> bool) -> bool {
        self.iter().copied().any(searcher)
    }

    fn strip_prefix_by(&self, searcher: impl FnMut(u8) -> bool) -> Option<&Self> {
        if self.starts_by(searcher) {
            Some(&self[1..])
        } else {
            None
        }
    }
}
