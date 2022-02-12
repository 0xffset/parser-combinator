#![allow(unused_macros)]

use crate::string_utils::StringUtils;
use regex::Regex;

pub mod string_utils;

pub type Parser = Box<dyn Fn(Context) -> Result<Success, Failure>>;

#[derive(Debug, Clone)]
pub struct Context {
    pub txt: String,
    pub pos: usize,
}

/// `Success` is a successful parse result
/// * `val` holds the value of the parse
/// * `ctx` holds the context of the parse
#[derive(Debug, Clone)]
pub struct Success {
    pub val: Vec<String>,
    pub ctx: Context,
}

/// `Failure` is a failed parse result
/// * `exp` holds the error message
/// * `ctx` holds the context of the parse
#[derive(Debug, Clone)]
pub struct Failure {
    pub exp: String,
    pub ctx: Context,
}

/// Creates a new `Success` object with the given value and context
fn success(ctx: Context, val: Vec<String>) -> Success {
    Success { val, ctx }
}

/// Creates a new `Failure` object with the given error message and context
fn failure<S: AsRef<str>>(ctx: Context, exp: S) -> Failure {
    let exp = exp.as_ref().to_string();
    Failure { exp, ctx }
}

/// # String parser
/// Parses for a given target string
/// ### Arguments
/// * `target` - The target string to parse for
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{string, parse};
///
/// let res = parse("Hello World", string("Hello World"));
/// assert_eq!(res.unwrap().val[0], "Hello World");
/// ```
pub fn string<S: AsRef<str>>(target: S) -> Parser {
    let target = target.as_ref().to_string();

    Box::new(move |mut ctx: Context| {
        if ctx.txt.slice(ctx.pos..).starts_with(&target.clone()) {
            ctx.pos += target.len();
            return Ok(success(ctx, vec![target.clone()]));
        }

        return Err(failure(ctx, format!("Expected '{}'", target.clone())));
    })
}

/// # Regex parser
/// Parses for a given regex pattern
/// ### Arguments
/// * `target` - The target regex pattern
/// * `expected` - A custom error message
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{regex, parse};
///
/// let res = parse("+12 345 67890", regex(r"\+\d{2}\s\d{3}\s\d{5}", "Phone number"));
/// assert_eq!(res.unwrap().val[0], "+12 345 67890");
///
/// let res = parse("+12 45 6890", regex(r"\+\d{2}\s\d{3}\s\d{5}", "Phone number"));
/// assert_eq!(
///     res.unwrap_err(),
///     "[Parser error] Expected 'Phone number' at position: '0'"
/// );
/// ```
pub fn regex<A: AsRef<str>, B: AsRef<str>>(target: A, expected: B) -> Parser {
    let target = target.as_ref().to_string();
    let expected = expected.as_ref().to_string();

    Box::new(move |mut ctx: Context| {
        let regex = match Regex::new(&target.clone()) {
            Ok(regex) => regex,
            Err(_) => panic!("Invalid regex: {}", target),
        };

        let sliced_ctx = ctx.txt.slice(ctx.pos..);
        let mat = regex.find(&sliced_ctx);
        if mat.is_some() {
            let mat = mat.unwrap();
            if mat.start() == 0 {
                ctx.pos += mat.end();
                return Ok(success(ctx, vec![mat.as_str().to_string()]));
            }
        }

        return Err(failure(ctx, format!("Expected '{}'", expected.clone())));
    })
}

/// # Optional parser
/// Tries to parse the given parser, but if it fails, it returns a successful result with an empty value
/// ### Arguments
/// * `parser` - The parser to try to parse
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{optional, string, parse};
///
/// let res = parse("Hello World", optional(string("Hello World")));
/// assert_eq!(res.unwrap().val[0], "Hello World".to_string());
///
/// let res = parse("Hello World", optional(string("Hallo World")));
/// assert_eq!(res.unwrap().val[0], String::new());
/// ```
pub fn optional(parser: Parser) -> Parser {
    Box::new(move |ctx: Context| {
        let res = parser(ctx.clone());

        if res.is_err() {
            return Ok(success(res.unwrap_err().ctx, vec![String::new()]));
        }

        return Ok(res.unwrap());
    })
}

/// # Sequence parser
/// Parses for a sequence of parsers.
/// Convenience macro, works identical to `sequence()` but without having to manually create a vector.
/// ### Arguments
/// * `parsers` - The parsers to parse for
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// #[macro_use] extern crate ox_parser;
/// use ox_parser::{sequence, string, spaces, parse};
///
/// let res = parse("Hello World", sequence!(string("Hello"), spaces(), string("World")));
/// assert_eq!(
///     res.unwrap().val,
///     vec!["Hello".to_string(), " ".to_string(), "World".to_string()]
/// );
/// ```
#[macro_export]
macro_rules! sequence {
    ($($p:expr),+) => {
        sequence(vec![$($p),*])
    };
}

/// # Sequence parser
/// Parses for a sequence of parsers
/// ### Arguments
/// * `parsers` - The parsers to parse for
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// #[macro_use] extern crate ox_parser;
/// use ox_parser::{sequence, string, spaces, parse};
///
/// let res = parse("Hello World", sequence(vec![string("Hello"), spaces(), string("World")]));
/// assert_eq!(
///     res.unwrap().val,
///     vec!["Hello".to_string(), " ".to_string(), "World".to_string()]
/// );
/// ```
pub fn sequence(parsers: Vec<Parser>) -> Parser {
    Box::new(move |mut ctx: Context| {
        let mut result = Vec::new();
        for parser in parsers.iter() {
            let res = parser(ctx.clone());
            if res.is_err() {
                return Err(res.unwrap_err());
            }
            let mut res = res.unwrap();
            ctx = res.ctx;
            result.append(&mut res.val);
        }

        return Ok(success(ctx, result));
    })
}

/// # Any parser
/// Parses for any of the given parsers and returns the first successful result, or an error if no parser matched
/// ### Arguments
/// * `parsers` - The parsers to parse for
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{any, string, parse};
///
/// let res = parse("Hello World", any(vec![string("Hallo"), string("Hello")]));
/// assert_eq!(res.unwrap().val, vec!["Hello".to_string()]);
/// ```
pub fn any(parsers: Vec<Parser>) -> Parser {
    Box::new(move |ctx: Context| {
        for parser in parsers.iter() {
            let res = parser(ctx.clone());
            if res.is_ok() {
                return res;
            }
        }

        return Err(failure(ctx, "No match in any()".to_string()));
    })
}

/// # Either parser
/// Parses either of the two given parsers and returns either the first to match or an error if both failed
/// ### Arguments
/// * `parser_a` - The first parser to parse for
/// * `parser_b` - The second parser to parse for
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{either, string, parse};
/// 
/// let res = parse("Hello World", either(string("Hallo Welt"), string("Hello World")));
/// assert_eq!(res.unwrap().val, vec!["Hello World".to_string()]);
pub fn either(parser_a: Parser, parser_b: Parser) -> Parser {
    any(vec![parser_a, parser_b])
}

/// # Map parser
/// Maps the result of a parser to a new value
/// ### Arguments
/// * `parser` - The parser to map
/// * `mapper` - The function to map the result of the parser
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// #[macro_use] extern crate ox_parser;
/// use ox_parser::{map, sequence, string, parse};
///
/// let res = parse(
///     "Hello World",
///     map(
///         sequence!(string("Hello"), string(" "), string("World")),
///         |res| Ok(vec![res.join("")]),
///     ),
/// );
/// assert_eq!(res.unwrap().val, vec!["Hello World".to_string()]);
/// ```
pub fn map(parser: Parser, mapper: fn(Vec<String>) -> Result<Vec<String>, String>) -> Parser {
    Box::new(move |ctx: Context| {
        let res = parser(ctx.clone());
        if res.is_err() {
            return Err(res.unwrap_err());
        }
        let res = res.unwrap();

        let ctx = res.ctx;
        let new_res = mapper(res.val);
        if new_res.is_ok() {
            return Ok(success(ctx, new_res.unwrap()));
        }

        return Err(failure(ctx, new_res.unwrap_err()));
    })
}

/// # Many parser
/// Parses as many times as possible, returns an error if no parsing was successful
/// ### Arguments
/// * `parser` - The parser to parse for
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// * Look at the `spaces()` parser implementation for an example
pub fn many(parser: Parser) -> Parser {
    Box::new(move |mut ctx: Context| {
        let mut ret = Vec::new();

        loop {
            let res = parser(ctx.clone());

            if res.is_err() {
                if ret.len() == 0 {
                    let res = res.unwrap_err();
                    return Err(failure(res.ctx, res.exp));
                }

                return Ok(success(ctx, ret));
            }
            let mut res = res.unwrap();

            ctx = res.ctx;
            ret.append(&mut res.val);
        }
    })
}

/// # Between parser
/// Parses between two parsers
/// ### Arguments
/// * `front` - The left parser
/// * `middle` - The parser to parse between the left and right parser
/// * `back` - The right parser
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{between, string, parse};
///
/// let res = parse(
///     "\"Hello\"",
///     between(string("\""), string("Hello"), string("\"")),
/// );
/// assert_eq!(res.unwrap().val, vec!["Hello"]);
/// ```
pub fn between(front: Parser, middle: Parser, back: Parser) -> Parser {
    map(sequence(vec![front, middle, back]), |v| {
        Ok(vec![v[1].clone()])
    })
}

/// # Spaces parser
/// Parses for at least one and as many spaces as possible
/// # Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// #[macro_use] extern crate ox_parser;
/// use ox_parser::{spaces, string, parse, sequence};
///
/// let res = parse(
///     "Hello World",
///     sequence!(string("Hello"), spaces(), string("World")),
/// );
/// assert_eq!(
///     res.unwrap().val,
///     vec!["Hello".to_string(), " ".to_string(), "World".to_string()]
/// );
/// ```
pub fn spaces() -> Parser {
    return map(many(string(" ")), |s| Ok(vec![s.join("")]));
}

/// # Letters parser
/// Parses for at least one letter
/// # Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{letters, parse};
///
/// let res = parse("Hello", letters());
/// assert_eq!(res.unwrap().val, vec!["Hello"]);
/// ```
pub fn letters() -> Parser {
    return regex("[a-zA-Z]+", "letters");
}

/// # Integer parser
/// Parses for an integer
/// # Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{integer, parse};
///
/// let res = parse("123", integer());
/// assert_eq!(res.unwrap().val, vec!["123"]);
/// ```
pub fn integer() -> Parser {
    return regex(r"\d+", "integer");
}

/// # Float parser
/// Parses for a float
/// # Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{float, parse};
///
/// let res = parse("123.456", float());
/// assert_eq!(res.unwrap().val, vec!["123.456"]);
/// ```
pub fn float() -> Parser {
    return regex(r"\d+\.\d*", "float");
}

/// Runs a given parser on the context, if fails, returns a custom error message
/// ### Arguments
/// * `parser` - The parser to run
/// * `expected` - The error message
/// ### Returns
/// * A parser that can be used in other parsers or directly ran in the `parse(...)` function
/// ## Example
/// ```
/// use ox_parser::{string, expect, parse};
///
/// let res = parse("Hallo Welt", expect(string("Hello World"), "Expected \"Hello World\""));
/// assert_eq!(res.unwrap_err(), "[Parser error] Expected \"Hello World\" at position: '0'");
/// ```
pub fn expect<S: AsRef<str>>(parser: Parser, expected: S) -> Parser {
    let expected = expected.as_ref().to_string();

    Box::new(move |ctx: Context| {
        let res = parser(ctx.clone());
        if res.is_err() {
            return Err(failure(res.unwrap_err().ctx, expected.clone()));
        }

        return res;
    })
}

/// Runs a given parser on a given string.
/// ### Arguments
/// * `txt` - the text to parse
/// * `parser` - The parser to run
/// ### Returns
/// * `Result<Success, String>` containing the result of the parser or the error message
/// ## Example
/// ```
/// #[macro_use] extern crate ox_parser;
/// use ox_parser::{map, parse, string, spaces, sequence};
///
/// let res = parse("Hello World",
///     map(sequence!(string("Hello"), spaces(), string("World")),
///         |r| Ok(vec![r.join("")]),
///     ),
/// );
///
/// assert_eq!(
///     res.unwrap().val,
///     vec!["Hello World".to_string()]
/// );
/// ```
pub fn parse<S: AsRef<str>>(txt: S, parser: Parser) -> Result<Success, String> {
    let txt = txt.as_ref().to_string();

    let res = parser(Context { txt, pos: 0 });
    if res.is_err() {
        let res = res.unwrap_err();
        return Err(format!(
            "[Parser error] {} at position: '{}'",
            res.exp, res.ctx.pos
        ));
    }

    return Ok(res.unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_test() {
        let res = parse("Hello World", string("Hello World"));
        assert_eq!(res.unwrap().val[0], "Hello World");

        let res = parse("Hello World", string("Hallo World"));
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'Hallo World' at position: '0'"
        );

        let res = parse("My Hello World", string("Hello World"));
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'Hello World' at position: '0'"
        );
    }

    #[test]
    fn regex_test() {
        let res = parse("DE0012 2322 2323", regex(r"DE\d{4}\s\d{4}\s\d{4}", "IBAN"));
        assert_eq!(res.unwrap().val[0], "DE0012 2322 2323");

        let res = parse("DE012 2322 2323", regex(r"DE\d{4}\s\d{4}\s\d{4}", "IBAN"));
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'IBAN' at position: '0'"
        );

        let res = parse(
            "Bank account: DE012 2322 2323",
            regex(r"DE\d{4}\s\d{4}\s\d{4}", "IBAN"),
        );
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'IBAN' at position: '0'"
        );
    }

    #[test]
    fn optional_test() {
        let res = parse("Hello World", optional(string("Hello World")));
        assert_eq!(res.unwrap().val[0], "Hello World".to_string());

        let res = parse("Hello World", optional(string("Hallo World")));
        assert_eq!(res.unwrap().val[0], String::new());
    }

    #[test]
    fn sequence_test() {
        let res = parse("Hello World", sequence!(string("Hello"), string(" World")));
        assert_eq!(
            res.unwrap().val,
            vec!["Hello".to_string(), " World".to_string()]
        );

        let res = parse("Hello World", sequence!(string("Hallo"), string(" World")));
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'Hallo' at position: '0'"
        );

        let res = parse("Hello World", sequence!(string("Hello"), string("World")));
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'World' at position: '5'"
        );

        let res = parse(
            "Hello World",
            sequence!(string("Hello"), string(" "), string("World")),
        );
        assert_eq!(
            res.unwrap().val,
            vec!["Hello".to_string(), " ".to_string(), "World".to_string()]
        );
    }

    #[test]
    fn any_test() {
        let res = parse(
            "Hello World",
            sequence(vec![
                any(vec![string("Hallo"), string("Hello")]),
                string(" World"),
            ]),
        );

        assert_eq!(
            res.unwrap().val,
            vec!["Hello".to_string(), " World".to_string()]
        );

        let res = parse(
            "Hello World",
            sequence(vec![
                any(vec![string("Hallo"), string("Hola")]),
                string(" World"),
            ]),
        );

        assert_eq!(
            res.unwrap_err(),
            "[Parser error] No match in any() at position: '0'"
        );
    }

    #[test]
    fn map_test() {
        let res = parse(
            "Hello World",
            map(
                sequence(vec![string("Hello"), string(" "), string("World")]),
                |res| Ok(vec![res.join("")]),
            ),
        );
        assert_eq!(res.unwrap().val, vec!["Hello World".to_string()]);

        let res = parse(
            "Hello World",
            map(
                sequence(vec![string("Hello"), string(" "), string("World")]),
                |_| Err("Expected 'mapping()'".to_string()),
            ),
        );
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'mapping()' at position: '11'"
        );
    }

    #[test]
    fn many_test() {
        let res = parse("Hello World", many(regex(r".{1}", "anything")));
        assert_eq!(res.unwrap().val.join(""), "Hello World");

        let res = parse("Hello World", many(regex(r"\d{1}", "number")));
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'number' at position: '0'"
        );
    }

    #[test]
    fn between_test() {
        let res = parse(
            "\"Hello\"",
            between(string("\""), string("Hello"), string("\"")),
        );
        assert_eq!(res.unwrap().val, vec!["Hello"]);

        let res = parse(
            "1Hello\"",
            between(integer(), string("Hello"), string("\"")),
        );
        assert_eq!(res.unwrap().val, vec!["Hello"]);

        let res = parse(
            "\"Hello1",
            between(string("\""), string("Hello"), string("\"")),
        );
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected '\"' at position: '6'"
        );
    }

    #[test]
    fn spaces_test() {
        let res = parse(
            "Hello World",
            sequence(vec![string("Hello"), spaces(), string("World")]),
        );

        assert_eq!(
            res.unwrap().val,
            vec!["Hello".to_string(), " ".to_string(), "World".to_string()]
        );

        let res = parse(
            "HelloWorld",
            sequence(vec![string("Hello"), spaces(), string("World")]),
        );
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected ' ' at position: '5'"
        );

        let res = parse(
            "Hello    World",
            sequence(vec![string("Hello"), spaces(), string("World")]),
        );
        assert_eq!(
            res.unwrap().val,
            (vec!["Hello".to_string(), "    ".to_string(), "World".to_string()])
        );
    }

    #[test]
    fn letters_test() {
        let res = parse("Hello", letters());
        assert_eq!(res.unwrap().val, vec!["Hello"]);

        let res = parse("Hello!", letters());
        assert_eq!(res.unwrap().val, vec!["Hello"]);

        let res = parse("1Hello", letters());
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'letters' at position: '0'"
        );
    }

    #[test]
    fn integer_test() {
        let res = parse("123456789", integer());
        assert_eq!(res.unwrap().val, vec!["123456789"]);

        let res = parse("a123456789", integer());
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'integer' at position: '0'"
        );
    }

    #[test]
    fn float_test() {
        let res = parse("12345.6789", float());
        assert_eq!(res.unwrap().val, vec!["12345.6789"]);

        let res = parse("a1234.56789", float());
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected 'float' at position: '0'"
        );
    }

    #[test]
    fn expect_test() {
        let res = parse("Hello World", expect(string("Hello"), "\"Hello\""));
        assert_eq!(res.unwrap().val, vec!["Hello"]);

        let res = parse("Hello World", expect(string("Hallo"), "Expected \"Hallo\""));
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] Expected \"Hallo\" at position: '0'"
        );
    }

    #[test]
    fn either_test() {
        let res = parse(
            "Hello World",
            either(string("Hello World"), string("Hallo Welt")),
        );
        assert_eq!(res.unwrap().val, vec!["Hello World"]);

        let res = parse(
            "Hola mundo",
            either(string("Hello World"), string("Hallo Welt")),
        );
        assert_eq!(
            res.unwrap_err(),
            "[Parser error] No match in any() at position: '0'"
        );
    }
}
