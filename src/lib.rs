#![allow(non_snake_case)]

use crate::string_utils::StringUtils;
use regex::Regex;

pub mod string_utils;

pub type Parser = Box<dyn Fn(Context) -> Result<Success, Failure>>;

#[derive(Debug, Clone)]
pub struct Context {
    pub txt: String,
    pub pos: usize,
}

#[derive(Debug, Clone)]
pub struct Success {
    pub val: Vec<String>,
    pub ctx: Context,
}

#[derive(Debug, Clone)]
pub struct Failure {
    pub exp: String,
    pub ctx: Context,
}

pub fn success(ctx: Context, val: Vec<String>) -> Success {
    Success { val, ctx }
}

pub fn failure<S: AsRef<str>>(ctx: Context, exp: S) -> Failure {
    let exp = exp.as_ref().to_string();
    Failure { exp, ctx }
}

pub fn string<S: AsRef<str>>(target: S) -> Parser {
    let target = target.as_ref().to_string();

    Box::new(move |mut ctx: Context| {
        if ctx.txt.slice(ctx.pos..).starts_with(&target.clone()) {
            ctx.pos += target.len();
            return Ok(success(ctx, vec![target.clone()]));
        }

        return Err(failure(ctx, target.clone()));
    })
}

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

        return Err(failure(ctx, expected.clone()));
    })
}

pub fn optional(parser: Parser) -> Parser {
    Box::new(move |ctx: Context| {
        let res = parser(ctx.clone());

        if res.is_err() {
            return Ok(success(res.unwrap_err().ctx, vec![String::new()]));
        }

        return Ok(res.unwrap());
    })
}

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

pub fn any(parsers: Vec<Parser>) -> Parser {
    Box::new(move |ctx: Context| {
        for parser in parsers.iter() {
            let res = parser(ctx.clone());
            if res.is_ok() {
                return res;
            }
        }

        return Err(failure(ctx, String::from("any()")));
    })
}

pub fn map(
    parser: Parser,
    mapper: fn(Vec<String>) -> Result<Vec<String>, String>,
) -> Parser {
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

pub fn between(front: Parser, middle: Parser, back: Parser) -> Parser {
    map(sequence(vec![front, middle, back]), |v| {
        Ok(vec![v[1].clone()])
    })
}

pub fn spaces() -> Parser {
    return map(many(string(" ")), |s| Ok(vec![s.join("")]));
}

pub fn letters() -> Parser {
    return regex("[a-zA-Z]+", "letters");
}

pub fn integer() -> Parser {
    return regex(r"\d+", "integer");
}

pub fn float() -> Parser {
    return regex(r"\d+\.\d*", "float");
}

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

pub fn parse<S: AsRef<str>>(txt: S, parser: Parser) -> Result<Success, String> {
    let txt = txt.as_ref().to_string();

    let res = parser(Context { txt, pos: 0 });
    if res.is_err() {
        let res = res.unwrap_err();
        return Err(format!(
            "Parser error, expected '{}' at position '{}'",
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
            "Parser error, expected 'Hallo World' at position '0'"
        );

        let res = parse("My Hello World", string("Hello World"));
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'Hello World' at position '0'"
        );
    }

    #[test]
    fn regex_test() {
        let res = parse("DE0012 2322 2323", regex(r"DE\d{4}\s\d{4}\s\d{4}", "IBAN"));
        assert_eq!(res.unwrap().val[0], "DE0012 2322 2323");

        let res = parse("DE012 2322 2323", regex(r"DE\d{4}\s\d{4}\s\d{4}", "IBAN"));
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'IBAN' at position '0'"
        );

        let res = parse(
            "Bank account: DE012 2322 2323",
            regex(r"DE\d{4}\s\d{4}\s\d{4}", "IBAN"),
        );
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'IBAN' at position '0'"
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
        let res = parse(
            "Hello World",
            sequence(vec![string("Hello"), string(" World")]),
        );
        assert_eq!(
            res.unwrap().val,
            vec!["Hello".to_string(), " World".to_string()]
        );

        let res = parse(
            "Hello World",
            sequence(vec![string("Hallo"), string(" World")]),
        );
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'Hallo' at position '0'"
        );

        let res = parse(
            "Hello World",
            sequence(vec![string("Hello"), string("World")]),
        );
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'World' at position '5'"
        );

        let res = parse(
            "Hello World",
            sequence(vec![string("Hello"), string(" "), string("World")]),
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
            "Parser error, expected 'any()' at position '0'"
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
                |_| Err("mapping()".to_string()),
            ),
        );
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'mapping()' at position '11'"
        );
    }

    #[test]
    fn many_test() {
        let res = parse("Hello World", many(regex(r".{1}", "anything")));
        assert_eq!(res.unwrap().val.join(""), "Hello World");

        let res = parse("Hello World", many(regex(r"\d{1}", "number")));
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'number' at position '0'"
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
            "Parser error, expected '\"' at position '6'"
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
            "Parser error, expected ' ' at position '5'"
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
            "Parser error, expected 'letters' at position '0'"
        );
    }

    #[test]
    fn integer_test() {
        let res = parse("123456789", integer());
        assert_eq!(res.unwrap().val, vec!["123456789"]);

        let res = parse("a123456789", integer());
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'integer' at position '0'"
        );
    }

    #[test]
    fn float_test() {
        let res = parse("12345.6789", float());
        assert_eq!(res.unwrap().val, vec!["12345.6789"]);

        let res = parse("a1234.56789", float());
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected 'float' at position '0'"
        );
    }

    #[test]
    fn expect_test() {
        let res = parse("Hello World", expect(string("Hello"), "\"Hello\""));
        assert_eq!(res.unwrap().val, vec!["Hello"]);

        let res = parse("Hello World", expect(string("Hallo"), "\"Hallo\""));
        assert_eq!(
            res.unwrap_err(),
            "Parser error, expected '\"Hallo\"' at position '0'"
        );
    }
}
