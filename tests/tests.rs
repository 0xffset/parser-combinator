use ox_parser::*;

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
        sequence!(any(vec![string("Hallo"), string("Hola")]), string(" World")),
    );

    assert_eq!(
        res.unwrap_err(),
        "[Parser error] Expected any of ['Hallo', 'Hola'] at position: '0'"
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
            |_| Err("'mapping()'".to_string()),
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

    let res = parse("Hello World", expect(string("Hallo"), "\"Hallo\""));
    assert_eq!(
        res.unwrap_err(),
        "[Parser error] Expected '\"Hallo\"' at position: '0'"
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
        "[Parser error] Expected any of ['Hello World', 'Hallo Welt'] at position: '0'"
    );
}
