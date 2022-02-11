# ox_parser

Parser Combinator crate. (Continuation of the OxParse crate)

https://crates.io/crates/ox_parser (https://crates.io/crates/OxParse is deprecated)

#### Info

README is subject to change.

### How to use
Look at the documentation for more examples.
```Rust
let res = parse(
	"Hello World",
	map(
		sequence!(string("Hello"), spaces(), string("World")),
		|r| Ok(vec![r.join("")]),
	),
);

assert_eq!(
	res.unwrap().val,
	vec!["Hello World".to_string()]
);
```
