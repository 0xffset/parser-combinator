# ox_parser

Parser Combinator crate. Provides a simple parser crate for Rust.

#### Info

README is subject to change.

### How to use
Look at the documentation for more examples.
```Rust
let res = parse(
	"Hello World",
	map(
		sequence!(string("Hello"), spaces(), string("World")),
		|r| Ok(vec![r.val.join("")]),
	),
);

assert_eq!(
	res.unwrap().val,
	vec!["Hello World".to_string()]
);
```
