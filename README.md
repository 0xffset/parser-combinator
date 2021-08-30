# 0xParser

Parser Combinator crate.

#### Info

README is subject to change.

### How to use

```Rust
let res = parse(
	"Hello World",
	map(
		sequence(vec![string("Hello"), spaces(), string("World")]),
		|r| Ok(vec![r.join("")]),
	),
);

assert_eq!(
	res.unwrap().val,
	vec!["Hello World".to_string()]
);
```