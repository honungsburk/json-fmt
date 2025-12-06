# JSON Format

An educational JSON formatter that displays the use of a CST (Concrete Syntax Tree)
combined with a layout engine to produce nice formatting. The design is heavily
inspired by these articles:

- https://mcyoung.xyz/2025/03/11/formatters/
- https://rust-analyzer.github.io/book/contributing/syntax.html
- https://users.rust-lang.org/t/why-would-someone-use-rowan/78480
- https://rust-analyzer.github.io/blog/2020/10/24/introducing-ungrammar.html

and these repos:

- https://github.com/mcy/strings/tree/main/allman
- https://github.com/rust-analyzer/rowan

- https://github.com/rust-lang/rust/blob/21a13b8864a7dd614e9a96afd57b58c7fcf0bd6b/compiler/rustc_lexer/src/lib.rs#L51

TODO:

- [ ] CLI
- [ ] Fuzzing