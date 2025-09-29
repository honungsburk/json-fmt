#![allow(bad_style)]

use std::rc::Rc;

#[derive(PartialEq, Eq, Clone)]
pub enum Branch {
    Node(Node),
    Token(Token),
}

#[derive(PartialEq, Eq, Clone)]
pub struct Node {
    kind: SyntaxKind,
    text_len: usize,
    children: Vec<Rc<Branch>>,
}

#[derive(PartialEq, Eq, Clone)]
pub struct Token {
    kind: SyntaxKind,
    text: String,
}

#[derive(PartialEq, Eq, Clone)]
pub enum SyntaxKind {
    ERROR,
    EOF,
    COMMA,
    COLON,
    L_CURLY,
    R_CURLY,
    L_BRACK,
    R_BRACK,

    //Structure
    OBJECT,
    ARRAY,

    // Values
    STRING,
    NUMBER,

    //Keywords
    NULL,
    TRUE,
    FALSE,

    //TRIVIA
    WHITESPACE,
    COMMENT,
}
