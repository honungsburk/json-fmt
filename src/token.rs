/// Enum representing common lexeme types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Comma,
    Colon,
    LineComment,
    Whitespace,
    Number,
    String { terminated: bool },
    True,
    False,
    Null,
    LeftBracket,
    RightBracket,
    LeftCurly,
    RightCurly,
    Slash,
    Unknown,
    Eof,
}

/// Parsed token.
/// It doesn't contain information about data that has been parsed,
/// only the type of the token and its size.
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}
