use crate::{
    scanner::Scanner,
    token::{Token, TokenKind},
};
use std::borrow::Borrow;

pub fn run(source: &str) -> Vec<Token> {
    let mut scanner = Scanner::new(source);
    execute(&mut scanner)
}

fn execute(scanner: &mut Scanner) -> Vec<Token> {
    let mut tokens = Vec::new();

    while let Some(c) = scanner.next() {
        let token = scan_token(scanner, c);
        let with_span = scanner.create_token(token);
        tokens.push(with_span);
        scanner.shift();
    }
    tokens
}

fn scan_token(scanner: &mut Scanner, c: char) -> TokenKind {
    match c {
        // Single-character tokens
        '{' => TokenKind::LeftCurly,
        '}' => TokenKind::RightCurly,
        '[' => TokenKind::LeftBracket,
        ']' => TokenKind::RightBracket,
        ',' => TokenKind::Comma,
        ':' => TokenKind::Colon,
        // Two-character tokens
        '/' => {
            if scanner.next_match('/') {
                while scanner.peek() != Some(&'\n') && scanner.peek().is_some() {
                    scanner.next();
                }
                TokenKind::LineComment
            } else {
                TokenKind::Slash
            }
        }
        ' ' | '\r' | '\t' | '\n' => whitespace(scanner),
        '"' => string(scanner),
        _ if c.is_ascii_digit() => number(scanner),

        // keywords are reserved identifiers!
        _ if c.is_ascii_alphabetic() => {
            let ident = identifier(scanner, c);
            match ident.borrow() {
                "false" => TokenKind::False,
                "true" => TokenKind::True,
                "null" => TokenKind::Null,
                _ => TokenKind::Unknown,
            }
        }
        _ => TokenKind::Unknown,
    }
}

fn whitespace(scanner: &mut Scanner) -> TokenKind {
    scanner.consume_while(|c| c.is_ascii_whitespace());
    TokenKind::Whitespace
}

fn identifier(scanner: &mut Scanner, first: char) -> String {
    let mut identifier = String::new();
    identifier.push(first);

    scanner
        .consume_while(|c| c.is_ascii_alphanumeric())
        .iter()
        .for_each(|c| identifier.push(*c));

    identifier
}

fn string(scanner: &mut Scanner) -> TokenKind {
    let mut terminated = false;

    // Single quote string - handle escape sequences
    while let Some(c) = scanner.next() {
        if c == '"' {
            terminated = true;
            break;
        } else if c == '\\' {
            // Skip the escaped character
            scanner.next();
        }
    }

    TokenKind::String { terminated }
}

fn number(scanner: &mut Scanner) -> TokenKind {
    scanner.consume_while(|c| c.is_ascii_digit());
    if scanner.peek() == Some(&'.') && scanner.consume_if_next(|c| c.is_ascii_digit()) {
        scanner.consume_while(|c| c.is_ascii_digit());
    }
    TokenKind::Number
}
