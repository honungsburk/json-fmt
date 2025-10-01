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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    #[test]
    fn test_basic_json_tokens() {
        let tokens = run(r#"{"key": "value", "number": 123, "array": [true, false, null]}"#);

        let expected = vec![
            TokenKind::LeftCurly,
            TokenKind::String { terminated: true },
            TokenKind::Colon,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Colon,
            TokenKind::Whitespace,
            TokenKind::Number,
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Colon,
            TokenKind::Whitespace,
            TokenKind::LeftBracket,
            TokenKind::True,
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::False,
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::Null,
            TokenKind::RightBracket,
            TokenKind::RightCurly,
        ];

        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, expected);
    }

    #[test]
    fn test_numbers() {
        let tokens = run("123 456.789 0.5");
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![
            TokenKind::Number,
            TokenKind::Whitespace,
            TokenKind::Number,
            TokenKind::Whitespace,
            TokenKind::Number,
        ]);
    }

    #[test]
    fn test_single_character_tokens() {
        let tokens = run("{}[],:{}");
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![
            TokenKind::LeftCurly,
            TokenKind::RightCurly,
            TokenKind::LeftBracket,
            TokenKind::RightBracket,
            TokenKind::Comma,
            TokenKind::Colon,
            TokenKind::LeftCurly,
            TokenKind::RightCurly,
        ]);
    }

    #[test]
    fn test_keywords() {
        let tokens = run("true false null");
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![
            TokenKind::True,
            TokenKind::Whitespace,
            TokenKind::False,
            TokenKind::Whitespace,
            TokenKind::Null,
        ]);
    }

    #[test]
    fn test_unicode_characters() {
        let tokens = run(r#"{"emoji": "🦀", "accented": "café", "chinese": "你好"}"#);

        let expected = vec![
            TokenKind::LeftCurly,
            TokenKind::String { terminated: true },
            TokenKind::Colon,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Colon,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Colon,
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::RightCurly,
        ];

        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, expected);
    }

    #[test]
    fn test_unicode_mathematical_symbols() {
        let tokens = run(r#"{"pi": "π", "sum": "∑", "delta": "Δ"}"#);

        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert!(token_kinds.contains(&TokenKind::String { terminated: true }));
        assert!(token_kinds.contains(&TokenKind::LeftCurly));
        assert!(token_kinds.contains(&TokenKind::RightCurly));
    }

    #[test]
    fn test_unicode_right_to_left() {
        let tokens = run(r#"{"arabic": "مرحبا", "hebrew": "שלום"}"#);

        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert!(token_kinds.iter().filter(|&k| matches!(k, TokenKind::String { terminated: true })).count() >= 4);
    }

    #[test]
    fn test_string_with_escape_sequences() {
        let tokens = run(r#""Hello \"World\"""#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::String { terminated: true }]);
    }

    #[test]
    fn test_string_with_backslashes() {
        let tokens = run(r#""path\\to\\file""#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::String { terminated: true }]);
    }

    #[test]
    fn test_string_with_newline_escape() {
        let tokens = run(r#""line1\nline2""#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::String { terminated: true }]);
    }

    #[test]
    fn test_empty_string() {
        let tokens = run(r#""""#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::String { terminated: true }]);
    }

    #[test]
    fn test_multiple_strings() {
        let tokens = run(r#""first" "second" "third""#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![
            TokenKind::String { terminated: true },
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
            TokenKind::Whitespace,
            TokenKind::String { terminated: true },
        ]);
    }

    #[test]
    fn test_unterminated_string() {
        let tokens = run(r#""unterminated string"#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::String { terminated: false }]);
    }

    #[test]
    fn test_unterminated_string_with_escape() {
        let tokens = run(r#""escaped quote \" but no end"#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::String { terminated: false }]);
    }

    #[test]
    fn test_line_comment() {
        let tokens = run("// this is a comment");
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::LineComment]);
    }

    #[test]
    fn test_json_with_comments() {
        let tokens = run(r#"{"key": "value"} // comment at end"#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert!(token_kinds.contains(&TokenKind::LineComment));
        assert!(token_kinds.contains(&TokenKind::String { terminated: true }));
    }

    #[test]
    fn test_slash_without_comment() {
        let tokens = run("/");
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::Slash]);
    }

    #[test]
    fn test_whitespace_variations() {
        let tokens = run(" \t\n\r ");
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, vec![TokenKind::Whitespace]);
    }

    #[test]
    fn test_unknown_characters() {
        let tokens = run("@#$%");
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert!(token_kinds.iter().all(|&k| k == TokenKind::Unknown));
    }

    #[test]
    fn test_mixed_valid_invalid() {
        let tokens = run(r#"{"valid": true, @ invalid}"#);
        let token_kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert!(token_kinds.contains(&TokenKind::String { terminated: true }));
        assert!(token_kinds.contains(&TokenKind::True));
        assert!(token_kinds.contains(&TokenKind::Unknown));
    }
}
