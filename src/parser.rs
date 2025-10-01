use crate::{
    syntax::{SyntaxBranch, SyntaxKind, SyntaxNode, SyntaxToken},
    token::{Token, TokenKind},
};
use std::rc::Rc;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    offset: usize,
    builder: TreeBuilder,
    errors: Vec<ParseError>,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub token_index: usize,
}

pub struct TreeBuilder {
    stack: Vec<NodeBuilder>,
}

struct NodeBuilder {
    kind: SyntaxKind,
    children: Vec<Rc<SyntaxBranch>>,
    text_len: usize,
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.stack.push(NodeBuilder {
            kind,
            children: Vec::new(),
            text_len: 0,
        });
    }

    pub fn add_token(&mut self, token: Token, source: &str, offset: usize) {
        let start = offset;
        let end = start + token.len;
        let text = if end <= source.len() {
            &source[start..end]
        } else {
            ""
        };

        let syntax_token = SyntaxToken::new(token_kind_to_syntax_kind(token.kind), text.to_string());
        let syntax_branch = Rc::new(SyntaxBranch::Token(syntax_token));

        if let Some(current) = self.stack.last_mut() {
            current.children.push(syntax_branch);
            current.text_len += token.len;
        }
    }

    pub fn finish_node(&mut self) -> Option<SyntaxNode> {
        if let Some(builder) = self.stack.pop() {
            let node = SyntaxNode::new(builder.kind, builder.text_len, builder.children);

            if let Some(parent) = self.stack.last_mut() {
                parent.children.push(Rc::new(SyntaxBranch::Node(node.clone())));
                parent.text_len += builder.text_len;
            }

            Some(node)
        } else {
            None
        }
    }

    fn current_text_len(&self) -> usize {
        self.stack.last().map_or(0, |b| b.text_len)
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            offset: 0,
            builder: TreeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self, source: &str) -> (SyntaxNode, Vec<ParseError>) {
        self.builder.start_node(SyntaxKind::ROOT);

        if self.tokens.is_empty() {
            self.add_error("Empty input".to_string());
        } else {
            self.parse_value(source);
        }

        let root = self.builder.finish_node().unwrap_or_else(|| {
            SyntaxNode::new(SyntaxKind::ERROR, 0, Vec::new())
        });

        (root, self.errors)
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.current + 1)
    }

    fn advance(&mut self, source: &str) -> bool {
        if let Some(token) = self.current_token() {
            let token_clone = token.clone();
            self.builder.add_token(token_clone.clone(), source, self.offset);
            self.offset += token_clone.len;
            self.current += 1;
            true
        } else {
            false
        }
    }

    fn expect_token(&mut self, expected: TokenKind, source: &str) -> bool {
        if let Some(token) = self.current_token() {
            if std::mem::discriminant(&token.kind) == std::mem::discriminant(&expected) {
                self.advance(source);
                true
            } else {
                self.add_error(format!("Expected {:?}, found {:?}", expected, token.kind));
                false
            }
        } else {
            self.add_error(format!("Expected {:?}, found EOF", expected));
            false
        }
    }

    fn add_error(&mut self, message: String) {
        self.errors.push(ParseError {
            message,
            token_index: self.current,
        });
    }

    fn skip_trivia(&mut self, source: &str) {
        while let Some(token) = self.current_token() {
            match token.kind {
                TokenKind::Whitespace | TokenKind::LineComment => {
                    self.advance(source);
                }
                _ => break,
            }
        }
    }

    fn parse_value(&mut self, source: &str) -> bool {
        self.skip_trivia(source);

        if let Some(token) = self.current_token() {
            match token.kind {
                TokenKind::LeftCurly => self.parse_object(source),
                TokenKind::LeftBracket => self.parse_array(source),
                TokenKind::String { .. } => {
                    self.builder.start_node(SyntaxKind::STRING);
                    self.advance(source);
                    self.builder.finish_node();
                    true
                }
                TokenKind::Number => {
                    self.builder.start_node(SyntaxKind::NUMBER);
                    self.advance(source);
                    self.builder.finish_node();
                    true
                }
                TokenKind::True => {
                    self.builder.start_node(SyntaxKind::TRUE);
                    self.advance(source);
                    self.builder.finish_node();
                    true
                }
                TokenKind::False => {
                    self.builder.start_node(SyntaxKind::FALSE);
                    self.advance(source);
                    self.builder.finish_node();
                    true
                }
                TokenKind::Null => {
                    self.builder.start_node(SyntaxKind::NULL);
                    self.advance(source);
                    self.builder.finish_node();
                    true
                }
                _ => {
                    self.add_error(format!("Unexpected token: {:?}", token.kind));
                    self.builder.start_node(SyntaxKind::ERROR);
                    self.advance(source);
                    self.builder.finish_node();
                    false
                }
            }
        } else {
            self.add_error("Expected value, found EOF".to_string());
            false
        }
    }

    fn parse_object(&mut self, source: &str) -> bool {
        self.builder.start_node(SyntaxKind::OBJECT);

        if !self.expect_token(TokenKind::LeftCurly, source) {
            self.builder.finish_node();
            return false;
        }

        self.skip_trivia(source);

        // Handle empty object
        if let Some(token) = self.current_token() {
            if matches!(token.kind, TokenKind::RightCurly) {
                self.advance(source);
                self.builder.finish_node();
                return true;
            }
        }

        // Parse object fields
        loop {
            self.skip_trivia(source);

            // Parse key
            if !self.expect_token(TokenKind::String { terminated: true }, source) {
                self.recover_to_object_sync_point(source);
                break;
            }

            self.skip_trivia(source);

            // Parse colon
            if !self.expect_token(TokenKind::Colon, source) {
                self.recover_to_object_sync_point(source);
                break;
            }

            self.skip_trivia(source);

            // Parse value
            if !self.parse_value(source) {
                self.recover_to_object_sync_point(source);
                break;
            }

            self.skip_trivia(source);

            // Check for comma or end
            if let Some(token) = self.current_token() {
                match token.kind {
                    TokenKind::Comma => {
                        self.advance(source);
                        continue;
                    }
                    TokenKind::RightCurly => {
                        self.advance(source);
                        break;
                    }
                    _ => {
                        self.add_error("Expected ',' or '}' in object".to_string());
                        self.recover_to_object_sync_point(source);
                        break;
                    }
                }
            } else {
                self.add_error("Unterminated object".to_string());
                break;
            }
        }

        self.builder.finish_node();
        true
    }

    fn parse_array(&mut self, source: &str) -> bool {
        self.builder.start_node(SyntaxKind::ARRAY);

        if !self.expect_token(TokenKind::LeftBracket, source) {
            self.builder.finish_node();
            return false;
        }

        self.skip_trivia(source);

        // Handle empty array
        if let Some(token) = self.current_token() {
            if matches!(token.kind, TokenKind::RightBracket) {
                self.advance(source);
                self.builder.finish_node();
                return true;
            }
        }

        // Parse array elements
        loop {
            self.skip_trivia(source);

            if !self.parse_value(source) {
                self.recover_to_array_sync_point(source);
                break;
            }

            self.skip_trivia(source);

            // Check for comma or end
            if let Some(token) = self.current_token() {
                match token.kind {
                    TokenKind::Comma => {
                        self.advance(source);
                        continue;
                    }
                    TokenKind::RightBracket => {
                        self.advance(source);
                        break;
                    }
                    _ => {
                        self.add_error("Expected ',' or ']' in array".to_string());
                        self.recover_to_array_sync_point(source);
                        break;
                    }
                }
            } else {
                self.add_error("Unterminated array".to_string());
                break;
            }
        }

        self.builder.finish_node();
        true
    }

    fn recover_to_object_sync_point(&mut self, source: &str) {
        while let Some(token) = self.current_token() {
            match token.kind {
                TokenKind::Comma | TokenKind::RightCurly => break,
                _ => {
                    self.advance(source);
                }
            }
        }
    }

    fn recover_to_array_sync_point(&mut self, source: &str) {
        while let Some(token) = self.current_token() {
            match token.kind {
                TokenKind::Comma | TokenKind::RightBracket => break,
                _ => {
                    self.advance(source);
                }
            }
        }
    }
}

fn token_kind_to_syntax_kind(token_kind: TokenKind) -> SyntaxKind {
    match token_kind {
        TokenKind::Comma => SyntaxKind::COMMA,
        TokenKind::Colon => SyntaxKind::COLON,
        TokenKind::LeftCurly => SyntaxKind::L_CURLY,
        TokenKind::RightCurly => SyntaxKind::R_CURLY,
        TokenKind::LeftBracket => SyntaxKind::L_BRACK,
        TokenKind::RightBracket => SyntaxKind::R_BRACK,
        TokenKind::String { .. } => SyntaxKind::STRING,
        TokenKind::Number => SyntaxKind::NUMBER,
        TokenKind::True => SyntaxKind::TRUE,
        TokenKind::False => SyntaxKind::FALSE,
        TokenKind::Null => SyntaxKind::NULL,
        TokenKind::Whitespace => SyntaxKind::WHITESPACE,
        TokenKind::LineComment => SyntaxKind::COMMENT,
        TokenKind::Slash | TokenKind::Unknown | TokenKind::Eof => SyntaxKind::ERROR,
    }
}

pub fn parse(source: &str) -> (SyntaxNode, Vec<ParseError>) {
    let tokens = crate::tokenizer::run(source);
    let parser = Parser::new(tokens);
    parser.parse(source)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::nodes::*;
    use crate::ast::AstNode;

    #[test]
    fn test_parse_simple_string() {
        let (tree, errors) = parse(r#""hello""#);
        assert!(errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_string().is_some());
    }

    #[test]
    fn test_parse_number() {
        let (tree, errors) = parse("123");
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_number().is_some());
    }

    #[test]
    fn test_parse_boolean_literals() {
        let (tree, errors) = parse("true");
        assert!(errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);

        let (tree, errors) = parse("false");
        assert!(errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);

        let (tree, errors) = parse("null");
        assert!(errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);
    }

    #[test]
    fn test_parse_empty_object() {
        let (tree, errors) = parse("{}");
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let object = value.as_object().unwrap();
        assert_eq!(object.fields().count(), 0);
    }

    #[test]
    fn test_parse_simple_object() {
        let (tree, errors) = parse(r#"{"key": "value"}"#);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_object().is_some());
    }

    #[test]
    fn test_parse_nested_object() {
        let (tree, errors) = parse(r#"{"outer": {"inner": "value"}}"#);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_object().is_some());
    }

    #[test]
    fn test_parse_empty_array() {
        let (tree, errors) = parse("[]");
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let array = value.as_array().unwrap();
        assert_eq!(array.elements().count(), 0);
    }

    #[test]
    fn test_parse_simple_array() {
        let (tree, errors) = parse(r#"[1, 2, 3]"#);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let array = value.as_array().unwrap();
        assert_eq!(array.elements().count(), 3);
    }

    #[test]
    fn test_parse_mixed_array() {
        let (tree, errors) = parse(r#"[1, "hello", true, null]"#);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let array = value.as_array().unwrap();
        assert_eq!(array.elements().count(), 4);
    }

    #[test]
    fn test_parse_complex_json() {
        let json = r#"{
            "name": "John",
            "age": 30,
            "active": true,
            "address": {
                "street": "123 Main St",
                "city": "Anytown"
            },
            "hobbies": ["reading", "swimming", "coding"]
        }"#;

        let (tree, errors) = parse(json);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_object().is_some());
    }

    #[test]
    fn test_parse_with_whitespace_and_comments() {
        let json = r#"{
            "key": "value", // this is a comment
            "number": 42
        }"#;

        let (tree, errors) = parse(json);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_object().is_some());
    }

    #[test]
    fn test_parse_unterminated_string_recovers() {
        let (tree, errors) = parse(r#"{"key": "unterminated"#);
        assert!(!errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);

        // Should still create some tree structure despite the error
        let root = Root::cast(tree).unwrap();
        assert!(root.value().is_some());
    }

    #[test]
    fn test_parse_invalid_json_recovers() {
        let (tree, errors) = parse(r#"{"key": invalid_value}"#);
        assert!(!errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);

        // Parser should recover and continue
        let root = Root::cast(tree).unwrap();
        assert!(root.value().is_some());
    }

    #[test]
    fn test_parse_missing_comma() {
        let (tree, errors) = parse(r#"{"key1": "value1" "key2": "value2"}"#);
        assert!(!errors.is_empty());

        // Should still parse and recover
        let root = Root::cast(tree).unwrap();
        assert!(root.value().is_some());
    }

    #[test]
    fn test_parse_missing_closing_brace() {
        let (tree, errors) = parse(r#"{"key": "value""#);
        assert!(!errors.is_empty());

        // Should still create object structure
        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_object().is_some());
    }

    #[test]
    fn test_parse_unicode_strings() {
        let (tree, errors) = parse(r#"{"emoji": "🦀", "chinese": "你好"}"#);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_object().is_some());
    }

    #[test]
    fn test_parse_floating_point_numbers() {
        let (tree, errors) = parse("123.456");
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        assert!(value.as_number().is_some());
    }

    #[test]
    fn test_parse_empty_input() {
        let (tree, errors) = parse("");
        assert!(!errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);
    }

    #[test]
    fn test_parse_only_whitespace() {
        let (tree, errors) = parse("   \t\n  ");
        assert!(!errors.is_empty());
        assert_eq!(tree.kind(), SyntaxKind::ROOT);
    }
}