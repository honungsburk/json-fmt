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
    /// Pending trivia that will be attached as leading trivia to the next token
    pending_leading_trivia: Vec<SyntaxToken>,
    source: String,
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

    pub fn add_token_with_trivia(
        &mut self,
        token: Token,
        source: &str,
        offset: usize,
        leading_trivia: Vec<SyntaxToken>,
        trailing_trivia: Vec<SyntaxToken>,
    ) {
        let start = offset;
        let end = start + token.len;
        let text = if end <= source.len() {
            &source[start..end]
        } else {
            panic!("This branch should never be reached?");
        };

        let syntax_token = SyntaxToken::new_with_trivia(
            token_kind_to_syntax_kind(token.kind),
            text.to_string(),
            leading_trivia,
            trailing_trivia,
        );
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
                parent
                    .children
                    .push(Rc::new(SyntaxBranch::Node(node.clone())));
                parent.text_len += builder.text_len;
            }

            Some(node)
        } else {
            None
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: &str) -> Self {
        let mut parser = Self {
            tokens,
            current: 0,
            offset: 0,
            builder: TreeBuilder::new(),
            errors: Vec::new(),
            pending_leading_trivia: Vec::new(),
            source: source.to_string(),
        };
        // Prime the pump: collect any leading trivia before the first token
        parser.collect_leading_trivia();
        parser
    }

    pub fn parse(mut self) -> (SyntaxNode, Vec<ParseError>) {
        self.builder.start_node(SyntaxKind::ROOT);

        if self.tokens.is_empty() {
            self.add_error("Empty input".to_string());
        } else {
            self.parse_value();
        }

        let root = self
            .builder
            .finish_node()
            .unwrap_or_else(|| SyntaxNode::new(SyntaxKind::ERROR, 0, Vec::new()));

        (root, self.errors)
    }

    fn get_token_slice(&self, token: &Token) -> &str {
        let start = self.offset;
        let end = start + token.len;
        if end <= self.source.len() {
            &self.source[start..end]
        } else {
            ""
        }
    }

    /// Returns the current significant (non-trivia) token
    /// Trivia is already collected into pending_leading_trivia
    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    /// Skip all trivia tokens and collect them into pending_leading_trivia
    fn collect_leading_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.current) {
            match token.kind {
                TokenKind::Whitespace | TokenKind::LineComment => {
                    let text = self.get_token_slice(token);

                    let trivia_token =
                        SyntaxToken::new(token_kind_to_syntax_kind(token.kind), text.to_string());
                    self.pending_leading_trivia.push(trivia_token);

                    self.offset += token.len;
                    self.current += 1;
                }
                _ => break,
            }
        }
    }

    /// Collect trailing trivia (whitespace without newlines, line comments)
    fn collect_trailing_trivia(&mut self) -> Vec<SyntaxToken> {
        let mut trivia = Vec::new();

        while let Some(token) = self.tokens.get(self.current) {
            match token.kind {
                TokenKind::Whitespace => {
                    let text = self.get_token_slice(token);

                    // Stop at newlines - they start leading trivia for next token
                    if text.contains('\n') {
                        break;
                    }

                    let trivia_token =
                        SyntaxToken::new(token_kind_to_syntax_kind(token.kind), text.to_string());
                    trivia.push(trivia_token);

                    self.offset += token.len;
                    self.current += 1;
                }
                TokenKind::LineComment => {
                    let text = self.get_token_slice(token);

                    let trivia_token =
                        SyntaxToken::new(token_kind_to_syntax_kind(token.kind), text.to_string());
                    trivia.push(trivia_token);

                    self.offset += token.len;
                    self.current += 1;
                }
                _ => break,
            }
        }
        trivia
    }

    /// Advance to the next token, automatically attaching trivia
    fn advance(&mut self) -> bool {
        if let Some(token) = self.current_token() {
            let token_clone = token.clone();
            let current_offset = self.offset;

            // Take pending leading trivia
            let leading_trivia = std::mem::take(&mut self.pending_leading_trivia);

            // Advance past the main token
            self.offset += token_clone.len;
            self.current += 1;

            // Collect trailing trivia
            let trailing_trivia = self.collect_trailing_trivia();

            // Add token with all trivia
            self.builder.add_token_with_trivia(
                token_clone,
                &self.source,
                current_offset,
                leading_trivia,
                trailing_trivia,
            );

            // Skip any leading trivia for the next token
            self.collect_leading_trivia();

            true
        } else {
            false
        }
    }

    /// Expect a specific token kind and advance if it matches
    fn expect_token(&mut self, expected: TokenKind) -> bool {
        if let Some(token) = self.current_token() {
            if std::mem::discriminant(&token.kind) == std::mem::discriminant(&expected) {
                self.advance();
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

    fn parse_value(&mut self) -> bool {
        if let Some(token) = self.current_token() {
            match token.kind {
                TokenKind::LeftCurly => self.parse_object(),
                TokenKind::LeftBracket => self.parse_array(),
                TokenKind::String { .. } => {
                    self.builder.start_node(SyntaxKind::STRING);
                    self.advance();
                    self.builder.finish_node();
                    true
                }
                TokenKind::Number => {
                    self.builder.start_node(SyntaxKind::NUMBER);
                    self.advance();
                    self.builder.finish_node();
                    true
                }
                TokenKind::True => {
                    self.builder.start_node(SyntaxKind::TRUE);
                    self.advance();
                    self.builder.finish_node();
                    true
                }
                TokenKind::False => {
                    self.builder.start_node(SyntaxKind::FALSE);
                    self.advance();
                    self.builder.finish_node();
                    true
                }
                TokenKind::Null => {
                    self.builder.start_node(SyntaxKind::NULL);
                    self.advance();
                    self.builder.finish_node();
                    true
                }
                _ => {
                    self.add_error(format!("Unexpected token: {:?}", token.kind));
                    self.builder.start_node(SyntaxKind::ERROR);
                    self.advance();
                    self.builder.finish_node();
                    false
                }
            }
        } else {
            self.add_error("Expected value, found EOF".to_string());
            false
        }
    }

    fn parse_object(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::OBJECT);

        if !self.expect_token(TokenKind::LeftCurly) {
            self.builder.finish_node();
            return false;
        }

        // Check for empty object
        if let Some(token) = self.current_token() {
            if matches!(token.kind, TokenKind::RightCurly) {
                self.advance();
                self.builder.finish_node();
                return true;
            }
        }

        // Parse object fields
        loop {
            if !self.parse_object_field() {
                self.recover_to_object_sync_point();
                break;
            }

            // Check for comma or end
            if let Some(token) = self.current_token() {
                match token.kind {
                    TokenKind::Comma => {
                        self.advance();
                        continue;
                    }
                    TokenKind::RightCurly => {
                        self.advance();
                        break;
                    }
                    _ => {
                        self.add_error("Expected ',' or '}' in object".to_string());
                        self.recover_to_object_sync_point();
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

    fn parse_object_field(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::OBJECT_FIELD);

        // Parse key
        if !self.expect_token(TokenKind::String { terminated: true }) {
            self.builder.finish_node();
            return false;
        }

        // Parse colon
        if !self.expect_token(TokenKind::Colon) {
            self.builder.finish_node();
            return false;
        }

        // Parse value
        if !self.parse_value() {
            self.builder.finish_node();
            return false;
        }

        self.builder.finish_node();
        true
    }

    fn parse_array(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::ARRAY);

        if !self.expect_token(TokenKind::LeftBracket) {
            self.builder.finish_node();
            return false;
        }

        // Handle empty array
        if let Some(token) = self.current_token() {
            if matches!(token.kind, TokenKind::RightBracket) {
                self.advance();
                self.builder.finish_node();
                return true;
            }
        }

        // Parse array elements
        loop {
            if !self.parse_array_element() {
                self.recover_to_array_sync_point();
                break;
            }

            // Check for comma or end
            if let Some(token) = self.current_token() {
                match token.kind {
                    TokenKind::Comma => {
                        self.advance();
                        continue;
                    }
                    TokenKind::RightBracket => {
                        self.advance();
                        break;
                    }
                    _ => {
                        self.add_error("Expected ',' or ']' in array".to_string());
                        self.recover_to_array_sync_point();
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

    fn parse_array_element(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::ARRAY_ELEMENT);

        if !self.parse_value() {
            self.builder.finish_node();
            return false;
        }

        self.builder.finish_node();
        true
    }

    fn recover_to_object_sync_point(&mut self) {
        while let Some(token) = self.current_token() {
            match token.kind {
                TokenKind::Comma | TokenKind::RightCurly => break,
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn recover_to_array_sync_point(&mut self) {
        while let Some(token) = self.current_token() {
            match token.kind {
                TokenKind::Comma | TokenKind::RightBracket => break,
                _ => {
                    self.advance();
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
    let parser = Parser::new(tokens, source);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::AstNode;
    use crate::ast::nodes::*;

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

    #[test]
    fn test_trivia_preservation_in_object() {
        let json = r#"{
            "key1": "value1",  // comment1
            "key2": "value2"   // comment2
        }"#;

        let (tree, errors) = parse(json);
        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let object = value.as_object().unwrap();

        // Check that we have OBJECT_FIELD nodes
        let fields: Vec<_> = object.fields().collect();
        assert_eq!(fields.len(), 2);

        // Verify field structure
        let first_field = &fields[0];
        assert!(first_field.key().is_some());
        assert!(first_field.value().is_some());
    }

    #[test]
    fn test_trivia_preservation_in_array() {
        let json = r#"[
            1,  // first number
            2,  // second number
            3   // third number
        ]"#;

        let (tree, errors) = parse(json);
        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let array = value.as_array().unwrap();

        // Check that we have ARRAY_ELEMENT nodes
        let elements: Vec<_> = array.elements().collect();
        assert_eq!(elements.len(), 3);

        // Verify element structure
        for element in elements {
            assert!(element.value().is_some());
        }
    }

    #[test]
    fn test_object_field_structure() {
        let (tree, errors) = parse(r#"{"name": "John", "age": 30}"#);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let object = value.as_object().unwrap();

        let fields: Vec<_> = object.fields().collect();
        assert_eq!(fields.len(), 2);

        // Check first field
        let first_field = &fields[0];
        assert!(first_field.key().is_some());
        assert!(first_field.value().is_some());

        let key = first_field.key().unwrap();
        assert_eq!(key.text(), r#""name""#);
    }

    #[test]
    fn test_array_element_structure() {
        let (tree, errors) = parse(r#"[1, "hello", true]"#);
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let array = value.as_array().unwrap();

        let elements: Vec<_> = array.elements().collect();
        assert_eq!(elements.len(), 3);

        // Check each element has a value
        for element in elements {
            assert!(element.value().is_some());
        }
    }

    #[test]
    fn test_nested_structure_with_trivia() {
        let json = r#"{
            "user": {
                "name": "Alice",  // user name
                "hobbies": [
                    "reading",  // hobby 1
                    "coding"    // hobby 2
                ]
            }
        }"#;

        let (tree, errors) = parse(json);
        assert!(errors.is_empty(), "Errors: {:?}", errors);

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let outer_object = value.as_object().unwrap();

        let fields: Vec<_> = outer_object.fields().collect();
        assert_eq!(fields.len(), 1);

        let user_field = &fields[0];
        let user_value = user_field.value().unwrap();
        let user_object = user_value.as_object().unwrap();

        let user_fields: Vec<_> = user_object.fields().collect();
        assert_eq!(user_fields.len(), 2);

        // Find hobbies field
        let hobbies_field = user_fields
            .iter()
            .find(|field| field.key().map_or(false, |k| k.text() == r#""hobbies""#))
            .unwrap();

        let hobbies_value = hobbies_field.value().unwrap();
        let hobbies_array = hobbies_value.as_array().unwrap();

        let hobby_elements: Vec<_> = hobbies_array.elements().collect();
        assert_eq!(hobby_elements.len(), 2);
    }

    #[test]
    fn test_empty_structures_preserve_trivia() {
        let (tree, errors) = parse("{ }");
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let object = value.as_object().unwrap();

        let fields: Vec<_> = object.fields().collect();
        assert_eq!(fields.len(), 0);

        let (tree, errors) = parse("[ ]");
        assert!(errors.is_empty());

        let root = Root::cast(tree).unwrap();
        let value = root.value().unwrap();
        let array = value.as_array().unwrap();

        let elements: Vec<_> = array.elements().collect();
        assert_eq!(elements.len(), 0);
    }
}
