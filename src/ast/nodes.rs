// https://github.com/rust-lang/rust-analyzer/blob/36a70b7435c48837018c71576d7bb4e8f763f501/crates/syntax/src/ast/generated/nodes.rs

use crate::{
    ast::{AstNode, AstToken},
    syntax::*,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Root {
    pub(crate) syntax: SyntaxNode,
}

impl std::fmt::Display for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}

impl AstNode for Root {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ROOT
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl Root {
    pub fn value(&self) -> Option<JsonValue> {
        self.syntax.children().iter().find_map(|child| {
            if let SyntaxBranch::Node(node) = child.as_ref() {
                JsonValue::cast(node.clone())
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JsonValue {
    pub(crate) syntax: SyntaxNode,
}

impl std::fmt::Display for JsonValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}
impl JsonValue {
    pub fn is_object(&self) -> bool {
        self.syntax.kind() == SyntaxKind::OBJECT
    }

    pub fn is_array(&self) -> bool {
        self.syntax.kind() == SyntaxKind::ARRAY
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self.syntax.kind(),
            SyntaxKind::STRING
                | SyntaxKind::NUMBER
                | SyntaxKind::TRUE
                | SyntaxKind::FALSE
                | SyntaxKind::NULL
        )
    }

    pub fn value_kind(&self) -> ValueKind {
        match self.syntax.kind() {
            SyntaxKind::OBJECT => ValueKind::Object,
            SyntaxKind::ARRAY => ValueKind::Array,
            SyntaxKind::STRING => ValueKind::String,
            SyntaxKind::NUMBER => ValueKind::Number,
            SyntaxKind::TRUE | SyntaxKind::FALSE => ValueKind::Boolean,
            SyntaxKind::NULL => ValueKind::Null,
            _ => ValueKind::Invalid,
        }
    }
}

pub enum ValueKind {
    Object,
    Array,
    String,
    Number,
    Boolean,
    Null,
    Invalid,
}

impl AstNode for JsonValue {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::OBJECT
                | SyntaxKind::ARRAY
                | SyntaxKind::STRING
                | SyntaxKind::NUMBER
                | SyntaxKind::TRUE
                | SyntaxKind::FALSE
                | SyntaxKind::NULL
                | SyntaxKind::ARRAY_ELEMENT
        )
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl JsonValue {
    pub fn as_object(&self) -> Option<JsonObject> {
        if self.syntax.kind() == SyntaxKind::OBJECT {
            JsonObject::cast(self.syntax.clone())
        } else {
            None
        }
    }

    pub fn as_array(&self) -> Option<JsonArray> {
        if self.syntax.kind() == SyntaxKind::ARRAY {
            JsonArray::cast(self.syntax.clone())
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<crate::ast::tokens::String> {
        if self.syntax.kind() == SyntaxKind::STRING {
            self.syntax.children().iter().find_map(|child| {
                if let SyntaxBranch::Token(token) = child.as_ref() {
                    crate::ast::tokens::String::cast(token.clone())
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    pub fn as_number(&self) -> Option<crate::ast::tokens::Number> {
        if self.syntax.kind() == SyntaxKind::NUMBER {
            self.syntax.children().iter().find_map(|child| {
                if let SyntaxBranch::Token(token) = child.as_ref() {
                    crate::ast::tokens::Number::cast(token.clone())
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    /// For primitive values (STRING, NUMBER, TRUE, FALSE, NULL), get the token at index 0
    /// Primitive values always have exactly one child: a token
    pub fn primitive_token(&self) -> Option<&SyntaxToken> {
        const TOKEN_INDEX: usize = 0;
        match self.syntax.children().get(TOKEN_INDEX)?.as_ref() {
            SyntaxBranch::Token(token) => Some(token),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JsonObject {
    pub(crate) syntax: SyntaxNode,
}

impl std::fmt::Display for JsonObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}

impl AstNode for JsonObject {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::OBJECT
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

/// Represents a field in an object along with its trailing comma (if present)
#[derive(Debug, Clone)]
pub struct ObjectFieldWithComma {
    pub field: JsonObjectField,
    pub comma: Option<SyntaxToken>,
}

impl JsonObject {
    pub fn l_curly_token(&self) -> Option<&SyntaxToken> {
        match self.syntax.children().first()?.as_ref() {
            SyntaxBranch::Token(token) if token.kind() == SyntaxKind::L_CURLY => Some(token),
            _ => None,
        }
    }

    pub fn r_curly_token(&self) -> Option<&SyntaxToken> {
        match self.syntax.children().last()?.as_ref() {
            SyntaxBranch::Token(token) if token.kind() == SyntaxKind::R_CURLY => Some(token),
            _ => None,
        }
    }

    pub fn fields(&self) -> impl Iterator<Item = JsonObjectField> {
        self.syntax.children().iter().filter_map(|child| {
            if let SyntaxBranch::Node(node) = child.as_ref() {
                JsonObjectField::cast(node.clone())
            } else {
                None
            }
        })
    }

    /// Returns an iterator over fields with their trailing commas
    /// Structure: L_CURLY, FIELD, [COMMA, FIELD]*, R_CURLY
    pub fn fields_with_commas(&self) -> impl Iterator<Item = ObjectFieldWithComma> {
        let children = self.syntax.children();
        let mut iter = children.iter().peekable();

        std::iter::from_fn(move || {
            // Skip until we find a field node
            while let Some(child) = iter.next() {
                if let SyntaxBranch::Node(node) = child.as_ref() {
                    if let Some(field) = JsonObjectField::cast(node.clone()) {
                        // Check if next token is a comma and clone it
                        let comma = iter
                            .peek()
                            .and_then(|next_child| match next_child.as_ref() {
                                SyntaxBranch::Token(token) if token.kind() == SyntaxKind::COMMA => {
                                    Some(token.clone())
                                }
                                _ => None,
                            });

                        // If we found a comma, consume it
                        if comma.is_some() {
                            iter.next();
                        }

                        return Some(ObjectFieldWithComma { field, comma });
                    }
                }
            }
            None
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JsonObjectField {
    pub(crate) syntax: SyntaxNode,
}

impl std::fmt::Display for JsonObjectField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}

impl AstNode for JsonObjectField {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::OBJECT_FIELD
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl JsonObjectField {
    /// OBJECT_FIELD always has exactly 3 children: [STRING, COLON, JsonValue]
    const KEY_INDEX: usize = 0;
    const COLON_INDEX: usize = 1;
    const VALUE_INDEX: usize = 2;

    pub fn key(&self) -> Option<&SyntaxToken> {
        match self.syntax.children().get(Self::KEY_INDEX)?.as_ref() {
            SyntaxBranch::Token(token) => Some(token),
            _ => None,
        }
    }

    pub fn colon(&self) -> Option<&SyntaxToken> {
        match self.syntax.children().get(Self::COLON_INDEX)?.as_ref() {
            SyntaxBranch::Token(token) => Some(token),
            _ => None,
        }
    }

    pub fn value(&self) -> Option<JsonValue> {
        match self.syntax.children().get(Self::VALUE_INDEX)?.as_ref() {
            SyntaxBranch::Node(node) => JsonValue::cast(node.clone()),
            _ => None,
        }
    }
}

/// Represents an element in an array along with its trailing comma (if present)
#[derive(Debug, Clone)]
pub struct ArrayElementWithComma {
    pub element: JsonArrayElement,
    pub comma: Option<SyntaxToken>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JsonArray {
    pub(crate) syntax: SyntaxNode,
}

impl std::fmt::Display for JsonArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}

impl AstNode for JsonArray {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ARRAY
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl JsonArray {
    pub fn l_bracket_token(&self) -> Option<&SyntaxToken> {
        match self.syntax.children().first()?.as_ref() {
            SyntaxBranch::Token(token) if token.kind() == SyntaxKind::L_BRACK => Some(token),
            _ => None,
        }
    }

    pub fn r_bracket_token(&self) -> Option<&SyntaxToken> {
        match self.syntax.children().last()?.as_ref() {
            SyntaxBranch::Token(token) if token.kind() == SyntaxKind::R_BRACK => Some(token),
            _ => None,
        }
    }

    pub fn elements(&self) -> impl Iterator<Item = JsonArrayElement> {
        self.syntax.children().iter().filter_map(|child| {
            if let SyntaxBranch::Node(node) = child.as_ref() {
                JsonArrayElement::cast(node.clone())
            } else {
                None
            }
        })
    }

    /// Returns an iterator over elements with their trailing commas
    /// Structure: L_BRACK, ELEMENT, [COMMA, ELEMENT]*, R_BRACK
    pub fn elements_with_commas(&self) -> impl Iterator<Item = ArrayElementWithComma> {
        let children = self.syntax.children();
        let mut iter = children.iter().peekable();

        std::iter::from_fn(move || {
            // Skip until we find an element node
            while let Some(child) = iter.next() {
                if let SyntaxBranch::Node(node) = child.as_ref() {
                    if let Some(element) = JsonArrayElement::cast(node.clone()) {
                        // Check if next token is a comma and clone it
                        let comma = iter
                            .peek()
                            .and_then(|next_child| match next_child.as_ref() {
                                SyntaxBranch::Token(token) if token.kind() == SyntaxKind::COMMA => {
                                    Some(token.clone())
                                }
                                _ => None,
                            });

                        // If we found a comma, consume it
                        if comma.is_some() {
                            iter.next();
                        }

                        return Some(ArrayElementWithComma { element, comma });
                    }
                }
            }
            None
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JsonArrayElement {
    pub(crate) syntax: SyntaxNode,
}

impl std::fmt::Display for JsonArrayElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}

impl AstNode for JsonArrayElement {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ARRAY_ELEMENT
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl JsonArrayElement {
    /// ARRAY_ELEMENT always has exactly 1 child: [JsonValue]
    const VALUE_INDEX: usize = 0;

    pub fn value(&self) -> Option<JsonValue> {
        match self.syntax.children().get(Self::VALUE_INDEX)?.as_ref() {
            SyntaxBranch::Node(node) => JsonValue::cast(node.clone()),
            _ => None,
        }
    }
}
