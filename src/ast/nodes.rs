// https://github.com/rust-lang/rust-analyzer/blob/36a70b7435c48837018c71576d7bb4e8f763f501/crates/syntax/src/ast/generated/nodes.rs

use crate::{ast::{AstNode, AstToken}, syntax::*};

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
        self.syntax.children().iter()
            .find_map(|child| {
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

impl AstNode for JsonValue {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind,
            SyntaxKind::OBJECT | SyntaxKind::ARRAY | SyntaxKind::STRING |
            SyntaxKind::NUMBER | SyntaxKind::TRUE | SyntaxKind::FALSE | SyntaxKind::NULL
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
            self.syntax.children().iter()
                .find_map(|child| {
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
            self.syntax.children().iter()
                .find_map(|child| {
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

impl JsonObject {
    pub fn fields(&self) -> impl Iterator<Item = JsonObjectField> {
        self.syntax.children().iter()
            .filter_map(|child| {
                if let SyntaxBranch::Node(node) = child.as_ref() {
                    JsonObjectField::cast(node.clone())
                } else {
                    None
                }
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
        kind == SyntaxKind::OBJECT // For now, fields are represented within objects
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        // This is a simplified implementation - in a real parser,
        // we'd have a separate OBJECT_FIELD syntax kind
        None
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
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
    pub fn elements(&self) -> impl Iterator<Item = JsonValue> {
        self.syntax.children().iter()
            .filter_map(|child| {
                if let SyntaxBranch::Node(node) = child.as_ref() {
                    JsonValue::cast(node.clone())
                } else {
                    None
                }
            })
    }
}
