#![allow(bad_style)]

use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum SyntaxBranch {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct SyntaxNode {
    kind: SyntaxKind,
    text_len: usize,
    children: Vec<Rc<SyntaxBranch>>,
}

impl SyntaxNode {
    pub fn new(kind: SyntaxKind, text_len: usize, children: Vec<Rc<SyntaxBranch>>) -> Self {
        Self {
            kind,
            text_len,
            children,
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text_len(&self) -> usize {
        self.text_len
    }

    pub fn children(&self) -> &[Rc<SyntaxBranch>] {
        &self.children
    }

    /// Clones the tree creating an entire new subtree that is not just a ref to the old one
    pub fn clone_subtree(&self) -> SyntaxNode {
        todo!("not implemented")
    }

    /// Not sure how this one clones
    pub fn clone_for_update(&self) -> SyntaxNode {
        todo!("not implemented")
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct SyntaxToken {
    kind: SyntaxKind,
    text: String,
}

impl SyntaxToken {
    pub fn new(kind: SyntaxKind, text: String) -> Self {
        Self { kind, text }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind.clone()
    }

    pub fn text(&self) -> &str {
        &self.text
    }
}

impl std::fmt::Display for SyntaxToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text())
    }
}

impl std::fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // For now, just display the kind - in a real implementation,
        // we'd recursively print the tree structure
        write!(f, "{:?}", self.kind)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash, Copy)]
pub enum SyntaxKind {
    ERROR,
    EOF,
    ROOT,
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
