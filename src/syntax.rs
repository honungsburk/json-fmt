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

#[derive(PartialEq, Eq, Clone, Debug, Hash, Copy)]
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
