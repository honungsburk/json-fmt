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
        let new_children = self
            .children
            .iter()
            .map(|child| {
                Rc::new(match child.as_ref() {
                    SyntaxBranch::Node(node) => SyntaxBranch::Node(node.clone_subtree()),
                    SyntaxBranch::Token(token) => SyntaxBranch::Token(token.clone()),
                })
            })
            .collect();

        SyntaxNode {
            kind: self.kind,
            text_len: self.text_len,
            children: new_children,
        }
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
    leading_trivia: Vec<SyntaxToken>,
    trailing_trivia: Vec<SyntaxToken>,
}

impl SyntaxToken {
    pub fn new(kind: SyntaxKind, text: String) -> Self {
        Self {
            kind,
            text,
            leading_trivia: Vec::new(),
            trailing_trivia: Vec::new(),
        }
    }

    pub fn new_with_trivia(
        kind: SyntaxKind,
        text: String,
        leading_trivia: Vec<SyntaxToken>,
        trailing_trivia: Vec<SyntaxToken>,
    ) -> Self {
        Self {
            kind,
            text,
            leading_trivia,
            trailing_trivia,
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind.clone()
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    /// Only get leading trivia that are comments
    pub fn leading_comments(&self) -> impl Iterator<Item = &SyntaxToken> {
        self.leading_trivia
            .iter()
            .filter(|t| t.kind() == SyntaxKind::COMMENT)
    }

    /// Only get trailing trivia that are comments
    pub fn trailing_comments(&self) -> impl Iterator<Item = &SyntaxToken> {
        self.trailing_trivia
            .iter()
            .filter(|t| t.kind() == SyntaxKind::COMMENT)
    }

    pub fn leading_trivia(&self) -> &[SyntaxToken] {
        &self.leading_trivia
    }

    pub fn trailing_trivia(&self) -> &[SyntaxToken] {
        &self.trailing_trivia
    }

    pub fn full_text(&self) -> String {
        let mut result = String::new();

        // Add leading trivia
        for trivia in &self.leading_trivia {
            result.push_str(trivia.text());
        }

        // Add main text
        result.push_str(&self.text);

        // Add trailing trivia
        for trivia in &self.trailing_trivia {
            result.push_str(trivia.text());
        }

        result
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
    OBJECT_FIELD,  // "key": value pair
    ARRAY_ELEMENT, // Individual array element wrapper

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clone_subtree_creates_new_rc_instances() {
        // Create a simple tree: ROOT -> [Token("hello"), Node(OBJECT -> [Token("world")])]
        let token1 = Rc::new(SyntaxBranch::Token(SyntaxToken::new(
            SyntaxKind::STRING,
            "hello".to_string(),
        )));
        let token2 = Rc::new(SyntaxBranch::Token(SyntaxToken::new(
            SyntaxKind::STRING,
            "world".to_string(),
        )));

        let inner_node = SyntaxNode::new(SyntaxKind::OBJECT, 5, vec![token2.clone()]);
        let inner_node_rc = Rc::new(SyntaxBranch::Node(inner_node));

        let root = SyntaxNode::new(
            SyntaxKind::ROOT,
            10,
            vec![token1.clone(), inner_node_rc.clone()],
        );

        // Clone the subtree
        let cloned = root.clone_subtree();

        // Verify the cloned tree has the same structure and values
        assert_eq!(root.kind(), cloned.kind());
        assert_eq!(root.text_len(), cloned.text_len());
        assert_eq!(root.children().len(), cloned.children().len());

        // Verify that the Rc pointers are different (not the same Rc instance)
        let original_child1_ptr = Rc::as_ptr(&root.children()[0]);
        let cloned_child1_ptr = Rc::as_ptr(&cloned.children()[0]);
        assert_ne!(
            original_child1_ptr, cloned_child1_ptr,
            "Child Rc should be different"
        );

        let original_child2_ptr = Rc::as_ptr(&root.children()[1]);
        let cloned_child2_ptr = Rc::as_ptr(&cloned.children()[1]);
        assert_ne!(
            original_child2_ptr, cloned_child2_ptr,
            "Child Rc should be different"
        );

        // Verify nested node also has different Rc
        if let SyntaxBranch::Node(original_inner) = root.children()[1].as_ref() {
            if let SyntaxBranch::Node(cloned_inner) = cloned.children()[1].as_ref() {
                let original_inner_child_ptr = Rc::as_ptr(&original_inner.children()[0]);
                let cloned_inner_child_ptr = Rc::as_ptr(&cloned_inner.children()[0]);
                assert_ne!(
                    original_inner_child_ptr, cloned_inner_child_ptr,
                    "Nested child Rc should be different"
                );
            } else {
                panic!("Cloned child should be a Node");
            }
        } else {
            panic!("Original child should be a Node");
        }
    }

    #[test]
    fn test_clone_subtree_preserves_structure() {
        // Create a more complex tree
        let token1 = Rc::new(SyntaxBranch::Token(SyntaxToken::new(
            SyntaxKind::L_CURLY,
            "{".to_string(),
        )));
        let token2 = Rc::new(SyntaxBranch::Token(SyntaxToken::new(
            SyntaxKind::STRING,
            "key".to_string(),
        )));
        let token3 = Rc::new(SyntaxBranch::Token(SyntaxToken::new(
            SyntaxKind::NUMBER,
            "42".to_string(),
        )));

        let field_node = SyntaxNode::new(
            SyntaxKind::OBJECT_FIELD,
            7,
            vec![token2.clone(), token3.clone()],
        );
        let field_rc = Rc::new(SyntaxBranch::Node(field_node));

        let object = SyntaxNode::new(
            SyntaxKind::OBJECT,
            10,
            vec![token1.clone(), field_rc.clone()],
        );

        let cloned = object.clone_subtree();

        // Verify structure is preserved
        assert_eq!(object.kind(), cloned.kind());
        assert_eq!(object.text_len(), cloned.text_len());
        assert_eq!(object.children().len(), cloned.children().len());

        // Verify nested structure
        if let SyntaxBranch::Node(original_field) = object.children()[1].as_ref() {
            if let SyntaxBranch::Node(cloned_field) = cloned.children()[1].as_ref() {
                assert_eq!(original_field.kind(), cloned_field.kind());
                assert_eq!(original_field.text_len(), cloned_field.text_len());
                assert_eq!(
                    original_field.children().len(),
                    cloned_field.children().len()
                );
            } else {
                panic!("Cloned field should be a Node");
            }
        }
    }

    #[test]
    fn test_clone_subtree_empty_node() {
        let empty_node = SyntaxNode::new(SyntaxKind::ROOT, 0, vec![]);
        let cloned = empty_node.clone_subtree();

        assert_eq!(empty_node.kind(), cloned.kind());
        assert_eq!(empty_node.text_len(), cloned.text_len());
        assert_eq!(empty_node.children().len(), 0);
        assert_eq!(cloned.children().len(), 0);
    }

    #[test]
    fn test_clone_subtree_deep_nesting() {
        // Create a deeply nested structure
        let deepest_token = Rc::new(SyntaxBranch::Token(SyntaxToken::new(
            SyntaxKind::STRING,
            "deep".to_string(),
        )));
        let level3 = SyntaxNode::new(SyntaxKind::ARRAY, 4, vec![deepest_token.clone()]);
        let level3_rc = Rc::new(SyntaxBranch::Node(level3));

        let level2 = SyntaxNode::new(SyntaxKind::OBJECT, 4, vec![level3_rc.clone()]);
        let level2_rc = Rc::new(SyntaxBranch::Node(level2));

        let level1 = SyntaxNode::new(SyntaxKind::ARRAY, 4, vec![level2_rc.clone()]);
        let level1_rc = Rc::new(SyntaxBranch::Node(level1));

        let root = SyntaxNode::new(SyntaxKind::ROOT, 4, vec![level1_rc.clone()]);

        let cloned = root.clone_subtree();

        // Verify all levels have different Rc pointers
        let root_child_ptr = Rc::as_ptr(&root.children()[0]);
        let cloned_child_ptr = Rc::as_ptr(&cloned.children()[0]);
        assert_ne!(root_child_ptr, cloned_child_ptr);

        // Navigate through levels and verify Rc pointers are different at each level
        if let SyntaxBranch::Node(original_l1) = root.children()[0].as_ref() {
            if let SyntaxBranch::Node(cloned_l1) = cloned.children()[0].as_ref() {
                let l1_child_ptr = Rc::as_ptr(&original_l1.children()[0]);
                let cloned_l1_child_ptr = Rc::as_ptr(&cloned_l1.children()[0]);
                assert_ne!(l1_child_ptr, cloned_l1_child_ptr);
            }
        }
    }
}
