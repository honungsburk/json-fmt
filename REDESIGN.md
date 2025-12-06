# AST and Syntax Tree Redesign Document

**Purpose:** This document outlines concrete improvements to the json-fmt AST and syntax tree design before applying these patterns to a logic programming language.

**Last Updated:** 2025-12-03

---

## Table of Contents

1. [Critical Issues (Fix First)](#1-critical-issues-fix-first)
2. [Consistency in API Design](#2-consistency-in-api-design)
3. [Reduce Formatter's Structural Knowledge](#3-reduce-formatters-structural-knowledge)
4. [Extract Generalizable Patterns](#4-extract-generalizable-patterns)
5. [Robust Error Handling](#5-robust-error-handling)
6. [Documentation Standards](#6-documentation-standards)
7. [Performance Optimizations](#7-performance-optimizations)
8. [Eliminate Code Duplication](#8-eliminate-code-duplication)
9. [Architecture for Logic Programming Language](#9-architecture-for-logic-programming-language)

---

## 1. Critical Issues (Fix First)

### 1.1 Remove Panic Calls in Production Code

COMPLETE

### 1.2 Implement or Remove Incomplete Methods

COMPLETE

### 1.3 Add Proper Error Types

**Create:** `src/ast/error.rs`

```rust
use crate::syntax::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstError {
    MissingChild {
        node_kind: SyntaxKind,
        expected: &'static str,
    },
    UnexpectedKind {
        expected: SyntaxKind,
        found: SyntaxKind,
    },
    MalformedNode {
        kind: SyntaxKind,
        reason: String,
    },
    InvalidCast {
        from: SyntaxKind,
        to: &'static str,
    },
}

impl std::fmt::Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstError::MissingChild { node_kind, expected } => {
                write!(f, "Node {:?} missing expected child: {}", node_kind, expected)
            }
            AstError::UnexpectedKind { expected, found } => {
                write!(f, "Expected {:?}, found {:?}", expected, found)
            }
            AstError::MalformedNode { kind, reason } => {
                write!(f, "Malformed {:?} node: {}", kind, reason)
            }
            AstError::InvalidCast { from, to } => {
                write!(f, "Cannot cast {:?} to {}", from, to)
            }
        }
    }
}

impl std::error::Error for AstError {}
```

**Priority:** CRITICAL - Do this before other improvements

---

## 2. Consistency in API Design

### 2.1 Establish Naming Conventions

**Problem:** Mixed return types for similar operations create confusion.

**Current Inconsistencies:**
- `JsonObject.l_curly_token()` → `Option<&SyntaxToken>` (raw reference)
- `JsonObjectField.key()` → `Option<&SyntaxToken>` (raw reference)
- `JsonObjectField.colon()` → `Option<&SyntaxToken>` (raw reference)
- `JsonValue.as_string()` → `Option<crate::ast::tokens::String>` (wrapped type)

**Establish Convention:**

| Method Pattern | Returns | Example |
|----------------|---------|---------|
| `xxx_token()` | `Option<&SyntaxToken>` | `key_token()`, `colon_token()` |
| `xxx()` for wrapped | `Option<WrapperType>` | `key()` returns `String` wrapper |
| `as_xxx()` for conversions | `Option<AstType>` | `as_object()`, `as_array()` |
| `xxx_syntax()` | `&SyntaxNode` | `field_syntax()` |

**Action Items:**
1. Audit all accessor methods in `src/ast/nodes.rs`
2. Rename methods to follow convention
3. Update formatter to use new names
4. Document convention in module-level docs

### 2.2 Consolidate Redundant Accessors

**Problem:** Having both `key()` and `key_token()` creates confusion.

**Decision Needed:**
- If users typically need raw tokens → keep only `xxx_token()`
- If users typically need typed wrappers → keep only `xxx()`
- If both are common → keep both but document use cases

**Current Assessment:**
- Formatter uses raw tokens (for trivia access)
- Higher-level code might want typed wrappers

**Recommendation:** Keep both but:
1. Document when to use each
2. Make naming crystal clear
3. Provide examples in docs

### 2.3 Consistent Ownership Semantics

**Problem:** Some methods return references, some clone, with no clear pattern.

**Current State:**
- Most accessors return `Option<&T>` (borrowed)
- `fields_with_commas()` clones comma tokens (owned)
- `elements_with_commas()` clones comma tokens (owned)

**Establish Rule:**
- Accessors that directly access children → return references
- Iterators that construct new values → may clone if necessary for lifetime reasons
- Make ownership explicit in method names if ambiguous

**Naming Pattern:**
```rust
xxx()         // Returns reference by default
xxx_owned()   // Returns owned value (cloned)
xxx_ref()     // Explicitly returns reference (if ambiguous)
```

---

## 3. Reduce Formatter's Structural Knowledge

### 3.1 Hide SyntaxKind from Formatter

COMPLETED

### 3.2 Abstract Trivia Handling

COMPLETED

---

## 4. Extract Generalizable Patterns

### 4.1 Generic Separator Iterator

**Problem:** `fields_with_commas()` and `elements_with_commas()` duplicate ~30 lines of identical logic.

**Locations:**
- `src/ast/nodes.rs:207-237` (JsonObject)
- `src/ast/nodes.rs:309-339` (JsonArray)

**Create Generic Solution:**

```rust
// In src/ast/util.rs or src/ast/separator.rs

use crate::syntax::{SyntaxBranch, SyntaxKind, SyntaxToken};
use std::rc::Rc;

/// Iterator that pairs elements with their trailing separator tokens
pub struct WithSeparator<T> {
    children: Vec<Rc<SyntaxBranch>>,
    position: usize,
    separator_kind: SyntaxKind,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> WithSeparator<T> {
    pub fn new(children: Vec<Rc<SyntaxBranch>>, separator_kind: SyntaxKind) -> Self {
        Self {
            children,
            position: 0,
            separator_kind,
            _phantom: std::marker::PhantomData,
        }
    }
}

pub struct ElementWithSeparator<T> {
    pub element: T,
    pub separator: Option<SyntaxToken>,
}

impl<T> Iterator for WithSeparator<T>
where
    T: crate::ast::AstNode,
{
    type Item = ElementWithSeparator<T>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip until we find an element node
        while self.position < self.children.len() {
            let child = &self.children[self.position];
            self.position += 1;

            if let SyntaxBranch::Node(node) = child.as_ref() {
                if let Some(element) = T::cast(node.clone()) {
                    // Check if next is separator
                    let separator = if self.position < self.children.len() {
                        match self.children[self.position].as_ref() {
                            SyntaxBranch::Token(token)
                                if token.kind() == self.separator_kind => {
                                self.position += 1; // Consume separator
                                Some(token.clone())
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };

                    return Some(ElementWithSeparator { element, separator });
                }
            }
        }
        None
    }
}
```

**Then Use in JsonObject:**

```rust
impl JsonObject {
    pub fn fields_with_commas(&self) -> impl Iterator<Item = ElementWithSeparator<JsonObjectField>> {
        WithSeparator::new(
            self.syntax.children().to_vec(),
            SyntaxKind::COMMA,
        )
    }
}
```

**And in JsonArray:**

```rust
impl JsonArray {
    pub fn elements_with_commas(&self) -> impl Iterator<Item = ElementWithSeparator<JsonArrayElement>> {
        WithSeparator::new(
            self.syntax.children().to_vec(),
            SyntaxKind::COMMA,
        )
    }
}
```

**Update Formatter:**

```rust
// Change from:
for (index, field_with_comma) in object.fields_with_commas().enumerate() {
    // ...
    maybe_format_token(doc, field_with_comma.comma.as_ref());
}

// To:
for (index, field) in object.fields_with_commas().enumerate() {
    // ...
    if let Some(sep) = &field.separator {
        format_token(doc, sep);
    }
}
```

### 4.2 Macro for AST Boilerplate

**Problem:** Every AST node has identical implementations of `can_cast`, `cast`, `syntax`, `Display`.

**Current Pattern (repeated 8+ times):**

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SomeNode {
    pub(crate) syntax: SyntaxNode,
}

impl std::fmt::Display for SomeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.syntax, f)
    }
}

impl AstNode for SomeNode {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SOME_NODE
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
```

**Create Macro:**

```rust
// In src/ast/macros.rs

/// Generate boilerplate for AST node types
///
/// Usage:
/// ```
/// ast_node!(Root, SyntaxKind::ROOT);
/// ast_node!(JsonObject, SyntaxKind::OBJECT);
/// ```
#[macro_export]
macro_rules! ast_node {
    ($name:ident, $kind:expr) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub(crate) syntax: SyntaxNode,
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(&self.syntax, f)
            }
        }

        impl AstNode for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == $kind
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
    };
}

/// For nodes that can match multiple kinds (like JsonValue)
#[macro_export]
macro_rules! ast_node_multi {
    ($name:ident, [$($kind:expr),+ $(,)?]) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub(crate) syntax: SyntaxNode,
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(&self.syntax, f)
            }
        }

        impl AstNode for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, $($kind)|+)
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
    };
}
```

**Similar Macro for Tokens:**

```rust
#[macro_export]
macro_rules! ast_token {
    ($name:ident, $kind:expr) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub(crate) syntax: SyntaxToken,
        }

        impl AstToken for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(syntax: SyntaxToken) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.syntax
            }

            fn text(&self) -> &str {
                self.syntax.text()
            }
        }
    };
}
```

**Usage:**

```rust
// In src/ast/nodes.rs
use crate::{ast_node, ast_node_multi};

ast_node!(Root, SyntaxKind::ROOT);
ast_node!(JsonObject, SyntaxKind::OBJECT);
ast_node!(JsonArray, SyntaxKind::ARRAY);
ast_node!(JsonObjectField, SyntaxKind::OBJECT_FIELD);
ast_node!(JsonArrayElement, SyntaxKind::ARRAY_ELEMENT);

ast_node_multi!(JsonValue, [
    SyntaxKind::OBJECT,
    SyntaxKind::ARRAY,
    SyntaxKind::STRING,
    SyntaxKind::NUMBER,
    SyntaxKind::TRUE,
    SyntaxKind::FALSE,
    SyntaxKind::NULL,
    SyntaxKind::ARRAY_ELEMENT,
]);

// Then add custom methods
impl Root {
    pub fn value(&self) -> Option<JsonValue> {
        // Custom logic here
    }
}
```

### 4.3 Delimited List Abstraction

**Problem:** Objects and arrays both handle `{items, separated, by, commas}` but no shared abstraction.

**Generic Pattern:**

```rust
// In src/ast/delimited.rs

pub trait DelimitedList: AstNode {
    type Element: AstNode;

    fn opening_kind() -> SyntaxKind;
    fn closing_kind() -> SyntaxKind;
    fn separator_kind() -> SyntaxKind;

    fn opening_token(&self) -> Option<&SyntaxToken> {
        match self.syntax().children().first()?.as_ref() {
            SyntaxBranch::Token(token) if token.kind() == Self::opening_kind() => Some(token),
            _ => None,
        }
    }

    fn closing_token(&self) -> Option<&SyntaxToken> {
        match self.syntax().children().last()?.as_ref() {
            SyntaxBranch::Token(token) if token.kind() == Self::closing_kind() => Some(token),
            _ => None,
        }
    }

    fn elements(&self) -> impl Iterator<Item = Self::Element> {
        self.syntax().children().iter().filter_map(|child| {
            if let SyntaxBranch::Node(node) = child.as_ref() {
                Self::Element::cast(node.clone())
            } else {
                None
            }
        })
    }

    fn elements_with_separators(&self) -> impl Iterator<Item = ElementWithSeparator<Self::Element>> {
        WithSeparator::new(
            self.syntax().children().to_vec(),
            Self::separator_kind(),
        )
    }
}
```

**Implementation:**

```rust
impl DelimitedList for JsonObject {
    type Element = JsonObjectField;

    fn opening_kind() -> SyntaxKind { SyntaxKind::L_CURLY }
    fn closing_kind() -> SyntaxKind { SyntaxKind::R_CURLY }
    fn separator_kind() -> SyntaxKind { SyntaxKind::COMMA }
}

impl DelimitedList for JsonArray {
    type Element = JsonArrayElement;

    fn opening_kind() -> SyntaxKind { SyntaxKind::L_BRACK }
    fn closing_kind() -> SyntaxKind { SyntaxKind::R_BRACK }
    fn separator_kind() -> SyntaxKind { SyntaxKind::COMMA }
}
```

---

## 5. Robust Error Handling

### 5.1 Distinguish Option Semantics

**Problem:** `Option<T>` doesn't distinguish between valid absence vs errors.

**Create Rich Query Type:**

```rust
// In src/ast/query.rs

/// Result of querying an AST node for a child or property
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeQuery<T> {
    /// The requested element was found
    Found(T),

    /// The element is validly absent (e.g., optional trailing comma)
    NotPresent,

    /// The node type doesn't support this query
    InvalidType,

    /// The node structure is malformed
    Malformed(String),
}

impl<T> NodeQuery<T> {
    pub fn ok(self) -> Option<T> {
        match self {
            NodeQuery::Found(t) => Some(t),
            _ => None,
        }
    }

    pub fn expect(self, msg: &str) -> T {
        match self {
            NodeQuery::Found(t) => t,
            NodeQuery::NotPresent => panic!("{}: element not present", msg),
            NodeQuery::InvalidType => panic!("{}: invalid type", msg),
            NodeQuery::Malformed(reason) => panic!("{}: malformed - {}", msg, reason),
        }
    }

    pub fn is_found(&self) -> bool {
        matches!(self, NodeQuery::Found(_))
    }

    pub fn is_error(&self) -> bool {
        matches!(self, NodeQuery::InvalidType | NodeQuery::Malformed(_))
    }
}
```

**Usage Example:**

```rust
impl JsonObjectField {
    pub fn value_query(&self) -> NodeQuery<JsonValue> {
        match self.syntax.children().get(Self::VALUE_INDEX) {
            None => NodeQuery::Malformed("Missing value child".to_string()),
            Some(child) => match child.as_ref() {
                SyntaxBranch::Node(node) => {
                    JsonValue::cast(node.clone())
                        .map(NodeQuery::Found)
                        .unwrap_or(NodeQuery::InvalidType)
                }
                SyntaxBranch::Token(_) => NodeQuery::InvalidType,
            }
        }
    }

    // Keep simple Option API for common cases
    pub fn value(&self) -> Option<JsonValue> {
        self.value_query().ok()
    }
}
```

### 5.2 Parser Validation Phase

**Problem:** TreeBuilder accepts any structure without validation.

**Add Validation:**

```rust
// In src/parser.rs or src/ast/validation.rs

pub struct ValidationError {
    pub node_kind: SyntaxKind,
    pub message: String,
}

pub fn validate_tree(root: &SyntaxNode) -> Result<(), Vec<ValidationError>> {
    let mut errors = Vec::new();
    validate_node(root, &mut errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn validate_node(node: &SyntaxNode, errors: &mut Vec<ValidationError>) {
    match node.kind() {
        SyntaxKind::OBJECT_FIELD => validate_object_field(node, errors),
        SyntaxKind::ARRAY_ELEMENT => validate_array_element(node, errors),
        SyntaxKind::OBJECT => validate_object(node, errors),
        SyntaxKind::ARRAY => validate_array(node, errors),
        _ => {}
    }

    // Recurse into children
    for child in node.children() {
        if let SyntaxBranch::Node(child_node) = child.as_ref() {
            validate_node(child_node, errors);
        }
    }
}

fn validate_object_field(node: &SyntaxNode, errors: &mut Vec<ValidationError>) {
    let children = node.children();

    if children.len() != 3 {
        errors.push(ValidationError {
            node_kind: node.kind(),
            message: format!("OBJECT_FIELD must have exactly 3 children, found {}", children.len()),
        });
        return;
    }

    // Validate child types
    match children[0].as_ref() {
        SyntaxBranch::Token(t) if t.kind() == SyntaxKind::STRING => {}
        _ => errors.push(ValidationError {
            node_kind: node.kind(),
            message: "OBJECT_FIELD child 0 must be STRING token".to_string(),
        }),
    }

    // ... validate other children
}
```

### 5.3 Update format_json to Use Result

**Current:**
```rust
pub fn format_json(source: &str, options: &Options) -> Result<String, String> {
    // Returns String error
}
```

**Improved:**
```rust
#[derive(Debug)]
pub enum FormatError {
    Parse(Vec<String>),
    InvalidRoot,
    Format(String),
    Render(std::io::Error),
    Utf8(std::string::FromUtf8Error),
}

impl std::fmt::Display for FormatError { /* ... */ }
impl std::error::Error for FormatError { /* ... */ }

pub fn format_json(source: &str, options: &Options) -> Result<String, FormatError> {
    let (tree, errors) = crate::parser::parse(source);

    if !errors.is_empty() {
        return Err(FormatError::Parse(errors));
    }

    let root = Root::cast(tree).ok_or(FormatError::InvalidRoot)?;
    let mut doc = Doc::<'static>::new();

    if let Some(value) = root.value() {
        format_value(&mut doc, &value)?;  // Now returns Result
    }

    let mut output = Vec::new();
    doc.render(&mut output, options)
        .map_err(FormatError::Render)?;

    String::from_utf8(output).map_err(FormatError::Utf8)
}
```

---

## 6. Documentation Standards

### 6.1 Module-Level Documentation

**Add to `src/ast/nodes.rs`:**

```rust
//! AST node definitions for JSON syntax tree
//!
//! # Architecture
//!
//! This module defines typed wrappers around the generic `SyntaxNode` type.
//! Each AST node type corresponds to a production in the JSON grammar.
//!
//! ## Design Principles
//!
//! 1. **Known Structure**: The parser guarantees the structure of each node.
//!    For example, `OBJECT_FIELD` always has exactly 3 children in order:
//!    `[STRING token, COLON token, JsonValue node]`.
//!
//! 2. **O(1) Access**: Since structure is known, we use direct indexing with
//!    constants rather than iteration.
//!
//! 3. **Trivia Separation**: Comments and whitespace are stored in tokens as
//!    trivia, not as separate tree nodes.
//!
//! 4. **Lossless**: The tree preserves all information from the source,
//!    including whitespace and comments.
//!
//! ## Grammar Reference
//!
//! ```text
//! Root          ::= JsonValue
//! JsonValue     ::= Object | Array | String | Number | Boolean | Null
//! Object        ::= '{' (ObjectField (',' ObjectField)*)? '}'
//! ObjectField   ::= STRING ':' JsonValue
//! Array         ::= '[' (ArrayElement (',' ArrayElement)*)? ']'
//! ArrayElement  ::= JsonValue
//! ```
//!
//! ## Usage Example
//!
//! ```rust
//! use json_fmt::parser::parse;
//! use json_fmt::ast::{AstNode, nodes::Root};
//!
//! let source = r#"{"name": "Alice", "age": 30}"#;
//! let (tree, errors) = parse(source);
//! let root = Root::cast(tree).unwrap();
//! let value = root.value().unwrap();
//! let object = value.as_object().unwrap();
//!
//! for field in object.fields() {
//!     if let Some(key) = field.key() {
//!         println!("Key: {}", key.text());
//!     }
//! }
//! ```

use crate::{
    ast::{AstNode, AstToken},
    syntax::*,
};
```

**Add to `src/syntax.rs`:**

```rust
//! Core syntax tree types
//!
//! This module defines the low-level syntax tree representation, which is a
//! lossless Concrete Syntax Tree (CST) that preserves all source information.
//!
//! # CST vs AST
//!
//! - **SyntaxNode/SyntaxToken** (this module): Generic, untyped CST nodes
//! - **JsonObject/JsonArray/etc** (ast::nodes): Typed AST wrappers
//!
//! The AST layer provides typed accessors over the CST layer.
//!
//! # Trivia Handling
//!
//! Trivia (whitespace and comments) is attached to tokens:
//! - **Leading trivia**: Appears before the token on the same line or on
//!   preceding lines
//! - **Trailing trivia**: Appears after the token on the same line (typically
//!   end-of-line comments)
//!
//! Example:
//! ```json
//! {
//!   // This is a comment   <- leading trivia of "name" token
//!   "name": "Alice",      // user name  <- trailing trivia of "Alice" token
//! }
//! ```
//!
//! # Memory Model
//!
//! Children are stored as `Rc<SyntaxBranch>` to enable cheap cloning of
//! subtrees. The tree is immutable after construction.

#![allow(bad_style)]
```

### 6.2 Method Documentation Template

**For AST Node Accessors:**

```rust
impl JsonObjectField {
    /// Returns the key token of this object field.
    ///
    /// # Structure Guarantee
    ///
    /// The parser guarantees that OBJECT_FIELD nodes always have a STRING
    /// token at index 0. This method returns `None` only if the node is
    /// malformed (should not happen with a correct parser).
    ///
    /// # Example
    ///
    /// ```rust
    /// # use json_fmt::parser::parse;
    /// # use json_fmt::ast::{AstNode, nodes::*};
    /// let source = r#"{"name": "Alice"}"#;
    /// let (tree, _) = parse(source);
    /// let root = Root::cast(tree).unwrap();
    /// let object = root.value().unwrap().as_object().unwrap();
    /// let field = object.fields().next().unwrap();
    ///
    /// let key = field.key().unwrap();
    /// assert_eq!(key.text(), r#""name""#);
    /// ```
    pub fn key(&self) -> Option<&SyntaxToken> {
        // Implementation
    }
}
```

### 6.3 Invariant Documentation

**Document structural guarantees clearly:**

```rust
impl JsonObjectField {
    /// Child index constants for OBJECT_FIELD nodes.
    ///
    /// # Grammar
    ///
    /// ```text
    /// OBJECT_FIELD ::= STRING ':' JsonValue
    /// ```
    ///
    /// # Invariant
    ///
    /// The parser (see `parser.rs::parse_object_field()`) enforces that
    /// OBJECT_FIELD nodes always contain exactly these 3 children in order:
    ///
    /// - Index 0: STRING token (the key)
    /// - Index 1: COLON token
    /// - Index 2: JsonValue node (the value)
    ///
    /// Any deviation from this structure indicates a parser bug.
    const KEY_INDEX: usize = 0;
    const COLON_INDEX: usize = 1;
    const VALUE_INDEX: usize = 2;
}
```

---

## 7. Performance Optimizations

### 7.1 Reduce Cloning in Iterators

**Problem:** Comma tokens are cloned unnecessarily in `fields_with_commas()`.

**Current:**
```rust
let comma = iter.peek().and_then(|next_child| {
    match next_child.as_ref() {
        SyntaxBranch::Token(token) if token.kind() == SyntaxKind::COMMA => {
            Some(token.clone())  // ❌ Clones every comma
        }
        _ => None,
    }
});
```

**Better (if lifetimes allow):**
```rust
pub struct ObjectFieldWithComma<'a> {
    pub field: JsonObjectField,
    pub comma: Option<&'a SyntaxToken>,
}
```

**Note:** This requires reworking the iterator to avoid lifetime issues. May need to use different pattern or accept the clone for simplicity. Evaluate trade-off.

### 7.2 String Interning for Common Tokens

**Problem:** Tokens like `{`, `}`, `,`, `:` allocate strings repeatedly.

**Solution:**

```rust
// In src/token_interner.rs

use std::collections::HashMap;
use once_cell::sync::Lazy;

static INTERNED_TOKENS: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("{", "{");
    map.insert("}", "}");
    map.insert("[", "[");
    map.insert("]", "]");
    map.insert(",", ",");
    map.insert(":", ":");
    map.insert("true", "true");
    map.insert("false", "false");
    map.insert("null", "null");
    map
});

pub fn intern_if_common(text: &str) -> String {
    INTERNED_TOKENS
        .get(text)
        .map(|&s| s.to_string())
        .unwrap_or_else(|| text.to_string())
}
```

**Use in parser:**
```rust
SyntaxToken::new(kind, intern_if_common(text))
```

### 7.3 Token Text as Source Slices

**Problem:** Token text is stored as `String`, duplicating source.

**Alternative Design (for future):**

```rust
pub struct SyntaxToken {
    kind: SyntaxKind,
    span: Span,  // Just start/end offsets
    leading_trivia: Vec<Span>,
    trailing_trivia: Vec<Span>,
}

impl SyntaxToken {
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.span.start..self.span.end]
    }
}

pub struct Span {
    start: usize,
    end: usize,
}
```

**Benefit:** Zero allocations for token text.
**Cost:** Need to thread source string through APIs.

**Decision:** Consider for logic programming language if memory is a concern.

### 7.4 Arena Allocation

**Problem:** Every node uses `Rc<SyntaxBranch>`, adding reference counting overhead.

**Alternative (for future):**

```rust
// Using typed_arena crate
use typed_arena::Arena;

pub struct Parser<'a> {
    arena: &'a Arena<SyntaxNode>,
    // ...
}

impl<'a> Parser<'a> {
    fn alloc_node(&self, kind: SyntaxKind, children: Vec<&'a SyntaxBranch<'a>>) -> &'a SyntaxNode {
        self.arena.alloc(SyntaxNode { kind, children })
    }
}
```

**Benefit:** Faster allocation, better cache locality, all freed at once.
**Cost:** Lifetimes become more complex, can't easily share trees across threads.

**Decision:** Evaluate if performance profiling shows allocation bottlenecks.

---

## 8. Eliminate Code Duplication

### 8.1 Unify Error Recovery

**Problem:** Parser has separate recovery functions that are nearly identical.

**Current:**
```rust
fn recover_to_object_sync_point(&mut self) {
    while !matches!(self.current_kind(), TokenKind::RCurly | TokenKind::Eof) {
        self.advance();
    }
}

fn recover_to_array_sync_point(&mut self) {
    while !matches!(self.current_kind(), TokenKind::RBracket | TokenKind::Eof) {
        self.advance();
    }
}
```

**Unified:**
```rust
fn recover_until(&mut self, sync_tokens: &[TokenKind]) {
    while !sync_tokens.contains(&self.current_kind()) && self.current_kind() != TokenKind::Eof {
        self.advance();
    }
}

// Usage:
self.recover_until(&[TokenKind::RCurly]);
self.recover_until(&[TokenKind::RBracket]);
self.recover_until(&[TokenKind::Comma, TokenKind::RCurly]); // Multiple sync points
```

### 8.2 Generate Token Kind Conversions

**Problem:** `token_kind_to_syntax_kind()` has 15+ identical match arms.

**Current:**
```rust
fn token_kind_to_syntax_kind(token_kind: TokenKind) -> SyntaxKind {
    match token_kind {
        TokenKind::Comma => SyntaxKind::COMMA,
        TokenKind::Colon => SyntaxKind::COLON,
        // ... 13 more
    }
}
```

**Use macro:**

```rust
macro_rules! define_token_kinds {
    ($(($token:ident, $syntax:ident)),* $(,)?) => {
        pub enum TokenKind {
            $($token,)*
        }

        pub enum SyntaxKind {
            $($syntax,)*
        }

        fn token_kind_to_syntax_kind(token_kind: TokenKind) -> SyntaxKind {
            match token_kind {
                $(TokenKind::$token => SyntaxKind::$syntax,)*
            }
        }
    };
}

define_token_kinds!(
    (Comma, COMMA),
    (Colon, COLON),
    (LCurly, L_CURLY),
    (RCurly, R_CURLY),
    // ...
);
```

**Or simpler:** Make TokenKind and SyntaxKind share the same representation for punctuation.

### 8.3 Consistent SyntaxKind Naming

**Problem:** Inconsistent abbreviations.

**Current:**
- `L_CURLY`, `R_CURLY` (abbreviated)
- `L_BRACK`, `R_BRACK` (abbreviated)
- `COMMA`, `COLON` (full words)

**Choose One Convention:**

**Option A - Full Words:**
```rust
pub enum SyntaxKind {
    LEFT_CURLY,
    RIGHT_CURLY,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    COMMA,
    COLON,
    // ...
}
```

**Option B - Consistent Abbreviations:**
```rust
pub enum SyntaxKind {
    L_CURLY,
    R_CURLY,
    L_BRACKET,  // or L_BRACK
    R_BRACKET,  // or R_BRACK
    COMMA,
    COLON,
    // ...
}
```

**Recommendation:** Option B with `L_BRACKET`/`R_BRACKET` (more readable than BRACK).

### 8.4 Unify Token Types

**Problem:** Two token representations with unclear relationship.

**Current State:**
- `src/token.rs::Token` - used by tokenizer (has `kind: TokenKind`, `text: String`)
- `src/syntax.rs::SyntaxToken` - used by AST (has `kind: SyntaxKind`, `text: String`, trivia)

**Clarify Separation:**

```rust
// token.rs - Lexer output
pub struct LexToken {
    pub kind: TokenKind,
    pub text: String,
    // Lexer doesn't handle trivia
}

// syntax.rs - Tree representation
pub struct SyntaxToken {
    kind: SyntaxKind,
    text: String,
    leading_trivia: Vec<SyntaxToken>,
    trailing_trivia: Vec<SyntaxToken>,
}
```

**Document conversion:**
```rust
impl From<LexToken> for SyntaxToken {
    fn from(lex_token: LexToken) -> Self {
        SyntaxToken::new(
            token_kind_to_syntax_kind(lex_token.kind),
            lex_token.text,
        )
    }
}
```

---

## 9. Architecture for Logic Programming Language

### 9.1 Design Checklist

When adapting this JSON formatter architecture to a logic programming language:

#### Must Have (Do First)

- [ ] **Write formal grammar** - BNF or EBNF specification
- [ ] **Design error types** - Logic programming needs excellent error messages
- [ ] **Add source spans** - Essential for error reporting and IDE features
- [ ] **Plan for unification** - Tree structure must support unification algorithm
- [ ] **Document trivia rules** - Where do comments attach in logic programs?

#### Should Have (Important)

- [ ] **Incremental parsing** - REPL scenarios need partial/invalid input handling
- [ ] **Multiple tree passes** - Type checking, constraint solving, optimization
- [ ] **Visitor/Folder patterns** - For tree transformations and analysis
- [ ] **Bidirectional construction** - May need to build trees programmatically for code generation
- [ ] **Module system support** - Logic programs typically have imports/modules

#### Nice to Have (Consider)

- [ ] **Macro system** - If the language supports macros
- [ ] **Source mapping** - For debugging generated code
- [ ] **Pretty printing** - Format logic programs back to source
- [ ] **LSP support** - Language server protocol integration

### 9.2 Logic Language Specific Considerations

#### Variables and Unification

```rust
// Logic variables need special handling
pub enum LogicExpr {
    Var(Variable),        // Unbound variable
    Constant(Constant),   // Ground term
    Compound(Functor, Vec<LogicExpr>),
}

impl LogicExpr {
    pub fn free_variables(&self) -> HashSet<Variable> {
        // Essential for unification
    }

    pub fn substitute(&self, var: &Variable, replacement: &LogicExpr) -> LogicExpr {
        // Variable substitution
    }
}
```

#### Clauses and Rules

```rust
pub struct Clause {
    head: Atom,
    body: Vec<Goal>,
}

pub struct Rule {
    conclusion: Atom,
    premises: Vec<Atom>,
}
```

#### Query Support

```rust
pub enum TopLevel {
    Clause(Clause),
    Query(Query),
    Directive(Directive),
}
```

### 9.3 Error Reporting for Logic Languages

Logic programming needs rich error messages:

```rust
pub enum LogicError {
    UnificationFailure {
        left: LogicExpr,
        right: LogicExpr,
        reason: String,
    },
    UndefinedPredicate {
        name: String,
        arity: usize,
    },
    OccursCheck {
        var: Variable,
        term: LogicExpr,
    },
    CyclicRule {
        predicate: String,
        cycle_path: Vec<String>,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        expr: LogicExpr,
    },
}
```

### 9.4 Span Tracking

**Essential for error messages:**

```rust
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub struct Span {
    pub start: Position,
    pub end: Position,
}

pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}
```

**Usage:**

```rust
pub struct Clause {
    head: Spanned<Atom>,
    body: Vec<Spanned<Goal>>,
}

impl Clause {
    pub fn error_at_head(&self, message: String) -> LogicError {
        LogicError::at_span(self.head.span, message)
    }
}
```

### 9.5 Recommended File Structure

```
logic-lang/
├── src/
│   ├── syntax/           # Low-level CST
│   │   ├── mod.rs
│   │   ├── node.rs       # SyntaxNode
│   │   ├── token.rs      # SyntaxToken
│   │   └── kind.rs       # SyntaxKind enum
│   ├── ast/              # Typed AST layer
│   │   ├── mod.rs
│   │   ├── macros.rs     # ast_node! macro
│   │   ├── nodes.rs      # AST node types
│   │   ├── expr.rs       # Expression nodes
│   │   ├── clause.rs     # Clause/rule nodes
│   │   ├── query.rs      # Query nodes
│   │   └── error.rs      # AST errors
│   ├── lexer/
│   │   ├── mod.rs
│   │   ├── scanner.rs
│   │   └── token.rs
│   ├── parser/
│   │   ├── mod.rs
│   │   ├── grammar.rs    # Grammar rules
│   │   ├── error.rs      # Parse errors
│   │   └── recovery.rs   # Error recovery
│   ├── semantics/        # Semantic analysis
│   │   ├── mod.rs
│   │   ├── unification.rs
│   │   ├── type_check.rs
│   │   └── scope.rs
│   ├── interpreter/      # Evaluation
│   └── formatter/        # Pretty printer
└── docs/
    ├── grammar.ebnf      # Formal grammar
    └── design.md         # Architecture docs
```

---

## Implementation Priority

### Phase 1: Critical Fixes (1-2 days)
1. Remove all `panic!()` calls - replace with `Result` types
2. Implement or remove `clone_subtree()` and `clone_for_update()`
3. Add error types (`AstError`, `FormatError`)
4. Add basic module-level documentation

### Phase 2: API Consistency (2-3 days)
5. Establish and document naming conventions
6. Add type queries to hide `SyntaxKind` from formatter
7. Abstract trivia handling with new methods
8. Document all structural invariants

### Phase 3: Reduce Duplication (3-4 days)
9. Extract generic `WithSeparator` iterator
10. Create AST boilerplate macros
11. Unify error recovery in parser
12. Clean up token kind conversions

### Phase 4: Documentation (1-2 days)
13. Write comprehensive module docs
14. Add method documentation with examples
15. Document trivia attachment rules
16. Create grammar reference

### Phase 5: Performance (Optional, 2-3 days)
17. Fix cloning in iterators if possible
18. Add token interning
19. Profile and optimize hot paths

### Phase 6: Logic Language Preparation
20. Design grammar for logic language
21. Plan error types specific to logic programming
22. Design AST for variables, clauses, queries
23. Implement span tracking from the start

---

## Success Metrics

- [ ] Zero `panic!()` in production code
- [ ] All public APIs documented
- [ ] Error types with clear messages
- [ ] No duplicated iterator logic
- [ ] AST macros reduce boilerplate by 50%+
- [ ] Formatter doesn't directly access `SyntaxKind`
- [ ] All structural invariants documented
- [ ] Ready to apply patterns to logic language

---

## References

**Related Code:**
- `src/format.rs` - Formatter implementation
- `src/ast/nodes.rs` - AST node definitions
- `src/syntax.rs` - Core syntax types
- `src/parser.rs` - Parser implementation

**Design Inspirations:**
- Rust Analyzer CST design: https://github.com/rust-lang/rust-analyzer
- Rome/Biome formatter architecture
- Tree-sitter parsing library

---

**Next Steps:** Review this document, prioritize sections, and begin implementation with Phase 1 critical fixes.
