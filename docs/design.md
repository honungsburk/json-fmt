# JSON Formatter Design Document

## Overview

This JSON formatter is designed as a lossless parser and formatter that can preserve all source information including comments, whitespace, and formatting while providing precise control over pretty-printing. The architecture is heavily inspired by rust-analyzer's green/red tree design.

## Architecture

### Green Tree (Immutable Syntax Tree)

The green tree consists of immutable nodes (`SyntaxNode`) and tokens (`SyntaxToken`) that represent the concrete syntax tree. This layer:

- **Preserves all source information**: Every character from the input is represented
- **Supports structural sharing**: Immutable nodes can be safely shared between trees
- **Enables efficient updates**: Changes create new nodes while reusing unchanged subtrees
- **Provides lossless roundtrip**: Can reconstruct the exact original source

#### Key Types:
- `SyntaxNode`: Interior nodes representing language constructs (objects, arrays, etc.)
- `SyntaxToken`: Leaf nodes representing actual text with attached trivia
- `SyntaxBranch`: Enum wrapping either a node or token

### Red Tree (Typed AST Interface)

The red tree provides a typed, ergonomic interface over the green tree through traits:

- `AstNode`: Interface for structural nodes (objects, arrays, values)
- `AstToken`: Interface for leaf tokens (strings, numbers, keywords)

This separation allows:
- **Type safety**: Compile-time guarantees about tree structure
- **Ergonomic APIs**: Easy traversal and manipulation methods
- **Performance**: No overhead when type information isn't needed

## Key Design Decisions

### 1. Trivia Attachment Strategy

**Decision**: Attach trivia (whitespace, comments) directly to tokens as `leading_trivia` and `trailing_trivia` vectors.

**Rationale**:
- **Precise control**: Each token knows its associated formatting
- **Lossless preservation**: All source information maintained
- **Flexible formatting**: Can modify trivia without structural changes
- **Context awareness**: Comments stay logically attached to their elements

**Alternative considered**: Separate trivia nodes scattered throughout the tree
- **Rejected because**: More complex traversal, less intuitive pretty-printing

### 2. Explicit Structural Nodes

**Decision**: Create explicit `OBJECT_FIELD` and `ARRAY_ELEMENT` nodes instead of implicit groupings.

**Rationale**:
- **Formatting precision**: Can control spacing around individual fields/elements
- **Consistent structure**: Every meaningful construct has a dedicated node type
- **Extensibility**: Easy to add field-specific metadata or transformations
- **Pretty-printing clarity**: Clear boundaries for formatting decisions

**Alternative considered**: Implicit grouping where objects contain alternating key/colon/value tokens
- **Rejected because**: Difficult to format consistently, error-prone traversal

### 3. Never-Failing Parser Strategy

**Decision**: Parser always produces a valid tree, using error recovery and `ERROR` nodes.

**Rationale**:
- **Robust tooling**: Formatters and analyzers work even on invalid JSON
- **Error resilience**: Can format partially correct files
- **IDE support**: Enables syntax highlighting and basic analysis of broken code
- **User experience**: Tools remain functional during editing

**Implementation**:
- Synchronization points at commas, brackets, and braces
- Error nodes wrap invalid tokens but parsing continues
- Missing tokens are handled gracefully with error reporting

### 4. Recursive Descent with Error Recovery

**Decision**: Use recursive descent parsing with explicit error recovery methods.

**Rationale**:
- **Clarity**: Parser structure mirrors the grammar directly
- **Maintainability**: Easy to understand and modify parsing logic
- **Error recovery**: Natural places to implement synchronization
- **Performance**: Efficient for typical JSON sizes

**Recovery strategies**:
- `recover_to_object_sync_point()`: Skips to next comma or closing brace
- `recover_to_array_sync_point()`: Skips to next comma or closing bracket
- Continue parsing after errors to find additional issues

### 5. Token-Level Trivia Collection

**Decision**: Collect trivia at the token level with `collect_leading_trivia()` and `collect_trailing_trivia()`.

**Rationale**:
- **Semantic attachment**: Trivia logically belongs to adjacent significant tokens
- **Formatting control**: Can distinguish between trailing spaces and leading indentation
- **Comment placement**: Line comments naturally attach as trailing trivia
- **Pretty-printing flexibility**: Can replace trivia while preserving structure

**Heuristics**:
- Leading trivia: Consumed before parsing significant tokens
- Trailing trivia: Whitespace without newlines, line comments
- Newlines typically start new leading trivia sequences

### 6. TreeBuilder Pattern

**Decision**: Use a stack-based `TreeBuilder` to construct the green tree during parsing.

**Rationale**:
- **Incremental construction**: Nodes built bottom-up as parsing progresses
- **Memory efficiency**: Tokens added directly without intermediate collections
- **Error handling**: Can create valid trees even when parsing fails
- **Simplicity**: Clear push/pop semantics mirror recursive parsing structure

**Operations**:
- `start_node(kind)`: Begin a new interior node
- `add_token_with_trivia()`: Add a token with attached trivia
- `finish_node()`: Complete current node and attach to parent

### 7. Comprehensive SyntaxKind Taxonomy

**Decision**: Define explicit syntax kinds for all meaningful language constructs.

```rust
pub enum SyntaxKind {
    // Structure
    ROOT, OBJECT, ARRAY, OBJECT_FIELD, ARRAY_ELEMENT,
    // Tokens
    STRING, NUMBER, TRUE, FALSE, NULL,
    // Punctuation
    L_CURLY, R_CURLY, L_BRACK, R_BRACK, COMMA, COLON,
    // Trivia
    WHITESPACE, COMMENT,
    // Meta
    ERROR, EOF,
}
```

**Rationale**:
- **Completeness**: Every possible tree node has a unique type
- **Type safety**: AST methods can match on specific kinds
- **Extensibility**: Easy to add new language constructs
- **Tool support**: IDEs and analyzers can provide kind-specific behavior

### 8. Lazy AST Construction

**Decision**: AST nodes are lightweight wrappers around `SyntaxNode` created on-demand.

**Rationale**:
- **Memory efficiency**: No duplicate storage of tree data
- **Performance**: AST creation cost paid only when needed
- **Flexibility**: Can create multiple AST views of the same tree
- **Consistency**: Green tree remains the single source of truth

## Implementation Patterns

### Error Recovery Pattern

```rust
fn parse_object_field(&mut self, source: &str) -> bool {
    self.builder.start_node(SyntaxKind::OBJECT_FIELD);

    // Try to parse components
    if !self.expect_token_with_trivia(TokenKind::String { terminated: true }, source) {
        self.builder.finish_node();
        return false; // Let caller handle recovery
    }

    // Continue parsing...
    self.builder.finish_node();
    true
}
```

### Trivia-Aware Parsing Pattern

```rust
fn advance_with_trivia(&mut self, source: &str) -> bool {
    let leading_trivia = self.collect_leading_trivia(source);

    if let Some(token) = self.current_token() {
        // Consume main token
        let trailing_trivia = self.collect_trailing_trivia(source);

        self.builder.add_token_with_trivia(
            token, source, offset, leading_trivia, trailing_trivia
        );
        true
    } else {
        false
    }
}
```

### AST Traversal Pattern

```rust
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
```

## Future Considerations

### Pretty Printing

The current design enables sophisticated pretty-printing by:
- **Trivia replacement**: Swap out whitespace/comments while preserving structure
- **Structural awareness**: Format based on `OBJECT_FIELD` and `ARRAY_ELEMENT` boundaries
- **Context sensitivity**: Access parent/sibling information for formatting decisions
- **Incremental updates**: Modify only changed portions of large documents

### Language Server Integration

The design supports IDE features:
- **Syntax highlighting**: Based on `SyntaxKind` information
- **Error reporting**: From parse error collection
- **Code completion**: Using partial parse trees with error recovery
- **Incremental parsing**: Green tree reuse for unchanged regions

### Performance Optimizations

Potential improvements:
- **Interning**: Share identical trivia tokens across the tree
- **Lazy trivia**: Defer trivia collection until actually needed
- **Streaming**: Parse large files without loading everything into memory
- **Parallel parsing**: Independent subtrees could be parsed concurrently

## Testing Strategy

The implementation includes comprehensive tests for:
- **Basic parsing**: All JSON value types and structures
- **Trivia preservation**: Comments and whitespace in various positions
- **Error recovery**: Malformed JSON handled gracefully
- **AST interface**: Type-safe access to parsed structures
- **Unicode support**: Proper handling of international characters
- **Edge cases**: Empty structures, unterminated strings, etc.

This design provides a solid foundation for building robust JSON tooling while maintaining the flexibility needed for advanced formatting and analysis features.