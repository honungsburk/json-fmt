//! JSON formatter that converts AST to the formatter Doc representation
//!
//! Note: The formatter API deisng only works when the AST is known to be well formed.
//! It is bad design if the AST might be bad.
//! one alternative design is that the AST bust be well formed when you get it, or you might not get a well formed string of the AST.
//! then we have some way of checking AST validity.

use crate::ast::AstNode;
use crate::ast::nodes::{JsonArray, JsonObject, JsonValue, Root};
use crate::formatter::{Doc, If, Options, Tag};
use crate::parser::ParseError;
use crate::syntax::{SyntaxKind, SyntaxToken};

#[derive(Debug)]
pub enum FormatError {
    InvalidValueKind(SyntaxKind),
    MalformedNode { kind: SyntaxKind, reason: String },
    RenderError(String),
    ParseErrors(Vec<ParseError>),
}

/// Format a JSON string with the given options
pub fn format_json(source: &str, options: &Options) -> Result<String, FormatError> {
    let (tree, errors) = crate::parser::parse(source);

    if !errors.is_empty() {
        return Err(FormatError::ParseErrors(errors));
    }
    // TODO: I do not like that cast eays the node. if cast fails the node is gone!
    let real_kind = tree.kind();
    let root = Root::cast(tree).ok_or(FormatError::MalformedNode {
        kind: real_kind,
        reason: "Expected Root".to_string(),
    })?;
    let mut doc = Doc::<'static>::new();

    if let Some(value) = root.value() {
        format_value(&mut doc, &value)?
    }

    let mut output = Vec::new();
    doc.render(&mut output, options)
        .map_err(|e| FormatError::RenderError(format!("Render error: {}", e)))?;

    String::from_utf8(output).map_err(|e| FormatError::RenderError(format!("UTF-8 error: {}", e)))
}

/// Format a JSON value into the Doc
fn format_value(doc: &mut Doc<'static>, value: &JsonValue) -> Result<(), FormatError> {
    if value.is_object() {
        if let Some(obj) = value.as_object() {
            format_object(doc, &obj)
        } else {
            Err(FormatError::MalformedNode {
                kind: value.syntax.kind(),
                reason: "Expected a json object".to_string(),
            })
        }
    } else if value.is_array() {
        if let Some(arr) = value.as_array() {
            format_array(doc, &arr)
        } else {
            Err(FormatError::MalformedNode {
                kind: value.syntax.kind(),
                reason: "Expected a json array".to_string(),
            })
        }
    } else if value.is_primitive() {
        if let Some(token) = value.primitive_token() {
            format_token(doc, token)
        } else {
            Err(FormatError::MalformedNode {
                kind: value.syntax.kind(),
                reason: "Expected a primitive json value".to_string(),
            })
        }
    } else {
        Err(FormatError::MalformedNode {
            kind: value.syntax.kind(),
            reason: "Expected a json value".to_string(),
        })
    }
}
/// Format a JSON object
fn format_object(doc: &mut Doc<'static>, object: &JsonObject) -> Result<(), FormatError> {
    maybe_format_token(doc, object.l_curly_token());

    // Non-empty object with grouping
    doc.tag_with(Tag::Group(120), |doc| {
        doc.tag_if(Tag::Break(1), If::Broken);

        doc.tag_with(Tag::Indent(2), |doc| {
            for (index, field_with_comma) in object.fields_with_commas().enumerate() {
                if index > 0 {
                    doc.tag_if(Tag::Space, If::Flat);
                    doc.tag_if(Tag::Break(1), If::Broken);
                }
                format_object_field(doc, &field_with_comma.field)?;
                maybe_format_token(doc, field_with_comma.comma.as_ref());
            }
            ok()
        })?;

        doc.tag_if(Tag::Break(1), If::Broken);
        ok()
    })?;
    maybe_format_token(doc, object.r_curly_token());

    Ok(())
}

/// Format a JSON object field
fn format_object_field(
    doc: &mut Doc<'static>,
    field: &crate::ast::nodes::JsonObjectField,
) -> Result<(), FormatError> {
    if let Some(key) = field.key() {
        format_token(doc, key)?
    }
    if let Some(colon) = field.colon() {
        format_token(doc, colon)?
    }
    doc.tag(Tag::Space);
    if let Some(value) = field.value() {
        format_value(doc, &value)?
    }
    Ok(())
}

/// Format a JSON array
fn format_array(doc: &mut Doc<'static>, array: &JsonArray) -> Result<(), FormatError> {
    maybe_format_token(doc, array.l_bracket_token());

    doc.tag_with(Tag::Group(120), |doc| {
        doc.tag_if(Tag::Break(1), If::Broken);
        doc.tag_with(Tag::Indent(2), |doc| {
            for (index, element_with_comma) in array.elements_with_commas().enumerate() {
                if index > 0 {
                    doc.tag_if(Tag::Space, If::Flat);
                    doc.tag_if(Tag::Break(1), If::Broken);
                }
                if let Some(value) = element_with_comma.element.value() {
                    format_value(doc, &value)?;
                }
                maybe_format_token(doc, element_with_comma.comma.as_ref());
            }
            ok()
        })?;
        doc.tag_if(Tag::Break(1), If::Broken);
        ok()
    })?;
    maybe_format_token(doc, array.r_bracket_token());
    Ok(())
}

fn maybe_format_token(doc: &mut Doc<'static>, maybe_token: Option<&SyntaxToken>) {
    if let Some(token) = maybe_token {
        _ = format_token(doc, token);
    }
}

fn format_token(doc: &mut Doc<'static>, token: &SyntaxToken) -> Result<(), FormatError> {
    for trivia in token.leading_comments() {
        doc.tag(trivia.text().to_string());
        doc.tag(Tag::Break(1));
    }
    doc.tag(token.text().to_string());

    // Add trailing comments after the key
    for trivia in token.trailing_comments() {
        doc.tag(Tag::Space);
        doc.tag(trivia.text().to_string());
        doc.tag(Tag::Break(1));
    }
    Ok(())
}

fn ok() -> Result<(), FormatError> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_string() {
        let json = r#""hello""#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert_eq!(result, r#""hello""#);
    }

    #[test]
    fn test_format_simple_number() {
        let json = "123";
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert_eq!(result, "123");
    }

    #[test]
    fn test_format_boolean_and_null() {
        let options = Options { max_columns: 120 };

        assert_eq!(format_json("true", &options).unwrap(), "true");
        assert_eq!(format_json("false", &options).unwrap(), "false");
        assert_eq!(format_json("null", &options).unwrap(), "null");
    }

    #[test]
    fn test_format_empty_object() {
        let json = "{}";
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert_eq!(result, "{}");
    }

    #[test]
    fn test_format_empty_object_with_spaces() {
        let json = "{  }";
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert_eq!(result, "{}");
    }

    #[test]
    fn test_format_empty_array() {
        let json = "[]";
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert_eq!(result, "[]");
    }

    #[test]
    fn test_format_simple_object_fits_one_line() {
        let json = r#"{"name":"John"}"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        // Should fit on one line with proper spacing
        assert_eq!(result, r#"{"name": "John"}"#);
    }

    #[test]
    fn test_format_simple_object_multiline() {
        let json = r#"{"name":"John","age":30,"city":"New York"}"#;
        let options = Options { max_columns: 40 };
        let result = format_json(json, &options).unwrap();
        // Should break into multiple lines
        assert!(result.contains('\n'));
        assert!(result.contains(r#""name": "John""#));
        assert!(result.contains(r#""age": 30"#));
    }

    #[test]
    fn test_format_nested_object() {
        let json = r#"{"user":{"name":"Alice","age":25}}"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        // Should preserve structure
        assert!(result.contains("user"));
        assert!(result.contains("name"));
        assert!(result.contains("Alice"));
    }

    #[test]
    fn test_format_simple_array_fits_one_line() {
        let json = r#"[1,2,3]"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        // Arrays format with spaces inside brackets when flat: [ elem, elem ]
        assert_eq!(result, "[1, 2, 3]");
    }

    #[test]
    fn test_format_array_multiline() {
        let json = r#"[1,2,3,4,5,6,7,8,9,10]"#;
        let options = Options { max_columns: 20 };
        let result = format_json(json, &options).unwrap();
        // Should break into multiple lines
        assert!(result.contains('\n'));
    }

    #[test]
    fn test_format_mixed_array() {
        let json = r#"[1,"hello",true,null]"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert!(result.contains("1"));
        assert!(result.contains(r#""hello""#));
        assert!(result.contains("true"));
        assert!(result.contains("null"));
    }

    #[test]
    fn test_format_nested_arrays() {
        let json = r#"[[1,2],[3,4]]"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        // Should preserve nested structure
        assert!(result.contains('['));
        assert!(result.contains(']'));
    }

    #[test]
    fn test_format_array_of_objects() {
        let json = r#"[{"id":1},{"id":2}]"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert!(result.contains("id"));
        assert!(result.contains("1"));
        assert!(result.contains("2"));
    }

    #[test]
    fn test_format_complex_nested_structure() {
        let json = r#"{"users":[{"name":"Alice","scores":[10,20,30]},{"name":"Bob","scores":[15,25,35]}]}"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        // Verify all elements are present
        assert!(result.contains("users"));
        assert!(result.contains("Alice"));
        assert!(result.contains("Bob"));
        assert!(result.contains("scores"));
    }

    #[test]
    fn test_format_with_comments() {
        let json = r#"{
            "name": "John", // user name
            "age": 30
        }"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        println!("Result: {:?}", result);
        // Comments should be preserved
        assert!(result.contains("// user name"));
        assert!(result.contains("name"));
    }

    #[test]
    fn test_format_deeply_nested() {
        let json = r#"{"a":{"b":{"c":{"d":"value"}}}}"#;
        let options = Options { max_columns: 120 };
        let result = format_json(json, &options).unwrap();
        assert!(result.contains("value"));
    }
}
