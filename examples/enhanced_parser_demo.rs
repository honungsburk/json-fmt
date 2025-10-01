use json_fmt::{
    ast::{AstNode, AstToken, nodes::*},
    parser::parse,
};

fn main() {
    let json_with_trivia = r#"{
        "name": "Alice",      // User's name
        "age": 30,           // User's age
        "hobbies": [
            "reading",       // First hobby
            "coding",        // Second hobby
            "swimming"       // Third hobby
        ],
        "address": {
            "street": "123 Main St",
            "city": "Anytown"
        }
    }"#;

    println!("Parsing JSON with comments and whitespace...\n");

    let (tree, errors) = parse(json_with_trivia);

    if !errors.is_empty() {
        println!("Parse errors:");
        for error in &errors {
            println!("  - {}", error.message);
        }
        return;
    }

    println!("✅ Parsed successfully with no errors!\n");

    // Demonstrate the enhanced tree structure
    let root = Root::cast(tree).unwrap();
    let value = root.value().unwrap();
    let object = value.as_object().unwrap();

    println!("🏗️  Enhanced Tree Structure:");
    println!("Root -> Object with {} fields", object.fields().count());

    for (i, field) in object.fields().enumerate() {
        let key = field.key().unwrap();
        let key_text = key.text();

        println!("  Field #{}: key = {}", i + 1, key_text);

        if let Some(field_value) = field.value() {
            match field_value.syntax().kind() {
                json_fmt::syntax::SyntaxKind::STRING => {
                    println!("    -> String value");
                }
                json_fmt::syntax::SyntaxKind::NUMBER => {
                    println!("    -> Number value");
                }
                json_fmt::syntax::SyntaxKind::ARRAY => {
                    let array = field_value.as_array().unwrap();
                    println!("    -> Array with {} elements", array.elements().count());

                    for (j, element) in array.elements().enumerate() {
                        if let Some(elem_value) = element.value() {
                            println!("      Element #{}: {:?}", j + 1, elem_value.syntax().kind());
                        }
                    }
                }
                json_fmt::syntax::SyntaxKind::OBJECT => {
                    let nested_object = field_value.as_object().unwrap();
                    println!(
                        "    -> Nested object with {} fields",
                        nested_object.fields().count()
                    );
                }
                _ => {
                    println!("    -> {:?}", field_value.syntax().kind());
                }
            }
        }
    }

    println!("\n🎯 Key Benefits:");
    println!("  ✓ All trivia (comments, whitespace) preserved in tokens");
    println!("  ✓ Explicit OBJECT_FIELD nodes for precise formatting control");
    println!("  ✓ Explicit ARRAY_ELEMENT nodes for consistent element handling");
    println!("  ✓ Lossless roundtrip capability (perfect source reconstruction)");
    println!("  ✓ Ready for sophisticated pretty-printing implementations");
}
