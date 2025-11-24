use json_fmt::format::format_json;
use json_fmt::formatter::Options;

fn main() {
    // Example 1: Simple object
    let simple = r#"{"name":"Alice","age":30,"active":true}"#;
    println!("=== Simple Object ===");
    println!("Input: {}", simple);
    let formatted = format_json(simple, &Options { max_columns: 120 }).unwrap();
    println!("Output:\n{}\n", formatted);

    // Example 2: Nested structure
    let nested = r#"{"user":{"name":"Bob","address":{"city":"NYC","zip":"10001"}},"items":[1,2,3,4,5]}"#;
    println!("=== Nested Structure ===");
    println!("Input: {}", nested);
    let formatted = format_json(nested, &Options { max_columns: 120 }).unwrap();
    println!("Output:\n{}\n", formatted);

    // Example 3: With line width constraint (forces breaking)
    let long = r#"{"field1":"value1","field2":"value2","field3":"value3","field4":"value4"}"#;
    println!("=== With Line Width Constraint (40 cols) ===");
    println!("Input: {}", long);
    let formatted = format_json(long, &Options { max_columns: 40 }).unwrap();
    println!("Output:\n{}\n", formatted);

    // Example 4: Simple value (not an object/array)
    let simple_value = r#""hello world""#;
    println!("=== Simple String Value ===");
    println!("Input: {}", simple_value);
    let formatted = format_json(simple_value, &Options { max_columns: 120 }).unwrap();
    println!("Output: {}\n", formatted);

    // Example 5: With comments
    let with_comments = r#"{
        "name": "Charlie", // user's name
        "score": 100, // current score
        "active": true
    }"#;
    println!("=== With Comments ===");
    println!("Input: {}", with_comments);
    let formatted = format_json(with_comments, &Options { max_columns: 120 }).unwrap();
    println!("Output:\n{}\n", formatted);

    // Example 6: Array of objects
    let array_of_objects = r#"[{"id":1,"name":"Item1"},{"id":2,"name":"Item2"},{"id":3,"name":"Item3"}]"#;
    println!("=== Array of Objects ===");
    println!("Input: {}", array_of_objects);
    let formatted = format_json(array_of_objects, &Options { max_columns: 120 }).unwrap();
    println!("Output:\n{}\n", formatted);

    // Example 7: Empty structures
    let empty = r#"{"obj":{},"arr":[],"nested":{"inner":{}}}"#;
    println!("=== Empty Structures ===");
    println!("Input: {}", empty);
    let formatted = format_json(empty, &Options { max_columns: 120 }).unwrap();
    println!("Output:\n{}\n", formatted);
}
