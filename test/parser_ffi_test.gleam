/// Integration tests for the FFI layer between Gleam and Elixir.
/// These tests verify that parsing JSON Schema files through the FFI
/// correctly produces typed Gleam structures.
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import json_schema_gleam/codegen
import json_schema_gleam/parser_ffi
import json_schema_gleam/schema.{
  ArrayType, BooleanType, EnumType, IntegerType, NumberType, ObjectType,
  OneOfType, RefType, SchemaNode, SchemaResult, StringType, StringValue,
  UnionType,
}

// Note: These tests require the Elixir FFI modules to be compiled.
// Run with `just test` which builds the FFI first.

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// File Parsing Tests
// =============================================================================

pub fn parse_simple_object_file_test() {
  let result = parser_ffi.parse_file("test/fixtures/simple_object.json")

  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  // Verify basic structure
  root.title |> should.equal(Some("Person"))
  root.description |> should.equal(Some("A person record"))

  // Verify type detection
  case root.schema_type {
    ObjectType -> should.be_true(True)
    _ -> should.fail()
  }

  // Verify properties exist
  dict.has_key(root.properties, "name") |> should.be_true()
  dict.has_key(root.properties, "age") |> should.be_true()
  dict.has_key(root.properties, "email") |> should.be_true()

  // Verify required field
  list.contains(root.required, "name") |> should.be_true()

  // Verify property types
  let assert Ok(name_prop) = dict.get(root.properties, "name")
  case name_prop.schema_type {
    StringType -> should.be_true(True)
    _ -> should.fail()
  }

  let assert Ok(age_prop) = dict.get(root.properties, "age")
  case age_prop.schema_type {
    IntegerType -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_enum_file_test() {
  let result = parser_ffi.parse_file("test/fixtures/enum_type.json")

  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.title |> should.equal(Some("Status"))

  // Check for enum values
  case root.enum_values {
    Some(values) -> {
      list.length(values) |> should.equal(5)
      list.contains(values, StringValue("pending")) |> should.be_true()
      list.contains(values, StringValue("shipped")) |> should.be_true()
    }
    None -> should.fail()
  }
}

pub fn parse_array_file_test() {
  let result = parser_ffi.parse_file("test/fixtures/array_type.json")

  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.title |> should.equal(Some("Tags"))

  case root.schema_type {
    ArrayType -> should.be_true(True)
    _ -> should.fail()
  }

  // Check items schema
  case root.items {
    Some(items_node) -> {
      case items_node.schema_type {
        StringType -> should.be_true(True)
        _ -> should.fail()
      }
    }
    None -> should.fail()
  }

  // Check array constraints
  root.min_items |> should.equal(Some(1))
  root.max_items |> should.equal(Some(10))
  root.unique_items |> should.be_true()
}

pub fn parse_nested_object_file_test() {
  let result = parser_ffi.parse_file("test/fixtures/nested_object.json")

  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.title |> should.equal(Some("Order"))

  // Check items array property
  let assert Ok(items_prop) = dict.get(root.properties, "items")
  case items_prop.schema_type {
    ArrayType -> should.be_true(True)
    _ -> should.fail()
  }

  // Check nested address property
  let assert Ok(address_prop) = dict.get(root.properties, "shipping_address")
  case address_prop.schema_type {
    ObjectType -> should.be_true(True)
    _ -> should.fail()
  }
  address_prop.title |> should.equal(Some("Address"))
}

pub fn parse_union_type_file_test() {
  let result = parser_ffi.parse_file("test/fixtures/union_type.json")

  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.title |> should.equal(Some("NullableString"))

  // Should be detected as a union type
  case root.schema_type {
    UnionType(types) -> {
      list.length(types) |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn parse_one_of_file_test() {
  let result = parser_ffi.parse_file("test/fixtures/one_of.json")

  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.title |> should.equal(Some("PaymentMethod"))

  case root.one_of {
    Some(variants) -> {
      list.length(variants) |> should.equal(2)

      let first = list.first(variants)
      case first {
        Ok(node) -> node.title |> should.equal(Some("CreditCard"))
        Error(_) -> should.fail()
      }
    }
    None -> should.fail()
  }
}

pub fn parse_refs_file_test() {
  let result = parser_ffi.parse_file("test/fixtures/refs.json")

  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.title |> should.equal(Some("Team"))

  // Check that lead property has a ref
  let assert Ok(lead_prop) = dict.get(root.properties, "lead")
  lead_prop.ref |> should.equal(Some("#/$defs/Member"))

  // Check definitions were extracted
  dict.has_key(schema_result.definitions, "Member") |> should.be_true()
}

// =============================================================================
// String Parsing Tests
// =============================================================================

pub fn parse_simple_object_string_test() {
  let json =
    "{
    \"title\": \"User\",
    \"type\": \"object\",
    \"properties\": {
      \"username\": {\"type\": \"string\"},
      \"active\": {\"type\": \"boolean\"}
    },
    \"required\": [\"username\"]
  }"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.title |> should.equal(Some("User"))

  case root.schema_type {
    ObjectType -> should.be_true(True)
    _ -> should.fail()
  }

  let assert Ok(username_prop) = dict.get(root.properties, "username")
  case username_prop.schema_type {
    StringType -> should.be_true(True)
    _ -> should.fail()
  }

  let assert Ok(active_prop) = dict.get(root.properties, "active")
  case active_prop.schema_type {
    BooleanType -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_enum_string_test() {
  let json =
    "{
    \"title\": \"Color\",
    \"type\": \"string\",
    \"enum\": [\"red\", \"green\", \"blue\"]
  }"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  case root.enum_values {
    Some(values) -> {
      list.length(values) |> should.equal(3)
      list.contains(values, StringValue("red")) |> should.be_true()
    }
    None -> should.fail()
  }
}

pub fn parse_array_string_test() {
  let json =
    "{
    \"type\": \"array\",
    \"items\": {\"type\": \"integer\"},
    \"minItems\": 0,
    \"maxItems\": 100
  }"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  case root.schema_type {
    ArrayType -> should.be_true(True)
    _ -> should.fail()
  }

  root.min_items |> should.equal(Some(0))
  root.max_items |> should.equal(Some(100))
}

pub fn parse_const_value_string_test() {
  let json = "{\"const\": \"fixed_value\"}"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  case root.const_value {
    Some(StringValue(val)) -> val |> should.equal("fixed_value")
    _ -> should.fail()
  }
}

pub fn parse_numeric_constraints_string_test() {
  let json =
    "{
    \"type\": \"number\",
    \"minimum\": 0,
    \"maximum\": 100
  }"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  case root.schema_type {
    NumberType -> should.be_true(True)
    _ -> should.fail()
  }

  root.minimum |> should.equal(Some(0.0))
  root.maximum |> should.equal(Some(100.0))
}

pub fn parse_string_constraints_test() {
  let json =
    "{
    \"type\": \"string\",
    \"minLength\": 1,
    \"maxLength\": 255,
    \"pattern\": \"^[a-z]+$\",
    \"format\": \"email\"
  }"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  root.min_length |> should.equal(Some(1))
  root.max_length |> should.equal(Some(255))
  root.pattern |> should.equal(Some("^[a-z]+$"))
  root.format |> should.equal(Some("email"))
}

pub fn parse_deprecated_flag_test() {
  let json = "{\"type\": \"string\", \"deprecated\": true}"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  schema_result.root.deprecated |> should.be_true()
}

pub fn parse_any_of_test() {
  let json =
    "{
    \"anyOf\": [
      {\"type\": \"string\"},
      {\"type\": \"integer\"}
    ]
  }"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  case root.any_of {
    Some(variants) -> list.length(variants) |> should.equal(2)
    None -> should.fail()
  }
}

pub fn parse_all_of_test() {
  let json =
    "{
    \"allOf\": [
      {\"type\": \"object\", \"properties\": {\"a\": {\"type\": \"string\"}}},
      {\"type\": \"object\", \"properties\": {\"b\": {\"type\": \"integer\"}}}
    ]
  }"

  let result = parser_ffi.parse_string(json)
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  let root = schema_result.root

  case root.all_of {
    Some(variants) -> list.length(variants) |> should.equal(2)
    None -> should.fail()
  }
}

// =============================================================================
// Error Handling Tests
// =============================================================================

pub fn parse_nonexistent_file_test() {
  let result = parser_ffi.parse_file("/nonexistent/path.json")
  result |> should.be_error()
}

pub fn parse_invalid_json_string_test() {
  let result = parser_ffi.parse_string("not valid json {{{")
  result |> should.be_error()
}

pub fn parse_empty_object_test() {
  let result = parser_ffi.parse_string("{}")
  result |> should.be_ok()

  let assert Ok(schema_result) = result
  // Empty object should still have a valid root
  case schema_result.root.schema_type {
    // Type could be unknown for empty schema
    _ -> should.be_true(True)
  }
}

// =============================================================================
// End-to-End Code Generation Tests
// =============================================================================

pub fn ffi_to_codegen_simple_object_test() {
  // Test the full pipeline: FFI parsing -> code generation
  let result = parser_ffi.parse_file("test/fixtures/simple_object.json")
  let assert Ok(schema_result) = result

  let options =
    codegen.GenerateOptions(
      module_name: "person",
      generate_decoders: True,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Verify generated code contains expected elements
  code |> string.contains("pub type Person") |> should.be_true()
  code |> string.contains("name: String") |> should.be_true()
  // age is optional (not in required)
  code |> string.contains("age:") |> should.be_true()
}

pub fn ffi_to_codegen_enum_test() {
  let result = parser_ffi.parse_file("test/fixtures/enum_type.json")
  let assert Ok(schema_result) = result

  let options =
    codegen.GenerateOptions(
      module_name: "status",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub type Status") |> should.be_true()
  code |> string.contains("Pending") |> should.be_true()
  code |> string.contains("Shipped") |> should.be_true()
  code |> string.contains("Delivered") |> should.be_true()
}

pub fn ffi_to_codegen_array_test() {
  let result = parser_ffi.parse_file("test/fixtures/array_type.json")
  let assert Ok(schema_result) = result

  let options =
    codegen.GenerateOptions(
      module_name: "tags",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub type Tags") |> should.be_true()
  code |> string.contains("List(String)") |> should.be_true()
}

pub fn ffi_to_codegen_with_decoder_test() {
  let json =
    "{
    \"title\": \"Config\",
    \"type\": \"object\",
    \"properties\": {
      \"name\": {\"type\": \"string\"},
      \"enabled\": {\"type\": \"boolean\"},
      \"count\": {\"type\": \"integer\"}
    },
    \"required\": [\"name\"]
  }"

  let result = parser_ffi.parse_string(json)
  let assert Ok(schema_result) = result

  let options =
    codegen.GenerateOptions(
      module_name: "config",
      generate_decoders: True,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub type Config") |> should.be_true()
  code |> string.contains("pub fn config_decoder") |> should.be_true()
  code |> string.contains("dynamic.field") |> should.be_true()
}
