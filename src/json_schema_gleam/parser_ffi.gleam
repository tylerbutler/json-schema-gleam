/// FFI bindings to the Elixir JSON Schema parser.
/// This module wraps the Elixir parser and converts the dynamic
/// results into typed Gleam structures.
import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import json_schema_gleam/schema.{
  type SchemaNode, type SchemaResult, type SchemaType, type SchemaValue,
  AllOfType, AnyOfType, ArrayType, ArrayValue, BoolValue, BooleanType, ConstType,
  EnumType, FloatValue, IntValue, IntegerType, NullType, NullValue, NumberType,
  ObjectType, ObjectValue, OneOfType, RefType, SchemaNode, SchemaResult,
  StringType, StringValue, UnionType, UnknownType,
}

/// Parse a JSON Schema from a file path
pub fn parse_file(path: String) -> Result(SchemaResult, String) {
  case do_parse_file(path) {
    Ok(dynamic_result) -> decode_schema_result(dynamic_result)
    Error(reason) -> Error(reason)
  }
}

/// Parse a JSON Schema from a JSON string
pub fn parse_string(json: String) -> Result(SchemaResult, String) {
  case do_parse_string(json) {
    Ok(dynamic_result) -> decode_schema_result(dynamic_result)
    Error(reason) -> Error(reason)
  }
}

@external(erlang, "Elixir.JsonSchemaGleam.Parser", "parse_file")
fn do_parse_file(path: String) -> Result(Dynamic, String)

@external(erlang, "Elixir.JsonSchemaGleam.Parser", "parse_string")
fn do_parse_string(json: String) -> Result(Dynamic, String)

// Decode the dynamic result from Elixir into typed Gleam structures
fn decode_schema_result(dyn: Dynamic) -> Result(SchemaResult, String) {
  // Elixir maps use atom keys - use any_field which tries both string and atom
  let root_result = get_atom_field(dyn, "root")
  let defs_result = get_atom_field(dyn, "definitions")

  case root_result, defs_result {
    Ok(root_dyn), Ok(defs_dyn) -> {
      let root = decode_schema_node(root_dyn)
      let definitions = decode_definitions(defs_dyn)
      Ok(
        SchemaResult(
          root: root,
          definitions: definitions,
          errors: [],
          warnings: [],
        ),
      )
    }
    _, _ -> {
      // Debug: try to see what we got
      Error(
        "Failed to decode schema result structure. Got: "
        <> dynamic.classify(dyn),
      )
    }
  }
}

// Get a field from an Elixir map (which uses atom keys)
@external(erlang, "json_schema_gleam_ffi", "get_map_field")
fn get_atom_field(map: Dynamic, key: String) -> Result(Dynamic, Nil)

fn decode_schema_node(dyn: Dynamic) -> SchemaNode {
  let path = decode_string_field(dyn, "path", "#")
  let schema_type = decode_schema_type(dyn)
  let title = decode_optional_string(dyn, "title")
  let description = decode_optional_string(dyn, "description")
  let properties = decode_properties(dyn)
  let required = decode_string_list(dyn, "required")
  let additional_properties =
    decode_bool_field(dyn, "additional_properties", True)
  let items = decode_optional_node(dyn, "items")
  let enum_values = decode_enum_values(dyn)
  let const_value = decode_const_value(dyn)
  let pattern = decode_optional_string(dyn, "pattern")
  let default_value = decode_default_value(dyn)
  let one_of = decode_optional_node_list(dyn, "one_of")
  let any_of = decode_optional_node_list(dyn, "any_of")
  let all_of = decode_optional_node_list(dyn, "all_of")
  let ref = decode_optional_string(dyn, "ref")
  let min_items = decode_optional_int(dyn, "min_items")
  let max_items = decode_optional_int(dyn, "max_items")
  let unique_items = decode_bool_field(dyn, "unique_items", False)
  let minimum = decode_optional_float(dyn, "minimum")
  let maximum = decode_optional_float(dyn, "maximum")
  let min_length = decode_optional_int(dyn, "min_length")
  let max_length = decode_optional_int(dyn, "max_length")
  let format = decode_optional_string(dyn, "format")
  let deprecated = decode_bool_field(dyn, "deprecated", False)

  SchemaNode(
    path: path,
    schema_type: schema_type,
    title: title,
    description: description,
    properties: properties,
    required: required,
    additional_properties: additional_properties,
    items: items,
    enum_values: enum_values,
    const_value: const_value,
    pattern: pattern,
    default_value: default_value,
    one_of: one_of,
    any_of: any_of,
    all_of: all_of,
    ref: ref,
    min_items: min_items,
    max_items: max_items,
    unique_items: unique_items,
    minimum: minimum,
    maximum: maximum,
    min_length: min_length,
    max_length: max_length,
    format: format,
    deprecated: deprecated,
  )
}

fn decode_schema_type(dyn: Dynamic) -> SchemaType {
  // Check for enum first - enum takes precedence even if type is present
  // (e.g., {"type": "string", "enum": ["a", "b"]} should be EnumType)
  // But only if enum actually has values (not nil)
  case has_non_nil_field(dyn, "enum") {
    True -> EnumType
    False ->
      // Check for const next (not nil)
      case has_non_nil_field(dyn, "const") {
        True -> ConstType
        False ->
          // Now check for explicit type
          case get_atom_field(dyn, "type") {
            Ok(type_dyn) -> parse_type_value(type_dyn)
            Error(_) ->
              // Check for composition types
              case has_non_nil_field(dyn, "one_of") {
                True -> OneOfType
                False ->
                  case has_non_nil_field(dyn, "any_of") {
                    True -> AnyOfType
                    False ->
                      case has_non_nil_field(dyn, "all_of") {
                        True -> AllOfType
                        False ->
                          case has_non_nil_field(dyn, "ref") {
                            True -> RefType
                            False -> UnknownType
                          }
                      }
                  }
              }
          }
      }
  }
}

// Helper to check if a field exists and is not nil
fn has_non_nil_field(dyn: Dynamic, field: String) -> Bool {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) -> {
      // Check if the value is nil using dynamic.optional
      case dynamic.optional(dynamic.dynamic)(value_dyn) {
        Ok(None) -> False
        Ok(Some(_)) -> True
        Error(_) -> True
      }
    }
    Error(_) -> False
  }
}

fn parse_type_value(dyn: Dynamic) -> SchemaType {
  case dynamic.string(dyn) {
    Ok("string") -> StringType
    Ok("integer") -> IntegerType
    Ok("number") -> NumberType
    Ok("boolean") -> BooleanType
    Ok("null") -> NullType
    Ok("array") -> ArrayType
    Ok("object") -> ObjectType
    Ok(_) -> UnknownType
    Error(_) -> {
      // Check for tuple {"union", [...types]}
      case dynamic.tuple2(dynamic.string, dynamic.list(dynamic.string))(dyn) {
        Ok(#("union", types)) -> {
          let parsed_types = list.map(types, string_to_schema_type)
          UnionType(parsed_types)
        }
        _ -> UnknownType
      }
    }
  }
}

fn string_to_schema_type(s: String) -> SchemaType {
  case s {
    "string" -> StringType
    "integer" -> IntegerType
    "number" -> NumberType
    "boolean" -> BooleanType
    "null" -> NullType
    "array" -> ArrayType
    "object" -> ObjectType
    _ -> UnknownType
  }
}

fn decode_string_field(dyn: Dynamic, field: String, default: String) -> String {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) ->
      case dynamic.string(value_dyn) {
        Ok(value) -> value
        Error(_) -> default
      }
    Error(_) -> default
  }
}

fn decode_bool_field(dyn: Dynamic, field: String, default: Bool) -> Bool {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) ->
      case dynamic.bool(value_dyn) {
        Ok(value) -> value
        Error(_) -> default
      }
    Error(_) -> default
  }
}

fn decode_optional_string(dyn: Dynamic, field: String) -> Option(String) {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) ->
      case dynamic.string(value_dyn) {
        Ok(value) -> Some(value)
        Error(_) -> None
      }
    Error(_) -> None
  }
}

fn decode_optional_int(dyn: Dynamic, field: String) -> Option(Int) {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) ->
      case dynamic.int(value_dyn) {
        Ok(value) -> Some(value)
        Error(_) -> None
      }
    Error(_) -> None
  }
}

fn decode_optional_float(dyn: Dynamic, field: String) -> Option(Float) {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) ->
      case dynamic.float(value_dyn) {
        Ok(value) -> Some(value)
        Error(_) ->
          // Try int and convert to float
          case dynamic.int(value_dyn) {
            Ok(int_value) -> Some(int_to_float(int_value))
            Error(_) -> None
          }
      }
    Error(_) -> None
  }
}

fn int_to_float(i: Int) -> Float {
  let assert Ok(f) =
    result.map(Ok(i), fn(x) {
      case x {
        0 -> 0.0
        _ -> do_int_to_float(x)
      }
    })
  f
}

@external(erlang, "erlang", "float")
fn do_int_to_float(i: Int) -> Float

fn decode_string_list(dyn: Dynamic, field: String) -> List(String) {
  case get_atom_field(dyn, field) {
    Ok(list_dyn) ->
      case dynamic.list(dynamic.string)(list_dyn) {
        Ok(values) -> values
        Error(_) -> []
      }
    Error(_) -> []
  }
}

fn decode_properties(dyn: Dynamic) -> Dict(String, SchemaNode) {
  case get_atom_field(dyn, "properties") {
    Ok(props_dyn) -> decode_property_map(props_dyn)
    Error(_) -> dict.new()
  }
}

fn decode_property_map(dyn: Dynamic) -> Dict(String, SchemaNode) {
  case dynamic.dict(dynamic.string, dynamic.dynamic)(dyn) {
    Ok(raw_dict) -> {
      dict.fold(raw_dict, dict.new(), fn(acc, key, value) {
        dict.insert(acc, key, decode_schema_node(value))
      })
    }
    Error(_) -> dict.new()
  }
}

fn decode_optional_node(dyn: Dynamic, field: String) -> Option(SchemaNode) {
  case get_atom_field(dyn, field) {
    Ok(node_dyn) -> {
      case dynamic.optional(dynamic.dynamic)(node_dyn) {
        Ok(Some(inner)) -> Some(decode_schema_node(inner))
        Ok(None) -> None
        Error(_) -> Some(decode_schema_node(node_dyn))
      }
    }
    Error(_) -> None
  }
}

fn decode_optional_node_list(
  dyn: Dynamic,
  field: String,
) -> Option(List(SchemaNode)) {
  case get_atom_field(dyn, field) {
    Ok(list_dyn) -> {
      case dynamic.list(dynamic.dynamic)(list_dyn) {
        Ok(items) -> Some(list.map(items, decode_schema_node))
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

fn decode_enum_values(dyn: Dynamic) -> Option(List(SchemaValue)) {
  case get_atom_field(dyn, "enum") {
    Ok(list_dyn) ->
      case dynamic.list(dynamic.dynamic)(list_dyn) {
        Ok(values) -> Some(list.map(values, decode_schema_value))
        Error(_) -> None
      }
    Error(_) -> None
  }
}

fn decode_const_value(dyn: Dynamic) -> Option(SchemaValue) {
  case get_atom_field(dyn, "const") {
    Ok(value_dyn) -> Some(decode_schema_value(value_dyn))
    Error(_) -> None
  }
}

fn decode_default_value(dyn: Dynamic) -> Option(SchemaValue) {
  case get_atom_field(dyn, "default") {
    Ok(value_dyn) -> Some(decode_schema_value(value_dyn))
    Error(_) -> None
  }
}

fn decode_schema_value(dyn: Dynamic) -> SchemaValue {
  case dynamic.string(dyn) {
    Ok(s) -> StringValue(s)
    Error(_) ->
      case dynamic.int(dyn) {
        Ok(i) -> IntValue(i)
        Error(_) ->
          case dynamic.float(dyn) {
            Ok(f) -> FloatValue(f)
            Error(_) ->
              case dynamic.bool(dyn) {
                Ok(b) -> BoolValue(b)
                Error(_) ->
                  case dynamic.optional(dynamic.dynamic)(dyn) {
                    Ok(None) -> NullValue
                    _ ->
                      case dynamic.list(dynamic.dynamic)(dyn) {
                        Ok(items) ->
                          ArrayValue(list.map(items, decode_schema_value))
                        Error(_) ->
                          case
                            dynamic.dict(dynamic.string, dynamic.dynamic)(dyn)
                          {
                            Ok(obj) -> {
                              let converted =
                                dict.fold(obj, dict.new(), fn(acc, k, v) {
                                  dict.insert(acc, k, decode_schema_value(v))
                                })
                              ObjectValue(converted)
                            }
                            Error(_) -> NullValue
                          }
                      }
                  }
              }
          }
      }
  }
}

fn decode_definitions(dyn: Dynamic) -> Dict(String, SchemaNode) {
  case dynamic.dict(dynamic.string, dynamic.dynamic)(dyn) {
    Ok(raw_dict) -> {
      dict.fold(raw_dict, dict.new(), fn(acc, key, value) {
        dict.insert(acc, key, decode_schema_node(value))
      })
    }
    Error(_) -> dict.new()
  }
}
