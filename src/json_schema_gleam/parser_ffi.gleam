/// FFI bindings to the Elixir JSON Schema parser.
/// This module wraps the Elixir parser and converts the dynamic
/// results into typed Gleam structures.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import json_schema_gleam/schema.{
  type JsonSchemaError, type SchemaNode, type SchemaResult, type SchemaType,
  type SchemaValue, AllOfType, AnyOfType, ArrayType, ArrayValue, BoolValue,
  BooleanType, ConstType, EnumType, FileError, FloatValue, IntValue, IntegerType,
  NullType, NullValue, NumberType, ObjectType, ObjectValue, OneOfType,
  ParseError, RefType, SchemaMetadata, SchemaNode, SchemaResult, SchemaStructure,
  SchemaValidation, StringType, StringValue, UnionType, UnknownType,
}

/// Parse a JSON Schema from a file path
pub fn parse_file(path: String) -> Result(SchemaResult, JsonSchemaError) {
  do_parse_file(path)
  |> result.map_error(fn(e) { FileError(path: path, detail: e) })
  |> result.try(decode_schema_result)
}

/// Parse a JSON Schema from a JSON string
pub fn parse_string(json: String) -> Result(SchemaResult, JsonSchemaError) {
  do_parse_string(json)
  |> result.map_error(fn(e) { ParseError(detail: e) })
  |> result.try(decode_schema_result)
}

@external(erlang, "Elixir.JsonSchemaGleam.Parser", "parse_file")
fn do_parse_file(path: String) -> Result(Dynamic, String)

@external(erlang, "Elixir.JsonSchemaGleam.Parser", "parse_string")
fn do_parse_string(json: String) -> Result(Dynamic, String)

/// Decode the dynamic result from Elixir into typed Gleam structures
fn decode_schema_result(dyn: Dynamic) -> Result(SchemaResult, JsonSchemaError) {
  case get_atom_field(dyn, "root"), get_atom_field(dyn, "definitions") {
    Ok(root_dyn), Ok(defs_dyn) ->
      Ok(
        SchemaResult(
          root: decode_schema_node(root_dyn),
          definitions: decode_definitions(defs_dyn),
          errors: [],
          warnings: [],
        ),
      )
    _, _ ->
      Error(ParseError(
        detail: "Failed to decode schema result structure. Got: "
        <> dynamic.classify(dyn),
      ))
  }
}

/// Get a field from an Elixir map (which uses atom keys)
@external(erlang, "json_schema_gleam_ffi", "get_map_field")
fn get_atom_field(map: Dynamic, key: String) -> Result(Dynamic, Nil)

fn decode_schema_node(dyn: Dynamic) -> SchemaNode {
  let metadata =
    SchemaMetadata(
      path: decode_string_field(dyn, "path", "#"),
      title: decode_optional_string(dyn, "title"),
      description: decode_optional_string(dyn, "description"),
      deprecated: decode_bool_field(dyn, "deprecated", False),
      default_value: decode_default_value(dyn),
    )

  let validation =
    SchemaValidation(
      pattern: decode_optional_string(dyn, "pattern"),
      format: decode_optional_string(dyn, "format"),
      min_length: decode_optional_int(dyn, "min_length"),
      max_length: decode_optional_int(dyn, "max_length"),
      minimum: decode_optional_float(dyn, "minimum"),
      maximum: decode_optional_float(dyn, "maximum"),
      min_items: decode_optional_int(dyn, "min_items"),
      max_items: decode_optional_int(dyn, "max_items"),
      unique_items: decode_bool_field(dyn, "unique_items", False),
    )

  let structure =
    SchemaStructure(
      properties: decode_properties(dyn),
      required: decode_string_list(dyn, "required"),
      additional_properties: decode_bool_field(dyn, "additional_properties", True),
      items: decode_optional_node(dyn, "items"),
      enum_values: decode_enum_values(dyn),
      const_value: decode_const_value(dyn),
      one_of: decode_optional_node_list(dyn, "one_of"),
      any_of: decode_optional_node_list(dyn, "any_of"),
      all_of: decode_optional_node_list(dyn, "all_of"),
      ref: decode_optional_string(dyn, "ref"),
    )

  SchemaNode(
    schema_type: decode_schema_type(dyn),
    metadata: metadata,
    validation: validation,
    structure: structure,
  )
}

fn decode_schema_type(dyn: Dynamic) -> SchemaType {
  case has_non_nil_field(dyn, "enum") {
    True -> EnumType
    False ->
      case has_non_nil_field(dyn, "const") {
        True -> ConstType
        False ->
          case get_atom_field(dyn, "type") {
            Ok(type_dyn) -> parse_type_value(type_dyn)
            Error(_) -> decode_composition_type(dyn)
          }
      }
  }
}

fn decode_composition_type(dyn: Dynamic) -> SchemaType {
  let checks = [
    #("one_of", OneOfType),
    #("any_of", AnyOfType),
    #("all_of", AllOfType),
    #("ref", RefType),
  ]
  list.find_map(checks, fn(pair) {
    let #(field, schema_type) = pair
    case has_non_nil_field(dyn, field) {
      True -> Ok(schema_type)
      False -> Error(Nil)
    }
  })
  |> result.unwrap(UnknownType)
}

/// Check if a field exists and is not nil
fn has_non_nil_field(dyn: Dynamic, field: String) -> Bool {
  case get_atom_field(dyn, field) {
    Error(_) -> False
    Ok(value_dyn) ->
      case decode.run(value_dyn, decode.optional(decode.dynamic)) {
        Ok(None) -> False
        Ok(Some(_)) -> True
        Error(_) -> True
      }
  }
}

fn parse_type_value(dyn: Dynamic) -> SchemaType {
  case decode.run(dyn, decode.string) {
    Ok(type_str) -> string_to_schema_type(type_str)
    Error(_) -> parse_union_type(dyn)
  }
}

fn parse_union_type(dyn: Dynamic) -> SchemaType {
  // Try to decode as a tuple with ("union", [types...])
  let decoder = {
    use tag <- decode.field(0, decode.string)
    use types <- decode.field(1, decode.list(decode.string))
    decode.success(#(tag, types))
  }
  case decode.run(dyn, decoder) {
    Ok(#("union", types)) -> UnionType(list.map(types, string_to_schema_type))
    _ -> UnknownType
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
      result.unwrap(decode.run(value_dyn, decode.string), default)
    Error(_) -> default
  }
}

fn decode_bool_field(dyn: Dynamic, field: String, default: Bool) -> Bool {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) -> result.unwrap(decode.run(value_dyn, decode.bool), default)
    Error(_) -> default
  }
}

fn decode_optional_string(dyn: Dynamic, field: String) -> Option(String) {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) -> option.from_result(decode.run(value_dyn, decode.string))
    Error(_) -> None
  }
}

fn decode_optional_int(dyn: Dynamic, field: String) -> Option(Int) {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) -> option.from_result(decode.run(value_dyn, decode.int))
    Error(_) -> None
  }
}

fn decode_optional_float(dyn: Dynamic, field: String) -> Option(Float) {
  case get_atom_field(dyn, field) {
    Ok(value_dyn) ->
      case decode.run(value_dyn, decode.float) {
        Ok(f) -> Some(f)
        Error(_) ->
          // Try int and convert to float
          case decode.run(value_dyn, decode.int) {
            Ok(i) -> Some(int.to_float(i))
            Error(_) -> None
          }
      }
    Error(_) -> None
  }
}

fn decode_string_list(dyn: Dynamic, field: String) -> List(String) {
  case get_atom_field(dyn, field) {
    Ok(list_dyn) ->
      result.unwrap(decode.run(list_dyn, decode.list(decode.string)), [])
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
  case decode.run(dyn, decode.dict(decode.string, decode.dynamic)) {
    Ok(raw_dict) ->
      dict.fold(raw_dict, dict.new(), fn(acc, key, value) {
        dict.insert(acc, key, decode_schema_node(value))
      })
    Error(_) -> dict.new()
  }
}

fn decode_optional_node(dyn: Dynamic, field: String) -> Option(SchemaNode) {
  case get_atom_field(dyn, field) {
    Error(_) -> None
    Ok(node_dyn) ->
      case decode.run(node_dyn, decode.optional(decode.dynamic)) {
        Ok(Some(inner)) -> Some(decode_schema_node(inner))
        Ok(None) -> None
        Error(_) -> Some(decode_schema_node(node_dyn))
      }
  }
}

fn decode_optional_node_list(
  dyn: Dynamic,
  field: String,
) -> Option(List(SchemaNode)) {
  case get_atom_field(dyn, field) {
    Error(_) -> None
    Ok(list_dyn) ->
      case decode.run(list_dyn, decode.list(decode.dynamic)) {
        Ok(items) -> Some(list.map(items, decode_schema_node))
        Error(_) -> None
      }
  }
}

fn decode_enum_values(dyn: Dynamic) -> Option(List(SchemaValue)) {
  case get_atom_field(dyn, "enum") {
    Error(_) -> None
    Ok(list_dyn) ->
      case decode.run(list_dyn, decode.list(decode.dynamic)) {
        Ok(values) -> Some(list.map(values, decode_schema_value))
        Error(_) -> None
      }
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

/// Decode a schema value using one_of to try each type in order
fn decode_schema_value(dyn: Dynamic) -> SchemaValue {
  let decoder =
    decode.one_of(decode.string |> decode.map(StringValue), [
      decode.int |> decode.map(IntValue),
      decode.float |> decode.map(FloatValue),
      decode.bool |> decode.map(BoolValue),
      decode.optional(decode.dynamic)
        |> decode.map(fn(opt) {
          case opt {
            None -> NullValue
            _ -> NullValue
          }
        }),
      decode.list(decode.dynamic)
        |> decode.map(fn(items) {
          ArrayValue(list.map(items, decode_schema_value))
        }),
      decode.dict(decode.string, decode.dynamic)
        |> decode.map(fn(obj) {
          ObjectValue(
            dict.fold(obj, dict.new(), fn(acc, k, v) {
              dict.insert(acc, k, decode_schema_value(v))
            }),
          )
        }),
    ])
  case decode.run(dyn, decoder) {
    Ok(value) -> value
    Error(_) -> NullValue
  }
}

fn decode_definitions(dyn: Dynamic) -> Dict(String, SchemaNode) {
  case decode.run(dyn, decode.dict(decode.string, decode.dynamic)) {
    Ok(raw_dict) ->
      dict.fold(raw_dict, dict.new(), fn(acc, key, value) {
        dict.insert(acc, key, decode_schema_node(value))
      })
    Error(_) -> dict.new()
  }
}
