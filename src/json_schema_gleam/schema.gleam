/// Types representing a parsed JSON Schema AST.
/// These types are populated by the Elixir FFI parser and used
/// by the code generator to produce Gleam type definitions.
import gleam/dict.{type Dict}
import gleam/option.{type Option}

/// The root result from parsing a JSON Schema file
pub type SchemaResult {
  SchemaResult(
    root: SchemaNode,
    definitions: Dict(String, SchemaNode),
    errors: List(String),
    warnings: List(String),
  )
}

/// A node in the JSON Schema AST
pub type SchemaNode {
  SchemaNode(
    path: String,
    schema_type: SchemaType,
    title: Option(String),
    description: Option(String),
    properties: Dict(String, SchemaNode),
    required: List(String),
    additional_properties: Bool,
    items: Option(SchemaNode),
    enum_values: Option(List(SchemaValue)),
    const_value: Option(SchemaValue),
    pattern: Option(String),
    default_value: Option(SchemaValue),
    one_of: Option(List(SchemaNode)),
    any_of: Option(List(SchemaNode)),
    all_of: Option(List(SchemaNode)),
    ref: Option(String),
    min_items: Option(Int),
    max_items: Option(Int),
    unique_items: Bool,
    minimum: Option(Float),
    maximum: Option(Float),
    min_length: Option(Int),
    max_length: Option(Int),
    format: Option(String),
    deprecated: Bool,
  )
}

/// JSON Schema type specifier
pub type SchemaType {
  StringType
  IntegerType
  NumberType
  BooleanType
  NullType
  ArrayType
  ObjectType
  RefType
  OneOfType
  AnyOfType
  AllOfType
  EnumType
  ConstType
  UnionType(types: List(SchemaType))
  UnknownType
}

/// A value that can appear in JSON Schema (for enum, const, default)
pub type SchemaValue {
  StringValue(String)
  IntValue(Int)
  FloatValue(Float)
  BoolValue(Bool)
  NullValue
  ArrayValue(List(SchemaValue))
  ObjectValue(Dict(String, SchemaValue))
}

/// Create an empty schema node (useful for building test schemas)
pub fn empty_node(path: String) -> SchemaNode {
  SchemaNode(
    path: path,
    schema_type: UnknownType,
    title: option.None,
    description: option.None,
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: option.None,
    enum_values: option.None,
    const_value: option.None,
    pattern: option.None,
    default_value: option.None,
    one_of: option.None,
    any_of: option.None,
    all_of: option.None,
    ref: option.None,
    min_items: option.None,
    max_items: option.None,
    unique_items: False,
    minimum: option.None,
    maximum: option.None,
    min_length: option.None,
    max_length: option.None,
    format: option.None,
    deprecated: False,
  )
}
