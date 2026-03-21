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

/// Metadata about a schema node
pub type SchemaMetadata {
  SchemaMetadata(
    path: String,
    title: Option(String),
    description: Option(String),
    deprecated: Bool,
    default_value: Option(SchemaValue),
  )
}

/// Validation constraints for a schema node
pub type SchemaValidation {
  SchemaValidation(
    pattern: Option(String),
    format: Option(String),
    min_length: Option(Int),
    max_length: Option(Int),
    minimum: Option(Float),
    maximum: Option(Float),
    min_items: Option(Int),
    max_items: Option(Int),
    unique_items: Bool,
  )
}

/// Structural information about a schema node (composition, references, etc.)
pub type SchemaStructure {
  SchemaStructure(
    properties: Dict(String, SchemaNode),
    required: List(String),
    additional_properties: Bool,
    items: Option(SchemaNode),
    enum_values: Option(List(SchemaValue)),
    const_value: Option(SchemaValue),
    one_of: Option(List(SchemaNode)),
    any_of: Option(List(SchemaNode)),
    all_of: Option(List(SchemaNode)),
    ref: Option(String),
  )
}

/// A node in the JSON Schema AST
pub type SchemaNode {
  SchemaNode(
    schema_type: SchemaType,
    metadata: SchemaMetadata,
    validation: SchemaValidation,
    structure: SchemaStructure,
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

/// Structured error type for JSON Schema operations
pub type JsonSchemaError {
  /// Error reading or accessing a file
  FileError(path: String, detail: String)
  /// Error parsing JSON or JSON Schema
  ParseError(detail: String)
  /// Error during code generation
  CodegenError(detail: String)
}

/// Convert a JsonSchemaError to a human-readable string
pub fn error_to_string(error: JsonSchemaError) -> String {
  case error {
    FileError(path, detail) -> "File error (" <> path <> "): " <> detail
    ParseError(detail) -> "Parse error: " <> detail
    CodegenError(detail) -> "Codegen error: " <> detail
  }
}

/// Create an empty schema metadata
pub fn empty_metadata(path: String) -> SchemaMetadata {
  SchemaMetadata(
    path: path,
    title: option.None,
    description: option.None,
    deprecated: False,
    default_value: option.None,
  )
}

/// Create an empty schema validation
pub fn empty_validation() -> SchemaValidation {
  SchemaValidation(
    pattern: option.None,
    format: option.None,
    min_length: option.None,
    max_length: option.None,
    minimum: option.None,
    maximum: option.None,
    min_items: option.None,
    max_items: option.None,
    unique_items: False,
  )
}

/// Create an empty schema structure
pub fn empty_structure() -> SchemaStructure {
  SchemaStructure(
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: option.None,
    enum_values: option.None,
    const_value: option.None,
    one_of: option.None,
    any_of: option.None,
    all_of: option.None,
    ref: option.None,
  )
}

/// Create an empty schema node (useful for building test schemas)
pub fn empty_node(path: String) -> SchemaNode {
  SchemaNode(
    schema_type: UnknownType,
    metadata: empty_metadata(path),
    validation: empty_validation(),
    structure: empty_structure(),
  )
}
