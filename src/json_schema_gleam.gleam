/// json_schema_gleam - Generate Gleam types from JSON Schema definitions
///
/// This library parses JSON Schema files and generates Gleam type definitions
/// along with optional JSON decoders and encoders.
///
/// ## Usage as a Library
///
/// ```gleam
/// import json_schema_gleam
///
/// pub fn main() {
///   let options = json_schema_gleam.default_options("my_types")
///     |> json_schema_gleam.with_type_prefix("Schema")
///     |> json_schema_gleam.with_decoders(False)
///   case json_schema_gleam.generate_from_file("schema.json", options) {
///     Ok(code) -> io.println(code)
///     Error(e) -> io.println("Error: " <> json_schema_gleam.error_to_string(e))
///   }
/// }
/// ```
///
/// ## Usage as CLI
///
/// ```sh
/// gleam run -- schema.json output.gleam --module my_types
/// ```
import gleam/result
import json_schema_gleam/cli
import json_schema_gleam/codegen.{type GenerateOptions, GenerateOptions}
import json_schema_gleam/parser_ffi
import json_schema_gleam/schema.{type JsonSchemaError, type SchemaResult}

// Re-export main types and functions for convenient access
pub type Options =
  GenerateOptions

/// Re-export JsonSchemaError for consumers
pub type SchemaError =
  JsonSchemaError

/// Convert a JsonSchemaError to a human-readable string
pub fn error_to_string(error: JsonSchemaError) -> String {
  schema.error_to_string(error)
}

/// Create default generation options
pub fn default_options(module_name: String) -> Options {
  codegen.default_options(module_name)
}

/// Disable or enable JSON decoder generation
pub fn with_decoders(options: Options, enabled: Bool) -> Options {
  GenerateOptions(..options, generate_decoders: enabled)
}

/// Enable or disable JSON encoder generation (not yet implemented; emits a TODO comment)
pub fn with_encoders(options: Options, enabled: Bool) -> Options {
  GenerateOptions(..options, generate_encoders: enabled)
}

/// Set a prefix for all generated type names
pub fn with_type_prefix(options: Options, prefix: String) -> Options {
  GenerateOptions(..options, type_prefix: prefix)
}

/// Set the module name
pub fn with_module_name(options: Options, name: String) -> Options {
  GenerateOptions(..options, module_name: name)
}

/// Generate Gleam code from a JSON Schema file
pub fn generate_from_file(
  path: String,
  options: Options,
) -> Result(String, JsonSchemaError) {
  use schema <- result.try(parser_ffi.parse_file(path))
  Ok(codegen.generate(schema, options))
}

/// Generate Gleam code from a JSON Schema string
pub fn generate_from_string(
  json: String,
  options: Options,
) -> Result(String, JsonSchemaError) {
  use schema <- result.try(parser_ffi.parse_string(json))
  Ok(codegen.generate(schema, options))
}

/// Parse a JSON Schema file without generating code
pub fn parse_file(path: String) -> Result(SchemaResult, JsonSchemaError) {
  parser_ffi.parse_file(path)
}

/// Parse a JSON Schema string without generating code
pub fn parse_string(json: String) -> Result(SchemaResult, JsonSchemaError) {
  parser_ffi.parse_string(json)
}

/// Generate Gleam code from a pre-parsed schema
pub fn generate(schema: SchemaResult, options: Options) -> String {
  codegen.generate(schema, options)
}

/// CLI entry point
pub fn main() {
  cli.run()
}
