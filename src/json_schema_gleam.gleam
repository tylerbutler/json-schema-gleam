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
///   case json_schema_gleam.generate_from_file("schema.json", options) {
///     Ok(code) -> io.println(code)
///     Error(e) -> io.println("Error: " <> e)
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
import json_schema_gleam/codegen.{type GenerateOptions}
import json_schema_gleam/parser_ffi
import json_schema_gleam/schema.{type SchemaResult}

// Re-export main types and functions for convenient access
pub type Options =
  GenerateOptions

/// Create default generation options
pub fn default_options(module_name: String) -> Options {
  codegen.default_options(module_name)
}

/// Generate Gleam code from a JSON Schema file
pub fn generate_from_file(
  path: String,
  options: Options,
) -> Result(String, String) {
  use schema <- result.try(parser_ffi.parse_file(path))
  Ok(codegen.generate(schema, options))
}

/// Generate Gleam code from a JSON Schema string
pub fn generate_from_string(
  json: String,
  options: Options,
) -> Result(String, String) {
  use schema <- result.try(parser_ffi.parse_string(json))
  Ok(codegen.generate(schema, options))
}

/// Parse a JSON Schema file without generating code
pub fn parse_file(path: String) -> Result(SchemaResult, String) {
  parser_ffi.parse_file(path)
}

/// Parse a JSON Schema string without generating code
pub fn parse_string(json: String) -> Result(SchemaResult, String) {
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
