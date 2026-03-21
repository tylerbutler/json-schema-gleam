/// CLI module for json_schema_gleam using glint for argument parsing.
import argv
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import json_schema_gleam/codegen
import json_schema_gleam/parser_ffi
import json_schema_gleam/schema
import json_schema_gleam/utils
import simplifile

/// Run the CLI with command line arguments
pub fn run() -> Nil {
  glint.new()
  |> glint.with_name("json_schema_gleam")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: [], do: generate_command())
  |> glint.run(argv.load().arguments)
}

fn generate_command() -> glint.Command(Nil) {
  use <- glint.command_help("Generate Gleam types from a JSON Schema file")
  use <- glint.unnamed_args(glint.MinArgs(1))
  use module_name <- glint.flag(
    glint.string_flag("module")
    |> glint.flag_default("")
    |> glint.flag_help(
      "Module name for generated types (derived from schema title if not specified)",
    ),
  )
  use no_decoders <- glint.flag(
    glint.bool_flag("no-decoders")
    |> glint.flag_default(False)
    |> glint.flag_help("Skip generating JSON decoders"),
  )
  use prefix <- glint.flag(
    glint.string_flag("prefix")
    |> glint.flag_default("")
    |> glint.flag_help("Prefix for generated type names"),
  )
  use _, args, flags <- glint.command()

  let module_flag = module_name(flags)
  let no_decoders_flag = no_decoders(flags)
  let prefix_flag = prefix(flags)

  let cli_result = {
    use schema_path <- result.try(
      list.first(args)
      |> result.replace_error("No schema file specified"),
    )
    let output_path = args |> list.drop(1) |> list.first |> option.from_result()
    let options =
      codegen.GenerateOptions(
        module_name: get_module_name(module_flag, schema_path),
        generate_decoders: !result.unwrap(no_decoders_flag, False),
        generate_encoders: False,
        type_prefix: result.unwrap(prefix_flag, ""),
      )
    use code <- result.try(
      generate_code(schema_path, options)
      |> result.map_error(schema.error_to_string),
    )
    write_output(code, output_path)
  }
  case cli_result {
    Ok(msg) -> io.println(msg)
    Error(e) -> io.println("Error: " <> e)
  }
}

fn get_module_name(
  module_flag: Result(String, a),
  schema_path: String,
) -> String {
  case module_flag {
    Ok(name) if name != "" -> name
    _ -> derive_module_name(schema_path)
  }
}

fn write_output(
  code: String,
  output_path: option.Option(String),
) -> Result(String, String) {
  case output_path {
    None -> Ok(code)
    Some(path) ->
      simplifile.write(path, code)
      |> result.replace("Generated types written to " <> path)
      |> result.map_error(fn(e) { "Error writing file: " <> string.inspect(e) })
  }
}

fn generate_code(
  schema_path: String,
  options: codegen.GenerateOptions,
) -> Result(String, schema.JsonSchemaError) {
  use parsed <- result.try(parser_ffi.parse_file(schema_path))
  Ok(codegen.generate(parsed, options))
}

fn derive_module_name(path: String) -> String {
  path
  |> string.split("/")
  |> list.last()
  |> result.unwrap("")
  |> string.replace(".json", "")
  |> utils.snake_case()
}
