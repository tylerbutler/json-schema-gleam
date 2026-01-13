/// CLI module for json_schema_gleam using glint for argument parsing.
import argv
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import json_schema_gleam/codegen
import json_schema_gleam/parser_ffi
import simplifile

/// Run the CLI with command line arguments
pub fn run() {
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

  case args {
    [schema_path, ..rest] -> {
      let output_path = case rest {
        [out, ..] -> Some(out)
        [] -> None
      }

      let options =
        codegen.GenerateOptions(
          module_name: case module_flag {
            Ok(name) if name != "" -> name
            _ -> derive_module_name(schema_path)
          },
          generate_decoders: case no_decoders_flag {
            Ok(True) -> False
            _ -> True
          },
          generate_encoders: False,
          type_prefix: case prefix_flag {
            Ok(p) -> p
            _ -> ""
          },
        )

      case generate_code(schema_path, options) {
        Ok(code) -> {
          case output_path {
            Some(path) -> {
              case simplifile.write(path, code) {
                Ok(_) -> io.println("Generated types written to " <> path)
                Error(e) ->
                  io.println("Error writing file: " <> string.inspect(e))
              }
            }
            None -> io.println(code)
          }
        }
        Error(e) -> io.println("Error: " <> e)
      }
    }
    [] -> io.println("Error: No schema file specified")
  }
}

fn generate_code(
  schema_path: String,
  options: codegen.GenerateOptions,
) -> Result(String, String) {
  use schema <- result.try(parser_ffi.parse_file(schema_path))
  Ok(codegen.generate(schema, options))
}

fn derive_module_name(path: String) -> String {
  path
  |> string.split("/")
  |> last_or("")
  |> string.replace(".json", "")
  |> string.replace("-", "_")
  |> string.lowercase()
}

fn last_or(list: List(a), default: a) -> a {
  case list {
    [] -> default
    [x] -> x
    [_, ..rest] -> last_or(rest, default)
  }
}
