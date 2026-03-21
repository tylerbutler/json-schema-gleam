/// Code generator that transforms JSON Schema AST into Gleam source code.
/// Produces type definitions and optionally JSON decoders.
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import json_schema_gleam/schema.{
  type SchemaNode, type SchemaResult, type SchemaType, AllOfType, AnyOfType,
  ArrayType, BooleanType, ConstType, EnumType, IntegerType, NullType, NumberType,
  ObjectType, OneOfType, RefType, StringType, StringValue, UnionType,
  UnknownType,
}
import json_schema_gleam/utils

/// Options for code generation
pub type GenerateOptions {
  GenerateOptions(
    /// Name for the generated module
    module_name: String,
    /// Whether to generate JSON decoders
    generate_decoders: Bool,
    /// Whether to generate JSON encoders (not yet implemented; emits a TODO comment)
    generate_encoders: Bool,
    /// Prefix for type names (useful for avoiding conflicts)
    type_prefix: String,
  )
}

/// Default generation options
pub fn default_options(module_name: String) -> GenerateOptions {
  GenerateOptions(
    module_name: module_name,
    generate_decoders: True,
    generate_encoders: False,
    type_prefix: "",
  )
}

/// Generate Gleam source code from a parsed schema
pub fn generate(schema: SchemaResult, options: GenerateOptions) -> String {
  let imports = generate_imports(options)
  let types = generate_types(schema, options)
  let decoders = case options.generate_decoders {
    True -> generate_decoders(schema, options)
    False -> ""
  }
  let encoders = case options.generate_encoders {
    True -> generate_encoders()
    False -> ""
  }

  string.join(
    [imports, "", types, "", decoders, encoders]
      |> list.filter(fn(s) { s != "" }),
    "\n",
  )
}

fn generate_imports(options: GenerateOptions) -> String {
  let base_imports = [
    "import gleam/option.{type Option, None, Some}",
    "import gleam/dynamic.{type Dynamic}",
  ]

  let decoder_imports = case options.generate_decoders {
    True -> ["import gleam/json", "import gleam/result"]
    False -> []
  }

  let encoder_imports = case options.generate_encoders {
    True -> ["import gleam/json"]
    False -> []
  }

  list.flatten([base_imports, decoder_imports, encoder_imports])
  |> list.unique()
  |> string.join("\n")
}

fn generate_types(schema: SchemaResult, options: GenerateOptions) -> String {
  let root_type =
    generate_type_from_node(
      schema.root,
      root_type_name(schema.root, options),
      options,
    )

  let def_types =
    schema.definitions
    |> dict.to_list()
    |> list.map(fn(pair) {
      let #(name, node) = pair
      generate_type_from_node(node, def_to_type_name(name, options), options)
    })

  // Collect inline anonymous object types from all nodes
  let inline_types = collect_inline_types(schema.root, options)
  let def_inline_types =
    schema.definitions
    |> dict.to_list()
    |> list.flat_map(fn(pair) {
      let #(_, node) = pair
      collect_inline_types(node, options)
    })

  [root_type, ..def_types]
  |> list.append(inline_types)
  |> list.append(def_inline_types)
  |> list.filter(fn(s) { s != "" })
  |> list.unique()
  |> string.join("\n\n")
}

fn root_type_name(node: SchemaNode, options: GenerateOptions) -> String {
  case node.title {
    Some(title) -> options.type_prefix <> utils.pascal_case(title)
    None ->
      case options.module_name {
        "" -> options.type_prefix <> "Root"
        name -> options.type_prefix <> utils.pascal_case(name)
      }
  }
}

fn def_to_type_name(def_name: String, options: GenerateOptions) -> String {
  options.type_prefix <> utils.pascal_case(def_name)
}

fn generate_type_from_node(
  node: SchemaNode,
  type_name: String,
  options: GenerateOptions,
) -> String {
  case node.schema_type {
    ObjectType -> generate_record_type(node, type_name, options)
    EnumType -> generate_enum_type(node, type_name)
    ArrayType -> generate_array_type_alias(node, type_name, options)
    OneOfType -> generate_union_type(node, type_name, options)
    AnyOfType -> generate_union_type(node, type_name, options)
    _ -> ""
  }
}

fn generate_record_type(
  node: SchemaNode,
  type_name: String,
  options: GenerateOptions,
) -> String {
  let doc = case node.description {
    Some(desc) -> "/// " <> sanitize_comment(desc) <> "\n"
    None -> ""
  }

  let deprecated_doc = case node.deprecated {
    True -> "/// @deprecated\n"
    False -> ""
  }

  let properties = dict.to_list(node.properties)

  case properties {
    [] ->
      doc
      <> deprecated_doc
      <> "pub type "
      <> type_name
      <> " {\n  "
      <> type_name
      <> "\n}"
    _ -> {
      let fields =
        properties
        |> list.map(fn(pair) {
          let #(name, prop_node) = pair
          let field_name = utils.snake_case(name)
          let field_type = node_to_gleam_type(prop_node, options)
          let is_required = list.contains(node.required, name)
          let final_type = case is_required {
            True -> field_type
            False -> "Option(" <> field_type <> ")"
          }
          "    " <> field_name <> ": " <> final_type <> ","
        })
        |> string.join("\n")

      doc
      <> deprecated_doc
      <> "pub type "
      <> type_name
      <> " {\n  "
      <> type_name
      <> "(\n"
      <> fields
      <> "\n  )\n}"
    }
  }
}

fn generate_enum_type(node: SchemaNode, type_name: String) -> String {
  case node.enum_values {
    None -> ""
    Some(values) -> {
      let doc = case node.description {
        Some(desc) -> "/// " <> sanitize_comment(desc) <> "\n"
        None -> ""
      }

      let string_values =
        values
        |> list.filter_map(fn(v) {
          case v {
            StringValue(s) -> Ok(s)
            _ -> Error(Nil)
          }
        })

      let all_strings = list.length(string_values) == list.length(values)

      let variants = case all_strings {
        True -> {
          let raw =
            string_values
            |> list.map(sanitize_constructor)
          deduplicate_names(raw)
          |> list.map(fn(s) { "  " <> s })
          |> string.join("\n")
        }
        False ->
          values
          |> list.index_map(fn(_v, i) {
            "  " <> type_name <> "Variant" <> int.to_string(i)
          })
          |> string.join("\n")
      }

      doc <> "pub type " <> type_name <> " {\n" <> variants <> "\n}"
    }
  }
}

fn generate_array_type_alias(
  node: SchemaNode,
  type_name: String,
  options: GenerateOptions,
) -> String {
  case node.items {
    Some(items_node) -> {
      let item_type = node_to_gleam_type(items_node, options)
      "pub type " <> type_name <> " =\n  List(" <> item_type <> ")"
    }
    None -> "pub type " <> type_name <> " =\n  List(Dynamic)"
  }
}

fn generate_union_type(
  node: SchemaNode,
  type_name: String,
  options: GenerateOptions,
) -> String {
  let variants = case node.one_of {
    Some(nodes) -> nodes
    None ->
      case node.any_of {
        Some(nodes) -> nodes
        None -> []
      }
  }

  case variants {
    [] -> ""
    _ -> {
      let doc = case node.description {
        Some(desc) -> "/// " <> sanitize_comment(desc) <> "\n"
        None -> ""
      }

      let variant_defs =
        variants
        |> list.index_map(fn(variant_node, i) {
          let variant_name = case variant_node.title {
            Some(title) -> utils.pascal_case(title)
            None -> type_name <> "Variant" <> int.to_string(i)
          }
          let inner_type = node_to_gleam_type(variant_node, options)
          "  " <> variant_name <> "(" <> inner_type <> ")"
        })
        |> string.join("\n")

      doc <> "pub type " <> type_name <> " {\n" <> variant_defs <> "\n}"
    }
  }
}

fn node_to_gleam_type(node: SchemaNode, options: GenerateOptions) -> String {
  case node.schema_type {
    StringType -> "String"
    IntegerType -> "Int"
    NumberType -> "Float"
    BooleanType -> "Bool"
    NullType -> "Nil"
    ArrayType ->
      case node.items {
        Some(items) -> "List(" <> node_to_gleam_type(items, options) <> ")"
        None -> "List(Dynamic)"
      }
    ObjectType ->
      case node.title {
        Some(title) -> options.type_prefix <> utils.pascal_case(title)
        None ->
          case derive_type_name_from_path(node.path) {
            Ok(name) -> options.type_prefix <> name
            Error(_) -> "Dynamic"
          }
      }
    RefType ->
      case node.ref {
        Some(ref) -> ref_to_type_name(ref, options)
        None -> "Dynamic"
      }
    EnumType ->
      case node.title {
        Some(title) -> options.type_prefix <> utils.pascal_case(title)
        None -> "String"
      }
    UnionType(types) -> {
      // For simple unions like ["string", "null"], use Option
      case types {
        [t, NullType] -> "Option(" <> schema_type_to_gleam(t) <> ")"
        [NullType, t] -> "Option(" <> schema_type_to_gleam(t) <> ")"
        _ -> "Dynamic"
      }
    }
    OneOfType | AnyOfType | AllOfType ->
      case node.title {
        Some(title) -> options.type_prefix <> utils.pascal_case(title)
        None -> "Dynamic"
      }
    ConstType -> "String"
    UnknownType -> "Dynamic"
  }
}

fn schema_type_to_gleam(t: SchemaType) -> String {
  case t {
    StringType -> "String"
    IntegerType -> "Int"
    NumberType -> "Float"
    BooleanType -> "Bool"
    NullType -> "Nil"
    ArrayType -> "List(Dynamic)"
    ObjectType -> "Dynamic"
    _ -> "Dynamic"
  }
}

fn ref_to_type_name(ref: String, options: GenerateOptions) -> String {
  // Parse refs like "#/$defs/behaviorName" or "#/definitions/Foo"
  let parts = string.split(ref, "/")
  case list.last(parts) {
    Ok(name) -> options.type_prefix <> utils.pascal_case(name)
    Error(_) -> "Dynamic"
  }
}

fn generate_decoders(schema: SchemaResult, options: GenerateOptions) -> String {
  let root_decoder =
    generate_decoder_for_node(
      schema.root,
      root_type_name(schema.root, options),
      options,
    )

  let def_decoders =
    schema.definitions
    |> dict.to_list()
    |> list.map(fn(pair) {
      let #(name, node) = pair
      generate_decoder_for_node(node, def_to_type_name(name, options), options)
    })

  // Collect inline anonymous object decoders
  let inline_decoders = collect_inline_decoders(schema.root, options)
  let def_inline_decoders =
    schema.definitions
    |> dict.to_list()
    |> list.flat_map(fn(pair) {
      let #(_, node) = pair
      collect_inline_decoders(node, options)
    })

  [root_decoder, ..def_decoders]
  |> list.append(inline_decoders)
  |> list.append(def_inline_decoders)
  |> list.filter(fn(s) { s != "" })
  |> list.unique()
  |> string.join("\n\n")
}

fn generate_decoder_for_node(
  node: SchemaNode,
  type_name: String,
  options: GenerateOptions,
) -> String {
  case node.schema_type {
    ObjectType -> generate_object_decoder(node, type_name, options)
    EnumType -> generate_enum_decoder(node, type_name)
    _ -> ""
  }
}

fn generate_object_decoder(
  node: SchemaNode,
  type_name: String,
  options: GenerateOptions,
) -> String {
  let fn_name = utils.snake_case(type_name) <> "_decoder"
  let properties = dict.to_list(node.properties)

  case properties {
    [] ->
      "pub fn "
      <> fn_name
      <> "(dyn: Dynamic) -> Result("
      <> type_name
      <> ", List(dynamic.DecodeError)) {\n  use _ <- result.try(dynamic.dict(dynamic.string, dynamic.dynamic)(dyn))\n  Ok("
      <> type_name
      <> ")\n}"
    _ -> {
      let field_decoders =
        properties
        |> list.map(fn(pair) {
          let #(name, prop_node) = pair
          let field_name = utils.snake_case(name)
          let is_required = list.contains(node.required, name)
          generate_field_decoder(
            name,
            field_name,
            prop_node,
            is_required,
            options,
          )
        })
        |> string.join("\n")

      let constructor_args =
        properties
        |> list.map(fn(pair) {
          let #(name, _) = pair
          utils.snake_case(name) <> ": " <> utils.snake_case(name)
        })
        |> string.join(", ")

      "pub fn "
      <> fn_name
      <> "(dyn: Dynamic) -> Result("
      <> type_name
      <> ", List(dynamic.DecodeError)) {\n"
      <> field_decoders
      <> "\n  Ok("
      <> type_name
      <> "("
      <> constructor_args
      <> "))\n}"
    }
  }
}

fn generate_field_decoder(
  json_name: String,
  field_name: String,
  node: SchemaNode,
  is_required: Bool,
  options: GenerateOptions,
) -> String {
  let decoder = gleam_decoder_for_type(node, options)

  case is_required {
    True ->
      "  use "
      <> field_name
      <> " <- result.try(dynamic.field(\""
      <> json_name
      <> "\", "
      <> decoder
      <> ")(dyn))"
    False ->
      "  use "
      <> field_name
      <> " <- result.try(dynamic.optional_field(\""
      <> json_name
      <> "\", "
      <> decoder
      <> ")(dyn))"
  }
}

fn gleam_decoder_for_type(node: SchemaNode, options: GenerateOptions) -> String {
  case node.schema_type {
    StringType -> "dynamic.string"
    IntegerType -> "dynamic.int"
    NumberType -> "dynamic.float"
    BooleanType -> "dynamic.bool"
    ArrayType ->
      case node.items {
        Some(items) ->
          "dynamic.list(" <> gleam_decoder_for_type(items, options) <> ")"
        None -> "dynamic.list(dynamic.dynamic)"
      }
    ObjectType ->
      case node.title {
        Some(title) ->
          utils.snake_case(options.type_prefix <> utils.pascal_case(title))
          <> "_decoder"
        None ->
          case derive_type_name_from_path(node.path) {
            Ok(name) ->
              utils.snake_case(options.type_prefix <> name) <> "_decoder"
            Error(_) -> "dynamic.dynamic"
          }
      }
    RefType ->
      case node.ref {
        Some(ref) -> {
          let type_name = ref_to_type_name(ref, options)
          utils.snake_case(type_name) <> "_decoder"
        }
        None -> "dynamic.dynamic"
      }
    EnumType -> "dynamic.string"
    _ -> "dynamic.dynamic"
  }
}

fn generate_enum_decoder(node: SchemaNode, type_name: String) -> String {
  let fn_name = utils.snake_case(type_name) <> "_decoder"

  case node.enum_values {
    None -> ""
    Some(values) -> {
      let string_values =
        values
        |> list.filter_map(fn(v) {
          case v {
            StringValue(s) -> Ok(s)
            _ -> Error(Nil)
          }
        })

      let all_strings = list.length(string_values) == list.length(values)

      case all_strings {
        False -> ""
        True -> {
          let sanitized =
            string_values
            |> list.map(sanitize_constructor)
          let deduped = deduplicate_names(sanitized)
          let pairs = list.zip(string_values, deduped)

          let cases =
            pairs
            |> list.map(fn(pair) {
              let #(original, constructor) = pair
              "      \"" <> original <> "\" -> Ok(" <> constructor <> ")"
            })
            |> string.join("\n")

          "pub fn "
          <> fn_name
          <> "(dyn: Dynamic) -> Result("
          <> type_name
          <> ", List(dynamic.DecodeError)) {\n  case dynamic.string(dyn) {\n    Ok(s) -> case s {\n"
          <> cases
          <> "\n      _ -> Error([dynamic.DecodeError(expected: \""
          <> type_name
          <> "\", found: s, path: [])])\n    }\n    Error(e) -> Error(e)\n  }\n}"
        }
      }
    }
  }
}

fn generate_encoders() -> String {
  "// TODO: Encoder generation is not yet implemented"
}

// Helper functions

/// Sanitize a string for embedding in a `///` doc comment.
/// Replaces newlines with proper multi-line doc comment continuations
/// and strips control characters.
fn sanitize_comment(text: String) -> String {
  text
  |> string.replace("\r\n", "\n")
  |> string.replace("\r", "\n")
  |> string.replace("\n", "\n/// ")
  |> strip_control_chars()
}

fn strip_control_chars(s: String) -> String {
  s
  |> string.to_graphemes()
  |> list.map(fn(c) {
    case c {
      "\n" -> c
      "\t" -> " "
      _ -> {
        case string.to_utf_codepoints(c) {
          [cp] -> {
            let code = string.utf_codepoint_to_int(cp)
            case code < 32 {
              True -> ""
              False -> c
            }
          }
          _ -> c
        }
      }
    }
  })
  |> string.join("")
}

/// Sanitize an enum value string into a valid Gleam constructor name.
/// Applies pascal_case, strips non-alphanumeric chars, and prepends "V" if
/// the result starts with a digit.
fn sanitize_constructor(s: String) -> String {
  let pc = utils.pascal_case(s)
  let cleaned =
    pc
    |> string.to_graphemes()
    |> list.filter(fn(c) { is_alphanumeric(c) })
    |> string.join("")
  let cleaned = case cleaned {
    "" -> "Value"
    _ -> cleaned
  }
  // If starts with a digit, prepend "V"
  case string.first(cleaned) {
    Ok(first) ->
      case is_digit(first) {
        True -> "V" <> cleaned
        False -> cleaned
      }
    Error(_) -> "Value"
  }
}

fn is_alphanumeric(c: String) -> Bool {
  let lower = string.lowercase(c)
  case lower {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_digit(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Deduplicate a list of constructor names by appending numeric suffixes.
fn deduplicate_names(names: List(String)) -> List(String) {
  let indexed = list.index_map(names, fn(name, i) { #(i, name) })
  list.map(indexed, fn(pair) {
    let #(i, name) = pair
    let count_before =
      indexed
      |> list.filter(fn(p) { p.1 == name && p.0 < i })
      |> list.length()
    case count_before {
      0 -> {
        // Check if there are duplicates after this one
        let count_total =
          names |> list.filter(fn(n) { n == name }) |> list.length()
        case count_total > 1 {
          True -> name <> "0"
          False -> name
        }
      }
      n -> name <> int.to_string(n)
    }
  })
}

/// Derive a type name from a JSON Pointer path.
/// e.g., "#/properties/address" -> Ok("Address")
fn derive_type_name_from_path(path: String) -> Result(String, Nil) {
  let parts = string.split(path, "/")
  case list.last(parts) {
    Ok(segment) ->
      case segment {
        "" | "#" -> Error(Nil)
        _ -> Ok(utils.pascal_case(segment))
      }
    _ -> Error(Nil)
  }
}

/// Collect type definitions for inline anonymous objects found in properties.
fn collect_inline_types(
  node: SchemaNode,
  options: GenerateOptions,
) -> List(String) {
  node.properties
  |> dict.to_list()
  |> list.flat_map(fn(pair) {
    let #(_, prop_node) = pair
    case prop_node.schema_type {
      ObjectType ->
        case prop_node.title {
          Some(_) -> []
          None ->
            case derive_type_name_from_path(prop_node.path) {
              Ok(name) -> {
                let type_name = options.type_prefix <> name
                let type_def =
                  generate_type_from_node(prop_node, type_name, options)
                // Recurse into nested inline objects
                let nested = collect_inline_types(prop_node, options)
                [type_def, ..nested]
              }
              Error(_) -> []
            }
        }
      _ -> []
    }
  })
}

/// Collect decoders for inline anonymous objects found in properties.
fn collect_inline_decoders(
  node: SchemaNode,
  options: GenerateOptions,
) -> List(String) {
  node.properties
  |> dict.to_list()
  |> list.flat_map(fn(pair) {
    let #(_, prop_node) = pair
    case prop_node.schema_type {
      ObjectType ->
        case prop_node.title {
          Some(_) -> []
          None ->
            case derive_type_name_from_path(prop_node.path) {
              Ok(name) -> {
                let type_name = options.type_prefix <> name
                let decoder =
                  generate_decoder_for_node(prop_node, type_name, options)
                let nested = collect_inline_decoders(prop_node, options)
                [decoder, ..nested]
              }
              Error(_) -> []
            }
        }
      _ -> []
    }
  })
}
