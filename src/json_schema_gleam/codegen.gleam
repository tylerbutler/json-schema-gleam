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

/// Options for code generation
pub type GenerateOptions {
  GenerateOptions(
    /// Name for the generated module
    module_name: String,
    /// Whether to generate JSON decoders
    generate_decoders: Bool,
    /// Whether to generate JSON encoders
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
  let base_imports = ["import gleam/option.{type Option, None, Some}"]

  let decoder_imports = case options.generate_decoders {
    True -> [
      "import gleam/dynamic.{type Dynamic}",
      "import gleam/json",
      "import gleam/result",
    ]
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

  [root_type, ..def_types]
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n\n")
}

fn root_type_name(node: SchemaNode, options: GenerateOptions) -> String {
  case node.title {
    Some(title) -> options.type_prefix <> pascal_case(title)
    None -> options.type_prefix <> "Root"
  }
}

fn def_to_type_name(def_name: String, options: GenerateOptions) -> String {
  options.type_prefix <> pascal_case(def_name)
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
    Some(desc) -> "/// " <> desc <> "\n"
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
          let field_name = snake_case(name)
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
        Some(desc) -> "/// " <> desc <> "\n"
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
        True ->
          string_values
          |> list.map(fn(s) { "  " <> pascal_case(s) })
          |> string.join("\n")
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
        Some(desc) -> "/// " <> desc <> "\n"
        None -> ""
      }

      let variant_defs =
        variants
        |> list.index_map(fn(variant_node, i) {
          let variant_name = case variant_node.title {
            Some(title) -> pascal_case(title)
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
        Some(title) -> options.type_prefix <> pascal_case(title)
        None -> "Dynamic"
      }
    RefType ->
      case node.ref {
        Some(ref) -> ref_to_type_name(ref, options)
        None -> "Dynamic"
      }
    EnumType ->
      case node.title {
        Some(title) -> options.type_prefix <> pascal_case(title)
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
        Some(title) -> options.type_prefix <> pascal_case(title)
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
    Ok(name) -> options.type_prefix <> pascal_case(name)
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

  [root_decoder, ..def_decoders]
  |> list.filter(fn(s) { s != "" })
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
  let fn_name = snake_case(type_name) <> "_decoder"
  let properties = dict.to_list(node.properties)

  case properties {
    [] ->
      "pub fn "
      <> fn_name
      <> "(dyn: Dynamic) -> Result("
      <> type_name
      <> ", List(dynamic.DecodeError)) {\n  Ok("
      <> type_name
      <> ")\n}"
    _ -> {
      let field_decoders =
        properties
        |> list.map(fn(pair) {
          let #(name, prop_node) = pair
          let field_name = snake_case(name)
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
          snake_case(name) <> ": " <> snake_case(name)
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
      "  let "
      <> field_name
      <> " = case dynamic.field(\""
      <> json_name
      <> "\", dynamic.optional("
      <> decoder
      <> "))(dyn) {\n    Ok(v) -> v\n    Error(_) -> None\n  }"
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
          snake_case(options.type_prefix <> pascal_case(title)) <> "_decoder"
        None -> "dynamic.dynamic"
      }
    RefType ->
      case node.ref {
        Some(ref) -> {
          let type_name = ref_to_type_name(ref, options)
          snake_case(type_name) <> "_decoder"
        }
        None -> "dynamic.dynamic"
      }
    EnumType -> "dynamic.string"
    _ -> "dynamic.dynamic"
  }
}

fn generate_enum_decoder(node: SchemaNode, type_name: String) -> String {
  let fn_name = snake_case(type_name) <> "_decoder"

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
          let cases =
            string_values
            |> list.map(fn(s) {
              "      \"" <> s <> "\" -> Ok(" <> pascal_case(s) <> ")"
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
  // TODO: Implement encoders if needed
  ""
}

// String utilities

fn pascal_case(s: String) -> String {
  s
  |> string.replace("-", "_")
  |> string.replace(" ", "_")
  |> string.split("_")
  |> list.map(capitalize)
  |> string.join("")
}

fn snake_case(s: String) -> String {
  s
  |> string.replace("-", "_")
  |> string.replace(" ", "_")
  |> insert_underscores_before_caps()
  |> string.lowercase()
}

fn insert_underscores_before_caps(s: String) -> String {
  s
  |> string.to_graphemes()
  |> list.index_fold("", fn(acc, char, i) {
    case i > 0 && is_uppercase(char) {
      True -> acc <> "_" <> char
      False -> acc <> char
    }
  })
}

fn is_uppercase(s: String) -> Bool {
  let upper = string.uppercase(s)
  let lower = string.lowercase(s)
  upper == s && upper != lower
}

fn capitalize(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}
