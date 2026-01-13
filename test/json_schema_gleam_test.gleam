import gleeunit
import gleeunit/should
import json_schema_gleam
import json_schema_gleam/codegen
import json_schema_gleam/schema.{
  ArrayType, IntegerType, ObjectType, SchemaNode, SchemaResult,
  StringType, StringValue,
}
import gleam/dict
import gleam/option.{None, Some}
import gleam/string

pub fn main() {
  gleeunit.main()
}

// Test the codegen module directly with manually constructed schema nodes

pub fn simple_object_codegen_test() {
  let name_prop = SchemaNode(
    path: "#/properties/name",
    schema_type: StringType,
    title: None,
    description: None,
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: None,
    enum_values: None,
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: False,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let age_prop = SchemaNode(
    path: "#/properties/age",
    schema_type: IntegerType,
    title: None,
    description: None,
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: None,
    enum_values: None,
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: False,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let root = SchemaNode(
    path: "#",
    schema_type: ObjectType,
    title: Some("Person"),
    description: Some("A person record"),
    properties: dict.from_list([#("name", name_prop), #("age", age_prop)]),
    required: ["name"],
    additional_properties: False,
    items: None,
    enum_values: None,
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: False,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let schema_result = SchemaResult(
    root: root,
    definitions: dict.new(),
    errors: [],
    warnings: [],
  )

  let options = codegen.GenerateOptions(
    module_name: "person",
    generate_decoders: False,
    generate_encoders: False,
    type_prefix: "",
  )

  let code = codegen.generate(schema_result, options)

  // Check that the generated code contains expected elements
  code |> should.not_equal("")
  code |> string.contains("pub type Person") |> should.be_true()
  code |> string.contains("name: String") |> should.be_true()
  code |> string.contains("age: Option(Int)") |> should.be_true()
}

pub fn enum_codegen_test() {
  let root = SchemaNode(
    path: "#",
    schema_type: schema.EnumType,
    title: Some("Status"),
    description: Some("Status values"),
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: None,
    enum_values: Some([
      StringValue("pending"),
      StringValue("active"),
      StringValue("completed"),
    ]),
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: False,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let schema_result = SchemaResult(
    root: root,
    definitions: dict.new(),
    errors: [],
    warnings: [],
  )

  let options = codegen.GenerateOptions(
    module_name: "status",
    generate_decoders: False,
    generate_encoders: False,
    type_prefix: "",
  )

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub type Status") |> should.be_true()
  code |> string.contains("Pending") |> should.be_true()
  code |> string.contains("Active") |> should.be_true()
  code |> string.contains("Completed") |> should.be_true()
}

pub fn array_codegen_test() {
  let item_node = SchemaNode(
    path: "#/items",
    schema_type: StringType,
    title: None,
    description: None,
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: None,
    enum_values: None,
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: False,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let root = SchemaNode(
    path: "#",
    schema_type: ArrayType,
    title: Some("Tags"),
    description: None,
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: Some(item_node),
    enum_values: None,
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: True,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let schema_result = SchemaResult(
    root: root,
    definitions: dict.new(),
    errors: [],
    warnings: [],
  )

  let options = codegen.GenerateOptions(
    module_name: "tags",
    generate_decoders: False,
    generate_encoders: False,
    type_prefix: "",
  )

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub type Tags") |> should.be_true()
  code |> string.contains("List(String)") |> should.be_true()
}

pub fn decoder_generation_test() {
  let name_prop = SchemaNode(
    path: "#/properties/name",
    schema_type: StringType,
    title: None,
    description: None,
    properties: dict.new(),
    required: [],
    additional_properties: True,
    items: None,
    enum_values: None,
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: False,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let root = SchemaNode(
    path: "#",
    schema_type: ObjectType,
    title: Some("User"),
    description: None,
    properties: dict.from_list([#("name", name_prop)]),
    required: ["name"],
    additional_properties: False,
    items: None,
    enum_values: None,
    const_value: None,
    pattern: None,
    default_value: None,
    one_of: None,
    any_of: None,
    all_of: None,
    ref: None,
    min_items: None,
    max_items: None,
    unique_items: False,
    minimum: None,
    maximum: None,
    min_length: None,
    max_length: None,
    format: None,
    deprecated: False,
  )

  let schema_result = SchemaResult(
    root: root,
    definitions: dict.new(),
    errors: [],
    warnings: [],
  )

  let options = codegen.GenerateOptions(
    module_name: "user",
    generate_decoders: True,
    generate_encoders: False,
    type_prefix: "",
  )

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub fn user_decoder") |> should.be_true()
  code |> string.contains("dynamic.field") |> should.be_true()
  code |> string.contains("dynamic.string") |> should.be_true()
}

pub fn default_options_test() {
  let options = json_schema_gleam.default_options("my_module")

  options.module_name |> should.equal("my_module")
  options.generate_decoders |> should.be_true()
  options.generate_encoders |> should.be_false()
  options.type_prefix |> should.equal("")
}
