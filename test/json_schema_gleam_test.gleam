import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import json_schema_gleam
import json_schema_gleam/codegen
import json_schema_gleam/schema.{
  ArrayType, EnumType, IntegerType, ObjectType, SchemaMetadata, SchemaResult,
  SchemaStructure, StringType, StringValue, empty_metadata, empty_node,
  empty_structure,
}

pub fn main() {
  gleeunit.main()
}

// Test the codegen module directly with manually constructed schema nodes

pub fn simple_object_codegen_test() {
  let name_prop =
    schema.SchemaNode(
      ..empty_node("#/properties/name"),
      schema_type: StringType,
    )
  let age_prop =
    schema.SchemaNode(
      ..empty_node("#/properties/age"),
      schema_type: IntegerType,
    )

  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(
        ..empty_metadata("#"),
        title: Some("Person"),
        description: Some("A person record"),
      ),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([#("name", name_prop), #("age", age_prop)]),
        required: ["name"],
        additional_properties: False,
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
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
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: schema.EnumType,
      metadata: SchemaMetadata(
        ..empty_metadata("#"),
        title: Some("Status"),
        description: Some("Status values"),
      ),
      structure: SchemaStructure(
        ..empty_structure(),
        enum_values: Some([
          StringValue("pending"),
          StringValue("active"),
          StringValue("completed"),
        ]),
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
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
  let item_node =
    schema.SchemaNode(..empty_node("#/items"), schema_type: StringType)

  let root =
    schema.SchemaNode(
      schema_type: ArrayType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Tags")),
      structure: SchemaStructure(
        ..empty_structure(),
        items: Some(item_node),
      ),
      validation: schema.SchemaValidation(
        ..schema.empty_validation(),
        unique_items: True,
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
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
  let name_prop =
    schema.SchemaNode(
      ..empty_node("#/properties/name"),
      schema_type: StringType,
    )

  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("User")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([#("name", name_prop)]),
        required: ["name"],
        additional_properties: False,
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
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

// ---- Issue #8: Dynamic import when decoders disabled ----

pub fn dynamic_import_without_decoders_test() {
  // When generate_decoders is False, Dynamic import should still be present
  // because types can reference Dynamic
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ArrayType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Items")),
      structure: SchemaStructure(..empty_structure(), items: None),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "items",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  code
  |> string.contains("import gleam/dynamic.{type Dynamic}")
  |> should.be_true()
}

// ---- Issue #17: Sanitize generated comments ----

pub fn multiline_description_comment_test() {
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(
        ..empty_metadata("#"),
        title: Some("Thing"),
        description: Some("Line one\nLine two\nLine three"),
      ),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.new(),
        required: [],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "thing",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Multi-line descriptions should produce proper multi-line doc comments
  code |> string.contains("/// Line one\n/// Line two") |> should.be_true()
}

// ---- Issue #4: Enum value normalization ----

pub fn enum_duplicate_constructors_test() {
  // "foo-bar" and "foo_bar" both pascal_case to "FooBar" — should be deduplicated
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: EnumType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Status")),
      structure: SchemaStructure(
        ..empty_structure(),
        enum_values: Some([StringValue("foo-bar"), StringValue("foo_bar")]),
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "status",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Should have deduplicated names (FooBar0, FooBar1)
  code |> string.contains("FooBar0") |> should.be_true()
  code |> string.contains("FooBar1") |> should.be_true()
}

pub fn enum_digit_prefix_test() {
  // Values starting with digits should get "V" prefix
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: EnumType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Version")),
      structure: SchemaStructure(
        ..empty_structure(),
        enum_values: Some([StringValue("1.0"), StringValue("2.0")]),
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "version",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Constructors starting with digits get "V" prefix
  code |> string.contains("V10") |> should.be_true()
  code |> string.contains("V20") |> should.be_true()
}

// ---- Issue #5: module_name option ----

pub fn module_name_fallback_test() {
  // When root schema has no title, module_name should be used for root type name
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: None),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.new(),
        required: [],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "my_config",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Should use pascal_case of module_name as type name
  code |> string.contains("pub type MyConfig") |> should.be_true()
}

pub fn module_name_empty_falls_back_to_root_test() {
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: None),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.new(),
        required: [],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Empty module_name should still fall back to "Root"
  code |> string.contains("pub type Root") |> should.be_true()
}

// ---- Issue #6: generate_encoders stub ----

pub fn encoder_stub_comment_test() {
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Foo")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.new(),
        required: [],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "foo",
      generate_decoders: False,
      generate_encoders: True,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Should emit a TODO comment instead of empty string
  code
  |> string.contains("// TODO: Encoder generation is not yet implemented")
  |> should.be_true()
}

// ---- Issue #3/#10: Optional decoder fields ----

pub fn optional_field_decoder_uses_optional_field_test() {
  let name_prop =
    schema.SchemaNode(
      ..empty_node("#/properties/name"),
      schema_type: StringType,
    )
  let bio_prop =
    schema.SchemaNode(..empty_node("#/properties/bio"), schema_type: StringType)

  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("User")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([#("name", name_prop), #("bio", bio_prop)]),
        required: ["name"],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "user",
      generate_decoders: True,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Optional field should use dynamic.optional_field instead of error-swallowing pattern
  code |> string.contains("dynamic.optional_field(\"bio\"") |> should.be_true()
  // Should NOT contain the old error-swallowing pattern
  code |> string.contains("Error(_) -> None") |> should.be_false()
}

// ---- Issue #11: Empty object validation ----

pub fn empty_object_decoder_validates_dict_test() {
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Empty")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.new(),
        required: [],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "empty",
      generate_decoders: True,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Should validate that input is actually a dict, not just return Ok
  code
  |> string.contains("dynamic.dict(dynamic.string, dynamic.dynamic)")
  |> should.be_true()
}

// ---- Issue #7: Untitled inline objects ----

pub fn inline_object_gets_type_name_from_path_test() {
  let address_prop =
    schema.SchemaNode(
      ..empty_node("#/properties/address"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(
        ..empty_metadata("#/properties/address"),
        title: None,
      ),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([
          #(
            "street",
            schema.SchemaNode(
              ..empty_node("#/properties/address/properties/street"),
              schema_type: StringType,
            ),
          ),
        ]),
        required: ["street"],
      ),
    )

  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Person")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([#("address", address_prop)]),
        required: ["address"],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "person",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // The inline object should get type name "Address" derived from path
  code |> string.contains("address: Address") |> should.be_true()
  // And a type definition should be generated for it
  code |> string.contains("pub type Address") |> should.be_true()
  code |> string.contains("street: String") |> should.be_true()
}

pub fn inline_object_decoder_generated_test() {
  let address_prop =
    schema.SchemaNode(
      ..empty_node("#/properties/address"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(
        ..empty_metadata("#/properties/address"),
        title: None,
      ),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([
          #(
            "city",
            schema.SchemaNode(
              ..empty_node("#/properties/address/properties/city"),
              schema_type: StringType,
            ),
          ),
        ]),
        required: ["city"],
      ),
    )

  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Person")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([#("address", address_prop)]),
        required: ["address"],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "person",
      generate_decoders: True,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // A decoder should be generated for the inline object
  code |> string.contains("pub fn address_decoder") |> should.be_true()
}

// ---- Issue #15: String utils deduplication ----

pub fn utils_pascal_case_test() {
  // Verify that the utils module is used correctly
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(
        ..empty_metadata("#"),
        title: Some("my-great-type"),
      ),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.new(),
        required: [],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "test",
      generate_decoders: False,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub type MyGreatType") |> should.be_true()
}

// ---- Issue #4: Enum decoder with sanitized constructors ----

// ---- Issue #19: Builder-style option configuration ----

pub fn with_decoders_disables_decoder_generation_test() {
  let name_prop =
    schema.SchemaNode(
      ..empty_node("#/properties/name"),
      schema_type: StringType,
    )

  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("User")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.from_list([#("name", name_prop)]),
        required: ["name"],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    json_schema_gleam.default_options("user")
    |> json_schema_gleam.with_decoders(False)

  let code = codegen.generate(schema_result, options)

  // Type should still be generated
  code |> string.contains("pub type User") |> should.be_true()
  // Decoder should NOT be generated
  code |> string.contains("pub fn user_decoder") |> should.be_false()
}

pub fn with_type_prefix_adds_prefix_test() {
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: ObjectType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Person")),
      structure: SchemaStructure(
        ..empty_structure(),
        properties: dict.new(),
        required: [],
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    json_schema_gleam.default_options("person")
    |> json_schema_gleam.with_type_prefix("Schema")

  let code = codegen.generate(schema_result, options)

  code |> string.contains("pub type SchemaPerson") |> should.be_true()
}

pub fn with_module_name_changes_module_test() {
  let options =
    json_schema_gleam.default_options("original")
    |> json_schema_gleam.with_module_name("renamed")

  options.module_name |> should.equal("renamed")
}

pub fn builder_chain_pattern_test() {
  let options =
    json_schema_gleam.default_options("my_types")
    |> json_schema_gleam.with_type_prefix("Schema")
    |> json_schema_gleam.with_decoders(False)
    |> json_schema_gleam.with_encoders(True)
    |> json_schema_gleam.with_module_name("custom")

  options.module_name |> should.equal("custom")
  options.generate_decoders |> should.be_false()
  options.generate_encoders |> should.be_true()
  options.type_prefix |> should.equal("Schema")
}

pub fn enum_decoder_with_sanitized_constructors_test() {
  let root =
    schema.SchemaNode(
      ..empty_node("#"),
      schema_type: EnumType,
      metadata: SchemaMetadata(..empty_metadata("#"), title: Some("Priority")),
      structure: SchemaStructure(
        ..empty_structure(),
        enum_values: Some([
          StringValue("foo-bar"),
          StringValue("foo_bar"),
        ]),
      ),
    )

  let schema_result =
    SchemaResult(root: root, definitions: dict.new(), errors: [], warnings: [])

  let options =
    codegen.GenerateOptions(
      module_name: "priority",
      generate_decoders: True,
      generate_encoders: False,
      type_prefix: "",
    )

  let code = codegen.generate(schema_result, options)

  // Decoder should map original strings to deduplicated constructors
  code
  |> string.contains("\"foo-bar\" -> Ok(FooBar0)")
  |> should.be_true()
  code
  |> string.contains("\"foo_bar\" -> Ok(FooBar1)")
  |> should.be_true()
}
