# json_schema_gleam

Generate Gleam types from JSON Schema definitions.

This library parses JSON Schema files (Draft-07) and generates Gleam type definitions along with optional JSON decoders.

## Features

- **JSON Schema Draft-07 support** - Handles objects, arrays, enums, oneOf/anyOf, $ref, and more
- **Type generation** - Creates Gleam record types from object schemas
- **Enum generation** - Creates Gleam custom types from enum schemas
- **Decoder generation** - Optionally generates JSON decoders for parsing
- **CLI tool** - Command-line interface for code generation
- **Library API** - Programmatic access for build-time generation

## Architecture

This is a **hybrid Gleam/Elixir project**:
- **Gleam** handles code generation logic and provides the public API
- **Elixir** (via FFI) provides JSON Schema parsing using the battle-tested `json_schema` library

Consumers get a pure Gleam API - no FFI knowledge required.

## Installation

```sh
gleam add json_schema_gleam
```

## Usage

### CLI

```sh
# Generate types to stdout
gleam run -- schema.json

# Generate types to a file
gleam run -- schema.json src/my_types.gleam

# With options
gleam run -- schema.json --module my_types --prefix My --no-decoders
```

### Library API

```gleam
import json_schema_gleam

pub fn main() {
  let options = json_schema_gleam.default_options("my_types")

  case json_schema_gleam.generate_from_file("schema.json", options) {
    Ok(code) -> {
      // Write to file or use directly
      io.println(code)
    }
    Error(e) -> io.println("Error: " <> e)
  }
}
```

### Custom Options

```gleam
import json_schema_gleam/codegen.{GenerateOptions}

let options = GenerateOptions(
  module_name: "my_types",
  generate_decoders: True,
  generate_encoders: False,
  type_prefix: "Schema",
)
```

## Supported JSON Schema Features

| Feature | Support |
|---------|---------|
| `type: "object"` | ✅ Generates record types |
| `type: "array"` | ✅ Generates `List(T)` |
| `type: "string"` | ✅ Maps to `String` |
| `type: "integer"` | ✅ Maps to `Int` |
| `type: "number"` | ✅ Maps to `Float` |
| `type: "boolean"` | ✅ Maps to `Bool` |
| `type: "null"` | ✅ Maps to `Nil` |
| `enum` | ✅ Generates custom types |
| `oneOf` / `anyOf` | ✅ Generates union types |
| `$ref` / `$defs` | ✅ Resolves references |
| `required` | ✅ Non-required fields use `Option(T)` |
| `pattern` | ⚠️ Parsed but not enforced |
| `format` | ⚠️ Parsed but not enforced |

## Example

Given this JSON Schema:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Person",
  "type": "object",
  "required": ["name"],
  "properties": {
    "name": { "type": "string" },
    "age": { "type": "integer" }
  }
}
```

Generates:

```gleam
import gleam/option.{type Option, None, Some}
import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/result

/// A person record
pub type Person {
  Person(
    name: String,
    age: Option(Int),
  )
}

pub fn person_decoder(dyn: Dynamic) -> Result(Person, List(dynamic.DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(dyn))
  let age = case dynamic.field("age", dynamic.optional(dynamic.int))(dyn) {
    Ok(v) -> v
    Error(_) -> None
  }
  Ok(Person(name: name, age: age))
}
```

## Development

```sh
# Build
gleam build

# Run tests
gleam test

# Run CLI
gleam run -- --help
```

## Requirements

- Gleam >= 1.0
- Erlang/OTP (for BEAM target)
- Elixir (for FFI - handled automatically via Mix)

## License

MIT
