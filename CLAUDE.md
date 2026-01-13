# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

**json_schema_gleam** is a Gleam library and CLI tool that generates Gleam types from JSON Schema definitions.

### Architecture

This is a **hybrid Gleam/Elixir project**:
- **Gleam** - Code generation logic, public API, CLI
- **Elixir FFI** - JSON Schema parsing using the `json_schema` library

Consumers get a pure Gleam API - the FFI is an internal implementation detail.

### Key Components

```
src/
├── json_schema_gleam.gleam      # Main public API
├── json_schema_gleam/
│   ├── cli.gleam                # CLI using glint
│   ├── codegen.gleam            # Code generation from schema AST
│   ├── parser_ffi.gleam         # FFI bindings to Elixir parser
│   └── schema.gleam             # Schema AST types
lib/
└── json_schema_gleam/
    └── parser.ex                # Elixir JSON Schema parser wrapper
```

## Development Commands

```bash
# Use just for all commands
just build          # Build project
just test           # Run tests
just check          # Type check
just format         # Format code
just lint           # Run all checks
just ci             # Full CI pipeline

# CLI
just run --help                           # Show CLI help
just generate schema.json output.gleam    # Generate types
```

## Code Conventions

- Use functional programming patterns with immutable data
- Pattern match exhaustively
- Use Result types for error handling
- Follow Gleam's built-in formatter

## JSON Schema Support

The library supports JSON Schema Draft-07 with these features:
- Object types → Gleam record types
- Array types → `List(T)`
- Primitive types → `String`, `Int`, `Float`, `Bool`, `Nil`
- Enums → Custom types with variants
- `oneOf`/`anyOf` → Union types
- `$ref`/`$defs` → Reference resolution
- Required vs optional → `Option(T)` for optional fields

## Testing

Tests use gleeunit and test the code generation with manually constructed schema ASTs:
- `simple_object_codegen_test` - Record type generation
- `enum_codegen_test` - Enum type generation
- `array_codegen_test` - Array type generation
- `decoder_generation_test` - JSON decoder generation
- `default_options_test` - Options API

## Dependencies

### Gleam
- `gleam_stdlib` ~> 0.56.0
- `gleam_json` ~> 2.0
- `simplifile` ~> 2.0
- `glint` ~> 1.0 (CLI)
- `argv` ~> 1.0

### Elixir (for FFI)
- `json_schema` ~> 0.5.0

## Known Limitations

1. **Deprecation warnings** - Uses deprecated `gleam/dynamic` API (should migrate to `gleam/dynamic/decode`)
2. **Pattern/format** - Parsed but not enforced in generated code
3. **Encoders** - Not yet implemented (decoders only)
