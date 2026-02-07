# Development Guide

This document provides detailed instructions for developing and contributing to json_schema_gleam.

## Prerequisites

This is a **hybrid Gleam/Elixir project**. You need:

| Tool | Version | Purpose |
|------|---------|---------|
| Erlang/OTP | 27+ | BEAM runtime |
| Elixir | 1.17+ | FFI for JSON Schema parsing |
| Gleam | 1.14.0+ | Compiler and tooling |
| just | 1.38.0+ | Task runner |

**Recommended:** Use [mise](https://mise.jdx.dev/) or [asdf](https://asdf-vm.com/) with `.tool-versions`.

```bash
# Install all tools
mise install
```

## Getting Started

```bash
# Clone the repository
git clone <repo-url>
cd json-schema-gleam

# Install dependencies (Gleam + Elixir)
just deps

# Verify everything works
just ci
```

## Development Workflow

### Daily Development

```bash
# Type check
just check

# Run tests (Gleam + Elixir)
just test

# Format code
just format
```

### Before Committing

```bash
# Run full CI checks
just pr
```

### Before Merging to Main

```bash
# Run extended checks including docs
just main
```

## Project Structure

```
.
├── src/
│   └── json_schema_gleam/
│       ├── json_schema_gleam.gleam  # Public API
│       ├── cli.gleam                # CLI interface
│       ├── codegen.gleam            # Code generation
│       ├── parser_ffi.gleam         # Elixir FFI bindings
│       └── schema.gleam             # Schema AST types
├── lib/
│   └── json_schema_gleam/
│       └── parser.ex                # Elixir parser wrapper
├── test/
│   ├── json_schema_gleam_test.gleam # Gleam tests
│   └── elixir/                      # Elixir tests
├── gleam.toml
├── mix.exs                          # Elixir config
└── justfile
```

## CLI Usage

```bash
# Show help
just help

# Generate types from schema
just generate schema.json output.gleam

# With options
just run schema.json output.gleam --no-decoders --prefix MyApp
```

## Architecture Notes

### Gleam/Elixir FFI

The Elixir `json_schema` library does the heavy lifting of parsing JSON Schema. We call it via FFI:

```gleam
// parser_ffi.gleam
@external(erlang, "Elixir.JsonSchemaGleam.Parser", "parse_file")
fn do_parse_file(path: String) -> Result(Dynamic, String)
```

### Building

The build process:
1. `mix deps.get` - Install Elixir deps
2. `mix compile` - Compile Elixir
3. `gleam build` - Compile Gleam
4. Copy BEAM files to Gleam build dir

### Running with FFI

Tests and CLI must include Elixir BEAM paths:

```bash
erl -noshell \
    -pa build/dev/erlang/*/ebin \
    -pa _build/dev/lib/*/ebin \
    -pa "$(elixir -e 'IO.puts(:code.lib_dir(:elixir))')/ebin" \
    ...
```

## Code Style

### Formatting

```bash
just format
```

### Error Handling

Use Result types throughout:

```gleam
pub fn generate_from_file(path: String) -> Result(String, String)
```

## Testing

```bash
# Run all tests
just test

# Run only Gleam tests
just test-gleam

# Run only Elixir tests
just test-elixir
```

## Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
feat(codegen): add encoder generation
fix(parser): handle recursive refs
docs: add CLI examples
```

## Troubleshooting

### Elixir Dependencies Missing

```bash
mix deps.get
mix compile
```

### BEAM Module Not Found

```bash
just clean
just deps
just build
```

### Dynamic Decoding Issues

Check that Elixir modules are in the load path. The `just run` command handles this automatically.
