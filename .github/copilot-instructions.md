# Copilot Instructions for `json_schema_gleam`

## Build, test, and lint commands

Use `just` as the canonical entrypoint in this repository because the project mixes Gleam code with Elixir FFI modules.

- `just deps` installs and compiles the Elixir side (`mix deps.get && mix compile`)
- `just check` runs Gleam type checking
- `just build` compiles Gleam, compiles the Erlang FFI helper, and copies Elixir `.beam` files into the Gleam build output so the CLI and FFI-backed tests can run
- `just format` formats Gleam code
- `just format-check` checks formatting without rewriting files
- `just lint` runs `just check` and `just format-check`
- `just test` runs both Elixir and Gleam test suites
- `just test-elixir` runs only ExUnit tests in `test/elixir`
- `just test-gleam` runs Gleam tests through `erl` with the Elixir and Gleam BEAM paths wired together; use this instead of plain `gleam test` for FFI-backed tests
- `just test-gleam-unit` runs `gleam test` directly for the pure Gleam suite
- `just ci` is the closest local equivalent to CI

Single-test workflows:

- Run one Elixir test file: `mix test test/elixir/parser_test.exs`
- Run one Elixir test by line: `mix test test/elixir/parser_test.exs:14`

CLI workflows:

- `just help`
- `just run --help`
- `just generate schema.json output.gleam`

## High-level architecture

This is a hybrid Gleam/Elixir codebase. The public surface is Gleam, but JSON Schema parsing is delegated to Elixir.

- `src/json_schema_gleam.gleam` is the public API. It re-exports the generation options type, exposes `generate_from_file`, `generate_from_string`, `parse_file`, `parse_string`, and routes CLI execution to `cli.run()`.
- `src/json_schema_gleam/cli.gleam` builds the command-line interface with `glint`. It derives the module name from the schema path when `--module` is omitted, constructs `GenerateOptions`, then uses the same parsing/codegen pipeline as the library API.
- `lib/json_schema_gleam/parser.ex` is the Elixir parser wrapper. For files, it first tries the `json_schema` library; if that fails, it falls back to direct `Jason` decoding and manual schema-map conversion. That fallback is important because it handles schemas the upstream library rejects, such as some non-URI `$id` values.
- `src/json_schema_gleam/parser_ffi.gleam` is the bridge back into Gleam. It calls the Elixir parser via `@external`, expects atom-keyed maps, and decodes them into the typed AST defined in `src/json_schema_gleam/schema.gleam`.
- `src/json_schema_gleam/schema.gleam` is the contract between parsing and generation. `SchemaResult`, `SchemaNode`, `SchemaType`, and `SchemaValue` capture the normalized schema AST used everywhere else.
- `src/json_schema_gleam/codegen.gleam` turns that AST into generated Gleam source. It emits record/custom/union types plus optional decoders. Encoder support is represented in `GenerateOptions`, but `generate_encoders()` is still a stub.

The important flow is:

`schema file/string -> Elixir parser -> typed Gleam schema AST -> code generator -> generated Gleam module`

## Key conventions

- Treat the Elixir layer as an internal implementation detail. Changes to parsing behavior usually need coordinated updates across `lib/json_schema_gleam/parser.ex`, `src/json_schema_gleam/parser_ffi.gleam`, `src/json_schema_gleam/schema.gleam`, and the affected codegen/tests.
- The schema AST is the main extension seam. When adding support for a new JSON Schema feature, thread it through the parser output, FFI decoding, schema types, and generation logic rather than patching only one layer.
- Tests are intentionally split by layer:
  - `test/json_schema_gleam_test.gleam` builds `SchemaNode` values directly with `schema.empty_node` to unit test code generation without FFI
  - `test/parser_ffi_test.gleam` exercises the full Gleam-to-Elixir parsing path using JSON fixtures under `test/fixtures`
  - `test/elixir/parser_test.exs` verifies the Elixir parser output structure and explicitly checks atom-keyed maps for FFI compatibility
- In generated object types, non-required JSON Schema properties become `Option(T)`.
- In `codegen.gleam`, simple unions of `[T, null]` are collapsed to `Option(T)`; more complex unions fall back to `Dynamic` unless modeled as `oneOf`/`anyOf`.
- Type names are derived from schema titles and `$defs` names via the local `pascal_case` helper, while generated decoder function names and record fields use the local `snake_case` helper. If a schema node has no title, generation falls back to names like `Root` or `Dynamic` depending on context.
- For local CLI and FFI work, prefer `just run` / `just generate` / `just test-gleam` over raw `gleam run` or `gleam test`, because the `just` recipes add the Elixir and compiled BEAM paths required at runtime.
