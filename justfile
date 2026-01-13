#!/usr/bin/env just --justfile

# Common aliases for faster development
alias b := build
alias t := test
alias c := check
alias f := format
alias r := run

# Default recipe - shows available commands
default:
    @just --list

# Install all dependencies (Gleam + Elixir FFI)
deps:
    mix deps.get
    mix compile

# Check project (type check without building)
check:
    gleam check

# Build project (includes Elixir FFI compilation)
build: deps
    gleam build
    # Compile Erlang FFI helper module
    erlc -o build/dev/erlang/json_schema_gleam/ebin lib/json_schema_gleam_ffi.erl
    # Copy Elixir modules to Gleam build dir for runtime
    cp _build/dev/lib/json_schema_gleam/ebin/*.beam build/dev/erlang/json_schema_gleam/ebin/ 2>/dev/null || true
    cp _build/dev/lib/json_schema/ebin/*.beam build/dev/erlang/json_schema_gleam/ebin/ 2>/dev/null || true
    cp _build/dev/lib/typed_struct/ebin/*.beam build/dev/erlang/json_schema_gleam/ebin/ 2>/dev/null || true

# Run tests
test: build
    gleam test

# Format code
format:
    gleam format

# Check formatting without modifying
format-check:
    gleam format --check

# Run all quality checks
lint: check format-check

# Clean build artifacts
clean:
    rm -rf build _build deps

# Run full CI pipeline
ci: deps lint build test

# Get Elixir ebin path
elixir-ebin := `elixir -e "IO.puts(Path.join([:code.lib_dir(:elixir), \"ebin\"]))"`
logger-ebin := `elixir -e "IO.puts(Path.join([:code.lib_dir(:logger), \"ebin\"]))"`

# Run the CLI (using erl directly with both build paths)
run *ARGS: build
    erl -noshell \
        -pa build/dev/erlang/*/ebin \
        -pa _build/dev/lib/*/ebin \
        -pa "{{elixir-ebin}}" \
        -pa "{{logger-ebin}}" \
        -eval "'json_schema_gleam@@main':run(json_schema_gleam)" \
        -s erlang halt \
        -- {{ARGS}}

# Run CLI with help
help: build
    just run --help

# Generate types from a schema file
generate SCHEMA OUTPUT="" *OPTS: build
    just run {{SCHEMA}} {{OUTPUT}} {{OPTS}}

# Update dependencies
update:
    gleam update
    mix deps.update --all

# Generate documentation
docs:
    gleam docs build
