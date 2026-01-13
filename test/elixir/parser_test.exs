defmodule JsonSchemaGleam.ParserTest do
  @moduledoc """
  Integration tests for the Elixir JSON Schema parser module.
  These tests verify that the FFI layer correctly parses JSON Schema files
  and returns properly structured data for consumption by Gleam code.
  """
  use ExUnit.Case, async: true

  alias JsonSchemaGleam.Parser

  @fixtures_path Path.expand("../fixtures", __DIR__)

  describe "parse_file/1" do
    test "parses a simple object schema from file" do
      path = Path.join(@fixtures_path, "simple_object.json")
      assert {:ok, result} = Parser.parse_file(path)

      assert is_map(result)
      assert Map.has_key?(result, :root)
      assert is_map(result.root)

      root = result.root
      assert root.title == "Person"
      assert root.type == "object"
      assert Map.has_key?(root.properties, "name")
      assert Map.has_key?(root.properties, "age")
      assert "name" in root.required
    end

    test "parses an enum schema from file" do
      path = Path.join(@fixtures_path, "enum_type.json")
      assert {:ok, result} = Parser.parse_file(path)

      root = result.root
      assert root.title == "Status"
      assert root.type == "string"
      assert is_list(root.enum)
      assert "pending" in root.enum
      assert "shipped" in root.enum
    end

    test "parses an array schema from file" do
      path = Path.join(@fixtures_path, "array_type.json")
      assert {:ok, result} = Parser.parse_file(path)

      root = result.root
      assert root.title == "Tags"
      assert root.type == "array"
      assert is_map(root.items)
      assert root.items.type == "string"
      assert root.min_items == 1
      assert root.max_items == 10
      assert root.unique_items == true
    end

    test "parses nested object schemas" do
      path = Path.join(@fixtures_path, "nested_object.json")
      assert {:ok, result} = Parser.parse_file(path)

      root = result.root
      assert root.title == "Order"
      assert root.type == "object"

      # Check nested items array
      items_prop = root.properties["items"]
      assert items_prop.type == "array"
      assert is_map(items_prop.items)
      assert items_prop.items.type == "object"

      # Check nested address object
      address_prop = root.properties["shipping_address"]
      assert address_prop.type == "object"
      assert address_prop.title == "Address"
    end

    test "parses union types" do
      path = Path.join(@fixtures_path, "union_type.json")
      assert {:ok, result} = Parser.parse_file(path)

      root = result.root
      assert root.title == "NullableString"
      assert root.type == {"union", ["string", "null"]}
    end

    test "parses oneOf schemas" do
      path = Path.join(@fixtures_path, "one_of.json")
      assert {:ok, result} = Parser.parse_file(path)

      root = result.root
      assert root.title == "PaymentMethod"
      assert is_list(root.one_of)
      assert length(root.one_of) == 2

      [credit_card, bank_transfer] = root.one_of
      assert credit_card.title == "CreditCard"
      assert bank_transfer.title == "BankTransfer"
    end

    test "parses schemas with $ref and $defs" do
      path = Path.join(@fixtures_path, "refs.json")
      assert {:ok, result} = Parser.parse_file(path)

      root = result.root
      assert root.title == "Team"

      # Check definitions were extracted
      assert Map.has_key?(result, :definitions)
      assert Map.has_key?(result.definitions, "Member")

      member_def = result.definitions["Member"]
      assert member_def.title == "Member"
      assert Map.has_key?(member_def.properties, "name")
    end

    test "returns error for non-existent file" do
      assert {:error, reason} = Parser.parse_file("/nonexistent/path.json")
      assert is_binary(reason)
    end
  end

  describe "parse_string/1" do
    test "parses a simple object schema from JSON string" do
      json = ~s({
        "title": "User",
        "type": "object",
        "properties": {
          "username": {"type": "string"},
          "active": {"type": "boolean"}
        },
        "required": ["username"]
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert is_map(result)
      assert result.root.title == "User"
      assert result.root.type == "object"
    end

    test "parses an enum from JSON string" do
      json = ~s({
        "title": "Color",
        "type": "string",
        "enum": ["red", "green", "blue"]
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert result.root.enum == ["red", "green", "blue"]
    end

    test "parses array with items from JSON string" do
      json = ~s({
        "type": "array",
        "items": {"type": "integer"},
        "minItems": 0,
        "maxItems": 100
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert result.root.type == "array"
      assert result.root.items.type == "integer"
      assert result.root.min_items == 0
      assert result.root.max_items == 100
    end

    test "parses schema with const value" do
      json = ~s({
        "const": "fixed_value"
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert result.root.const == "fixed_value"
    end

    test "parses schema with default value" do
      json = ~s({
        "type": "string",
        "default": "hello"
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert result.root.default == "hello"
    end

    test "parses schema with numeric constraints" do
      json = ~s({
        "type": "integer",
        "minimum": 0,
        "maximum": 100
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert result.root.minimum == 0
      assert result.root.maximum == 100
    end

    test "parses schema with string constraints" do
      json = ~s({
        "type": "string",
        "minLength": 1,
        "maxLength": 255,
        "pattern": "^[a-z]+$",
        "format": "email"
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert result.root.min_length == 1
      assert result.root.max_length == 255
      assert result.root.pattern == "^[a-z]+$"
      assert result.root.format == "email"
    end

    test "parses schema with deprecated flag" do
      json = ~s({
        "type": "string",
        "deprecated": true
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert result.root.deprecated == true
    end

    test "parses anyOf schemas" do
      json = ~s({
        "anyOf": [
          {"type": "string"},
          {"type": "integer"}
        ]
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert is_list(result.root.any_of)
      assert length(result.root.any_of) == 2
    end

    test "parses allOf schemas" do
      json = ~s({
        "allOf": [
          {"type": "object", "properties": {"a": {"type": "string"}}},
          {"type": "object", "properties": {"b": {"type": "integer"}}}
        ]
      })

      assert {:ok, result} = Parser.parse_string(json)
      assert is_list(result.root.all_of)
      assert length(result.root.all_of) == 2
    end

    test "returns error for invalid JSON" do
      assert {:error, reason} = Parser.parse_string("not valid json {{{")
      assert String.contains?(reason, "JSON parse error")
    end

    test "handles empty object schema" do
      json = ~s({})

      assert {:ok, result} = Parser.parse_string(json)
      assert is_map(result)
      assert is_map(result.root)
    end

    test "handles deeply nested schemas" do
      json = ~s({
        "type": "object",
        "properties": {
          "level1": {
            "type": "object",
            "properties": {
              "level2": {
                "type": "object",
                "properties": {
                  "level3": {"type": "string"}
                }
              }
            }
          }
        }
      })

      assert {:ok, result} = Parser.parse_string(json)
      level1 = result.root.properties["level1"]
      level2 = level1.properties["level2"]
      level3 = level2.properties["level3"]
      assert level3.type == "string"
    end
  end

  describe "atom key handling" do
    @tag :ffi
    test "result maps use atom keys for Gleam FFI compatibility" do
      json = ~s({"type": "string", "title": "Test"})
      assert {:ok, result} = Parser.parse_string(json)

      # The result should use atom keys
      assert is_atom(hd(Map.keys(result)))
      assert Map.has_key?(result, :root)

      # Root should also use atom keys
      root_keys = Map.keys(result.root)
      assert Enum.all?(root_keys, &is_atom/1)
    end
  end
end
