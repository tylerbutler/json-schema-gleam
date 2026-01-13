defmodule JsonSchemaGleam.Parser do
  @moduledoc """
  Elixir wrapper around the json_schema library for parsing JSON Schema files.
  This module is called from Gleam via FFI to parse schemas into a structured AST.
  """

  alias JsonSchema.Parser.SchemaResult

  @doc """
  Parse a JSON schema from a file path.
  Returns {:ok, schema_ast} or {:error, reason}.

  First tries the json_schema library, falls back to direct JSON parsing if that fails
  (e.g., when $id is not a valid URI).
  """
  def parse_file(file_path) when is_binary(file_path) do
    # Try the json_schema library first
    result = JsonSchema.parse_schema_files([file_path])

    case convert_schema_result(result, file_path) do
      {:ok, schema} ->
        {:ok, schema}

      {:error, _reason} ->
        # Fall back to direct JSON parsing (handles schemas with non-URI $id)
        parse_file_direct(file_path)
    end
  rescue
    e ->
      {:error, "Failed to parse schema: #{Exception.message(e)}"}
  end

  # Parse file directly using Jason (fallback for schemas that json_schema library rejects)
  defp parse_file_direct(file_path) do
    case File.read(file_path) do
      {:ok, content} ->
        parse_string(content)

      {:error, reason} ->
        {:error, "Failed to read file: #{inspect(reason)}"}
    end
  end

  @doc """
  Parse a JSON schema from a JSON string.
  Returns {:ok, schema_ast} or {:error, reason}.
  """
  def parse_string(json_string) when is_binary(json_string) do
    case Jason.decode(json_string) do
      {:ok, schema_map} ->
        # Create a schema AST directly from the parsed JSON
        schema_ast = convert_schema_map(schema_map, "#")
        {:ok, %{root: schema_ast, definitions: extract_definitions(schema_map)}}

      {:error, %Jason.DecodeError{} = error} ->
        {:error, "JSON parse error: #{Exception.message(error)}"}
    end
  end

  # Convert the JsonSchema library result to a Gleam-friendly format
  # SchemaResult has: schema_dict, warnings, errors
  defp convert_schema_result(%SchemaResult{} = result, file_path) do
    if result.errors != [] do
      error_messages =
        result.errors
        |> Enum.flat_map(fn {_path, errors} -> errors end)
        |> Enum.map(fn err -> err.message end)
        |> Enum.join("; ")

      {:error, error_messages}
    else
      # schema_dict is a map of URI -> SchemaDefinition
      # We need to find the schema for our file
      schema_def = find_schema_for_file(result.schema_dict, file_path)

      case schema_def do
        nil ->
          {:error, "No schema found in parsed result"}

        schema ->
          {:ok, convert_schema_definition(schema)}
      end
    end
  end

  defp convert_schema_result(result, _file_path) when is_map(result) do
    # Fallback for unexpected map format
    {:ok, %{
      root: convert_schema_map(result, "#"),
      definitions: %{},
      errors: [],
      warnings: []
    }}
  end

  # Find the schema definition for a given file path
  defp find_schema_for_file(schema_dict, file_path) when is_map(schema_dict) do
    # The schema_dict keys are URIs, try to match our file
    uri = URI.new!("file://#{Path.expand(file_path)}")

    # Try exact match first, then try to find any schema
    case Map.get(schema_dict, uri) do
      nil ->
        # Just get the first (and likely only) schema
        case Map.values(schema_dict) do
          [schema | _] -> schema
          [] -> nil
        end

      schema ->
        schema
    end
  end

  defp find_schema_for_file(_, _), do: nil

  # Convert a SchemaDefinition to our format
  defp convert_schema_definition(%JsonSchema.Types.SchemaDefinition{} = schema_def) do
    # type_dict contains all the parsed types keyed by their JSON pointer paths
    definitions =
      schema_def.types
      |> Enum.map(fn {path, type} ->
        {to_string(path), convert_type(type)}
      end)
      |> Map.new()

    # Find the root type (usually at "#" or the schema's id)
    root_path = to_string(schema_def.id)
    root = Map.get(definitions, root_path) || Map.get(definitions, "#")

    %{
      root: root,
      definitions: definitions
    }
  end

  defp convert_schema_definition(other) do
    %{
      root: convert_schema_map(other, "#"),
      definitions: %{}
    }
  end

  # Convert schema map directly (for string parsing or fallback)
  defp convert_schema_map(schema, path) when is_map(schema) do
    %{
      path: path,
      type: get_schema_type(schema),
      title: Map.get(schema, "title"),
      description: Map.get(schema, "description"),
      properties: convert_properties(Map.get(schema, "properties"), path),
      required: Map.get(schema, "required", []),
      additional_properties: Map.get(schema, "additionalProperties", true),
      items: convert_items(Map.get(schema, "items"), path),
      enum: Map.get(schema, "enum"),
      const: Map.get(schema, "const"),
      pattern: Map.get(schema, "pattern"),
      default: Map.get(schema, "default"),
      one_of: convert_one_of(Map.get(schema, "oneOf"), path),
      any_of: convert_any_of(Map.get(schema, "anyOf"), path),
      all_of: convert_all_of(Map.get(schema, "allOf"), path),
      ref: Map.get(schema, "$ref"),
      min_items: Map.get(schema, "minItems"),
      max_items: Map.get(schema, "maxItems"),
      unique_items: Map.get(schema, "uniqueItems", false),
      minimum: Map.get(schema, "minimum"),
      maximum: Map.get(schema, "maximum"),
      min_length: Map.get(schema, "minLength"),
      max_length: Map.get(schema, "maxLength"),
      format: Map.get(schema, "format"),
      deprecated: Map.get(schema, "deprecated", false)
    }
  end

  defp convert_schema_map(nil, _path), do: nil

  defp get_schema_type(schema) do
    case Map.get(schema, "type") do
      nil ->
        cond do
          Map.has_key?(schema, "oneOf") -> "oneOf"
          Map.has_key?(schema, "anyOf") -> "anyOf"
          Map.has_key?(schema, "allOf") -> "allOf"
          Map.has_key?(schema, "$ref") -> "ref"
          Map.has_key?(schema, "const") -> "const"
          Map.has_key?(schema, "enum") -> "enum"
          true -> nil
        end

      type when is_list(type) ->
        # Multiple types like ["string", "null"]
        {"union", type}

      type ->
        type
    end
  end

  defp convert_properties(nil, _parent_path), do: %{}

  defp convert_properties(props, parent_path) when is_map(props) do
    props
    |> Enum.map(fn {key, value} ->
      {key, convert_schema_map(value, "#{parent_path}/properties/#{key}")}
    end)
    |> Map.new()
  end

  defp convert_items(nil, _parent_path), do: nil

  defp convert_items(items, parent_path) when is_map(items) do
    convert_schema_map(items, "#{parent_path}/items")
  end

  defp convert_items(items, parent_path) when is_list(items) do
    items
    |> Enum.with_index()
    |> Enum.map(fn {item, idx} ->
      convert_schema_map(item, "#{parent_path}/items/#{idx}")
    end)
  end

  defp convert_one_of(nil, _parent_path), do: nil

  defp convert_one_of(schemas, parent_path) when is_list(schemas) do
    schemas
    |> Enum.with_index()
    |> Enum.map(fn {schema, idx} ->
      convert_schema_map(schema, "#{parent_path}/oneOf/#{idx}")
    end)
  end

  defp convert_any_of(nil, _parent_path), do: nil

  defp convert_any_of(schemas, parent_path) when is_list(schemas) do
    schemas
    |> Enum.with_index()
    |> Enum.map(fn {schema, idx} ->
      convert_schema_map(schema, "#{parent_path}/anyOf/#{idx}")
    end)
  end

  defp convert_all_of(nil, _parent_path), do: nil

  defp convert_all_of(schemas, parent_path) when is_list(schemas) do
    schemas
    |> Enum.with_index()
    |> Enum.map(fn {schema, idx} ->
      convert_schema_map(schema, "#{parent_path}/allOf/#{idx}")
    end)
  end

  defp extract_definitions(schema) when is_map(schema) do
    defs = Map.get(schema, "$defs") || Map.get(schema, "definitions") || %{}

    defs
    |> Enum.map(fn {key, value} ->
      {key, convert_schema_map(value, "#/$defs/#{key}")}
    end)
    |> Map.new()
  end

  # Convert JsonSchema library types to our generic format
  defp convert_type(%JsonSchema.Types.ObjectType{} = obj) do
    %{
      kind: "object",
      path: to_string(obj.path),
      title: obj.name,
      description: obj.description,
      properties: convert_type_properties(obj.properties),
      required: obj.required || [],
      additional_properties: obj.additional_properties
    }
  end

  defp convert_type(%JsonSchema.Types.ArrayType{} = arr) do
    %{
      kind: "array",
      path: to_string(arr.path),
      items: convert_type(arr.items)
    }
  end

  defp convert_type(%JsonSchema.Types.PrimitiveType{} = prim) do
    %{
      kind: "primitive",
      path: to_string(prim.path),
      type: to_string(prim.type)
    }
  end

  defp convert_type(%JsonSchema.Types.EnumType{} = enum_type) do
    %{
      kind: "enum",
      path: to_string(enum_type.path),
      values: enum_type.values
    }
  end

  defp convert_type(%JsonSchema.Types.TypeReference{} = ref) do
    %{
      kind: "ref",
      path: to_string(ref.path)
    }
  end

  defp convert_type(%JsonSchema.Types.UnionType{} = union) do
    %{
      kind: "union",
      path: to_string(union.path),
      types: Enum.map(union.types, &convert_type/1)
    }
  end

  defp convert_type(%JsonSchema.Types.AllOfType{} = all_of) do
    %{
      kind: "allOf",
      path: to_string(all_of.path),
      types: Enum.map(all_of.types, &convert_type/1)
    }
  end

  defp convert_type(%JsonSchema.Types.OneOfType{} = one_of) do
    %{
      kind: "oneOf",
      path: to_string(one_of.path),
      types: Enum.map(one_of.types, &convert_type/1)
    }
  end

  defp convert_type(%JsonSchema.Types.AnyOfType{} = any_of) do
    %{
      kind: "anyOf",
      path: to_string(any_of.path),
      types: Enum.map(any_of.types, &convert_type/1)
    }
  end

  defp convert_type(%JsonSchema.Types.ConstType{} = const_type) do
    %{
      kind: "const",
      path: to_string(const_type.path),
      value: const_type.const
    }
  end

  defp convert_type(nil), do: nil

  defp convert_type(other) do
    %{kind: "unknown", raw: inspect(other)}
  end

  # Convert properties map where values are type references or inline types
  defp convert_type_properties(properties) when is_map(properties) do
    properties
    |> Enum.map(fn {name, type_ref} ->
      {to_string(name), convert_type(type_ref)}
    end)
    |> Map.new()
  end

  defp convert_type_properties(properties) when is_list(properties) do
    properties
    |> Enum.map(fn {name, type_ref} ->
      {to_string(name), convert_type(type_ref)}
    end)
    |> Map.new()
  end

  defp convert_type_properties(_), do: %{}
end
