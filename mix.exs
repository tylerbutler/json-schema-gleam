defmodule JsonSchemaGleam.MixProject do
  use Mix.Project

  def project do
    [
      app: :json_schema_gleam,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      # Test configuration for ExUnit
      test_paths: ["test/elixir"],
      test_pattern: "*_test.exs"
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      # JSON Schema parsing - provides AST for code generation
      {:json_schema, "~> 0.5.0"},
      # JSON parsing for parse_string
      {:jason, "~> 1.4"}
    ]
  end
end
