/// Shared string utility functions for code generation.
import gleam/list
import gleam/string

/// Convert a string to PascalCase.
/// Splits on hyphens, spaces, and underscores, then capitalizes each segment.
pub fn pascal_case(s: String) -> String {
  s
  |> string.replace("-", "_")
  |> string.replace(" ", "_")
  |> string.split("_")
  |> list.map(capitalize)
  |> string.join("")
}

/// Convert a string to snake_case.
/// Inserts underscores before uppercase letters, then lowercases everything.
pub fn snake_case(s: String) -> String {
  s
  |> string.replace("-", "_")
  |> string.replace(" ", "_")
  |> insert_underscores_before_caps()
  |> string.lowercase()
}

/// Capitalize the first character of a string.
pub fn capitalize(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}

/// Insert underscores before uppercase characters (for camelCase → snake_case).
fn insert_underscores_before_caps(s: String) -> String {
  s
  |> string.to_graphemes()
  |> list.index_fold("", fn(acc, char, i) {
    case i > 0 && is_uppercase(char) {
      True -> acc <> "_" <> char
      False -> acc <> char
    }
  })
}

/// Check if a single-character string is uppercase.
fn is_uppercase(s: String) -> Bool {
  let upper = string.uppercase(s)
  let lower = string.lowercase(s)
  upper == s && upper != lower
}
