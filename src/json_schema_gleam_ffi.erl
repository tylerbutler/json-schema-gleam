-module(json_schema_gleam_ffi).
-export([get_map_field/2]).

%% Get a field from an Elixir/Erlang map using atom key
%% Returns {ok, Value} or {error, nil}
get_map_field(Map, KeyBinary) when is_map(Map), is_binary(KeyBinary) ->
    Key = binary_to_existing_atom(KeyBinary, utf8),
    case maps:find(Key, Map) of
        {ok, Value} -> {ok, Value};
        error -> {error, nil}
    end;
get_map_field(_, _) ->
    {error, nil}.
