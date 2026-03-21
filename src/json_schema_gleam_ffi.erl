-module(json_schema_gleam_ffi).
-export([get_map_field/2]).

%% Get a field from an Elixir map (which uses atom keys)
%% Converts the string key to an existing atom for lookup
get_map_field(Map, Key) when is_map(Map), is_binary(Key) ->
    AtomKey = try binary_to_existing_atom(Key, utf8)
              catch error:badarg -> undefined
              end,
    case AtomKey of
        undefined ->
            %% Atom doesn't exist, try string key as fallback
            case maps:find(Key, Map) of
                {ok, Value} -> {ok, Value};
                error -> {error, nil}
            end;
        _ ->
            case maps:find(AtomKey, Map) of
                {ok, Value} -> {ok, Value};
                error -> {error, nil}
            end
    end;
get_map_field(_, _) ->
    {error, nil}.
