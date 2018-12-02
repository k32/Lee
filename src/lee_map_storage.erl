%% Simple config storage type that stores avarything in a map
-module(lee_map_storage).

-behavior(lee_storage).

-export([ create/1
        , get/3
        , put/3
        , list/3
        ]).

create(_) ->
    #{}.

get(_, Map, Key) ->
    case Map of
        #{Key := Val} ->
            {ok, Val};
        _ ->
            undefined
    end.

list(_, Map, Prefix) ->
    [K || K <- maps:keys(Map), lists:prefix(Prefix, K)].

put(_, Map, List) ->
    maps:merge(Map, maps:from_list(List)).
