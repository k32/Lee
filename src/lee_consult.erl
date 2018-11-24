-module(lee_consult).

%% Sketchiness level: > 9000

-export([ metamodel/0
        , read/2
        ]).

-spec metamodel() -> lee:model_fragment().
metamodel() ->
    %% TODO: There's no metamodel validation yet...
    #{}.

-spec read(lee:model_fragment(), string()) -> term().
read(Model, Filename) ->
    Idx = lee_model:mk_metatype_index(Model),
    CliArgs = map_sets:to_list(maps:get(consult, Idx, #{})),
    {ok, Terms} = file:consult(Filename),
    {_, #{ empty := Empty
         , put   := Put
         }, _} = lee_model:get([lee, storage], Model),
    lists:foldl( fun(Var, Acc) ->
                         read_val(Model, Terms, Put, Var, Acc)
                 end
               , Empty(Model)
               , CliArgs
               ).

read_val(Model, Terms, Put, Var, Acc) ->
    {_, Attrs, _} = lee_model:get(Var, Model),
    Key = maps:get(file_key, Attrs),
    case proplists:lookup(Key, Terms) of
        none ->
            Acc;
        {_, Val} ->
            Put(Model, Acc, Var, Val)
    end.
