-module(lee_cli).

%% Sketchiness level: > 9000. For demonstration only

-export([ metamodel/0
        , read/2
        ]).

-spec metamodel() -> lee:model_fragment().
metamodel() ->
    %% TODO: There's no metamodel validation yet...
    #{}.

-spec read(lee:model_fragment(), [string()]) -> term().
read(Model, Args) ->
    Idx = lee_model:mk_metatype_index(Model),
    Params = map_sets:to_list(maps:get(cli_param, Idx, #{})),
    Spec = [mk_getopt_spec(Model, I) || I <- Params],
    %% This is oboviously wrong:
    case getopt:parse(Spec, Args) of
        {ok, {Terms, _}} -> ok;
        _ -> Terms = []
    end,
    {_, #{ empty := Empty
         , put   := Put
         }, _} = lee_model:get([lee, storage], Model),
    lists:foldl( fun(Var, Acc) ->
                         write_val(Model, Terms, Put, Var, Acc)
                 end
               , Empty(Model)
               , Params
               ).

mk_getopt_spec(Model, Var) ->
    {_, Attrs, _} = lee_model:get(Var, Model),
    Short = maps:get(cli_short, Attrs, undefined),
    Long = maps:get(cli_param, Attrs, undefined),
    Type = maps:get(type, Attrs),
    Oneliner = maps:get(oneliner, Attrs),
    {Var, Short, Long, transform_type(Type), Oneliner}.

transform_type(Type) ->
    Int = lee_types:integer(),
    String = lee_types:string(),
    case Type of
        Int ->
            integer;
        String ->
            string
    end.

write_val(Model, Terms, Put, Var, Acc) ->
    case proplists:lookup(Var, Terms) of
        none ->
            Acc;
        {_, Val} ->
            Put(Model, Acc, Var, Val)
    end.
