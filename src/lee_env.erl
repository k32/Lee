-module(lee_env).

%% Sketchiness level > 9000, for demonstration only

-export([ metamodel/0
        , read/1
        ]).

-spec metamodel() -> lee:model_fragment().
metamodel() ->
    %% TODO: There's no metamodel validation yet...
    #{}.

-spec read(lee:model_fragment()) -> term().
read(Model) ->
    Idx = lee_model:mk_metatype_index(Model),
    CliArgs = map_sets:to_list(maps:get(environment_variable, Idx, #{})),
    {_, #{ empty := Empty
         , put   := Put
         }, _} = lee_model:get([lee, storage], Model),
    lists:foldl( fun(Var, Acc) ->
                         read_val(Model, Put, Var, Acc)
                 end
               , Empty(Model)
               , CliArgs
               ).

read_val(Model, Put, Var, Acc) ->
    {_, Attrs, _} = lee_model:get(Var, Model),
    EnvVar = maps:get(env, Attrs),
    case os:getenv(EnvVar) of
        false ->
            Acc;
        Str ->
            Put(Model, Acc, Var, Str)
    end.
