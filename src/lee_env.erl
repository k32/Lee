-module(lee_env).

-export([ metamodel/0
        , read/1
        , read_to/2
        ]).

-include("lee_internal.hrl").

-spec metamodel() -> lee:model_fragment().
metamodel() ->
    #{ metatype => #{ environment_variable =>
                          {[metatype]
                          , #{}
                          }}
     }.

-spec read(lee:model()) -> [{lee:key(), term()}].
read(#model{model = Model}) ->
    Idx = lee_model:mk_metatype_index(Model),
    EnvVars = map_sets:to_list(maps:get(environment_variable, Idx, #{})),
    lists:foldl( fun(Var, Acc) ->
                         read_val(Model, Var, Acc)
                 end
               , []
               , EnvVars
               ).

-spec read_to(lee:model(), lee:data()) -> lee:data().
read_to(Model, Data) ->
    Patch = read(Model),
    lee_storage:put(Model, Data, Patch).

read_val(Model, Var, Acc) ->
    {_, Attrs, _} = lee_model:get(Var, Model),
    EnvVar = maps:get(env, Attrs),
    case os:getenv(EnvVar) of
        false ->
            Acc;
        Str ->
            [{Var, Str} | Acc]
    end.
