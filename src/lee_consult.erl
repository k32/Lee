-module(lee_consult).

-export([ metamodel/0
        , read/2
        , read_to/3
        ]).

-include("lee_internal.hrl").

-spec metamodel() -> lee:model_fragment().
metamodel() ->
    #{ metatype => #{ consult =>
                          {[metatype]
                          , #{}
                          }}
     }.

-spec read_to(lee:model(), file:filename(), lee:data()) ->
                     lee:data().
read_to(Model, Filename, Data) ->
    Patch = read(Model, Filename),
    lee_storage:put(Model, Data, Patch).

-spec read(lee:model(), file:filename()) ->
                     [{lee:key(), term()}].
read(#model{model = Model}, Filename) ->
    Idx = lee_model:mk_metatype_index(Model),
    CliArgs = map_sets:to_list(maps:get(consult, Idx, #{})),
    {ok, Terms} = file:consult(Filename),
    lists:foldl( fun(Var, Acc) ->
                         read_val(Model, Terms, Var, Acc)
                 end
               , []
               , CliArgs
               ).

read_val(Model, Terms, Var, Acc) ->
    {_, Attrs, _} = lee_model:get(Var, Model),
    Key = maps:get(file_key, Attrs),
    case proplists:lookup(Key, Terms) of
        none ->
            Acc;
        {_, Val} ->
            [{Var, Val} | Acc]
    end.
