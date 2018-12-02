-module(lee_cli).

-export([ metamodel/0
        , read/2
        , read_to/3
        ]).

-include("lee_internal.hrl").

-spec metamodel() -> lee:model_fragment().
metamodel() ->
    #{metatype => #{ cli_param =>
                         {[metatype]
                         , #{validate_mo => fun(_,_,_,_) -> ok end}
                         }
                   }}.

-spec read(lee:model(), [string()]) -> [{lee:key(), term()}].
read(Model, Args) ->
    Idx = lee_model:mk_metatype_index(Model#model.model),
    Params = map_sets:to_list(maps:get(cli_param, Idx, #{})),
    Spec = [mk_getopt_spec(Model#model.model, I) || I <- Params],
    %% This is oboviously wrong:
    case getopt:parse(Spec, Args) of
        {ok, {Terms, _}} -> ok;
        _ -> Terms = []
    end,
    lists:foldl( fun(Var, Acc) ->
                         case proplists:lookup(Var, Terms) of
                             none ->
                                 Acc;
                             {_, Val} ->
                                 [{Var, Val}|Acc]
                         end
                 end
               , []
               , Params
               ).

-spec read_to(lee:model(), [string()], lee:data()) -> lee:data().
read_to(Model, Args, Data) ->
    Patch = read(Model, Args),
    lee_storage:put(Model, Data, Patch).

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
