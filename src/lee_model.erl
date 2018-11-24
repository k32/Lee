-module(lee_model).

%% API exports
-export([ merge/1
        , merge/2
        , desugar/1
        , traverse/3
        , map/2
        , map_with_key/2
        , get/2
        , mk_metatype_index/1
        ]).

%%====================================================================
%% Types
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Merge multiple model fragments while checking for clashing
%% names
-spec merge([lee:model_fragment()]) ->
                   {ok, lee:model_fragment()}
                 | {error, string()}
                 .
merge(FragList) ->
    try
        {ok, lists:foldl( fun(MF, Acc) ->
                                  merge([], Acc, desugar(MF))
                          end
                        , #{}
                        , FragList
                        )}
    catch Err = {clashing_keys, _} ->
            Err
    end.

%% @doc Merge two model fragments while checking for clashing names
-spec merge(lee:model_fragment(), lee:model_fragment()) ->
                   {ok, lee:model_fragment()}
                 | {clashing_keys, [lee:node_id()]}
                 .
merge(M1, M2) ->
    try
        {ok, merge([], desugar(M1), desugar(M2))}
    catch
        Err = {clashing_keys, _} ->
            Err
    end.

%% @doc Get a MOC from the model, assumes that the key is present and
%% non-empty
-spec get(lee:key(), lee:model_fragment()) ->
             lee:moc().
get([Id], MF) ->
    maps:get(Id, MF);
get([Id|Rest], MF) ->
    case maps:get(Id, MF) of
      Map when is_map(Map) ->
        get(Rest, Map);
      {_, _, Children} ->
        get(Rest, Children)
    end.

%% @doc Transform all MOs to fully-qualified form
-spec desugar(lee:model_fragment()) -> lee:model_fragment().
desugar(M0) ->
    {M, _} = traverse(fun desugar_mo/3, undefined, M0),
    M.

-spec map( fun((lee:properties()) -> lee:properties())
         , lee:model_fragment()
         ) -> lee:model_fragment().
map(Fun, M) ->
    map_with_key( fun(_, Attrs) ->
                          Fun(Attrs)
                  end
                , M
                ).

-spec map_with_key( fun((lee:key(), lee:properties()) -> lee:properties())
                  , lee:model_fragment()
                  ) -> lee:model_fragment().
map_with_key(Fun, M) ->
    {Term, _Acc} =
        traverse( fun(Key, MO, _) ->
                          case MO of
                              {Metatype, Attrs} ->
                                  {{Metatype, Fun(Key, Attrs)}, undefined};
                              {Metatype, Attrs, Children} ->
                                  {{Metatype, Fun(Key, Attrs), Children}, undefined}
                          end
                  end
                , undefined
                , M
                ),
    Term.

%% @doc Recursion schema for model traversal
-spec traverse( fun((lee:key(), lee:moc(), Acc) -> {lee:moc(), Acc})
              , Acc
              , lee:model_fragment()
              ) -> {lee:model_fragment(), Acc}
              when Acc :: term().
traverse(Fun, Acc0, M) ->
    traverse([], Fun, Acc0, M).

%% @doc Make an index of MOCs belonging to metatypes. Assumes
%% desugared model
-spec mk_metatype_index(lee:model_fragment()) ->
                           #{lee:metatype() => map_sets:set(lee:key())}.
mk_metatype_index(MF) ->
    {_, Idx} = traverse( fun mk_metatype_index_/3
                       , #{}
                       , MF
                       ),
    Idx.

%%====================================================================
%% Internal functions
%%====================================================================

merge(Key0, M1, M2) ->
    maps:fold( fun(K, N1, Acc) ->
                       Key = Key0 ++ [K],
                       case Acc of
                           #{K := N2} when is_map(N1)
                                         , is_map(N2) ->
                               %% Both elements are namespaces, merge them
                               Acc#{K => merge(Key, N1, N2)};
                           #{K := _} ->
                               %% TODO: currently we stop on the first
                               %% error, it's not user-friendly
                               throw({clashing_keys, [Key]});
                           _ ->
                               Acc#{K => N1}
                       end
               end
             , M1
             , M2
             ).

traverse(Key0, Fun, AccIn, M) when is_map(M) ->
    maps:fold( fun(K, Val0, {Map0, Acc0}) ->
                       Key = Key0 ++ [K],
                       {Val, Acc} = traverse(Key, Fun, Acc0, Val0),
                       {Map0#{K => Val}, Acc}
               end
             , {#{}, AccIn}
             , M
             );
traverse(Key, Fun, Acc0, MO0) ->
    {MO, Acc1} = Fun(Key, MO0, Acc0),
    case MO of
        {_, _} ->
            {MO, Acc1};
        {_, _, []} ->
            {MO, Acc1};
        {Metatype, Attrs, Children0} ->
            {Children, Acc} = traverse(Key, Fun, Acc1, Children0),
            {{Metatype, Attrs, Children}, Acc}
    end.

mk_metatype_index_(Key, MOC = {Metatypes, _, _}, Acc0) ->
    Acc = lists:foldl( fun(MT, Acc) ->
                               S0 = maps:get(MT, Acc, #{}),
                               Acc#{MT => map_sets:add_element(Key, S0)}
                       end
                     , Acc0
                     , Metatypes
                     ),
    {MOC, Acc}.

desugar_mo(_, {MetaTypes, Attrs}, _) ->
    {{MetaTypes, Attrs, #{}}, undefined};
desugar_mo(_, MO = {_, _, _}, _) ->
    {MO, undefined}.

-ifdef(TEST).

-define(moc(Attr, Children), {[t1], Attr, Children}).

-define(moc(Attr), {[t1], Attr}).

-define(moc, {[], #{}, #{}}).

-define(model(Attr), #{ foo => ?moc(Attr#{key => [foo]})
                      , bar =>
                            #{ bar => ?moc(Attr#{key => [bar, bar]}, #{})
                             }
                      , baz => ?moc( Attr#{key => [baz]}
                                   , #{quux => ?moc(Attr#{key => [baz, quux]}, #{})}
                                   )
                      }).

merge_test() ->
    ?assertMatch( {ok, #{}}
                , merge(#{}, #{})
                ),
    ?assertMatch( {ok, #{foo := ?moc, bar := ?moc}}
                , merge(#{foo => ?moc}, #{bar => ?moc})
                ),
    ?assertMatch( {ok, #{foo := #{ bar := ?moc
                                 , baz := ?moc
                                 }}}
                , merge( #{foo => #{bar => ?moc}}
                       , #{foo => #{baz => ?moc}}
                       )
                ),
    ?assertMatch( {clashing_keys, [[foo]]}
                , merge( #{foo => ?moc}
                       , #{foo => ?moc, bar => ?moc}
                       )
                ).

traverse_test() ->
    CheckKey =
        fun(Key, MO, Acc) ->
                case MO of
                    {[t1], #{key := Key}} ->
                        ok;
                    {[t1], #{key := Key}, _} ->
                        ok
                end,
                {MO, Acc + 1}
        end,
    ?assertEqual( {?model(#{}), 4}
                , traverse(CheckKey, 0, ?model(#{}))
                ).

desugar_test() ->
    ?assertEqual( ?model(#{}) #{foo =>
                                    ?moc(#{key => [foo]}, #{})}
                , desugar(?model(#{}))
                ).

get_test() ->
    ?assertEqual( ?moc(#{key => [foo]})
                , lee_model:get([foo], ?model(#{}))
                ),
    ?assertEqual( ?moc(#{key => [baz, quux]}, #{})
                , lee_model:get([baz, quux], ?model(#{}))
                ).

mk_metatype_index_test() ->
    Expected = #{ t1 =>
                      map_sets:from_list([[foo], [bar, bar], [baz], [baz, quux]])
                , t2 =>
                      map_sets:from_list([[quux]])
                },
    Model = desugar(?model(#{}) #{quux => {[t2], #{}}}),
    ?assertEqual( Expected
                , mk_metatype_index(Model)
                ).

-endif.
