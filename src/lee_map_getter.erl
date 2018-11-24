%% Simple config storage type that stores avarything in a map
-module(lee_map_getter).

-export([ model/0
        ]).

model() ->
    lee:namespace([lee]
                 , #{storage =>
                         {[]
                         , #{ getter =>
                                  fun(_, Map, Key) ->
                                          case Map of
                                              #{Key := Val} ->
                                                  {ok, Val};
                                              _ ->
                                                  undefined
                                          end
                                  end

                            , list_all_keys =>
                                  fun(_, Map) ->
                                          maps:keys(Map)
                                  end

                            , put => fun(_, Map, Key, Val) ->
                                             Map #{Key => Val}
                                     end

                            , empty => fun(_) ->
                                               #{}
                                       end
                            }
                         , []
                         }}
                 ).
