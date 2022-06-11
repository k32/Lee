-module(lee_lib_tests).

-include_lib("eunit/include/eunit.hrl").

collect_errors_test() ->
    Ret = lee_lib:collect_errors(
            fun() ->
                    lee_lib:report_error("error ~p", [1]),
                    lee_lib:report_warning("warning ~p", [2]),
                    my_return
            end),
    ?assertEqual( {my_return, {["error 1"], ["warning 2"]}}
                , Ret
                ).


splitl_test() ->
    Pred = fun is_atom/1,
    ?assertMatch( []
                , lee_lib:splitl(Pred, [])
                ),
    L1 = [[a]],
    ?assertMatch( L1
                , lee_lib:splitl(Pred, lists:append(L1))
                ),
    L2 = [[a, b, 1], [c, d, 2], [d]],
    ?assertMatch( L2
                , lee_lib:splitl(Pred, lists:append(L2))
                ),
    L3 = [[1], [2], [3]],
    ?assertMatch( L3
                , lee_lib:splitl(Pred, lists:append(L3))
                ).

splitr_test() ->
    Pred = fun is_atom/1,
    ?assertMatch( []
                , lee_lib:splitr(Pred, [])
                ),
    L1 = [[a]],
    ?assertMatch( L1
                , lee_lib:splitr(Pred, lists:append(L1))
                ),
    L2 = [[a, b], [1, c, d], [2, d]],
    ?assertMatch( L2
                , lee_lib:splitr(Pred, lists:append(L2))
                ),
    L3 = [[1], [2], [3]],
    ?assertMatch( L3
                , lee_lib:splitr(Pred, lists:append(L3))
                ).
