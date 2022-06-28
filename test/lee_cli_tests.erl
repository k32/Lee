-module(lee_cli_tests).

-export([test_model_raw/0, test_model/0]).

-include_lib("lee/src/framework/lee_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

test_cli_params() ->
    #{ something_unrelated => {[], #{}}
     , long =>
           {[value, cli_param]
           , #{ cli_operand => "long"
              , type => typerefl:string()
              , default => "default"
              , oneliner => "An example of a long CLI argument"
              }
           }
     , short => {[value, cli_param]
                , #{ cli_short => $s
                   , type => typerefl:integer()
                   , oneliner => "An example of a short CLI argument"
                   }
                }
     , flag1 => {[value, cli_param]
                , #{ cli_short => $f
                   , type => typerefl:boolean()
                   , oneliner => "Flag1"
                   }
                }
     , flag2 => {[value, cli_param]
                , #{ cli_short => $g
                   , type => typerefl:boolean()
                   , oneliner => "Some flag"
                   }
                }
     , flag3 => {[value, cli_param]
                , #{ cli_short => $h
                   , type => typerefl:boolean()
                   , oneliner => "Another flag"
                   }
                }
     , both =>
           {[value, cli_param]
           , #{ cli_operand => "both"
              , cli_short => $b
              , type => typerefl:tuple([foo, typerefl:integer()])
              , oneliner => "This can be set via short or long argument"
              }
           }
     }.

-define(match_positional(POS),
        #sc{ positional = POS
           }).

test_cli_action(Name, KeyElems, Children) ->
    {[map, cli_action]
    , #{ cli_operand => Name
       , ?key_elements => KeyElems
       , oneliner => "This is a test CLI action. It does stuff"
       , doc => "<para>This is a long and elaborate
                 documentation of a CLI action</para>
                 <para>
                 Blah blah blah
                 </para>"
       }
    , lists:foldl(fun maps:merge/2, #{}, Children)
    }.

test_positional_args1() ->
    #{ posn_1 =>
           {[value, cli_positional]
           , #{ cli_arg_position => 1
              , type => typerefl:string()
              }
           }
     , posn_2 =>
           {[value, cli_positional]
           , #{ cli_arg_position => 2
              , type => typerefl:string()
              }
           }
     }.

test_positional_args2() ->
    #{ posn_n =>
           {[value, cli_positional]
           , #{ cli_arg_position => rest
              , type => typerefl:list(typerefl:atom())
              }
           }
     }.

test_model_raw() ->
    #{ global => test_cli_params()
     , action_1 =>
           test_cli_action("action_1", [[short], [long]]
                          , [test_cli_params()])
     , action_2 =>
           test_cli_action("action_2", [[posn_1]]
                          , [test_positional_args1()])
     , action_3 =>
           test_cli_action("action_3", []
                          , [test_positional_args2()])
     , action_4 =>
           test_cli_action("action_4", [[posn_1]]
                          , [ test_positional_args1()
                            , test_positional_args2()
                            ])
     }.

test_model() ->
    MF = test_model_raw(),
    {ok, M} = lee_model:compile( [ lee:base_metamodel()
                                 , lee_metatype:create(lee_cli)
                                 ]
                               , [MF]
                               ),
    M.

-define(tok(String, Pattern),
        ?assertEqual( Pattern
                    , lee_cli:tokenize($@, string:tokens(String, " "))
                    )).

tokenize_test() ->
    ?tok("--foo", [{long, "foo", "true"}]),
    ?tok("-sj42", [{short, $s, "true"}, {short, $j, "42"}]),
    ?tok("-sj foo", [{short, $s, "true"}, {short, $j, "foo"}]),
    ?tok("@foo", [{command, "foo"}]),
    ?tok( "--foo bar --bar foo"
        , [ {long, "foo", "bar"}
          , {long, "bar", "foo"}
          ]),
    ?tok( "--foo --bar foo --baz"
        , [ {long, "foo", "true"}
          , {long, "bar", "foo"}
          , {long, "baz", "true"}
          ]),
    ?tok( "--foo1 -- bar"
        , [ {long, "foo1", "true"}
          , {positional, "bar"}
          ]),
    ?tok( "foo --bar 1 --baz --jobs 33 baz quux foo"
        , [ {positional, "foo"}
          , {long, "bar", "1"}
          , {long, "baz", "true"}
          , {long, "jobs", "33"}
          , {positional, "baz"}
          , {positional, "quux"}
          , {positional, "foo"}
          ]),
    ?tok("-s0 -c9", [{short, $s, "0"}, {short, $c, "9"}]),
    ?tok( "kill -9 -fml0 --foo bar -j 11 - @cmd foo -- @bar"
        , [ {positional, "kill"}
          , {short, $9, "true"}
          , {short, $f, "true"}
          , {short, $m, "true"}
          , {short, $l, "0"}
          , {long, "foo", "bar"}
          , {short, $j, "11"}
          , {positional, "-"}
          , {command, "cmd"}
          , {positional, "foo"}
          , {positional, "@bar"}
          ]),
    %% Check separator:
    ?tok("-- --foo", [{positional, "--foo"}]),
    ?tok("-s -- foo", [{short, $s, "true"}, {positional, "foo"}]),
    ?tok("--long -- foo", [{long, "long", "true"}, {positional, "foo"}]),
    %% Check negation:
    ?tok("--no-foo --no-bar baz", [ {long, "foo", "false"}
                                  , {long, "bar", "false"}
                                  , {positional, "baz"}
                                  ]),
    ?tok("--no-foo --bar baz", [ {long, "foo", "false"}
                               , {long, "bar", "baz"}
                               ]),
    %% Short negation:
    ?tok("+sf foo", [ {short, $s, "false"}
                    , {short, $f, "false"}
                    , {positional, "foo"}
                    ]),
    ?tok("--no-foo +9f8 foo", [ {long, "foo", "false"}
                              , {short, $9, "false"}
                              , {short, $f, "false"}
                              , {short, $8, "false"}
                              , {positional, "foo"}
                              ]).

read_cli(String) ->
    Args = string:tokens(String, " "),
    lee_cli:read(test_model(), Args).

ok_patch(String, ExpectedPatch) ->
    {ok, Patch} = read_cli(String),
    ?assertEqual( lists:sort(ExpectedPatch)
                , lists:sort(Patch)
                , String
                ).

simple_long_test() ->
    ok_patch( "--long foo"
            , [{set, [global, long], "foo"}]
            ).

simple_short_test() ->
    ok_patch( "-s 1"
            , [{set, [global, short], 1}]
            ).

simple_both_test() ->
    ok_patch( "-b {foo,1}"
            , [{set, [global, both], {foo, 1}}]
            ),
    ok_patch( "--both {foo,2}"
            , [{set, [global, both], {foo, 2}}]
            ).

global_flags_test() ->
    ok_patch( "-fgs1"
            , [ {set, [global, flag1], true}
              , {set, [global, flag2], true}
              , {set, [global, short], 1}
              ]
            ).

children_test() ->
    ok_patch( "@action_1 -fgs1 --long heyhey @action_2 foo bar"
            , [ {set, [action_1, {1,"heyhey"}],        []}
              , {set, [action_1, {1,"heyhey"}, flag1], true}
              , {set, [action_1, {1,"heyhey"}, flag2], true}
              , {set, [action_1, {1,"heyhey"}, long],  "heyhey"}
              , {set, [action_1, {1,"heyhey"}, short], 1}
              , {set, [action_2, {"foo"}],             []}
              , {set, [action_2, {"foo"}, posn_1],     "foo"}
              , {set, [action_2, {"foo"}, posn_2],     "bar"}
              ]
            ).

rest1_test() ->
    ok_patch( "@action_3 foo quux 1"
            , [ {set, [action_3, {}],  []}
              , {set, [action_3, {}, posn_n], [foo, quux, '1']}
              ]
            ).

rest2_test() ->
    ok_patch( "@action_4 1 2 foo bar"
            , [ {set, [action_4, {"1"}], []}
              , {set, [action_4, {"1"}, posn_1], "1"}
              , {set, [action_4, {"1"}, posn_2], "2"}
              , {set, [action_4, {"1"}, posn_n], [foo, bar]}
              ]
            ).

default_key_test() ->
    ok_patch( "@action_1 -s 42"
            , [ {set, [action_1, {42, "default"}], []}
              , {set, [action_1, {42, "default"}, short], 42}
              ]
            ).

no_key_test() ->
    ?assertMatch( {error, _}
                , read_cli("@action_1")
                ).

rest_empty_list_test() ->
    ok_patch( "@action_3"
            , [ {set, [action_3, {}], []}
              , {set, [action_3, {}, posn_n], []}
              ]
            ).

err_compile(ExpectedErrors, Model) ->
    {error, Errors} = compile(Model),
    ?assertEqual( lists:sort(ExpectedErrors)
                , lists:sort(Errors)
                ).

validate_illegal_combinatio_test() ->
    M1 = #{ foo => {[cli_param, cli_action],
                    #{ cli_operand => "op"
                     }}
          },
    err_compile( ["[foo]: Illegal combination of CLI metatypes"]
               , M1
               ),
    M2 = #{ foo => {[cli_positional, cli_param],
                    #{ cli_operand => "op"
                     , cli_arg_position => 1
                     }}
          },
    err_compile( ["[foo]: Illegal combination of CLI metatypes"]
               , M2
               ).

validate_param_test() ->
    M1 = #{ foo => {[cli_param], #{}}
          },
    err_compile( ["[foo]: Missing `cli_operand' or `cli_short' attributes"]
               , M1
               ),
    M2 = #{ foo => {[cli_param],
                    #{ cli_short => a
                     }}
          },
    ?assertMatch( {error, ["[foo]: Type mismatch." ++ _]}
                , compile(M2)
                ),
    M3 = #{ foo => {[cli_param],
                    #{ cli_operand => a
                     }}
          },
    ?assertMatch( {error, ["[foo]: Type mismatch." ++ _]}
                , compile(M3)
                ),
    M4 = #{ foo => {[cli_param],
                    #{ cli_operand => "no-foo"
                     }}
          },
    ?assertMatch( {error, ["[foo]: CLI operands can't have `no-' prefix" ++ _]}
                , compile(M4)
                ).

validate_action_test() ->
    M1 = #{ foo => {[cli_action], #{}}
          },
    ?assertMatch({error, ["[foo]: Missing" ++ _]}, compile(M1)),
    M2 = #{ foo => {[cli_action],
                    #{ cli_operand => a
                     }}
          },
    ?assertMatch( {error, ["[foo]: Type mismatch." ++ _]}
                , compile(M2)
                ).

validate_duplicate_action_test() ->
    M1 = #{ foo => {[cli_action],
                    #{ cli_operand => "duplicate"
                     }}
          , bar => {[cli_action],
                    #{ cli_operand => "duplicate"
                     }}
          },
    err_compile( ["[foo]: Action name @duplicate is already used by [bar]"]
               , M1
               ).

validate_positional_test() ->
    M1 = #{ foo => {[cli_positional], #{}}
          },
    ?assertMatch({error, ["[foo]: Missing" ++ _]}, compile(M1)),
    M2 = #{ foo => {[cli_positional],
                    #{ cli_arg_position => a
                     }}
          },
    ?assertMatch( {error, ["[foo]: Type mismatch." ++ _]}
                , compile(M2)
                ).

validate_duplicate_long_test() ->
    M1 = #{ foo => {[cli_param], #{cli_operand => "long"}}
          , bar => {[cli_param], #{cli_operand => "long"}}
          },
    err_compile( ["[foo]: CLI operand --long is already used by [bar]"]
               , M1
               ).

validate_duplicate_short_test() ->
    M1 = #{ foo => {[cli_param], #{cli_short => $s}}
          , bar => {[cli_param], #{cli_short => $s}}
          },
    err_compile( ["[foo]: CLI operand -s is already used by [bar]"]
               , M1
               ).

compile(Module) ->
    lee_model:compile([lee:base_metamodel(), lee_metatype:create(lee_cli)], [Module]).
