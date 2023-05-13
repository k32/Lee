-module(lee_tests).

-export([validate_callback_spec/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("lee/src/framework/lee_internal.hrl").
-include_lib("typerefl/include/types.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

-define(moc, {[], #{}, #{}}).

namespace_test() ->
    ?assertMatch( #{foo := #{bar := #{baz := #{}}}}
                , lee:namespace([foo, bar], #{baz => #{}})
                ).

run_validate(Model, Config0) ->
    Config = [{set, K, V} || {K, V} <- maps:to_list(Config0)],
    Data = lee_storage:patch( lee_storage:new(lee_map_storage, [])
                            , Config
                            ),
    catch lee:validate(Model, Data).

-define(valid(Config),
        ?assertMatch( {ok, _}
                    , run_validate(Model, Config)
                    )).

-define(invalid(Config),
        ?assertMatch( {error, _, _}
                    , run_validate(Model, Config)
                    )).

validate_test() ->
    Model0 = #{ foo =>
                    {[value],
                     #{ type => typerefl:boolean()
                      }}
              , bar =>
                    {[value],
                     #{ type => typerefl:integer()
                      , default => 42
                      }}
              , barfoo =>
                    {[value],
                     #{ type => typerefl:boolean()
                      , default_ref => [foo]
                      }}
              , foobar =>
                    {[value],
                     #{ type => typerefl:integer()
                      , default_ref => [bar]
                      }}
              , foobaz =>
                    {[value],
                     #{ type => typerefl:integer()
                      , default_str => "42"
                      }}
              },
    Model1 = Model0
        #{ baz =>
               {[map],
                #{},
                Model0
               }
         },
    ?check_trace(
       begin
           {ok, Model} = compile(Model1),
           Model
       end,
       [{"Check different data against the model",
         fun(Model, _Trace) ->
                 ?valid(#{[foo] => true}),
                 ?valid(#{[foo] => true, [bar] => 1}),
                 ?valid(#{[foo] => false, [bar] => -12}),
                 ?invalid(#{}),
                 ?invalid(#{[bar] => 1}),
                 ?invalid(#{[foo] => 1}),
                 ?invalid(#{[foo] => true, [bar] => 1.0}),
                 ?valid(#{ [baz, {1}, foo] => true
                         , [baz, {1}, bar] => 1
                         , [foo] => true
                         }),
                 ?valid(#{ [baz, {1}, foo] => false
                         , [foo] => true
                         }),
                 ?invalid(#{ [foo] => true
                           , [baz, {1}, baz] => foo
                           }),
                 ?invalid(#{ [baz, {1}, bar] => 1
                           , [foo] => true
                           }),
                 ?invalid(#{ [foobar] => true
                           , [foo] => true
                           }),
                 ?invalid(#{ [barfoo] => 1
                           , [foo] => true
                           })
         end},
        fun ?MODULE:validate_callback_spec/2
       ]).

validate_callback_spec(Model, Trace) ->
    #model{ meta_class_idx = Idx
          , metamodules    = Modules
          } = Model,
    maps:map(
      fun(Metatype, Instances) ->
              MyModule = maps:get(Metatype, Modules),
              Validated = [Key || #{ ?snk_kind       := lee_mt_callback
                                   , metatype        := MT
                                   , callback_name   := meta_validate_node
                                   , callback_module := CallbackModule
                                   , arguments       := [Key]
                                   } <- Trace, MT =:= Metatype, CallbackModule =:= MyModule],
              ?assertEqual( lists:sort(Instances)
                          , lists:sort(Validated)
                          , {Metatype, MyModule, Idx}
                          )
      end,
      Idx),
    true.

meta_validate_value_test() ->
    Compile = fun(Attrs) ->
                      compile(#{foo => {[value], Attrs}})
              end,
    %% Missing `type':
    ?assertMatch( {error, ["[foo]: Metaparameters of value are invalid." ++ _]}
                , Compile(#{})
                ),
    %% Wrong type of `default':
    ?assertMatch( {error, ["[foo]: Mistyped default value" ++ _]}
                , Compile(#{type => integer(), default => foo})
                ),
    %% Wrong type of `default_ref':
    ?assertMatch( {error, ["[foo]: Metaparameters of value are invalid." ++ _]}
                , Compile(#{type => integer(), default_ref => foo})
                ),
    %% Wrong type of `default_str':
    ?assertMatch( {error, ["[foo]: Mistyped default value" ++ _]}
                , Compile(#{type => tuple(), default_str => "foo"})
                ),
    %% Non-existent `default_ref':
    ?assertMatch( {error, ["[foo]: Invalid `default_ref' reference key"]}
                , Compile(#{type => integer(), default_ref => [bar]})
                ),
    %% Wrong type of the value referred by `default_ref':
    ?assertMatch( {error, ["[foo]: Type of the `default_ref' is different"]}
                , compile(#{ foo => {[value], #{type => integer(), default_ref => [bar]}}
                           , bar => {[value], #{type => boolean()}}
                           })
                ),
    %% Correct `default_ref':
    ?assertMatch( {ok, _}
                , compile(#{ foo => {[value], #{type => integer(), default_ref => [bar]}}
                           , bar => {[value], #{type => integer()}}
                           })
                ),
    %% Incorrect `default_ref':
    ?assertMatch( {error,["[foo]: Type of the `default_ref' is different"]}
                , compile(#{ foo => {[value], #{type => integer(), default_ref => [bar]}}
                           , bar => {[value], #{type => boolean()}}
                           })),
    ?assertMatch( {error,["[foo]: Invalid `default_ref' reference key"]}
                , compile(#{ foo => {[value], #{type => integer(), default_ref => [bar]}}
                           })),
    ?assertMatch( {error,["[foo]: Invalid `default_ref' metatype"]}
                , compile(#{ foo => {[value], #{type => integer(), default_ref => [bar]}}
                           , bar => {[map], #{}}
                           })),
    %% Wrong type of `oneliner':
    ?assertMatch( {error, ["[foo]: Metaparameters of value are invalid." ++ _]}
                , Compile(#{type => integer(), oneliner => foo})
                ),
    %% Error in `doc':
    ?assertMatch( {error, ["[foo]: Metaparameters of value are invalid." ++ _]}
                , Compile(#{type => integer(), doc => "<para>foo"})
                ).

meta_validate_map_test() ->
    Model1 = #{foo => {[map], #{key_elements => foo}}},
    ?assertMatch( {error, ["[foo]: Metaparameters of map are invalid." ++ _]}
                , compile(Model1)
                ),
    Model2 = #{foo => {[map], #{key_elements => []}}},
    ?assertMatch({ok, _}, compile(Model2)),
    Model3 = #{foo => {[map], #{key_elements => [[bar]]}}},
    ?assertMatch( {error, ["[foo]: missing key element [bar]"]}
                , compile(Model3)
                ),
    Model4 = #{foo => {[map],
                       #{key_elements => [[bar]]},
                       #{foo => {[value], #{}}}
                      }},
    ?assertMatch( {error, [ "[foo,{},foo]: Metaparameters of value are invalid." ++ _
                          , "[foo]: missing key element [bar]"
                          ]}
                , compile(Model4)
                ),
    Model5 = #{foo =>
                   {[map],
                    #{key_elements => [[bar]]},
                    #{bar => {[value], #{type => boolean()}}}
                   }},
    ?assertMatch({ok, _}, compile(Model5)).

get_test() ->
    Model0 = #{ foo => {[value],
                        #{ type => typerefl:boolean()
                         }}
              , bar => {[value],
                        #{ type => typerefl:integer()
                         , default => 42
                         }}
              , foobar => {[value],
                           #{ type => typerefl:integer()
                            , default_ref => [bar]
                            }}
              , foobaz => {[value],
                           #{ type => integer()
                            , default_str => "25"
                            }}
              },
    Model1 = Model0 #{ baz => {[map], #{}, Model0}},
    Model2 = Model1 #{ baz => {[map], #{}, Model1}},
    {ok, Model} = compile(Model2),
    Patch = [ {set, [foo],                      true }
            , {set, [baz, {0}, foo],            true }
            , {set, [baz, {0}, bar],            0    }
            , {set, [baz, {1}, foo],            false}
            , {set, [baz, {0}, baz, {42}, foo], false}
            ],
    {ok, Config, _Warn} = lee:patch( Model
                                   , lee_storage:new(lee_map_storage)
                                   , Patch
                                   ),
    ?assertMatch(true, lee:get(Config, [foo])),
    ?assertMatch(42, lee:get(Config, [bar])),
    ?assertMatch(42, lee:get(Config, [foobar])),

    ?assertMatch(0, lee:get(Config, [baz, {0}, bar])),
    ?assertMatch(42, lee:get(Config, [baz, {0}, foobar])),
    ?assertMatch(true, lee:get(Config, [baz, {0}, foo])),

    ?assertMatch(false, lee:get(Config, [baz, {1}, foo])),
    ?assertMatch(42, lee:get(Config, [baz, {1}, bar])),

    ?assertMatch(false, lee:get(Config, [baz, {0}, baz, {42}, foo])),
    ?assertMatch(42, lee:get(Config, [baz, {0}, baz, {42}, bar])),

    ?assertMatch(25, lee:get(Config, [foobaz])),

    ?assertMatch([[baz, {0}], [baz, {1}]], lists:sort(lee:list(Config, [baz, {}]))),
    %%?assertMatch([[bar]], lee:list(Model, Config, [bar])),
    ok.

overlay_test() ->
    Model0 = #{ foo => {[value],
                        #{ type => typerefl:boolean()
                         }}
              , bar => {[value],
                        #{ type => typerefl:integer(), default => 42
                         }}
              },
    Model1 = Model0 #{ baz => {[map], #{}, Model0}},
    Model2 = Model1 #{ baz => {[map], #{}, Model1}},
    {ok, Model} = compile(Model2),
    Patch1 = [ {set, [foo],                      true }
             , {set, [baz, {0}, foo],            true }
             , {set, [baz, {0}, bar],            0    }
             , {set, [baz, {1}, foo],            false}
             , {set, [baz, {0}, baz, {42}, foo], false}
             ],
    Config1 = lee_storage:patch( lee_storage:new(lee_map_storage)
                               , Patch1
                               ),
    Patch2 = [ {set, [bar],                          32   }
             , {set, [baz, {2}, foo],                false}
             , {set, [baz, {2}, bar],                21   }
             ],
    Config2 = lee_storage:patch( lee_storage:new(lee_map_storage)
                               , Patch2
                               ),
    Config = [Config2, Config1],

    ?assertMatch(true, lee:get(Model, Config, [foo])),
    ?assertMatch(32, lee:get(Model, Config, [bar])),

    ?assertMatch(0, lee:get(Model, Config, [baz, {0}, bar])),
    ?assertMatch(true, lee:get(Model, Config, [baz, {0}, foo])),

    ?assertMatch(false, lee:get(Model, Config, [baz, {1}, foo])),
    ?assertMatch(42, lee:get(Model, Config, [baz, {1}, bar])),

    ?assertMatch(21, lee:get(Model, Config, [baz, {2}, bar])),

    ?assertMatch( [[baz, {0}], [baz, {1}], [baz, {2}]]
                , lee:list(Model, Config, [baz, ?children])
                ),
    ok.

compile(Model) ->
    lee_model:compile( [lee:base_metamodel()]
                     , [Model]
                     ).
