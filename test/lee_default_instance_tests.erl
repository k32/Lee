-module(lee_default_instance_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").

load(M) ->
    {ok, Model} = lee_model:compile(lee:base_metamodel(), [M]),
    {ok, Data, _} = lee:init_config(Model, lee_storage:new(lee_map_storage)),
    lee_storage:dump(Data).


default_patch_no_children_test() ->
    M1 = #{ foo => {[map, default_instance], #{}}
          },
    ?assertEqual( load(M1)
                , [{set, [foo, {}], []}]
                ).

default_patch_no_keys_test() ->
    M1 = #{ foo =>
                {[map, default_instance],
                 #{},
                 #{ foo =>
                        {[value],
                         #{ type    => boolean()
                          , default => false
                          }}
                  }}
          },
    ?assertEqual( [{set, [foo, {}], []}]
                , load(M1)
                ).

default_patch_single_key_test() ->
    M1 = #{ foo =>
                {[map, default_instance],
                 #{ key_elements => [[foo]]
                  },
                 #{ foo =>
                        {[value],
                         #{ type    => boolean()
                          , default => false
                          }}
                  }}
          },
    ?assertEqual( [{set, [foo, {false}], []}]
                , load(M1)
                ).

default_patch_multi_key_test() ->
    M1 = #{ foo =>
                {[map, default_instance],
                 #{ key_elements => [[foo], [bar]]
                  },
                 #{ foo =>
                        {[value],
                         #{ type    => boolean()
                          , default => false
                          }}
                  , bar =>
                        {[value],
                         #{ type    => integer()
                          , default => 42
                          }}
                  }}
          },
    ?assertEqual( [{set, [foo, {false, 42}], []}]
                , load(M1)
                ).

validate_wrong_mt_test() ->
    M = #{foo => {[default_instance], #{}}},
    ?assertMatch( {error, ["[foo]: only maps can have a default instance"]}
                , lee_model:compile(lee:base_metamodel(), [M])
                ).
