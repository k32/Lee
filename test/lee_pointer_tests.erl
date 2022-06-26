-module(lee_pointer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").

meta_validate_test() ->
    {ok, _} = compile(
                model({[value, pointer],
                       #{ type => atom()
                        , target_node => [map]
                        }})),
    %% Missing `target_node' attribute:
    {error, ["[ptr]: " ++ _]} =
        compile(
          model({[value, pointer],
                 #{ type => atom()
                  }})),
    %% Wrong type:
    {error, ["[ptr]: " ++ _]} =
        compile(
          model({[value, pointer],
                 #{ type => integer()
                  , target_node => [map]
                  }})).

validate_test() ->
    D0 = lee_storage:new(lee_map_storage),
    Model = model(),
    {error, ["[ptr]: instance `a' of map [map] doesn't exist"], _} =
        lee:patch(Model, D0, [{set, [ptr], a}]),
    {ok, D, _} =
        lee:patch(Model, D0,
                  [ {set, [ptr], a}
                  , {set, [map, {a}], []}
                  , {set, [map, {a}, k], a}
                  ]),
    [map, {a}] = lee_pointer:resolve(Model, D, [ptr]).

model() ->
    {ok, Model} = compile(model(({[value, pointer],
                                  #{ type => atom()
                                   , target_node => [map]
                                   }}))),
    Model.

model(Ptr) ->
    #{ ptr =>
           Ptr
     , map =>
           {[map],
            #{ key_elements => [[k]] },
            #{ k =>
                   {[value],
                    #{ type => atom()
                     }}
             }}
     }.


compile(Model) ->
    lee_model:compile( [lee:base_metamodel()]
                     , [Model]
                     ).
