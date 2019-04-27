%% This module shows how to define and use models
-module(example_model).

-export([main/0]).

-include_lib("lee/include/lee_types.hrl").

%% Model definition:
-spec model() -> lee:lee_module().
model() ->
    #{ foo =>
           {[value],
            #{ type => boolean()
             , oneliner => "This value controls fooing"
             }}
     , bar =>
           #{ baz =>
                  {[value],
                   #{ type => integer()
                    , oneliner => "This value controls bazing"
                    , default => 42
                    }}
            , quux =>
                  {[value],
                   #{ type => nonempty_list(atom())
                    , oneliner => "This value controls quuxing"
                    , default => [foo]
                    }}
            }
     }.

%% Test data:
-spec data() -> lee:data().
data() ->
    %% Create am empty storage:
    Data0 = lee_storage:new(lee_map_storage),
    %% Define a patch:
    Patch = [ %% Set some values:
              {set, [foo], false}
            , {set, [bar, quux], [quux]}
              %% Delete a value (if present):
            , {rm, [bar, baz]}
            ],
    %% Apply the patch:
    lee_storage:patch(Data0, Patch).

main() ->
    {ok, Model} = lee_model:compile( [lee:base_metamodel()]
                                   , [lee:base_model(), model()]
                                   ),
    Data = data(),
    {ok, _Warnings} = lee:validate(Model, Data),
    [quux] = lee:get(Model, Data, [bar, quux]),
    false = lee:get(Model, Data, [foo]),
    ok.
