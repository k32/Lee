%%--------------------------------------------------------------------
%% Copyright (c) 2022 k32 All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(lee_conf_file_tests).


-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").
-include_lib("lee/include/lee.hrl").


model() ->
    Model0 = #{ foo => {[value],
                         #{ type => string()
                          }}
              , bar => {[value],
                         #{ type => {string()}
                          }}
              , baz =>
                    #{ quux =>
                           {[value],
                            #{ type => integer()
                             }}
                     }
              },
    File = "test/data/conf-file-correct.eterm",
    Meta = [ lee:base_metamodel()
           , lee_metatype:create(lee_config_file, #{file => File, tag => conffile})
           ],
    {ok, Model} = lee_model:compile(Meta, [Model0]),
    Model.


read_test() ->
    Model = model(),
    {ok, Data, _} = lee:init_config(Model, lee_storage:new(lee_map_storage)),
    ?assertMatch( "I am foo"
                , lee:get(Model, Data, [foo])
                ),
    ?assertMatch( {"I am bar"}
                , lee:get(Model, Data, [bar])
                ),
    ?assertMatch( 42
                , lee:get(Model, Data, [baz, quux])
                ).
