%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(lee_map).

-behavior(lee_metatype).

%% API:
-export([]).

%% behavior callbacks:
-export([create/1, names/1, description/1, meta_validate_node/4]).

-include("../framework/lee_internal.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

create(_) ->
    [].

names(_) ->
    [map].

description(map) ->
    "<para>All maps.</para>".

-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) ->
                            lee_lib:check_result().
meta_validate_node(map, #model{model = Model}, Key, #mnode{metaparams = Params}) ->
    ValidateKey =
        fun(ChildKey) ->
                case lee_storage:get(Key ++ [?children|ChildKey], Model) of
                    undefined ->
                        Error = lee_lib:format( "~p: ~p is not a valid child key", [Key, ChildKey]),
                        {true, Error};
                    {ok, _} ->
                        false
                end
        end,
    { case Params of
          #{key_elements := KeyElems} when is_list(KeyElems) ->
              lists:filtermap(ValidateKey, KeyElems);
          #{key_elements := _}  ->
              [lee_lib:format("~p: `key_elements' should be a list of valid child keys", [Key])];
          _ ->
              []
      end
    , []
    }.

%%================================================================================
%% Internal functions
%%================================================================================
