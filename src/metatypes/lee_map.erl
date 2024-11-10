%%--------------------------------------------------------------------
%% Copyright (c) 2022-2024 k32 All Rights Reserved.
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
-export([key_elements/2]).

%% behavior callbacks:
-export([names/1, meta_validate_node/4, read_patch/2, metaparams/1]).

-include("../framework/lee_internal.hrl").

-define(PRIO, -999999).

%%================================================================================
%% API
%%================================================================================

%% @doc Return list of map key elements
-spec key_elements(lee:model(), lee:model_key()) -> list(lee:model_key()).
key_elements(Model, Key) ->
    #mnode{metaparams = MetaAttrs} = lee_model:get(Key, Model),
    case MetaAttrs of
        #{key_elements := KeyElems} ->
            [Key ++ [{} | I] || I <- KeyElems];
        #{} ->
            []
    end.

%%================================================================================
%% behavior callbacks
%%================================================================================

%% @private
names(_) ->
    [map, default_instance].

%% @private
metaparams(map) ->
    [{optional, key_elements, typerefl:list(lee:model_key())}];
metaparams(default_instance) ->
    [].

%% @private
-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) ->
                            lee_lib:check_result().
meta_validate_node(map, #model{model = Model}, Key, #mnode{metaparams = Params}) ->
    ValidateKey =
        fun(ChildKey) ->
                case lee_storage:get(Key ++ [?children|ChildKey], Model) of
                    undefined ->
                        Error = lee_lib:format("missing key element ~p", [ChildKey]),
                        {true, Error};
                    {ok, _} ->
                        false
                end
        end,
    KeyElems = ?m_attr(map, key_elements, Params, []),
    {lists:filtermap(ValidateKey, KeyElems), []};
meta_validate_node(default_instance, Model, Key, #mnode{metatypes = MTs}) ->
    Errors = ["only maps can have a default instance" || not lists:member(map, MTs)] ++
             check_all_defaults(Model, Key),
    {Errors, []}.

%% @private
read_patch(map, _) ->
    {ok, ?PRIO, []};
read_patch(default_instance, Model) ->
    Keys = lee_model:get_metatype_index(default_instance, Model),
    {ok, ?PRIO, [OP || K  <- Keys,
                       OP <- lee_lib:make_nested_patch(Model, K, #{})]}.

%%================================================================================
%% Internal functions
%%================================================================================

check_all_defaults(_Model, _Parent) ->
    []. %% TODO
