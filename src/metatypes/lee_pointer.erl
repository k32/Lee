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
-module(lee_pointer).
%% A value that contains a pointer to an instance of a map

-behavior(lee_metatype).

%% API:
-export([resolve/3]).

%% behavior callbacks:
-export([names/1, meta_validate_node/4, validate_node/5]).

-include("lee.hrl").

%%================================================================================
%% API
%%================================================================================

-spec resolve(lee:model(), lee:data(), lee:model_key()) -> lee:model_key().
resolve(Model, Data, Key) ->
    #mnode{metaparams = #{target_node := Target}} = lee_model:get(Key, Model),
    Instance = lee:get(Model, Data, Key),
    Target ++ [{Instance}].

%%================================================================================
%% behavior callbacks
%%================================================================================

%% @private
names(_) ->
  [pointer].

%% @private
meta_validate_node(pointer, Model, _Key, #mnode{metaparams = Attrs}) ->
  try
      Pointer = ?m_attr(pointer, target_node, Attrs),
      MyType = ?m_attr(value, type, Attrs),
      %% TODO: Currently only a single key is supported:
      [KeyElem] = lee_map:key_elements(Model, Pointer),
      #mnode{metaparams = #{type := KeyType}} = lee_model:get(KeyElem, Model),
      KeyType = MyType, % assert
      {[], []}
  catch
    _:_ ->
      Err = "pointer must be also an instance of `value' metatype, "
            "it must contain a `target_node' metaparameter, "
            "that must be a key of an existing map, "
            "and the type of the pointer must match the key "
            "element of the map.",
      {[Err], []}
  end.

%% @private
validate_node(pointer, Model, Data, Key, #mnode{metaparams = #{target_node := Target}}) ->
    Instance = lee:get(Model, Data, Key),
    try lee:get(Model, Data, Target ++ [{Instance}]) of
        _ -> {[], []}
    catch
        _:_ ->
            {[lee_lib:format("instance `~p' of map ~p doesn't exist", [Instance, Target])], []}
    end.

%%================================================================================
%% Internal functions
%%================================================================================
