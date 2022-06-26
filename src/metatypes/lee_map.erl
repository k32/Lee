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
-module(lee_map).

-behavior(lee_metatype).

%% API:
-export([ key_elements/2
        ]).

%% behavior callbacks:
-export([ names/1, meta_validate_node/4, read_patch/2
        , description/2, description_title/2, description_node/4
        ]).

-include("../framework/lee_internal.hrl").

-define(PRIO, -999999).

%%================================================================================
%% API
%%================================================================================

-spec key_elements(lee:model(), lee:model_key()) -> list(lee:model_key()).
key_elements(Model, Key) ->
    #mnode{metatypes = MTs, metaparams = MetaAttrs} = lee_model:get(Key, Model),
    case MetaAttrs of
        #{key_elements := KeyElems} ->
            [Key ++ [{} | I] || I <- KeyElems];
        #{} ->
            []
    end.

%%================================================================================
%% behavior callbacks
%%================================================================================

names(_) ->
    [map, default_instance].

-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) ->
                            lee_lib:check_result().
meta_validate_node(map, #model{model = Model}, Key, #mnode{metaparams = Params}) ->
    ValidateKey =
        fun(ChildKey) ->
                case lee_storage:get(Key ++ [?children|ChildKey], Model) of
                    undefined ->
                        Error = lee_lib:format( "~p: missing key element ~p", [Key, ChildKey]),
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
    };
meta_validate_node(default_instance, Model, Key, #mnode{metatypes = MTs}) ->
    Errors = ["only maps can have a default instance" || not lists:member(map, MTs)] ++
             check_all_defaults(Model, Key),
    lee_lib:inject_error_location(Key, {Errors, []}).


read_patch(map, _) ->
    {ok, ?PRIO, []};
read_patch(default_instance, Model) ->
    Keys = lee_model:get_metatype_index(default_instance, Model),
    {ok, ?PRIO, [OP || K  <- Keys,
                       OP <- lee_lib:make_nested_patch(Model, K, #{})]}.

description_title(map, _Model) ->
    "Maps".

description(map, _) ->
    [{para, ["This section lists all maps."]}].

description_node(map, Model, Key, MNode = #mnode{metaparams = Attrs}) ->
    KeyAttr = ?m_attr(map, key_elements, Attrs, []),
    Description = lee_doc:get_description(Model, Key, MNode),
    Oneliner    = lee_doc:get_oneliner(Model, Key, MNode),
    Id = lee_doc:format_key(Key),
    KeyElems =  [{orderedlist,
                  [{listitem,
                    [{para,
                      [ {link, [{linkend, lee_doc:format_key(Key ++ [?children|I])}],
                         [lee_doc:format_key(I)]}
                      ]}]}
                   || I <- KeyAttr]}
                ],
    { section, [{id, Id}]
    , [ {title, [Id]}
      , {para, [Oneliner]}
      , lee_doc:simplesect("Key elements:", KeyElems)
      ] ++ Description
    }.


%%================================================================================
%% Internal functions
%%================================================================================

check_all_defaults(_Model, _Parent) ->
    []. %% TODO
