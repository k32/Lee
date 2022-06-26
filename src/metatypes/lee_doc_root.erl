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
-module(lee_doc_root).

-behavior(lee_metatype).

%% API:
-export([]).

%% behavior callbacks:
-export([create/1, names/1, meta_validate_node/4,
         description_title/2, description/2]).

-include("../framework/lee_internal.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

create(_) ->
    [].

names(_) ->
    [doc_root].

meta_validate_node(doc_root, _Model, Key, #mnode{metaparams = Attrs}) ->
    Fun = fun(#{app_name := _}) -> {[], []};
             (_)                -> {["Missing `app_name' parameter"], []}
          end,
    lee_lib:perform_checks(Key, Attrs, [fun lee_doc:check_docstrings/1, Fun]).

description_title(doc_root, Model) ->
    [Key] = lee_model:get_metatype_index(doc_root, Model),
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    ?m_attr(doc_root, app_name, Attrs).

description(doc_root, Model) ->
    [Key]       = lee_model:get_metatype_index(doc_root, Model),
    MNode       = lee_model:get(Key, Model),
    Description = lee_doc:get_description(Model, Key, MNode),
    Oneliner    = lee_doc:get_oneliner(Model, Key, MNode),
    [{chapter, [{id, "_intro"}]
     , [{title, ["Introduction"]}, {para, [Oneliner]} | Description]
     }].

%%================================================================================
%% Internal functions
%%================================================================================
