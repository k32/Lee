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
-module(lee_doc_root).

-behavior(lee_metatype).

%% API:
-export([create/1]).

%% behavior callbacks:
-export([names/1, meta_validate_node/4, doc_chapter_title/2, doc_gen/4]).

-include("../framework/lee_internal.hrl").

%%================================================================================
%% API funcions
%%================================================================================
create(_) ->
    [].

%%================================================================================
%% behavior callbacks
%%================================================================================

names(_) ->
    [doc_root].

meta_validate_node(doc_root, _Model, Key, #mnode{metaparams = Attrs}) ->
    Fun = fun(#{app_name := _}) -> {[], []};
             (_)                -> {["Missing `app_name' parameter"], []}
          end,
    lee_lib:perform_checks(Key, Attrs, [fun lee_doc:check_docstrings/1, Fun]).

doc_chapter_title(doc_root, Model) ->
    [Key] = lee_model:get_metatype_index(doc_root, Model),
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    ?m_attr(doc_root, app_name, Attrs).

doc_gen(doc_root, Model, _Key, #mnode{metaparams = Attrs}) ->
    AppOneliner = ?m_attr(doc_root, oneliner, Attrs),
    AppDoc = lee_doc:docbook(?m_attr(doc_root, doc, Attrs, "")),
    {chapter, [{id, "_intro"}]
    , [{title, ["Introduction"]}, {para, [AppOneliner]} | AppDoc]
    }.

%%================================================================================
%% Internal functions
%%================================================================================
