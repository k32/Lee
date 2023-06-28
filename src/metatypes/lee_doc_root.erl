%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 k32 All Rights Reserved.
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
-export([doc_root/1, app_name/1, prog_name/1]).

%% behavior callbacks:
-export([create/1, names/1, metaparams/1,
         description/3]).

-include("lee.hrl").

%%================================================================================
%% API
%%================================================================================

-spec doc_root(lee:model()) -> lee:model_key().
doc_root(Model) ->
    [Key] = lee_model:get_metatype_index(doc_root, Model),
    Key.

-spec app_name(lee:model()) -> string().
app_name(Model) ->
    #mnode{metaparams = Attrs} = lee_model:get(doc_root(Model), Model),
    ?m_attr(doc_root, app_name, Attrs).

-spec prog_name(lee:model()) -> string().
prog_name(Model) ->
    #mnode{metaparams = Attrs} = lee_model:get(doc_root(Model), Model),
    ?m_attr(doc_root, prog_name, Attrs).

%%================================================================================
%% behavior callbacks
%%================================================================================

create(_) ->
    [].

names(_) ->
    [doc_root].

metaparams(doc_root) ->
    [ {mandatory, app_name, typerefl:string()}
    , {mandatory, prog_name, typerefl:string()}
    | lee_doc:documented()
    ].

description(doc_root, Model, _Options) ->
    %% Get doc root mnode:
    DocRoot = doc_root(Model),
    Content = lists:append(
                [ [lee_doc:p(Oneliner) || Oneliner <- lee_doc:get_oneliner(Model, DocRoot)]
                , lee_doc:get_description(Model, DocRoot)
                ]),
    [{title, [app_name(Model)]},
     {preface,
      [ {title, ["Introduction"]}
      | Content
      ]}].

%%================================================================================
%% Internal functions
%%================================================================================
