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
-export([create/0]).

%% behavior callbacks:
-export([name/0, meta_validate/4]).

-include("../framework/lee_internal.hrl").

%%================================================================================
%% API funcions
%%================================================================================
create() ->
    ok.

%%================================================================================
%% behavior callbacks
%%================================================================================

name() ->
    doc_root.

-spec meta_validate(lee:model(), _, lee:key(), #mnode{}) ->
                               lee_lib:check_result().
meta_validate(_, _, Key, #mnode{metaparams = Attrs}) ->
    Fun = fun(#{app_name := _}) -> {[], []};
             (_)                -> {["Missing `app_name' parameter"], []}
          end,
    lee_lib:perform_checks(Key, Attrs, [fun lee_doc:check_docstrings/1, Fun]).


%%================================================================================
%% Internal functions
%%================================================================================
