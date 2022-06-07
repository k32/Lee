%%--------------------------------------------------------------------
%% Copyright (c) 2022 k32. All Rights Reserved.
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
-module(lee_metatype).

%% API:
-export([name/1, validate_node/5, meta_validate/5, doc_chapter_title/1, doc_gen/3]).

-include("lee_internal.hrl").

%%================================================================================
%% Callback declarations
%%================================================================================

-callback name() -> atom().

-callback validate_node(lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().

-callback meta_validate(lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().

-callback doc_chapter_title() -> string().

-callback doc_gen(lee:model(), lee:data()) -> lee_doc:doc().

-optional_callbacks([validate_node/4, meta_validate/4, doc_chapter_title/0, doc_gen/2]).

%%================================================================================
%% API funcions
%%================================================================================

-spec name(module()) -> atom().
name(M) ->
    M:name().

-spec validate_node(module(), lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().
validate_node(M, Model, Data, Key, MNode) ->
    case erlang:function_exported(M, ?FUNCTION_NAME, 4) of
        true  -> M:?FUNCTION_NAME(Model, Data, Key, MNode);
        false -> {[], []}
    end.

-spec meta_validate(module(), lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().
meta_validate(M, Model, Data, Key, MNode) ->
    case erlang:function_exported(M, ?FUNCTION_NAME, 4) of
        true  -> M:?FUNCTION_NAME(Model, Data, Key, MNode);
        false -> {[], []}
    end.

-spec doc_chapter_title(module()) -> string().
doc_chapter_title(M) ->
    M:doc_chapter_title().

-spec doc_gen(module(), lee:model(), lee:data()) -> lee_doc:doc().
doc_gen(M, Model, Data) ->
    M:doc_gen(Model, Data).

%%================================================================================
%% Internal functions
%%================================================================================
