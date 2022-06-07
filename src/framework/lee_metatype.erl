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
-export([create/1, create/2, get_module/2, is_implemented/3,
         validate_node/5, meta_validate_node/4, doc_chapter_title/2, doc_gen/2]).

-export_types([cooked_metatype/0]).

-include("lee_internal.hrl").

%%================================================================================
%% Callback declarations
%%================================================================================

-callback names(_Config) -> [lee:metatype()].

-callback create(map()) -> _Config.

-callback validate_node(lee:metatype(), lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().

-callback meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_lib:check_result().

-callback doc_chapter_title(lee:metatype(), lee:model()) -> string() | undefined.

-callback doc_gen(lee:metatype(), lee:model()) -> lee_doc:doc().

-optional_callbacks([validate_node/5, meta_validate_node/4, doc_chapter_title/2, doc_gen/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type cooked_metatype() :: {module(), [atom()], _Config}.

-type callback() :: validate_node | meta_validate | doc_chapter_title | doc_gen.

%%================================================================================
%% Macros
%%================================================================================

-define(is_implemented(M),
        erlang:function_exported(M, ?FUNCTION_NAME, callback_arity(?FUNCTION_NAME))).

%%================================================================================
%% API funcions
%%================================================================================

-spec create(module(), map()) -> cooked_metatype().
create(M, Params) ->
    Conf = M:create(Params),
    Names = M:names(Params),
    {M, Names, Conf}.

-spec create(module()) -> cooked_metatype().
create(Module) ->
    create(Module, #{}).

-spec get_module(lee:model(), lee:metatype()) -> module().
get_module(#model{metamodules = Modules}, Metatype) ->
    maps:get(Metatype, Modules).

-spec is_implemented(lee:model(), lee:metatype(), callback()) -> boolean().
is_implemented(Model, Metatype, Callback) ->
    Mod = get_module(Model, Metatype),
    erlang:function_exported(Mod, Callback, callback_arity(Callback)).

-spec validate_node(lee:metatype(), lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().
validate_node(Metatype, Model, Data, Key, MNode) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Data, Key, MNode);
        false -> {[], []}
    end.

-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_lib:check_result().
meta_validate_node(Metatype, Model, Key, MNode) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Key, MNode);
        false -> {[], []}
    end.

-spec doc_chapter_title(lee:metatype(), lee:model()) -> string() | undefined.
doc_chapter_title(Metatype, Model) ->
    M = get_module(Model, Metatype),
    M:doc_chapter_title(Metatype, Model).

-spec doc_gen(lee:metatype(), lee:model()) -> lee_doc:doc().
doc_gen(Metatype, Model) ->
    M = get_module(Model, Metatype),
    M:doc_gen(Metatype, Model).

%%================================================================================
%% Internal functions
%%================================================================================

-spec callback_arity(callback()) -> arity().
callback_arity(validate_node) ->
    5;
callback_arity(meta_validate_node) ->
    4;
callback_arity(doc_chapter_title) ->
    2;
callback_arity(doc_gen) ->
    2;
callback_arity(CB) ->
    error({unknown_callback, CB}).
