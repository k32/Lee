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
         validate_node/5, meta_validate_node/4,
         description_title/2, description_node/4, description/2,
         read_patch/2, post_patch/5]).

-export_types([cooked_metatype/0]).

-include("lee_internal.hrl").

%%================================================================================
%% Callback declarations
%%================================================================================

-callback names(_Config) -> [lee:metatype()].

-callback create(map()) -> [{lee:key(), term()}].

-callback validate_node(lee:metatype(), lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().

-callback meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_lib:check_result().

-callback description_title(lee:metatype(), lee:model()) -> string() | undefined.

-callback description(lee:metatype(), lee:model()) -> string() | lee_doc:doc().

-callback description_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_doc:doc().

-callback read_patch(lee:metatype(), lee:model()) -> {integer(), lee:patch()}.

-callback post_patch(lee:metatype(), lee:model(), lee:data(), #mnode{}, lee_storage:patch_op(term())) -> ok.

-optional_callbacks([validate_node/5, meta_validate_node/4, description_title/2,
                     description_node/4, read_patch/2, post_patch/5, description/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type cooked_metatype() :: {module(), [atom()], [{lee:key(), term()}]}.

-type callback() :: validate_node | meta_validate_node | description_title | description_node
                  | read_patch | post_patch.

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

-spec description(lee:model(), lee:metatype()) -> lee_doc:doc().
description(Model, Metatype) ->
    Module = get_module(Model, Metatype),
    Module:?FUNCTION_NAME(Metatype, Model).

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

-spec description_title(lee:metatype(), lee:model()) -> string() | undefined.
description_title(Metatype, Model) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> Module:description_title(Metatype, Model);
        false -> undefined
    end.

-spec description_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_doc:doc().
description_node(Metatype, Model, Key, MNode) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Key, MNode);
        false -> []
    end.

-spec read_patch(lee:metatype(), lee:model()) -> lee:patch().
read_patch(Metatype, Model) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model);
        false -> {0, []}
    end.

-spec post_patch(lee:metatype(), lee:model(), lee:data(), #mnode{}, lee_storage:patch_op(term())) -> ok.
post_patch(Metatype, Model, Data, MNode, PatchOp) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Data, MNode, PatchOp);
        false -> ok
    end.

%%================================================================================
%% Internal functions
%%================================================================================

-spec callback_arity(callback()) -> arity().
callback_arity(validate_node) ->
    5;
callback_arity(meta_validate_node) ->
    4;
callback_arity(description_title) ->
    2;
callback_arity(description_node) ->
    4;
callback_arity(read_patch) ->
    2;
callback_arity(post_patch) ->
    5;
callback_arity(CB) ->
    error({unknown_callback, CB}).
