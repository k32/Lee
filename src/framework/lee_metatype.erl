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

%% Callback wrappers:
-export([create/1, create/2,
         pre_compile_mnode/4,
         metaparams/2,
         validate/3, validate_node/5,
         meta_validate/2, meta_validate_node/4,
         description_title/2, description_node/4, description/2,
         read_patch/2, post_patch/5]).

%% API:
-export([is_implemented/3]).

-export_types([cooked_metatype/0, metavalidate_result/0]).

-include("lee_internal.hrl").
-include_lib("snabbkaffe/include/trace.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type cooked_metatype() :: {module(), [atom()], [{lee:key(), term()}]}.

-type callback() :: create | metaparams | pre_compile_mnode
                  | meta_validate | meta_validate_node
                  | validate | validate_node
                  | description_title | description | description_node
                  | read_patch | post_patch.

-type metavalidate_result() :: {_Err :: [string()], _Warn :: [string()], [lee_storage:patch_op()]}.

%%================================================================================
%% Callback declarations
%%================================================================================

-optional_callbacks([metaparams/1, create/1, pre_compile_mnode/3,
                     validate/3, validate_node/5,
                     meta_validate/2, meta_validate_node/4,
                     description_title/2, description/2, description_node/4,
                     read_patch/2, post_patch/5
                    ]).

%%--------------------------------------------------------------------------------
%% Compilation
%%--------------------------------------------------------------------------------

%% (Mandatory) Names of the metatypes provided by the callback module
-callback names(_Config) -> [lee:metatype()].

%% Type reflection of the metaparemeter fields
-callback metaparams(lee:metatype()) -> typerefl:type().

%% Create configuration of the callback module, that can be accessed by `lee_model:get'
-callback create(map()) -> [{lee:key(), term()}].

%% Called for each mnode before compilation.
%%
%% Note: never return `{changed, OriginalNode}', or compilation will
%% run into an infinite loop!
-callback pre_compile_mnode(lee:metatype(), lee:lee_module(), lee:mnode()) ->
    {changed, lee:mnode()} | same | {error, [string()]}.

%%--------------------------------------------------------------------------------
%% Post-compilation
%%--------------------------------------------------------------------------------

%% Called after compilation of the model for each #mnode{} to check them
-callback meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) ->
    lee_lib:check_result().

%% Called once, after all nodes _pass_ meta-validation to check the
%% whole model.
%%
%% It can also return a metaconfiguration patch (e.g. if some values
%% should be checked for faster lookup at later stages)
-callback meta_validate(lee:metatype(), lee:model()) ->
    metavalidate_result().

%%--------------------------------------------------------------------------------
%% Pre-patch
%%--------------------------------------------------------------------------------

%% Called by `lee:init_config' to create a patch. Patches from each
%% metaclass are sorted by `_Priority'
-callback read_patch(lee:metatype(), lee:model()) ->
    {ok, _Priority :: integer(), lee:patch()} | {error, [string()]}.

%% Called for each configuration data value after applying a patch to the pending config
-callback validate_node(lee:metatype(), lee:model(), _Staging :: lee:data(), lee:key(), #mnode{}) ->
    lee_lib:check_result().

%% Called once after validate_node callback is ran for each node
-callback validate(lee:metatype(), lee:model(), _Staging :: lee:data()) ->
    lee_lib:check_result().

%%--------------------------------------------------------------------------------
%% Post-patch
%%--------------------------------------------------------------------------------

%% Called after applying patch from staging config area to the live one for each patch operation.
-callback post_patch(lee:metatype(), lee:model(), lee:data(), #mnode{}, lee_storage:patch_op(term())) ->
    ok.

%%--------------------------------------------------------------------------------
%% Documentation
%%--------------------------------------------------------------------------------

%% Called by `lee_doc:make_docs' to get title of the section in the generated configuration
-callback description_title(lee:metatype(), lee:model()) -> string() | undefined.

%% (Mandatory if `description_title' is defined) Generate the body of the documentation section
-callback description(lee:metatype(), lee:model()) -> string() | lee_doc:doc().

%% Generate documentation for each #mnode{}
-callback description_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_doc:doc().

%%================================================================================
%% Macros
%%================================================================================

-define(is_implemented(M),
        erlang:function_exported(M, ?FUNCTION_NAME, callback_arity(?FUNCTION_NAME))).

-define(callbacktp(MT, MODULE, ARGS),
        ?tp(lee_mt_callback,
            #{ callback_name   => ?FUNCTION_NAME
             , metatype        => MT
             , callback_module => MODULE
             , is_implemented  => ?is_implemented(MODULE)
             , arguments       => ARGS
             })).

%%================================================================================
%% API funcions
%%================================================================================

-spec is_implemented(lee:model(), lee:metatype(), callback()) -> boolean().
is_implemented(Model, Metatype, Callback) ->
    Mod = get_module(Model, Metatype),
    erlang:function_exported(Mod, Callback, callback_arity(Callback)).

%%================================================================================
%% Callback wrappers
%%================================================================================

%%--------------------------------------------------------------------------------
%% Compilation
%%--------------------------------------------------------------------------------

-spec metaparams(lee:metatype(), lee:model()) -> typerefl:type().
metaparams(MT, Model) ->
    Module = get_module(Model, MT),
    case ?is_implemented(Module) of
        true ->
            Module:metaparams(MT);
        false ->
            typerefl:map()
    end.

-spec create(module(), map()) -> cooked_metatype().
create(M, Params) ->
    ?callbacktp(undefined, M, [Params]),
    Names = M:names(Params),
    Conf = case ?is_implemented(M) of
               true ->
                   M:create(Params);
               false ->
                   []
           end,
    {M, Names, Conf}.

-spec create(module()) -> cooked_metatype().
create(Module) ->
    create(Module, #{}).

-spec pre_compile_mnode(lee:metatype(), lee:lee_module(), #{lee:metatype() => module()}, lee:mnode()) ->
          {changed, lee:mnode()} | same | {error, [string()]}.
pre_compile_mnode(Metatype, MetaModule, MetaModules, MNode) ->
    Module = maps:get(Metatype, MetaModules),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, MetaModule, MNode);
        false -> same
    end.

%%--------------------------------------------------------------------------------
%% Post-compilation
%%--------------------------------------------------------------------------------

-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_lib:check_result().
meta_validate_node(Metatype, Model, Key, MNode) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, [Key]),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Key, MNode);
        false -> {[], []}
    end.

-spec meta_validate(lee:metatype(), lee:model()) -> metavalidate_result().
meta_validate(Metatype, Model) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, []),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model);
        false -> {[], [], []}
    end.

%%--------------------------------------------------------------------------------
%% Pre-patch
%%--------------------------------------------------------------------------------

-spec read_patch(lee:metatype(), lee:model()) ->
                {ok, _Priority :: integer(), lee:patch()}
              | {error, [string()]}.
read_patch(Metatype, Model) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, []),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model);
        false -> {ok, 0, []}
    end.

-spec validate(lee:metatype(), lee:model(), lee:data()) -> lee_lib:check_result().
validate(Metatype, Model, Data) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, []),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Data);
        false -> {[], []}
    end.

-spec validate_node(lee:metatype(), lee:model(), lee:data(), lee:key(), #mnode{}) -> lee_lib:check_result().
validate_node(Metatype, Model, Data, Key, MNode) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, [Key]),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Data, Key, MNode);
        false -> {[], []}
    end.

%%--------------------------------------------------------------------------------
%% Post-patch
%%--------------------------------------------------------------------------------

-spec post_patch(lee:metatype(), lee:model(), lee:data(), #mnode{}, lee_storage:patch_op(term())) -> ok.
post_patch(Metatype, Model, Data, MNode, PatchOp) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, [PatchOp]),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Data, MNode, PatchOp);
        false -> ok
    end.

%%--------------------------------------------------------------------------------
%% Documentation
%%--------------------------------------------------------------------------------

-spec description(lee:model(), lee:metatype()) -> lee_doc:doc().
description(Model, Metatype) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, []),
    Module:?FUNCTION_NAME(Metatype, Model).

-spec description_title(lee:metatype(), lee:model()) -> string() | undefined.
description_title(Metatype, Model) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, []),
    case ?is_implemented(Module) of
        true  -> Module:description_title(Metatype, Model);
        false -> undefined
    end.

-spec description_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) -> lee_doc:doc().
description_node(Metatype, Model, Key, MNode) ->
    Module = get_module(Model, Metatype),
    ?callbacktp(Metatype, Module, [Key]),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Key, MNode);
        false -> []
    end.

%%================================================================================
%% Internal functions
%%================================================================================

-spec callback_arity(callback()) -> arity().
callback_arity(create) ->
    1;
callback_arity(metaparams) ->
    1;
callback_arity(pre_compile_mnode) ->
    3;
callback_arity(meta_validate) ->
    2;
callback_arity(meta_validate_node) ->
    4;
callback_arity(validate) ->
    3;
callback_arity(validate_node) ->
    5;
callback_arity(description_title) ->
    2;
callback_arity(description) ->
    2;
callback_arity(description_node) ->
    4;
callback_arity(read_patch) ->
    2;
callback_arity(post_patch) ->
    5;
callback_arity(CB) ->
    error({unknown_callback, CB}).

-spec get_module(lee:model(), lee:metatype()) -> module().
get_module(#model{metamodules = Modules}, Metatype) ->
    maps:get(Metatype, Modules).
