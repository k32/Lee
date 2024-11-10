%%--------------------------------------------------------------------
%% Copyright (c) 2022-2024 k32. All Rights Reserved.
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
         metaparams/2,
         validate/3, validate_node/5,
         pre_compile/3, meta_validate/2, meta_validate_node/4,
         description/3, doc_reference/3,
         read_patch/2, post_patch/5]).

%% API:
-export([is_implemented/3]).

-export_type([cooked_metatype/0, metavalidate_result/0]).

-include("lee_internal.hrl").
-include_lib("snabbkaffe/include/trace.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type cooked_metatype() :: {module(), [atom()], [{lee:key(), term()}]}.

-type callback() :: create | metaparams
                  | pre_compile | meta_validate | meta_validate_node
                  | validate | validate_node
                  | description | doc_reference
                  | read_patch | post_patch.

-type metavalidate_result() :: {_Err :: [string()], _Warn :: [string()], [lee_storage:patch_op()]}.

%%================================================================================
%% Callback declarations
%%================================================================================

-optional_callbacks([metaparams/1, create/1,
                     validate/3, validate_node/5,
                     pre_compile/2, meta_validate/2, meta_validate_node/4,
                     description/3, doc_reference/3,
                     read_patch/2, post_patch/5
                    ]).

%%--------------------------------------------------------------------------------
%% Compilation
%%--------------------------------------------------------------------------------

%% (Mandatory) Names of the metatypes provided by the callback module
-callback names(_Config) -> [lee:metatype()].

%% Type reflection of the metaparemeter fields
-callback metaparams(lee:metatype()) -> [{mandatory | optional | warn_if_missing, typerefl:type(), typerefl:type()}].

%% Create configuration of the callback module, that can be accessed by `lee_model:get'
-callback create(map()) -> [{lee:key(), term()}].

%% Called during compilation of the module into the model. One can perform
%% simple transformations of the metaparameters in this callback.
%%
%% This callback MUST NOT validate any data or parameters: all errors
%% will be ignored. Validation should be left to `meta_validate_node'
-callback pre_compile(lee:metatype(), Metaparams) -> Metaparams when
      Metaparams :: map().

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

%% Generate internal representation of the documentation chapter
%% describing the metatype.
-callback description(lee:metatype(), lee:model(), lee_doc:options()) -> [lee_doc:doclet()].

-callback doc_reference(lee:metatype(), lee:model(), lee:model_key()) -> [lee_doc:doclet()].

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
            []
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

%%--------------------------------------------------------------------------------
%% Post-compilation
%%--------------------------------------------------------------------------------

-spec pre_compile(lee:metatype(), module(), map()) -> map().
pre_compile(Metatype, Module,  Metaparams) ->
    ?callbacktp(Metatype, Module, [Metaparams]),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Metaparams);
        false -> Metaparams
    end.

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

-spec description(lee:metatype(), lee:model(), lee_doc:options()) -> [file:filename_all()].
description(Metatype, Model, Options) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> Module:?FUNCTION_NAME(Metatype, Model, Options);
        false -> []
    end.

-spec doc_reference(lee:metatype(), lee:model(), lee:model_key()) -> {ok, iolist()} | undefined.
doc_reference(Metatype, Model, Key) ->
    Module = get_module(Model, Metatype),
    case ?is_implemented(Module) of
        true  -> {ok, Module:?FUNCTION_NAME(Metatype, Model, Key)};
        false -> undefined
    end.

%%================================================================================
%% Internal functions
%%================================================================================

-spec callback_arity(callback()) -> arity().
callback_arity(create) ->
    1;
callback_arity(metaparams) ->
    1;
callback_arity(pre_compile) ->
    2;
callback_arity(meta_validate) ->
    2;
callback_arity(meta_validate_node) ->
    4;
callback_arity(validate) ->
    3;
callback_arity(validate_node) ->
    5;
callback_arity(description) ->
    3;
callback_arity(doc_reference) ->
    3;
callback_arity(read_patch) ->
    2;
callback_arity(post_patch) ->
    5;
callback_arity(CB) ->
    error({unknown_callback, CB}).

-spec get_module(lee:model(), lee:metatype()) -> module().
get_module(#model{metamodules = Modules}, Metatype) ->
    maps:get(Metatype, Modules).
