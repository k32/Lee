%%--------------------------------------------------------------------
%% Copyright (c) 2022-2024 k32 All Rights Reserved.
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
-module(lee_logger).

%% behavior callbacks:
-export([names/1, metaparams/1, meta_validate_node/4, post_patch/5]).

-include_lib("lee.hrl").
-include_lib("typerefl/include/types.hrl").

-type level() :: debug | info | notice | warning | error | critical | alert.

-reflect_type([level/0]).

%%================================================================================
%% behavior callbacks
%%================================================================================

%% @private
names(_) ->
    [logger_level].

%% @private
metaparams(logger_level) ->
    [{optional, logger_handler, atom()}].

%% @private
meta_validate_node(logger_level, _Model, _Key, MNode) ->
    ExpectedType = level(),
    case MNode of
        #mnode{metaparams = #{type := T}} when T =:= ExpectedType ->
            {[], []};
        _ ->
            {["Type of logger level must be lee_logger:level()"], []}
    end.

%% @private
post_patch(logger_level, Model, Data, #mnode{metaparams = Attrs}, PatchOp) ->
    Val = lee:get(Model, Data, lee_lib:patch_key(PatchOp)),
    case Attrs of
        #{logger_handler := Handler} ->
            logger:update_handler_config(Handler, level, Val);
        _ ->
            logger:set_primary_config(level, Val)
    end.

%%================================================================================
%% Internal functions
%%================================================================================
