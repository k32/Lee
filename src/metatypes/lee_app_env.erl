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
-module(lee_app_env).

%% behavior callbacks:
-export([create/1, names/1, meta_validate_node/4, post_patch/5]).

-include_lib("lee/src/framework/lee_internal.hrl").
-include_lib("typerefl/include/types.hrl").

-define(metatype, app_env).

%%================================================================================
%% behavior callbacks
%%================================================================================

create(_) ->
    [].

names(_) ->
    [?metatype].

meta_validate_node(?metatype, _Model, Key, MNode) ->
    %% TODO: Check that also a value. Also check transform type
    lee_lib:inject_error_location(
      Key,
      lee_lib:validate_meta_attr( app_env
                                , {atom(), atom()}
                                , MNode
                                )).

post_patch(?metatype, Model, Data, #mnode{metaparams = Attrs}, PatchOp) ->
    {App, Env} = ?m_attr(?metatype, app_env, Attrs),
    Transform = ?m_attr(?metatype, app_env_transform, Attrs, fun(A) -> A end),
    case PatchOp of
        {set, Key, _} -> ok;
        {rm, Key} -> ok
    end,
    Val = lee:get(Model, Data, Key),
    application:set_env(App, Env, Transform(Val)).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
