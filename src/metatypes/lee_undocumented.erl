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
-module(lee_undocumented).

-behavior(lee_metatype).

%% API:
-export([create/1]).

%% behavior callbacks:
-export([create/1, names/1, description/1]).

-include("../framework/lee_internal.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

create(_) ->
    [].

names(_Conf) ->
    [undocumented].

description(undocumented) ->
    "<para>Undocumented values</para>".

%%================================================================================
%% Internal functions
%%================================================================================
