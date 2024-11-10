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
-module(lee_config_file).

-behavior(lee_metatype).

%% API:
-export([read_to/3, read/2]).

%% behavior callbacks:
-export([create/1, names/1, read_patch/2]).

-define(filename_key(TAG), [?MODULE, TAG, filename]).
-define(prio_key(TAG), [?MODULE, TAG, priority]).

%%================================================================================
%% API
%%================================================================================

-spec read(lee:model(), file:filename()) -> {ok, lee:patch()} | {error, [string()]}.
read(Model, Filename) ->
    case file:consult(Filename) of
        {ok, [Term]} ->
            {ok, make_patch([], Model, Term, [])};
        {error, enoent} ->
            {ok, []};
        {error, Err} ->
            Msg = lee_lib:format("Can't read file ~p: ~p", [Filename, Err]),
            {error, [Msg]}
    end.

-spec read_to(lee:model(), lee:data(), file:filename()) ->
          lee:patch_result().
read_to(Model, Data, Filename) ->
    {ok, Patch} = read(Model, Filename),
    lee:patch(Model, Data, Patch).

%%================================================================================
%% behavior callbacks
%%================================================================================

%% @private
create(#{file := File, tag := Tag} = Attrs) ->
    Prio = maps:get(priority, Attrs, 0),
    [ {?filename_key(Tag), File}
    , {?prio_key(Tag), Prio}
    ].

%% @private
names(#{tag := Tag}) ->
    [Tag].

%% @private
read_patch(Tag, Model) ->
    {ok, Filename} = lee_model:get_meta(?filename_key(Tag), Model),
    {ok, Prio} = lee_model:get_meta(?prio_key(Tag), Model),
    case read(Model, Filename) of
        {ok, Patch} ->
            {ok, Prio, Patch};
        {error, Err} ->
            {error, [Err]}
    end.

%%================================================================================
%% Internal functions
%%================================================================================

make_patch(Path, Model, Term, Acc0) when is_map(Term) ->
    Fun = fun(K, V, Acc) ->
                  make_patch(Path ++ [K], Model, V, Acc)
          end,
    maps:fold(Fun, Acc0, Term);
make_patch(Path, _Model, Term, Acc) ->
    [{set, Path, Term}|Acc].
