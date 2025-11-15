%%--------------------------------------------------------------------
%% Copyright (c) 2022-2025 k32 All Rights Reserved.
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
-module(lee_value).

-behavior(lee_metatype).

%% API:
-export([doc_type/2, doc_default/2]).

%% behavior callbacks:
-export([names/1, metaparams/1, pre_compile/2, meta_validate_node/4, validate_node/5,
         description/3]).

-include("lee.hrl").

%%================================================================================
%% API
%%================================================================================

%% @doc Create a doclet describing default value (if present). Return
%% an empty list if default value is not specified.
-spec doc_default(lee:model(), lee:model_key()) -> [lee_doc:doclet()].
doc_default(Model, Key) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    case Attrs of
        #{default_str := DefStr} ->
            [#doclet{mt = value, tag = default, data = DefStr}];
        #{default := DefVal} ->
            Type = ?m_attr(value, type, Attrs),
            DefStr = typerefl:pretty_print_value(Type, DefVal),
            [#doclet{mt = value, tag = default, data = DefStr}];
        #{default_ref := Ref} ->
            DocRef = #doc_xref{mt = value, key = Ref},
            [#doclet{mt = value, tag = default, data = DocRef}];
        _ ->
            []
    end.

%% @doc Create a doclet describing type of the value.
-spec doc_type(lee:model(), lee:model_key()) -> [lee_doc:doclet()].
doc_type(Model, Key) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    [#doclet{mt = value, tag = type, data = ?m_attr(value, type, Attrs)}].

%%================================================================================
%% behavior callbacks
%%================================================================================

%% @private
names(_) ->
    [value].

%% @private
metaparams(value) ->
    [ {mandatory, type, typerefl:term()}
    , {optional, default, typerefl:term()}
    , {optional, default_ref, lee:model_key()}
    ] ++ lee_doc:documented().

%% @private Validate nodes of `value' metatype
-spec validate_node(lee:metatype(), lee:model(), lee:data(), lee:key(), #mnode{}) ->
                           lee_lib:check_result().
validate_node(value, _Model, Data, Key, #mnode{metaparams = Attrs}) ->
    Type = ?m_attr(value, type, Attrs),
    HasDefault = case Attrs of
                     #{default     := _} -> true;
                     #{default_ref := _} -> true;
                     _                   -> false
                 end,
    case lee_storage:get(Key, Data) of
        {ok, Term} ->
            case typerefl:typecheck(Type, Term) of
                ok ->
                    {[], []};
                {error, Err} ->
                    {[lee_lib:format_typerefl_error(Err)], []}
            end;
        undefined when HasDefault ->
            {[], []};
        undefined ->
            {["Mandatory value is missing in the config"] , []}
    end.

%% @private
pre_compile(value, MPs = #{default_str := Str, type := Type}) ->
    {ok, DefaultFromStr} = typerefl:from_string(Type, Str),
    case maps:is_key(default, MPs) of
        false ->
            MPs#{default => DefaultFromStr};
        true ->
            %% `default' takes precedence over `default_str'. This is
            %% used in situations when `default' is computed
            %% dynamically (e.g. it's a local path, local
            %% machine-dependent variable, etc.). In this case
            %% `default_str' is used only for documentation purposes.
            MPs
    end;
pre_compile(_, MPs) ->
    MPs.

%% @private
-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) ->
                            lee_lib:check_result().
meta_validate_node(value, Model, _Key, #mnode{metaparams = Attrs}) ->
    check_type_and_default(Model, Attrs).

%% @private
description(value, Model, Options) ->
    Scopes = lee_model:fold(
               fun mk_doc_tree/4,
               #{},
               {false, []},
               Model),
    case lists:sort(maps:to_list(Scopes)) of
        [{[], Global} | Maps] ->
            Values = mk_doc(Options, Model, [], Global);
        Maps ->
            Values = []
    end,
    Values ++ [document_map(Options, Model, Parent, Children)
               || {Parent, Children} <- Maps].

%%================================================================================
%% Internal functions
%%================================================================================

check_type_and_default(Model, Attrs) ->
    case Attrs of
        #{type := Type, default := Default} ->
            case typerefl:typecheck(Type, Default) of
                ok ->
                    {[], []};
                {error, Err} ->
                    Str = "Mistyped default value. " ++
                        lee_lib:format_typerefl_error(Err),
                    {[Str], []}
            end;
        #{type := Type, default_ref := DefaultRef} ->
            try lee_model:get(DefaultRef, Model) of
                #mnode{metatypes = MTs, metaparams = RefAttrs} ->
                    case lists:member(value, MTs) of
                        true ->
                            case RefAttrs of
                                #{type := Type} -> %% TODO: Better type comparison?
                                    {[], []};
                                _ ->
                                    {["Type of the `default_ref' is different"], []}
                            end;
                        false ->
                            {["Invalid `default_ref' metatype"], []}
                    end
            catch
                _:_ -> {["Invalid `default_ref' reference key"], []}
            end;
        #{type := _Type} ->
            %% TODO: typerefl:is_type?
            {[], []};
        _ ->
            {["Missing mandatory `type' metaparameter"], []}
    end.

mk_doc_tree(Key, #mnode{metatypes = MTs}, Acc, {ParentUndocumented, Parent}) ->
    case {lists:member(map, MTs), lists:member(value, MTs), ParentUndocumented orelse lists:member(undocumented, MTs)} of
        {true, false, Undocumented} ->
            {Acc, {Undocumented, Key}};
        {false, true, false} ->
            { maps:update_with(Parent, fun(Keys) -> [Key|Keys] end, [Key], Acc)
            , {ParentUndocumented, Parent}
            };
        {false, _, _} ->
            {Acc, {ParentUndocumented, Parent}}
    end.

document_value(Options, Model, ParentKey, Key) ->
    #mnode{metatypes = MTs} = lee_model:get(Key, Model),
    SeeAlso = lists:flatmap(fun(value) -> [];
                               (MT)    -> lee_metatype:doc_refer(MT, Model, Options, Key)
                            end,
                            MTs),
    Data = [#doclet{mt = value, tag = value_key, data = Key -- ParentKey}] ++
           lee_doc:get_oneliner(value, Model, Key) ++
           lee_doc:get_description(Model, Key) ++
           doc_type(Model, Key) ++
           doc_default(Model, Key) ++
           SeeAlso,
    #doclet{mt = value, tag = value, key = Key, data = Data}.

mk_doc(Options, Model, Parent, Keys) ->
    [document_value(Options, Model, Parent ++ [{}], Key) || Key <- Keys].

document_map(Options, Model, Key, Children) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    Data = [#doclet{mt = value, tag = value_key, data = Key}] ++
           lee_doc:get_oneliner(value, Model, Key) ++
           lee_doc:get_description(Model, Key) ++
           document_map_key_elems(Key, Attrs) ++
           [#doclet{mt = map, tag = map, data = mk_doc(Options, Model, Key, Children)}],
    #doclet{mt = value, tag = value, key = Key, data = Data}.

document_map_key_elems(Key, Attrs) ->
    case ?m_attr(map, key_elements, Attrs, []) of
        [] ->
            [];
        KeyAttrs ->
            Data = [#doc_xref{mt = value, key = Key ++ [?children|I]} || I <- KeyAttrs],
            [#doclet{mt = map, tag = key_elements, data = Data}]
    end.
