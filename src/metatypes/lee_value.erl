%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 k32 All Rights Reserved.
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
-export([]).

%% behavior callbacks:
-export([names/1, metaparams/1, pre_compile/2, meta_validate_node/4, validate_node/5,
         description/3, doc_refer_key/3]).

-include("lee.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

names(_) ->
    [value].

metaparams(value) ->
    [ {mandatory, type, typerefl:term()}
    , {optional, default, typerefl:term()}
    , {optional, default_ref, lee:model_key()}
    ] ++ lee_doc:documented().

%% description(value, _Model) ->
%%     [{para, ["This section lists all configurable values."]}].

%% Validate nodes of `value' metatype
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

pre_compile(value, MPs = #{default_str := Str, type := Type}) ->
    {ok, Default} = typerefl:from_string(Type, Str),
    MPs#{default => Default};
pre_compile(_, MPs) ->
    MPs.

-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) ->
                            lee_lib:check_result().
meta_validate_node(value, Model, _Key, #mnode{metaparams = Attrs}) ->
    check_type_and_default(Model, Attrs).

description(value = MT, Model, _Options) ->
    [{[], Global} | Rest] = lists:sort(maps:to_list(lee_model:fold(fun mk_doc_tree/4, #{}, {false, []}, Model))),
    Content = mk_doc(Model, [], Global) ++
        [document_map(Model, Parent, Children)
         || {Parent, Children} <- Rest],
    lee_doc:chapter(MT, "All configurable values", Content).

doc_refer_key(value, _Model, Key) ->
    [{xref, [{linkend, lee_doc:format_key(value, Key)}], []}].

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

document_value(Model, ParentKey, Key) ->
    #mnode{metatypes = MTs, metaparams = Attrs} = lee_model:get(Key, Model),
    Description = lee_doc:get_description(Model, Key),
    Oneliner = lee_doc:get_oneliner(Model, Key),
    Type = ?m_attr(value, type, Attrs),
    KeyStr = lee_lib:format("~p", [Key -- ParentKey]),
    Title = case Oneliner of
                [] -> KeyStr;
                [OL] -> OL
            end,
    Default =
        case Attrs of
            #{default_str := DefStr} ->
                [lee_doc:simplesect( "Default value: "
                                   , [lee_doc:erlang_listing(DefStr)]
                                   )];
            #{default := DefVal} ->
                DefStr = typerefl:pretty_print_value(Type, DefVal),
                [lee_doc:simplesect( "Default value: "
                                   , [lee_doc:erlang_listing(DefStr)]
                                   )];
            #{default_ref := Ref} ->
                [lee_doc:simplesect( "Default value: "
                                   , ["See ", lee_doc:xref_key(value, Ref)]
                                   )];
            _ ->
                []
        end,
    SeeAlso = case lists:flatmap(fun(value) -> [];
                                    (MT)    -> lee_metatype:doc_refer_key(MT, Model, Key)
                                 end, MTs)
              of
                  []        -> [];
                  OtherRefs -> [lee_doc:simplesect("See also: ", lists:join(", ", OtherRefs))]
              end,
    {section, [{'xml:id', lee_doc:format_key(value, Key)}],
     [ {title, [Title]}
     , lee_doc:simplesect("Key: ", [lee_doc:erlang_listing(KeyStr)])
     , lee_doc:simplesect("Type: ", [lee_doc:erlang_listing(typerefl:print(Type))])
     ] ++ Default ++ Description ++ SeeAlso}.

mk_doc(Model, Parent, Keys) ->
    [document_value(Model, Parent ++ [{}], Key) || Key <- Keys].

document_map(Model, Key, Children) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    Description = lee_doc:get_description(Model, Key),
    Oneliner = lee_doc:get_oneliner(Model, Key),
    Id = lee_doc:format_key(value, Key),
    KeyStr = lee_lib:format("~p", [Key]),
    Title = case Oneliner of
                [] -> KeyStr;
                [OL] -> OL
            end,
    {section, [{'xml:id', Id}],
     [ {title, [Title]}
     , lee_doc:simplesect("Key: ", [lee_doc:erlang_listing(KeyStr)])
     , {para, [{emphasis, ["Key elements:"]}]}
     | document_map_key_elems(Key, Attrs)
     ] ++ Description ++ mk_doc(Model, Key, Children)}.

document_map_key_elems(Key, Attrs) ->
    case ?m_attr(map, key_elements, Attrs, []) of
        [] ->
            [];
        KeyAttrs ->
            [{orderedlist,
              [{listitem,
                [{para,
                  [ {link, [{linkend, lee_doc:format_key(value, Key ++ [?children|I])}],
                     [lee_lib:format("~p", [I])]}
                  ]}]}
               || I <- KeyAttrs]}]
    end.
