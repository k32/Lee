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
-module(lee_value).

-behavior(lee_metatype).

%% API:
-export([]).

%% behavior callbacks:
-export([names/1, description/2, validate_node/5, meta_validate_node/4,
         description_title/2, description_node/4]).

-include("../framework/lee_internal.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

names(_) ->
    [value].

description(value, _Model) ->
    [{para, ["This section lists all configurable values."]}].

%% Validate nodes of `value' metatype
-spec validate_node(lee:metatype(), lee:model(), lee:data(), lee:key(), #mnode{}) ->
                           lee_lib:check_result().
validate_node(value, Model, Data, Key, #mnode{metaparams = Attrs}) ->
    Type = ?m_attr(value, type, Attrs),
    HasDefault = case Attrs of
                     #{default     := _} -> true;
                     #{default_ref := _} -> true;
                     _                   -> false
                 end,
    case lee_storage:get(Key, Data) of
        {ok, Term} ->
            Result = case typerefl:typecheck(Type, Term) of
                         ok ->
                             {[], []};
                         {error, Err} ->
                             {[lee_lib:format_typerefl_error(Err)], []}
                     end,
            lee_lib:inject_error_location(Key, Result);
        undefined when HasDefault ->
            {[], []};
        undefined ->
            Err = lee_lib:format("~p: Mandatory value is missing in the config", [Key]),
            {[Err] , []}
    end.

-spec meta_validate_node(lee:metatype(), lee:model(), lee:key(), #mnode{}) ->
                            lee_lib:check_result().
meta_validate_node(value, Model, Key, #mnode{metaparams = Attrs}) ->
    Results = lee_lib:compose_checks([ lee_doc:check_docstrings(Attrs)
                                     , check_type_and_default(Model, Attrs)
                                     ]),
    lee_lib:inject_error_location(Key, Results).

description_title(value, _) ->
    "Values".

description_node(value, Model, Key, MNode = #mnode{metaparams = Attrs}) ->
    Type = ?m_attr(value, type, Attrs),
    Default =
        case Attrs of
            #{default := DefVal} ->
                DefStr = typerefl:pretty_print_value(Type, DefVal),
                [lee_doc:simplesect( "Default value: "
                                   , [lee_doc:erlang_listing(DefStr)]
                                   )];
            #{default_ref := Ref} ->
                [lee_doc:simplesect( "Default value: "
                                   , [{para, ["See ", lee_doc:xref_key(Ref)]}]
                                   )];
            _ ->
                []
        end,
    Description = lee_doc:get_description(Model, Key, MNode),
    Oneliner    = lee_doc:get_oneliner(Model, Key, MNode),
    Id = lee_lib:format("~p", [Key]),
    { section, [{id, Id}]
    , [ {title, [Id]}
      , {para, [Oneliner]}
      , lee_doc:simplesect("Type:", [lee_doc:erlang_listing(typerefl:print(Type))])
      ] ++ Default ++ Description
    }.

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
                            {"Invalid `default_ref' metatype", []}
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
