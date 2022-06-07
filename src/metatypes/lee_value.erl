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
-module(lee_value).

-behavior(lee_metatype).

%% API:
-export([create/1]).

%% behavior callbacks:
-export([names/1, validate_node/5, meta_validate_node/4, doc_chapter_title/2, doc_gen/4]).

-include("../framework/lee_internal.hrl").

%%================================================================================
%% API funcions
%%================================================================================

create(_) ->
    #{}.

%%================================================================================
%% behavior callbacks
%%================================================================================

names(_) ->
    [value].

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
                         ok           -> {[], []};
                         {error, Err} -> {[Err], []}
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

doc_chapter_title(value, _) ->
    "Values".

doc_gen(value, Model, Key, #mnode{metaparams = Attrs}) ->
    Oneliner = ?m_attr(value, oneliner, Attrs, ""),
    Type = ?m_attr(value, type, Attrs),
    Default =
        case Attrs of
            #{default := DefVal} ->
                DefStr = io_lib:format("~p", [DefVal]),
                [lee_doc:simplesect( "Default value:"
                                   , [lee_doc:erlang_listing(DefStr)]
                                   )];
            _ ->
                []
        end,
    Description =
        case Attrs of
            #{doc := DocString0} ->
                DocString = ?m_valid(value, lee_doc:docbook(DocString0)),
                [lee_doc:simplesect("Description:", DocString)];
            _ ->
                []
        end,
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
                    Str = lee_lib:format("Mistyped default value: ~s", [Err]),
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
            {["Missing `type' metaparameter"], []}
    end.
