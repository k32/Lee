%% @doc Utilities for extracting documentation from the model
-module(lee_doc).

-export([make_docs/2, get_description/2, get_oneliner/3]).
-export([texinfo/3, texi_key/1]).

-export([documented/0]).
-export_type([doclet/0]).

-include("lee_internal.hrl").
-include_lib("typerefl/include/types.hrl").

-type doclet() :: #doclet{}.

-type docstring() :: iodata().
-reflect_type([docstring/0]).

-type formatter() :: fun((options(), io:device(), doclet()) -> _).

-type options() ::
        #{ output_dir  := file:filename()
         , extension   => string()
         , formatter   := formatter()
         , metatypes   := [lee:metatype()] | all
         , _           => _
         }.

-export_type([options/0, formatter/0]).

%%================================================================================
%% API
%%================================================================================

%% @doc Transform Lee's intermediate documentation format to TexInfo.
texinfo(Options, FD, L) when is_list(L) ->
    [texinfo(Options, FD, I) || I <- L],
    ok;
texinfo(Options, FD, Doclet) ->
    P = fun(L) -> io:put_chars(FD, L) end,
    case Doclet of
        %% Value:
        #doclet{mt = value, tag = value, key = Key, data = Data} ->
            P(["@node ", texi_key([value | Key]), $\n]),
            texinfo(Options, FD, Data);
        #doclet{mt = value, tag = value_key, data = RelKey} ->
            P(["@section ", texi_key(RelKey), $\n]);
        #doclet{tag = see_also, data = #doc_xref{mt = MT, key = Key}} ->
            P(["\n@xref{", texi_key([MT | Key]), "}\n"]);
        #doclet{mt = MT, tag = oneliner, data = Oneliner} ->
            io:format(FD, "@cindex ~s (~p)\n~s\n\n", [Oneliner, MT, Oneliner]);
        #doclet{mt = value, tag = doc, data = Doc} ->
            P([Doc, "\n"]);
        #doclet{mt = value, tag = default, data = #doc_xref{mt = MT, key = Key}} ->
            P([ "@b{Default value}: "
              , "@xref{", texi_key([MT| Key]), "}\n\n"
              ]);
        #doclet{mt = value, tag = default, data = Default} ->
            P([ "@b{Default value}\n@example erlang\n@verbatim\n"
              , Default
              , "\n@end verbatim\n@end example\n"
              ]);
        #doclet{mt = value, tag = type, data = Type} ->
            P([ "@b{Type}\n@example erlang\n@verbatim\n"
              , typerefl:print(Type)
              , "\n@end verbatim\n@end example\n"
              ]);
        %% Map:
        #doclet{mt = map, tag = map, data = Data} ->
            P(["@heading Children\n@lowersections\n"]),
            texinfo(Options, FD, Data),
            P("@raisesections\n");
        #doclet{mt = map, tag = key_elements, data = KeyElems} ->
            %% Produce the list of the map key elements:
            P([ "@b{Key elements:}\n@itemize"
              , [["\n@item @ref{", texi_key([MT | Key]), "}"] || #doc_xref{mt = MT, key = Key} <- KeyElems]
              , "\n@end itemize\n"
              ]);
        %% CLI scopes:
        #doclet{tag = cli_global_scope, data = CliScope} ->
            texinfo(Options, FD, CliScope);
        #doclet{mt = cli_action, tag = cli_action, key = Key, data = [Header | CliScope]} ->
            #doclet{mt = cli_action, tag = cli_action_name, data = Action} = Header,
            P([ "@node CLI Action @@", Action, $\n
              , "@anchor{", texi_key([cli_action | Key]), "}\n"
              , "@section @command{@@", Action, "}, CLI Action\n"
              , "@findex @@", Action, $\n
              ]),
            texinfo(Options, FD, CliScope);
        %% CLI named parameters:
        #doclet{tag = cli_named_arguments, data = Data} ->
            P("@ftable @option\n"),
            texinfo(Options, FD, Data),
            P("@end ftable\n");
        #doclet{mt = cli_param, tag = cli_param, key = Key, data = Data} ->
            [#doclet{tag = cli_param_name, data = Names} | Rest] = Data,
            P(["\n@anchor{", texi_key([cli_param | Key]), "}"]),
            P("@item "),
            [P(["@option{", I, "} "]) || I <- Names],
            P("\n"),
            texinfo(Options, FD, Rest);
        %% CLI positional arguments:
        #doclet{tag = cli_positionals, data = Data} ->
            P("@heading Positional arguments\n@enumerate\n"),
            texinfo(Options, FD, Data),
            P("@end enumerate\n");
        #doclet{tag = cli_positional, data = Data, key = Key} ->
            P(["@item\n@anchor{", texi_key([cli_positional | Key]), "}"]),
            texinfo(Options, FD, Data);
        #doclet{mt = cli_positional, tag = rest, data = Data, key = Key} ->
            P([ "@anchor{", texi_key([cli_positional | Key]), "}\n"
              , "@heading The rest of positional arguments\n"
              ]),
            texinfo(Options, FD, Data);
        %% OS Environment variables
        #doclet{mt = os_env, tag = container, data = Data} ->
            P(["@vtable @var\n"]),
            texinfo(Options, FD, Data),
            P(["@end vtable\n"]);
        #doclet{mt = os_env, tag = os_env, data = Data} ->
            texinfo(Options, FD, Data);
        #doclet{mt = os_env, tag = var_name, key = Key, data = Name} ->
            P([ "@anchor{", texi_key([os_env | Key]), "}\n"
              , "@item @env{", Name, "}\n"
              ])
    end.

-spec make_docs(lee:model(), options()) -> [file:filename_all()].
make_docs(Model, Options = #{formatter := F, metatypes := Metatypes}) ->
    Dir = maps:get(output_dir, Options, "lee_doc"),
    Extension = maps:get(extension, Options, ""),
    MTs = case Metatypes of
              all ->
                  lee_model:all_metatypes(Model);
              L when is_list(L) ->
                  L
          end,
    ok = filelib:ensure_path(Dir),
    lists:flatmap(fun(MT) ->
                          Filename = filename:join(Dir, atom_to_list(MT) ++ Extension),
                          {ok, FD} = file:open(Filename, [write]),
                          try
                              F(Options, FD, lee_metatype:description(MT, Model, Options)),
                              [Filename]
                          after
                              ok = file:close(FD)
                          end
                  end,
                  MTs).

-spec get_description(lee:model(), lee:model_key()) -> [doclet()].
get_description(Model, Key) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    case Attrs of
        #{doc := Doc} -> [#doclet{mt = value, tag = doc, data = Doc}];
        #{} -> []
    end.

%% @doc Return a list with single element containing the oneliner, or
%% empty list
-spec get_oneliner(atom(), lee:model(), lee:model_key()) -> [doclet()].
get_oneliner(MT, Model, Key) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    case Attrs of
        #{oneliner := Doc} -> [#doclet{mt = MT, tag = oneliner, data = Doc}];
        #{} -> []
    end.

-spec texi_key(lee:model_key()) -> string().
texi_key([{}]) ->
    "_";
texi_key([A]) when is_atom(A) ->
    atom_to_binary(A);
texi_key([A|Rest]) ->
    [texi_key([A]), "/" | texi_key(Rest)].

%%================================================================================
%% Internal exports
%%================================================================================

-spec documented() -> list().
documented() ->
    [{warn_if_missing, oneliner, string()}, {warn_if_missing, doc, docstring()}].

%%================================================================================
%% Internal functions
%%================================================================================
