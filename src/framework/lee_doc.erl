%% @doc Utilities for extracting documentation from the model
-module(lee_doc).

-export([make_docs/2]).

-export([ simplesect/2
        , erlang_listing/1
        , xref_key/1
        , refer_value/4
        , docbook/1
        , check_docstrings/1
        , validate_doc_root/4
        ]).

-include("lee_internal.hrl").
-include_lib("typerefl/include/types.hrl").

-type doc() :: term().

-type doc_options() ::
        #{ metatypes    := [lee:metatype() | {lee:metatype(), term()}]
         , output_dir   => file:filename()
         , run_pandoc   => boolean()
         }.

-export_type([doc/0, doc_options/0]).

%% @doc Represent text as Erlang code
-spec erlang_listing(iolist()) -> doc().
erlang_listing(Str) ->
    { programlisting, [{language, "erlang"}]
    , [Str]
    }.

%% @doc Make a simple subsection
-spec simplesect(string(), iolist() | [doc()]) -> doc().
simplesect(Title, Doc0) ->
    Doc = case io_lib:deep_char_list(Doc0) of
              true ->
                  [{para, [lists:flatten(Doc0)]}];
              false ->
                  Doc0
          end,
    {para, [{emphasis, [Title]} | Doc]}.

%% @doc Generate a link to the description of a value
-spec xref_key(lee:key()) -> doc().
xref_key(Key) ->
    Node = lee_lib:format("~p", [Key]),
    {xref, [{linkend, Node}], []}.

%% @doc Generate a section that contains short description of a value
%% and a link to the full description
-spec refer_value(lee:model_key(), lee:metatype(), string(), #mnode{}) ->
                         doc().
refer_value(Key, Metatype, Title, MNode) ->
    SectionId = lee_lib:format("~p", [{Metatype, Title}]),
    #mnode{metaparams = Attrs} = MNode,
    Oneliner = ?m_attr(value, oneliner, Attrs, ""),
    {section, [{id, SectionId}]
    , [ {title, [Title]}
      , {para, [Oneliner ++ ", see: ", lee_doc:xref_key(Key)]}
      ]
    }.

%% @doc Parse string as list of XML nodes. Example:
%% ```
%% lee_doc:docbook("<para>Some text</para>
%%                  <para>More text</para>")'''
-spec docbook(string()) -> [doc()].
docbook([]) ->
    [];
docbook(String) ->
    {Doc, Rest} = xmerl_scan:string(String, [{document, false}]),
    [Doc | docbook(Rest)].

%% @private Meta-validation of docstrings
-spec check_docstrings(lee:parameters()) -> lee_lib:check_result().
check_docstrings(Attrs) ->
    CheckOneliner = lee_lib:validate_optional_meta_attr( oneliner
                                                       , printable_unicode_list()
                                                       , Attrs
                                                       , true
                                                       ),
    CheckDoc = case Attrs of
                   #{doc := Doc} ->
                       try docbook(Doc) of
                           _ -> {[], []}
                       catch
                           _:_ -> {["`doc' attribute is not a valid docbook XML"], []}
                       end;
                   _ ->
                       {[], ["`doc' attribute is expected"]}
               end,
    lee_lib:compose_checks([CheckOneliner, CheckDoc]).

%% @private Meta-validation of doc root
-spec validate_doc_root(lee:model(), _, lee:key(), #mnode{}) ->
                               lee_lib:check_result().
validate_doc_root(_, _, Key, #mnode{metaparams = Attrs}) ->
    Fun = fun(#{app_name := _}) -> {[], []};
             (_)                -> {["Missing `app_name' parameter"], []}
          end,
    lee_lib:perform_checks(Key, Attrs, [fun check_docstrings/1, Fun]).

%% @private
-spec make_file(atom(), doc(), string()) -> file:filename().
make_file(Top, Data, Id) ->
    RootAttrs = [ {xmlns, "http://docbook.org/ns/docbook"}
                , {version, "5.0"}
                , {id, Id}
                ],
    Doc = {Top, RootAttrs, Data},
    DocStr = xmerl:export_simple([Doc], xmerl_xml, [{prolog, ""}]),
    Filename = filename:join("docs", Id ++ ".xml"),
    ok = filelib:ensure_dir(Filename),
    {ok, FD} = file:open(Filename, [write]),
    try ok = io:format(FD, "~s~n", [DocStr])
    after
        file:close(FD)
    end,
    Filename.

%% @private
-spec metatype_docs( lee:metatype()
                   , lee:model()
                   ) -> [doc()].
metatype_docs(Metatype, Model) ->
    case lee_metatype:doc_chapter_title(Metatype, Model) of
        undefined ->
            [];
        Title ->
            #model{meta_class_idx = Idx} = Model,
            Keys = maps:get(Metatype, Idx, []),
            Content =
                lists:filtermap( fun(Key) ->
                                         document_node(Metatype, Model, Key)
                                 end
                               , Keys
                               ),
            [{chapter, [{id, atom_to_list(Metatype)}]
             , [{title, [Title]} | Content]
             }]
    end.

document_node(Metatype, Model, Key) ->
    MNode = lee_model:get(Key, Model),
    case lists:member(undocumented, MNode#mnode.metatypes) of
        false -> {true, lee_metatype:doc_gen(Metatype, Model, Key, MNode)};
        true  -> false
    end.

-spec make_docs(lee:model(), doc_options()) -> ok.
make_docs(Model, Options) ->
    #{metatypes := Metatypes} = Options,
    case lee_model:get_metatype_index(doc_root, Model) of
        [DocRoot] ->
            #mnode{metaparams = Attrs} = lee_model:get(DocRoot, Model),
            BookTitle = lee_metatype:doc_chapter_title(doc_root, Model),
            Chapters = lists:flatten([metatype_docs(MT, Model) || MT <- [doc_root|Metatypes]]),
            Book = [{title, [BookTitle]} | Chapters],
            Top = make_file(book, Book, BookTitle),
            case maps:get(run_pandoc, Options, false) of
                true ->
                    {0, _} = run_pandoc(Top, "html"),
                    {0, _} = run_pandoc(Top, "man"),
                    {0, _} = run_pandoc(Top, "texinfo");
                false ->
                    ok
            end;
        _ ->
            error("Exactly one doc root should be present in the model")
    end.

run_pandoc(SrcFile, OutFormat) ->
    %% TODO: this is sketchy and wrong
    OutName = filename:rootname(SrcFile) ++ [$.|OutFormat],
    Cmd = lee_lib:format( "pandoc -o '~s' -f docbook -t ~s '~s'"
                        , [OutName, OutFormat, SrcFile]
                        ),
    lee_lib:run_cmd("pandoc", [ "--toc", "-s", "-f", "docbook", "-t", OutFormat, "-o"
                              , OutName, SrcFile
                              ]).
