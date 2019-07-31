-module(lee_doc).

-export([make_doc/3, make_doc/4, document_values/1]).

-export([simplesect/2, erlang_listing/1, xref_key/1, docbook/1]).

-export([test/0]).

-include("lee_internal.hrl").

-type doc() :: term().

-spec document_value(lee:model_key(), lee:model()) ->
                            {ok, doc(), [string()]}.
document_value(Key, Model) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    Oneliner = ?m_valid(value, maps:get(oneliner, Attrs, "")),
    Type = ?m_attr(value, type, Attrs),
    Default =
        case Attrs of
            #{default := DefVal} ->
                DefStr = io_lib:format("~p", [DefVal]),
                [simplesect( "Default value:"
                           , [erlang_listing(DefStr)]
                           )];
            _ ->
                []
        end,
    Description =
        case Attrs of
            #{doc := DocString0} ->
                DocString = ?m_valid(value, docbook(DocString0)),
                [simplesect("Description:", DocString)];
            _ ->
                []
        end,
    Id = lee_lib:format("~p", [Key]),
    { section, [{id, Id}]
    , [ {title, [Id]}
      , {para, [Oneliner]}
      , simplesect("Type:", [erlang_listing(typerefl:print(Type))])
      ] ++ Default ++ Description
    }.

-spec xref_key(lee:key()) -> xmerl:document().
xref_key(Key) ->
    Node = lee_lib:format("~p", [Key]),
    {xref, [{linkend, Node}], []}.

-spec document_values(lee:model()) -> xmerl:document().
document_values(Model) ->
    #model{meta_class_idx = Idx} = Model,
    Keys = maps:get(value, Idx, []),
    [document_value(Key, Model) || Key <- Keys].

-spec erlang_listing(iolist()) -> xmerl:document().
erlang_listing(Str) ->
    { programlisting, [{language, "erlang"}]
    , [Str]
    }.

-spec simplesect(string(), iolist() | [xmerl:document()]) ->
                        xmer:document().
simplesect(Title, Doc0) ->
    Doc = case io_lib:deep_char_list(Doc0) of
              true ->
                  [{para, [lists:flatten(Doc0)]}];
              false ->
                  Doc0
          end,
    {para, [{emphasis, [Title]} | Doc]}.

-spec make_doc( lee:model()
              , string()
              , [lee:metatype()]
              , file:filename()
              ) -> ok.
make_doc(Model, Title, Metatypes, Filename) ->
    Doc = make_doc(Model, Title, Metatypes),
    {ok, FD} = file:open(Filename, [write]),
    try io:format(FD, "~s~n", [Doc])
    after
        file:close(FD)
    end,
    ok.

-spec make_doc(lee:model(), string(), [lee:metatype()]) -> term().
make_doc(Model, Title, Metatypes) ->
    RootAttrs = [ {xmlns, "http://docbook.org/ns/docbook"}
                , {version, "5.0"}
                ],
    Doc = { book
          , RootAttrs
          , [ {title, [Title]}
            | [metatype_docs(MetaType, Model) || MetaType <- Metatypes]
            ]
          },
    xmerl:export_simple([Doc], xmerl_xml, [{prolog, ""}]).

-spec metatype_docs( lee:metatype()
                   , lee:model()
                   ) -> doc().
metatype_docs(MetaType, Model) ->
    #model{metamodel = Meta} = Model,
    #mnode{metaparams = Attrs} = lee_model:get([metatype, MetaType], Meta),
    Title   = ?m_attr(doc, doc_chapter_title, Attrs),
    GenDocs = ?m_attr(doc, doc_gen, Attrs),
    Content = GenDocs(Model),
    {chapter, [{title, [Title]} | Content]}.

-spec docbook(string()) -> [xmerl:document()].
docbook([]) ->
    [];
docbook(String) ->
    {Doc, Rest} = xmerl_scan:string(String, [{document, false}]),
    [Doc | docbook(Rest)].

test() ->
    Model = #{ foo =>
                   {[value],
                    #{ oneliner => "This parameter controls fooing"
                     , doc      => "<para>
                                    This is a <emphasis>document</emphasis>.
                                    </para>
                                    <para>Ohayo!!!!!<xref linkend=\"[foo]\"/></para>"
                     , type     => typerefl:string()
                     , default  => "Ohayo"
                     }}
             , bar =>
                   {[value, os_env],
                    #{ oneliner => "This parameter controls baring"
                     , type     => {typerefl:nonempty_list([]), typerefl:iolist()}
                     , os_env   => "BAR"
                     }}
             },
    {ok, Mod} = lee_model:compile( [ lee:base_metamodel()
                                   , lee_os_env:metamodel()
                                   ]
                                 , [Model]
                                 ),
    lee_doc:make_doc(Mod, "My application", [value, os_env], "foo.xml"),
    [io:put_chars(
       os:cmd("cat foo.xml;"
              "pandoc -o foo." ++ Fmt ++
              " --toc -sf docbook -t " ++ Fmt ++" foo.xml | cat foo." ++ Fmt))
     || Fmt <- ["html", "man", "texinfo"]
    ].
