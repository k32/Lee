-module(lee_doc).

-export([make_doc/3, make_doc/4, document_values/2]).

-export([simplesect/2, erlang_listing/1, xref_key/1, docbook/1]).

-include("lee_internal.hrl").

-type doc() :: term().

-export_type([doc/0]).

-spec erlang_listing(iolist()) -> doc().
erlang_listing(Str) ->
    { programlisting, [{language, "erlang"}]
    , [Str]
    }.

-spec simplesect(string(), iolist() | [doc()]) -> doc().
simplesect(Title, Doc0) ->
    Doc = case io_lib:deep_char_list(Doc0) of
              true ->
                  [{para, [lists:flatten(Doc0)]}];
              false ->
                  Doc0
          end,
    {para, [{emphasis, [Title]} | Doc]}.

-spec xref_key(lee:key()) -> doc().
xref_key(Key) ->
    Node = lee_lib:format("~p", [Key]),
    {xref, [{linkend, Node}], []}.

-spec docbook(string()) -> [doc()].
docbook([]) ->
    [];
docbook(String) ->
    {Doc, Rest} = xmerl_scan:string(String, [{document, false}]),
    [Doc | docbook(Rest)].

-spec document_value(lee:model_key(), lee:model()) ->
                            doc().
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

-spec document_values(lee:model(), _Config) -> doc().
document_values(Model, _Config) ->
    #model{meta_class_idx = Idx} = Model,
    Keys = maps:get(value, Idx, []),
    [document_value(Key, Model) || Key <- Keys].

-spec make_doc( lee:model()
              , string()
              , [lee:metatype() | {lee:metatype(), term()}]
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

-spec make_doc( lee:model()
              , string()
              , [lee:metatype() | {lee:metatype(), term()}]
              ) -> doc().
make_doc(Model, Title, Metatypes0) ->
    RootAttrs = [ {xmlns, "http://docbook.org/ns/docbook"}
                , {version, "5.0"}
                ],
    Metatypes = [case I of
                     {_, _} -> I;
                     _ when is_atom(I) -> {I, undefined}
                 end || I <- Metatypes0],
    Doc = { book
          , RootAttrs
          , [ {title, [Title]}
            | [metatype_docs(MetaType, Model) || MetaType <- Metatypes]
            ]
          },
    xmerl:export_simple([Doc], xmerl_xml, [{prolog, ""}]).

-spec metatype_docs( {lee:metatype(), term()}
                   , lee:model()
                   ) -> doc().
metatype_docs({MetaType, DocConfig}, Model) ->
    #model{metamodel = Meta} = Model,
    #mnode{metaparams = Attrs} = lee_model:get([metatype, MetaType], Meta),
    Title   = ?m_attr(documented, doc_chapter_title, Attrs),
    GenDocs = ?m_attr(documented, doc_gen, Attrs),
    Content = GenDocs(Model, DocConfig),
    {chapter, [{title, [Title]} | Content]}.
