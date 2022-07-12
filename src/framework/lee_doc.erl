%% @doc Utilities for extracting documentation from the model
-module(lee_doc).

-export([make_docs/2, get_description/3, get_oneliner/3, format_key/1]).

-export([ p/1
        , li/2
        , href/2
        , sect/3
        , simplesect/2
        , erlang_listing/1
        , xref_key/1
        , refer_value/4
        , docbook/1
        , from_file/1
        , from_file/2
        , documented/0
        , is_docbook_xml/1
        ]).

-include("lee_internal.hrl").
-include_lib("typerefl/include/types.hrl").

-type docbook_xml() :: term().
-typerefl_verify({docbook_xml/0, ?MODULE, is_docbook_xml}).
-reflect_type([docbook_xml/0]).

-type doc_options() ::
        #{ metatypes    := [lee:metatype() | {lee:metatype(), term()}]
         , output_dir   => file:filename()
         , doc_xml      => file:filename()
         , run_pandoc   => boolean()
         }.

-export_type([doc_options/0]).

-define(external_doc, [?MODULE, external_doc]).

-spec p(list()) -> docbook_xml().
p(Content) ->
    {para, [Content]}.

-spec href(string(), string()) -> docbook_xml().
href(To, Text) ->
    {link, [{'xlink:href', To}],
     [Text]}.

-spec sect(string(), string(), list()) -> docbook_xml().
sect(ID, Title, Content) ->
    {section, [{id, ID}],
     [ {title, [Title]}
     | Content
     ]}.

%% @doc Represent text as Erlang code
-spec erlang_listing(iolist()) -> docbook_xml().
erlang_listing(Str) ->
    { programlisting, [{language, "erlang"}]
    , [Str]
    }.

%% @doc Get string representation of the key
-spec format_key(lee:model_key()) -> string().
format_key(Key) ->
    lists:flatten(lists:join("/", [io_lib:format("~p", [I]) || I <- Key])).

%% @doc Make a simple subsection
-spec simplesect(string(), iolist() | [docbook_xml()]) -> docbook_xml().
simplesect(Title, Doc0) ->
    Doc = case io_lib:deep_char_list(Doc0) of
              true ->
                  [{para, [lists:flatten(Doc0)]}];
              false ->
                  Doc0
          end,
    {para, [{emphasis, [Title]} | Doc]}.

li(Title, Contents) ->
  {listitem,
   [{para,
     [ {emphasis, [Title]}, ": "
     | Contents
     ]}]}.

%% @doc Generate a link to the description of a value
-spec xref_key(lee:key()) -> docbook_xml().
xref_key(Key) ->
    Node = format_key(Key),
    {xref, [{linkend, Node}], []}.

%% @doc Generate a section that contains short description of a value
%% and a link to the full description
-spec refer_value(lee:model_key(), lee:metatype(), string(), #mnode{}) ->
                         docbook_xml().
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
-spec docbook(iolist()) -> docbook_xml().
docbook([]) ->
    [];
docbook(Cooked = [Tup|_]) when is_tuple(Tup) -> %% TODO: make detection better
    Cooked;
docbook(String) ->
    {Doc, Rest} = xmerl_scan:string(String, [{document, false}]),
    [Doc | docbook(Rest)].

is_docbook_xml(Thing) ->
    is_list(catch docbook(Thing)).

%% @doc Read docbook XML from a file.
from_file(Filename) ->
    case xmerl_scan:file(Filename, [{document, false}]) of
        {error, Error} ->
            error(Error);
        {Doc, Rest} ->
            [Doc | docbook(Rest)]
    end.

%% @doc Read docbook XML from a file located in the priv directory of
%% an Erlang application
-spec from_file(application:application(), string()) -> docbook_xml().
from_file(Application, Filename) ->
    Path = filename:join([code:priv_dir(Application), Filename]),
    from_file(Path).

-spec documented() -> list().
documented() ->
    [{warn_if_missing, oneliner, string()}, {warn_if_missing, doc, docbook_xml()}].

%% @private
-spec make_file(atom(), docbook_xml(), string()) -> file:filename().
make_file(Top, Data, Id) ->
    RootAttrs = [ {xmlns, "http://docbook.org/ns/docbook"}
                , {version, "5.0"}
                , {'xmlns:xlink', "http://www.w3.org/1999/xlink"}
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
                   ) -> docbook_xml().
metatype_docs(Metatype, Model) ->
    case lee_metatype:description_title(Metatype, Model) of
        undefined ->
            [];
        Title ->
            #model{meta_class_idx = Idx} = Model,
            Descr = lee_metatype:description(Model, Metatype),
            Keys = maps:get(Metatype, Idx, []),
            NodesDescr =
                lists:filtermap( fun(Key) ->
                                         document_node(Metatype, Model, Key)
                                 end
                               , Keys
                               ),
            [{chapter, [{id, atom_to_list(Metatype)}]
             , [{title, [Title]} | Descr ++ NodesDescr]
             }]
    end.

document_node(Metatype, Model, Key) ->
    MNode = lee_model:get(Key, Model),
    case lists:member(undocumented, MNode#mnode.metatypes) of
        false ->
            case lee_metatype:description_node(Metatype, Model, Key, MNode) of
                [] -> false;
                Val -> {true, Val}
            end;
        true ->
            false
    end.

-spec make_docs(lee:model(), doc_options()) -> ok.
make_docs(Model0, Options) ->
    Model = maybe_inject_docs(Model0, Options),
    #{metatypes := Metatypes} = Options,
    case lee_model:get_metatype_index(doc_root, Model) of
        [_] ->
            BookTitle = lee_metatype:description_title(doc_root, Model),
            Chapters = lists:flatten([metatype_docs(MT, Model) || MT <- [doc_root|Metatypes]]),
            Book = [{title, [BookTitle]} |
                    Chapters],
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

-spec get_description(lee:model(), lee:model_key(), #mnode{}) -> list().
get_description(Model, Key, #mnode{metaparams = Attrs}) ->
    External = case lee_model:get_meta(?external_doc, Model) of
                   undefined ->
                       [];
                   {ok, XML} ->
                       doc_xml_selector(XML, Key)
               end,
    case External of
        [] ->
            case Attrs of
                #{doc := DocString0} ->
                    DocString = ?m_valid(value, lee_doc:docbook(DocString0)),
                    [lee_doc:simplesect("Description:", DocString)];
                _ ->
                    []
            end;
        _ ->
            External
    end.

-spec get_oneliner(lee:model(), lee:model_key(), #mnode{}) -> list().
get_oneliner(_Model, _Key, #mnode{metaparams = Attrs}) ->
    maps:get(oneliner, Attrs, ""). %% TODO: lookup from XML

%% Inject docs from an external source, if needed.
maybe_inject_docs(Model, Options) ->
    case Options of
        #{doc_xml := Filename} ->
            {XML, _} = xmerl_scan:file(Filename),
            lee_model:patch_meta(Model, [ {set, ?external_doc, XML}
                                        ]);
        _ ->
            Model
    end.

doc_xml_selector(XML, Key) ->
    KeyStr = format_key(Key),
    xmerl_xpath:string("/lee/node[@id = \"" ++ KeyStr ++ "\"]/doc/*", XML).

run_pandoc(SrcFile, OutFormat) ->
    %% TODO: this is sketchy and wrong
    OutName = filename:rootname(SrcFile) ++ [$.|OutFormat],
    lee_lib:run_cmd("pandoc", [ "--toc", "-s", "-f", "docbook", "-t", OutFormat, "-o"
                              , OutName, SrcFile
                              ]).
