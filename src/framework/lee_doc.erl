%% @doc Utilities for extracting documentation from the model
-module(lee_doc).

-export([make_docs/2, get_description/2, get_oneliner/2, format_key/1, format_key/2]).

-export([ p/1, li/2, href/2, simplesect/2, erlang_listing/1, xref_key/2, refer_value/4, chapter/3, refsection/3
          %% Misc:
        , docbook/1
        , documented/0
        , is_docbook_xml/1
        ]).

-include("lee_internal.hrl").
-include_lib("typerefl/include/types.hrl").

-type docbook_xml() :: term().
-type chapter_id() :: atom().
-typerefl_verify({docbook_xml/0, ?MODULE, is_docbook_xml}).
-reflect_type([docbook_xml/0]).

-type options() ::
        #{ metatypes   => [lee:metatype()] | all
         , output_file := file:filename()
         , _           => _
         }.

-export_type([options/0]).

%%================================================================================
%% API
%%================================================================================

-spec make_docs(lee:model(), options()) -> ok.
make_docs(Model, Options) ->
    Filename = maps:get(output_file, Options),
    MTs = case maps:get(metatypes, Options, all) of
              all ->
                  [doc_root | lee_model:all_metatypes(Model) -- [doc_root]];
              L when is_list(L) ->
                  L
          end,
    RootAttrs = [{xmlns, "http://docbook.org/ns/docbook"}, {version, "5.0"}, {'xmlns:xl', "http://www.w3.org/1999/xlink"}],
    Chapters = lists:flatmap(fun(MT) ->
                                     lee_metatype:description(MT, Model, Options)
                             end,
                             MTs),
    Doc = {book, RootAttrs, Chapters},
    DocStr = xmerl:export_simple([Doc], xmerl_xml, [{prolog, ""}]),
    %% io:format(user, "~p", [Doc]),
    ok = filelib:ensure_dir(Filename),
    file:write_file(Filename, unicode:characters_to_binary(DocStr)).

-spec get_description(lee:model(), lee:model_key()) -> docbook_xml().
get_description(Model, Key) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    docbook(?m_attr(value, doc, Attrs, [])).

%% @doc Return a list with single element containing the oneliner, or
%% empty list
-spec get_oneliner(lee:model(), lee:model_key()) -> [string()].
get_oneliner(Model, Key) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    case ?m_attr(value, oneliner, Attrs, undefined) of
        undefined -> [];
        Oneliner  -> [Oneliner]
    end.

%% @doc Get string representation of the key
-spec format_key(lee:model_key()) -> string().
format_key([{}]) ->
    "_";
format_key([A]) when is_atom(A) ->
    atom_to_list(A);
format_key([A|Rest]) ->
    format_key([A]) ++ "." ++ format_key(Rest).

-spec format_key(chapter_id(), lee:model_key()) -> string().
format_key(MT, Key) ->
    format_key([MT|Key]).

%%================================================================================
%%   Common docbook elements:
%%================================================================================

-spec chapter(atom() | string(), string(), [docbook_xml()]) -> [docbook_xml()].
chapter(Id, Title, Content0) ->
    Content = lists:flatten(Content0),
    case Content of
        [] ->
            [];
        _ ->
            [{chapter, [{'xml:id', Id}],
              [ {info, [{title, [Title]}]}
              | Content
              ]}]
    end.

-spec refsection(string(), string(), [docbook_xml()]) -> [docbook_xml()].
refsection(Id, Title, Content0) ->
    Content = lists:flatten(Content0),
    case Content of
        [] ->
            [];
        _ ->
            [{refsection, [{'xml:id', Id}],
              [ {title, [Title]}
              | Content
              ]}]
    end.

-spec p(list()) -> docbook_xml().
p(Content) ->
    {para, [Content]}.

-spec href(string(), string()) -> docbook_xml().
href(To, Text) ->
    {link, [{'xlink:href', To}], [Text]}.

%% @doc Represent text as Erlang code
-spec erlang_listing(iolist()) -> docbook_xml().
erlang_listing(Str) ->
    { programlisting, [{language, "erlang"}]
    , [Str]
    }.

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

%% @doc Generate a section that contains short description of a value
%% and a link to the full description
-spec refer_value(lee:model(), chapter_id(), lee:model_key(), string()) ->
          docbook_xml().
refer_value(Model, Chapter, Key, Name) ->
    {refsection,
     [{'xml:id', format_key(Chapter, Key)}],
     [ {title, [Name]}
     , {para, [lists:flatten(get_oneliner(Model, Key)) ++ ", see: ", xref_key(value, Key)]}
     ]}.

%% @doc Generate a link to the description of a value
-spec xref_key(lee:metatype(), lee:key()) -> docbook_xml().
xref_key(Metatype, Key) ->
    Node = format_key(Metatype, Key),
    {xref, [{linkend, Node}], []}.

%%================================================================================
%% Internal exports
%%================================================================================

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

-spec documented() -> list().
documented() ->
    [{warn_if_missing, oneliner, string()}, {warn_if_missing, doc, docbook_xml()}].

%%================================================================================
%% Internal functions
%%================================================================================
