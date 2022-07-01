%% @doc Model-driven CLI argument parser
%%
%% This module helps building command line interfaces that look like
%% this:
%%
%% ```
%% my_program -vfc1 --foo --bar 1 @action1 --foo 11 @action2 quux 1 -- -v @foo
%%            ----- ----- ------- -------- -------- -------- ---- -    -------
%%              ^      ^    ^        ^        ^        ^      ^   ^       ^
%%              |      |    |        |        |        |      |   |       |
%%         short args  | long arg    |  long arg in    |   positional ----+
%%                     |             | "action1" scope |      args
%%                     |             |                 |
%%                  long flag   CLI action    another CLI action
%% '''
%%
%% CLI actions start with sigil @ and define their own scope
-module(lee_cli).

%% TODO: This module is experimental; it's too convoluted and it needs
%% some serious refactoring

%% API
-export([ read/2
        , read_to/3
        ]).

%% behavior callbacks
-export([ create/1, names/1, metaparams/1
        , description_title/2, description/2
        , meta_validate/2, meta_validate_node/4
        , read_patch/2
        ]).

-ifdef(TEST).
-export([tokenize/2]).
-endif.

-include_lib("typerefl/include/types.hrl").
-include("lee.hrl").

-define(sigil, $@).
-define(cli_opts_key, [?MODULE, cli_opts]).
-define(prio_key, [?MODULE, priority]).
-define(index_key, [?MODULE, index]).

%%====================================================================
%% Types
%%====================================================================

-type position() :: integer() | rest.

%% CLI command scope:
-record(sc,
        { name = global    :: string() | global
        , short = #{}      :: #{char() => lee:key()}
        , long  = #{}      :: #{string() => lee:key()}
        , positional = []  :: [{position(), lee:key()}]
        , parent = []      :: lee:key()
        }).

-type token() :: {long, string(), string()}
               | {short, char(), string()}
               | {positional, string()}
               | {command, string()}
               | separator
               .

-reflect_type([position/0]).

%%====================================================================
%% API
%%====================================================================

%% Metamodel module containing definitions of CLI-related metatypes:
%%
%% == cli_param ==
%% `cli_param' denotes a regular CLI argument.
%%
%% === Metaparameters ===
%%   <ul><li>`cli_operand' of type `string(I)':
%%       long operand name without leading dashes</li>
%%       <li>`cli_short' of type `char()':
%%       short operand name</li>
%%   </ul>
%%
%% <b>Note:</b> instances of this metatype must include either or both
%% parameters.
%%
%% === Depends on ===
%% {@link lee:base_metamodel/0 . value}
%%
%% === Example ===
%% ```
%% {[value, cli_param],
%%  #{ type        => boolean()
%%   , cli_operand => "verbose"
%%   , cli_short   => $V
%%   }}'''
%%
%% == cli_action ==
%% `cli_action' defines a CLI action
%%
%% === Metaparameters ===
%% <ul><li>`cli_operand' of type `string(I)':
%%     CLI action name without leading @</li>
%% </ul>
%%
%% === Depends on ===
%% {@link lee:base_metamodel/0 . value}
%%
%% === Example ===
%% ```
%% {[map, cli_action],
%%   #{ cli_operand  => "compile"
%%    , key_elements => [[foo]] %% Relative key of `foo' child
%%    },
%%   #{ foo =>
%%        {[value, cli_param],
%%         ...}
%%    }}'''
%%
%% == cli_positional ==
%% `cli_positional' denotes a positional CLI argument
%%
%% === Metaparameters ===
%%
%% <ul><li>`cli_arg_position' of type `integer() | rest':
%% Position of CLI argument</li></ul>
%%
%% === Notes ===
%% Actual integer values `cli_arg_position' are
%% irrelevant, order is what matters.
%%
%% Values that are declared as `rest' should be have `list(A)'
%% type. Each scope can have at most one `rest' argument.
%%
%% === Depends on ===
%% {@link lee:base_metamodel/0 . map}
%%
%% === Example ===
%% ```
%% #{ foo =>
%%     {[value, cli_positional],
%%       #{ type             => integer()
%%        , cli_arg_position => 10
%%        }},
%%  , bar =>
%%      {[value, cli_positional],
%%       #{ type             => string()
%%        , cli_arg_position => 20
%%        }}
%%  , baz =>
%%      {[value, cli_positional],
%%       #{ type             => list(integer())
%%        , cli_arg_position => rest
%%        }}
%%  }'''

%% @doc Read CLI arguments and create a configuration patch
%% @throws {error, string()}
-spec read(lee:model(), [string()]) -> {ok, lee:patch()} | {error, [string()]}.
read(Model, Args) ->
    {ok, Scopes} = lee_model:get_meta(?index_key, Model),
    Tokens = tokenize(?sigil, Args),
    case split_commands(Tokens) of
        [[{command, _} | _] | _] = Commands ->
            Global = [];
        [Global | Commands] ->
            ok;
        [] ->
            Global = [],
            Commands = []
    end,
    try
        Globals = parse_args(Model, maps:get(global, Scopes), Global),
        Acc0 = lee_lib:make_nested_patch(Model, [], Globals),
        {ok, lists:foldl( fun(Tokns, Acc) ->
                                  parse_command(Model, Scopes, Tokns) ++ Acc
                          end
                        , Acc0
                        , Commands)}
    catch
        {error, Error} -> {error, ["cli: " ++ Error]};
        Error -> {error, ["cli: " ++ Error]}
    end.

%% @doc Read CLI arguments and apply the changes to the storage
%% @throws {error, string()}
-spec read_to(lee:model(), lee_storage:data(), [string()]) ->
                     lee:patch_result().
read_to(Model, Data, Args) ->
    {ok, Patch} = read(Model, Args),
    lee:patch(Model, Data, Patch).

%%====================================================================
%% Behavior callbacks
%%====================================================================

create(Attrs) ->
    [ {?cli_opts_key, maps:get(cli_opts, Attrs, [])}
    , {?prio_key, maps:get(priority, Attrs, 100)}
    ].

names(_) ->
    [cli_param, cli_action, cli_positional].

metaparams(cli_param) ->
    [{optional, cli_operand, string()}, {optional, cli_short, char()}];
metaparams(cli_action) ->
    [{mandatory, cli_operand, string()}];
metaparams(cli_positional) ->
    [{mandatory, cli_arg_position, position()}].

meta_validate(cli_param, Model) -> %% Run once
    meta_validate_model(Model);
meta_validate(_, _) ->
    {[], [], []}.

meta_validate_node(cli_param, Model, Key, MNode) ->
    meta_validate_param(Model, Key, MNode);
meta_validate_node(_, _Model, _Key, _MNode) ->
    {[], []}.

read_patch(cli_action, Model) ->
    {ok, Args0} = lee_model:get_meta(?cli_opts_key, Model),
    {ok, Prio} = lee_model:get_meta(?prio_key, Model),
    Args = if is_function(Args0) -> Args0();
              true               -> Args0
           end,
    case read(Model, Args) of
        {ok, Patch} ->
            {ok, Prio, Patch};
        Error = {error, _} ->
            Error
    end;
read_patch(_, _) ->
    {ok, 0, []}.

description_title(_MetaType, _Model) ->
    "CLI arguments".

description(_MetaType, Model) ->
    {ok, Index} = lee_model:get_meta(?index_key, Model),
    [{global, Global}|Scopes] = lists:sort(maps:to_list(Index)),
    GlobalDoc = [{section, make_scope_docs(Global, Model)}],
    ActionDocs =
        [{section, [{id, "cli-command-" ++ Name}]
         , [{title, [[?sigil|Name]]} | make_cli_action_docs(I, Model)]}
         || {Name, I} <- Scopes],
    GlobalDoc ++ ActionDocs.

%%====================================================================
%% Internal functions
%%====================================================================

-spec meta_validate_model(lee:model()) -> lee_metatype:metavalidate_result().
meta_validate_model(Model) ->
    {Index, {Err, Warn}} = lee_lib:collect_errors(fun() -> mk_index(Model) end),
    Patch = [{set, ?index_key, Index}],
    {Err, Warn, Patch}.

-spec meta_validate_param(lee:model(), lee:key(), #mnode{}) ->
                            lee_lib:check_result().
meta_validate_param(_, _Key, MNode) ->
    case MNode#mnode.metaparams of
        #{cli_operand := "no-" ++ _} ->
            Err = "CLI operands can't have `no-' prefix to "
                  "avoid confusion with negation syntax",
            {[Err], []};
        #{cli_operand := _} ->
            {[], []};
        #{cli_short := _} ->
            {[], []};
        _ ->
            {["Missing `cli_operand' or `cli_short' attributes"], []}
    end.

-spec split_commands([token()]) -> [[token()]].
split_commands(Tokens) ->
    Pred = fun({command, _}) -> false;
              (_)            -> true
           end,
    lee_lib:splitr(Pred, Tokens).

tokenize_(_, []) ->
    [];
tokenize_(Sigil, [[]|Rest]) ->
    tokenize_(Sigil, Rest);
tokenize_(_, ["--"|Rest]) ->
    [separator | [{positional, I} || I <- Rest]];
tokenize_(Sigil, [[Sigil|Command] | Rest]) ->
    [{command, Command} | tokenize_(Sigil, Rest)];
tokenize_(Sigil, ["--no-" ++ Long|Rest]) ->
    [{long, Long, "false"} | tokenize_(Sigil, Rest)];
tokenize_(Sigil, ["--" ++ Long|Rest]) ->
    [{long, Long, "true"} | tokenize_(Sigil, Rest)];
tokenize_(Sigil, ["+" ++ Shorts | Rest]) ->
    [{short, I, "false"} || I <- Shorts] ++ tokenize_(Sigil, Rest);
tokenize_(Sigil, ["-" ++ [S1|Shorts] | Rest]) ->
    {Flags, Arg0} = lists:splitwith( fun(A) -> A < $0 orelse A > $9 end
                                   , Shorts
                                   ),
    Arg = case Arg0 of
              [] -> [];
              _  -> [{positional, Arg0}]
          end,
    [{short, I, "true"} || I <- [S1|Flags]] ++ Arg ++ tokenize_(Sigil, Rest);
tokenize_(Sigil, [A|Rest]) ->
    [{positional, A}|tokenize_(Sigil, Rest)].

group_tokens([]) ->
    [];
group_tokens([separator|Rest]) ->
    Rest;
group_tokens([{short, S, "true"}, {positional, A} | Rest]) ->
    [{short, S, A} | group_tokens(Rest)];
group_tokens([{long, L, "true"}, {positional, A} | Rest]) ->
    [{long, L, A} | group_tokens(Rest)];
group_tokens([A|Rest]) ->
    [A|group_tokens(Rest)].

parse_command(Model, Scopes, [{command, Cmd} | Rest]) ->
    case maps:get(Cmd, Scopes, undefined) of
        SC = #sc{parent = Parent} ->
            Patch = parse_args(Model, SC, Rest),
            lee_lib:make_nested_patch(Model, Parent, Patch);
        undefined ->
            ErrorMsg = lee_lib:format("Unknown CLI command ~s", [Cmd]),
            throw(ErrorMsg)
    end.

parse_args( Model
          , #sc{ name = Name
               , long = Long
               , short = Short
               , parent = Parent
               } = Scope
          , [{ArgType, Arg, Val} | Rest]
          ) when ArgType =:= long; ArgType =:= short ->
    {Dash, ArgMap, Readable} =
        case ArgType of
            long  -> {"-", Long, Arg};
            short -> {"", Short, [Arg]}
        end,
    case maps:get(Arg, ArgMap, undefined) of
        undefined ->
            ErrorMsg = lee_lib:format( "Unexpected CLI argument -~s~s in context ~s"
                                     , [Dash, Readable, Name]
                                     ),
            throw(ErrorMsg);
        Key ->
            RelKey = make_relative(Key, Parent),
            case lee:from_string(Model, Key, Val) of
                {ok, Term} ->
                    maps:merge( #{RelKey => Term}
                              , parse_args(Model, Scope, Rest)
                              );
                _ ->
                    ErrorMsg = lee_lib:format( "Unable to parse term: ~p~nKey: ~p"
                                             , [Val, Key]
                                             ),
                    throw(ErrorMsg)
            end
    end;
parse_args( Model
          , #sc{ name = Name
               , positional = Specs
               , parent = Parent
               }
          , Positionals
          ) ->
    case zip_positionals(Model, Parent, Specs, Positionals) of
        {error, underflow} ->
            throw(lee_lib:format( "Not enough CLI arguments in context \"~s\""
                                , [Name]
                                ));
        {error, overflow, Val} ->
            throw(lee_lib:format( "Unexpected positional CLI argument ~p in context \"~s\""
                                , [Val, Name]
                                ));
        Zipped ->
            Zipped
    end.

zip_positionals(Model, Parent, [{rest, Key}], Vals0) ->
    Vals = [I || {_, I} <- Vals0],
    case lee:from_strings(Model, Key, Vals) of
        {ok, Terms} ->
            RelKey = make_relative(Key, Parent),
            #{RelKey => Terms};
        Err = {error, _} ->
            throw(Err)
    end;
zip_positionals(Model, Parent, [{_Position, Key}|T1], [{positional, Val}|T2]) ->
    case lee:from_string(Model, Key, Val) of
        {ok, Term} ->
            RelKey = make_relative(Key, Parent),
            (zip_positionals(Model, Parent, T1, T2)) #{RelKey => Term};
        Err = {error, _} ->
            throw(Err)
    end;
zip_positionals(_, _, [], [{positional, Val}|_]) ->
    {error, overflow, Val};
zip_positionals(_, _, [_|_], []) ->
    {error, underflow};
zip_positionals(_, _, [], []) ->
    #{}.

%% should be called inside lee_lib:collect_errors
-spec mk_index(lee:model()) -> #{global | string() => #sc{}}.
mk_index(Model) ->
    Scopes0 = lee_model:fold( fun mk_index/4
                            , #{global => #sc{}}
                            , global
                            , Model
                            ),
    maps:map( fun(_, S0 = #sc{positional = P0}) ->
                      P1 = lists:sort(P0),
                      S0#sc{positional = P1}
              end
            , Scopes0
            ).

mk_index(Key, #mnode{metatypes = Meta, metaparams = Attrs}, Acc, Scope) ->
    case { lists:member(cli_param, Meta)
         , lists:member(cli_positional, Meta)
         , lists:member(cli_action, Meta)
         }
    of
        {false, false, false} -> %% Ignored
            {Acc, Scope};
        {true, false, false} -> %% CLI parameter
            SC = add_param(Key, Attrs, maps:get(Scope, Acc)),
            {Acc #{Scope => SC}, Scope};
        {false, true, false} -> %% Positional parameter
            SC = add_positional(Key, Attrs, maps:get(Scope, Acc)),
            {Acc #{Scope => SC}, Scope};
        {false, false, true} -> %% CLI action
            NewScope = ?m_attr(cli_action, cli_operand, Attrs),
            SC = #sc{ name = NewScope
                    , parent = Key
                    },
            case Acc of
                #{NewScope := #sc{parent = OldKey}} ->
                    lee_lib:report_error( "~p: Action name @~s is already used by ~p"
                                        , [Key, NewScope, OldKey]
                                        );
                _ ->
                    ok
            end,
            {Acc #{NewScope => SC}, NewScope};
        _ ->
            lee_lib:report_error("~p: Illegal combination of CLI metatypes", [Key]),
            {Acc, Scope}
    end.

add_param(Key, Attrs, SC0) ->
    Long = ?m_attr(cli_param, cli_operand, Attrs, undefined),
    Short = ?m_attr(cli_param, cli_short, Attrs, undefined),
    SC1 = maybe_update_sc(SC0, #sc.long, Long, Key),
    maybe_update_sc(SC1, #sc.short, Short, Key).

maybe_update_sc(Record, _, undefined, _) ->
    Record;
maybe_update_sc(Record, ElemNumber, Key, Value) ->
    OldMap = element(ElemNumber, Record),
    case OldMap of
        #{Key := OldValue} ->
            lee_lib:report_error( "~p: CLI operand ~s is already used by ~p"
                                , [Value, pretty_print_operand(Key), OldValue]
                                ),
            Record;
        _ ->
            erlang:insert_element( ElemNumber
                                 , erlang:delete_element(ElemNumber, Record)
                                 , OldMap#{Key => Value}
                                 )
    end.

pretty_print_operand(Short) when is_integer(Short) ->
    [$-, Short];
pretty_print_operand(Long) ->
    ["--"|Long].

add_positional(Key, Attrs, SC0 = #sc{positional = Pos0}) ->
    %% Make key relative:
    Pos = maps:get(cli_arg_position, Attrs),
    SC0#sc{ positional = [{Pos, Key} | Pos0] }.

make_relative(Key, []) ->
    Key;
make_relative(Key0, Parent) ->
    [?children | Key] = Key0 -- Parent,
    Key.

make_cli_action_docs(Scope = #sc{parent = Parent}, Model) ->
    MNode = lee_model:get(Parent, Model),
    Oneliner = lee_doc:get_oneliner(Model, Parent, MNode),
    Doc = lee_doc:get_description(Model, Parent, MNode),
    Preamble = [{para, [Oneliner]}|Doc],
    Preamble ++ [{section,
                  [ {title, ["CLI arguments"]}
                  | make_scope_docs(Scope, Model)
                  ]}].

make_scope_docs(#sc{ short = Short
                   , long = Long
                   , positional = Positional
                   }, Model) ->
    NamedDocs = merge_operands(Model, lists:keysort(2, maps:to_list(Long) ++ maps:to_list(Short))),
    PositionalDocs =
        [document_param(lee_lib:format("Position: ~p", [P]), Key, Model)
         || {P, Key} <- Positional],
    NamedDocs ++ PositionalDocs.

merge_operands(_Model, []) ->
    [];
merge_operands(Model, [{A, K}, {B, K} | Rest]) ->
    Name = pretty_print_operand(A) ++ ", " ++ pretty_print_operand(B),
    [document_param(Name, K, Model) | merge_operands(Model, Rest)];
merge_operands(Model, [{A, K} | Rest]) ->
    Name = pretty_print_operand(A),
    [document_param(Name, K, Model) | merge_operands(Model, Rest)].

document_param(Name, Key, Model) ->
    MNode = lee_model:get(Key, Model),
    lee_doc:refer_value(Key, cli_param, Name, MNode).

%% @private
-spec tokenize(char(), [string()]) -> [token()].
tokenize(Sigil, L) ->
    Tokens = [I || I <- tokenize_(Sigil, L), I /= []],
    group_tokens(Tokens).
