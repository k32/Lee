-module(lee_types).

%% API exports
-export([ union/2
        , union/1
        , boolean/0
        , validate_union/3
        , print_union/2

        , term/0
        , any/0
        , validate_term/3

        , integer/0
        , non_neg_integer/0
        , range/2
        , validate_integer/3
        , print_integer/2

        , float/0
        , validate_float/3

        , atom/0
        , validate_atom/3

        , binary/0
        , validate_binary/3

        , list/0
        , list/1
        , nonempty_list/1
        , string/0
        , validate_list/3
        , print_list/2

        , tuple/0
        , validate_any_tuple/3

        , tuple/1
        , validate_tuple/3
        , print_tuple/2

        , map/2
        , validate_map/3

        , exact_map/1
        , validate_exact_map/3
        , print_exact_map/2

        , number/0

        , print_type/2
        ]).

-type validate_result() :: ok | {error, term()}.

%%====================================================================
%% Types
%%====================================================================

-export_type([ boolean/0
             , term/0
             , any/0
             , integer/0
             , non_neg_integer/0
             , float/0
             , atom/0
             , binary/0
             , list/1
             , list/0
             , nonempty_list/1
             , string/0
             , tuple/0
             , map/2
             , number/0
             ]).

-type map(A, B) :: map(A, B). %% TODO Wtf?

%%====================================================================
%% Macros
%%====================================================================

-define(te(Name, Arity, Attrs, Parameters),
        { [lee, base_types, {Name, Arity}]
        , Attrs
        , Parameters
        }).
-define(te(Attrs, Parameters),
        ?te(?FUNCTION_NAME, ?FUNCTION_ARITY, Attrs, Parameters)).
-define(te(Parameters),
        ?te(?FUNCTION_NAME, ?FUNCTION_ARITY, #{}, Parameters)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API functions
%%====================================================================
-spec union(lee:typedef(), lee:typedef()) -> lee:typedef().
union(A, B) ->
    ?te([A, B]).

-spec union([lee:typedef()]) -> lee:typedef().
union([A, B|T]) ->
    lists:foldl( fun union/2
               , union(A, B)
               , T
               ).

-spec validate_union( lee:model_fragment()
                    , lee:typedef()
                    , term()
                    ) -> validate_result().
validate_union(Model, {_, _, [A, B]}, Term) ->
    case lee:validate_term(Model, A, Term) of
        ok ->
            ok;
        {error, _} ->
            case lee:validate_term(Model, B, Term) of
                ok ->
                    ok;
                {error, _} ->
                    Msg = format( "Expected ~s | ~s, got ~p"
                                , [ print_type(Model, A)
                                  , print_type(Model, B)
                                  , Term
                                  ]
                                ),
                    {error, Msg}
            end
    end.

-spec print_union(lee:model_fragment(), lee:typedef()) ->
                         iolist().
print_union(Model, {_, _, [A, B]}) ->
    [print_type_(Model, A), " | ", print_type_(Model, B)].

-spec boolean() -> lee:typedef().
boolean() ->
    union(true, false).

-spec integer() -> lee:typedef().
integer() ->
    range(neg_infinity, infinity).

-spec validate_integer( lee:model_fragment()
                      , lee:typedef()
                      , term()
                      ) -> validate_result().
validate_integer(Model, Self = {_, #{range := {A, B}}, []}, Term) ->
    try
        is_integer(Term) orelse throw(badint),
        A =:= neg_infinity orelse Term >= A orelse throw(badint),
        B =:= infinity     orelse Term =< B orelse throw(badint),
        ok
    catch
        badint ->
            {error, format( "Expected ~s, got ~p"
                          , [print_type(Model, Self), Term]
                          )}
    end.

-spec print_integer(lee:model_fragment(), lee:typedef()) ->
                         iolist().
print_integer(Model, {_, #{range := Range}, _}) ->
    case Range of
        {neg_infinity, infinity} ->
            "integer()";
        {0, infinity} ->
            "non_neg_integer()";
        {0, 255} ->
            "byte()";
        {0, 16#10ffff} ->
            "char()";
        {A, B} ->
            [integer_to_list(A), "..", integer_to_list(B)]
    end.

-spec non_neg_integer() -> lee:typedef().
non_neg_integer() ->
    range(0, infinity).

-spec range( integer() | neg_infinity
           , integer() | infinity
           ) -> lee:typedef().
range(A, B) ->
    ?te( integer
       , 0
       , #{range => {A, B}}
       , []
       ).

-spec string() -> lee:typedef().
string() ->
    list(range(0, 16#10ffff)).

-spec float() -> lee:typedef().
float() ->
    ?te([]).

-spec validate_float( lee:model_fragment()
                    , lee:typedef()
                    , term()
                    ) -> validate_result().
validate_float(_, _, Term) ->
    if is_float(Term) ->
            ok;
       true ->
            {error, format("Expected float(), got ~p", [Term])}
    end.

-spec atom() -> lee:typedef().
atom() ->
    ?te([]).

-spec validate_atom( lee:model_fragment()
                   , lee:typedef()
                   , term()
                   ) -> validate_result().
validate_atom(_, _, Term) ->
    if is_atom(Term) ->
            ok;
       true ->
            {error, format("Expected atom(), got ~p", [Term])}
    end.

-spec binary() -> lee:typedef().
binary() ->
    ?te([]).

-spec validate_binary( lee:model_fragment()
                     , lee:typedef()
                     , term()
                     ) -> validate_result().
validate_binary(_, _, Term) ->
    if is_binary(Term) ->
            ok;
       true ->
            {error, format("Expected binary(), got ~p", [Term])}
    end.

-spec tuple() -> lee:typedef().
tuple() ->
    ?te([]).

-spec validate_any_tuple( lee:model_fragment()
                        , lee:typedef()
                        , term()
                        ) -> validate_result().
validate_any_tuple(_, _, Term) ->
    if is_tuple(Term) ->
            ok;
       true ->
            {error, format("Expected tuple(), got ~p", [Term])}
    end.

-spec tuple([lee:typedef()]) -> lee:typedef().
tuple(Params) ->
    ?te(Params).

-spec validate_tuple( lee:model_fragment()
                    , lee:typedef()
                    , term()
                    ) -> validate_result().
validate_tuple(Model, Self = {_, _, Params}, Term) ->
    try
        is_tuple(Term)
            orelse throw(badtuple),
        List = tuple_to_list(Term),
        length(Params) =:= length(List)
            orelse throw(badtuple),
        lists:zipwith( fun(Type, Val) ->
                               %% TODO: make better error message
                               lee:validate_term(Model, Type, Val) =:= ok
                                   orelse throw(badtuple)
                       end
                     , Params
                     , List
                     ),
        ok
    catch
        badtuple ->
            {error, format( "Expected ~s, got ~p"
                          , [print_type(Model, Self), Term]
                          )}
    end.

-spec print_tuple(lee:model_fragment(), lee:typedef()) ->
                             iolist().
print_tuple(Model, {_, _, Params}) ->
    PS = [print_type_(Model, I) || I <- Params],
    ["{", lists:join(",", PS), "}"].

-spec term() -> lee:typedef().
term() ->
    ?te([]).

-spec any() -> lee:typedef().
any() ->
    term().

-spec validate_term( lee:model_fragment()
                   , lee:typedef()
                   , term()
                   ) -> validate_result().
validate_term(_, _, _) ->
    ok.

-spec list() -> lee:typedef().
list() ->
    list(term()).

-spec list(lee:typedef()) -> lee:typedef().
list(Type) ->
    ?te(#{non_empty => false}, [Type]).


-spec nonempty_list(lee:typedef()) -> lee:typedef().
nonempty_list(Type) ->
    ?te(list, 1, #{non_empty => true}, [Type]).

-spec validate_list( lee:model_fragment()
                   , lee:typedef()
                   , term()
                   ) -> validate_result().
validate_list( Model
             , Self = {_, #{non_empty := NonEmpty}, [Param]}
             , Term
             ) ->
    try
        is_list(Term) orelse throw(badlist),
        not(NonEmpty) orelse length(Term) > 0 orelse throw(badlist),
        validate_list_(Model, Param, Term),
        ok
    catch
        {badelem, Elem} ->
            {error, format( "Expected ~s, got ~p in ~s"
                          , [ print_type(Model, Param)
                            , Elem
                            , print_type(Model, Self)
                            ]
                          )};
        badlist ->
            {error, format( "Expected ~s, got ~p"
                          , [ print_type(Model, Self)
                            , Term
                            ]
                          )}
    end.

-spec print_list(lee:model_fragment(), lee:typedef()) ->
                             iolist().
print_list(Model, {_, #{non_empty := NonEmpty}, [Par]}) ->
    case NonEmpty of
        true ->
            Prefix = "nonempty_";
        false ->
            Prefix = ""
    end,
    [Prefix, "list(", print_type_(Model, Par), ")"].

-spec map(lee:typedef(), lee:typedef()) -> lee:typedef().
map(K, V) ->
    ?te([K, V]).

-spec validate_map( lee:model_fragment()
                  , lee:typedef()
                  , term()
                  ) -> validate_result().
validate_map( Model
            , Self = {_, _, [KeyT, ValueT]}
            , Term
            ) ->
    try
        is_map(Term) orelse throw(badmap),
        [begin
             lee:validate_term(Model, KeyT, K) =:= ok
                 orelse throw(badmap),
             lee:validate_term(Model, ValueT, V) =:= ok
                 orelse throw({badval, K, V})
         end
         || {K, V} <- maps:to_list(Term)],
        ok
    catch
        {badval, Key, Val} ->
            {error, format( "Expected ~s, but key ~p got value ~p instead"
                          , [ print_type(Model, Self)
                            , Key
                            , Val
                            ]
                          )};
        badmap ->
            {error, format( "Expected ~s, got ~p"
                          , [ print_type(Model, Self)
                            , Term
                            ]
                          )}
    end.

%% "Literal" map
-spec exact_map(#{term() := lee:typedef()}) -> lee:typedef().
exact_map(Spec) ->
    ?te( #{ exact_map_spec => Spec
          , mandatory_map_fields => maps:keys(Spec)
          }
       , []
       ).

-spec validate_exact_map( lee:model_fragment()
                        , lee:typedef()
                        , term()
                        ) -> validate_result().
validate_exact_map( Model
                  , Self = {_, Attr, _}
                  , Term
                  ) ->
    #{ exact_map_spec := Spec
     , mandatory_map_fields := Mandatory0
     } = Attr,
    try
        is_map(Term) orelse throw(badmap),
        Mandatory = map_sets:from_list(Mandatory0),
        maps:map( fun(K, Type) ->
                          case {Term, map_sets:is_element(K, Mandatory)} of
                              {#{K := Val}, _} ->
                                  lee:validate_term(Model, Type, Val) =:= ok
                                      orelse throw({badval, K, Val, Type});
                              {_, true} ->
                                  throw({badkey, K});
                              {_, false} ->
                                  ok
                          end
                  end
                , Spec
                ),
        ok
    catch
        {badval, Key, Val, ValType} ->
            {error, format( "Expected ~s in key ~p of ~s, got ~p"
                          , [ print_type(Model, ValType)
                            , Key
                            , print_type(Model, Self)
                            , Val
                            ]
                          )};
        {badkey, Key} ->
            {error, format( "Missing key(s) ~p in ~s"
                          , [ Term
                            , print_type(Model, Self)
                            ]
                          )};
        badmap ->
            {error, format( "Expected ~s, got ~p"
                          , [ print_type(Model, Self)
                            , Term
                            ]
                          )}
    end.

-spec print_exact_map(lee:model_fragment(), lee:typedef()) ->
                             iolist().
print_exact_map(Model, {_, #{exact_map_spec := Spec}, _}) ->
    %% FIXME: Wrong!
    io_lib:format( "~w"
                 , [maps:map( fun(_, V) ->
                                      print_type(Model, V)
                              end
                            , Spec
                            )]
                 ).

number() ->
    union(integer(), float()).

-spec valid_file(file:filename_all()) ->
                        lee:typedef().
valid_file(Filename) ->
    ?te(valid_file, 0, #{filename => Filename}, []).

print_type_(_, {var, Var}) ->
    io_lib:format("_~w", [Var]);
print_type_(_, Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
print_type_(_, Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
print_type_(Model, Type) ->
    {TypeId, _, TypeParams} = Type,
    {Meta, Attrs, _} = lee_model:get(TypeId, Model),
    case Attrs of
        #{print := Print} ->
            Print(Model, Type);
        #{typename := TypeName} ->
            StrParams = [print_type_(Model, I)
                         || I <- TypeParams
                        ],
            io_lib:format("~s(~s)", [ TypeName
                                    , lists:join($,, StrParams)
                                    ])
    end ++
    case lists:member(typedef, Meta) of
        true ->
            [" :: TODO "];
        false ->
            []
    end.

print_type(Model, Type) ->
    lists:flatten(print_type_(Model, Type)).

%%====================================================================
%% Internal functions
%%====================================================================

format(Fmt, Attrs) ->
    lists:flatten(io_lib:format(Fmt, Attrs)).

validate_list_(_, _, []) ->
    ok;
validate_list_(Model, Param, [Term|Tail]) ->
    is_list(Tail) orelse throw(badlist),
    lee:validate_term(Model, Param, Term) =:= ok
        orelse throw({badelem, Term}),
    validate_list_(Model, Param, Tail).

%%====================================================================
%% Unit tests
%%====================================================================

%% Can't use PropER in this module

-ifdef(TEST).

-define(valid(Type, Term),
        ?assertMatch( ok
                    , lee:validate_term(Model, Type, Term)
                    )).

-define(invalid(Type, Term),
        ?assertMatch( {error, _}
                    , lee:validate_term(Model, Type, Term)
                    )).

validate_concrete_atom_test() ->
    Model = lee:base_model(),
    ?valid(true, true),
    ?valid(false, false),
    ?invalid(foo, 1),
    ?invalid(foo, []),
    ?invalid(foo, bar).

validate_bool_test() ->
    Model = lee:base_model(),
    ?valid(boolean(), true),
    ?valid(boolean(), false),
    ?invalid(boolean(), 1),
    ?invalid(boolean(), {}),
    ?invalid(boolean(), foo).

integer_test() ->
    Model = lee:base_model(),
    ?valid(integer(), -1),
    ?valid(integer(), 1000),
    ?valid(integer(), 0),
    ?invalid(integer(), 1.0),
    ?invalid(integer(), foo),
    ?valid(range(-1, 1), 1),
    ?valid(range(-1, 1), -1),
    ?invalid(range(-1, 1), -2),
    ?invalid(range(-1, 1), 2),
    ?valid(non_neg_integer(), 0),
    ?valid(non_neg_integer(), 1),
    ?invalid(non_neg_integer(), -1).

union_test() ->
    Model = lee:base_model(),
    ?valid(number(), 1),
    ?valid(number(), 1.1),
    ?invalid(number(), []).

term_test() ->
    Model = lee:base_model(),
    ?valid(term(), 1),
    ?valid(term(), 1.1),
    ?valid(term(), {1, 2, [], foo}),
    ?valid(term(), [foo, 1, [] | gg]).

atom_test() ->
    Model = lee:base_model(),
    ?valid(atom(), foo),
    ?valid(atom(), bar),
    ?invalid(atom(), {}),
    ?invalid(atom(), 1).

list_test() ->
    Model = lee:base_model(),
    ?valid(list(), []),
    ?valid(nonempty_list(integer()), [1, 2, 3]),
    ?invalid(nonempty_list(term()), []),
    UnionL = list(union(boolean(), integer())),
    ?valid(UnionL, [true, false, 1, 10, -1]),
    ?invalid(UnionL, [true, false, 1, bar]),
    ?invalid(list(), [foo, bar | baz]).

string_test() ->
    Model = lee:base_model(),
    ?valid(string(), "this is a string"),
    ?valid(string(), "(✿ ┛O‿‿O)┛彡♥   ¯\_(ツ)_/¯"),
    ?invalid(string(), "foo" ++ [bar, {}] ++ "baz"),
    ?invalid(string(), [-1, 2]).

tuple_test() ->
    Model = lee:base_model(),
    ?valid(tuple(), {}),
    ?valid(tuple(), {foo, 1, []}),
    ?invalid(tuple(), 1),
    ?invalid(tuple(), []),

    ?valid(tuple([]), {}),
    ?invalid(tuple([]), {1}),

    T = tuple([atom(), integer()]),
    ?valid(T, {foo, 1}),
    ?valid(T, {false, -1}),
    ?invalid(T, {false, -1, 5}),
    ?invalid(T, {false}),
    ?invalid(T, {false, "1"}).

binary_test() ->
    Model = lee:base_model(),
    ?valid(binary(), <<>>),
    ?valid(binary(), <<"foo">>),
    ?invalid(binary(), "fooo"),
    ?invalid(binary(), 1).

map_test() ->
    Model = lee:base_model(),
    T = map(atom(), string()),
    ?valid(T, #{}),
    ?valid(T, #{foo => "bar"}),
    ?invalid(T, #{ foo => "bar"
                 , "bar" => foo
                 , baz => "quux"
                 }),
    ?invalid(T, #{ foo => 1
                 , bar => "bar"
                 }).

exact_map_test() ->
    Model = lee:base_model(),
    T = exact_map(#{ foo => boolean()
                   , bar => string()
                   }),
    ?valid(T, #{foo => true, bar => "bar"}),
    ?valid(T, #{foo => false, bar => []}),
    ?invalid(T, #{foo => foo, bar => "bar"}),
    ?invalid(T, #{foo => true}),
    ?invalid(T, #{foo => true, bar => 1}).

typedef_test() ->
    StupidList =
        fun(A) ->
                {[stupid_list], #{}, [A]}
        end,
    Model0 = #{stupid_list =>
                   {[typedef]
                   , #{ type => union( tuple([{var, '$a'}, StupidList({var, '$a'})])
                                     , nil
                                     )
                      , type_variables => ['$a']
                      , typename => "stupid_list"
                      }
                   , #{}
                   }
              },
    {ok, Model} = lee_model:merge(Model0, lee:base_model()),
    T = StupidList(union(integer(), foo)),
    ?valid(T, nil),
    ?valid(T, {1, nil}),
    ?valid(T, {foo, {1, nil}}),
    ?valid(T, {42, {foo, {1, nil}}}),
    ?invalid(T, bar),
    ?invalid(T, 1.1),
    ?invalid(T, {1, foo}),
    ?invalid(T, {foo, {42, {1.1, nil}}}).

typedef_2_test() ->
    Model0 = #{foo => {[typedef]
                      , #{ type => {var, '1'}
                         , type_variables => ['1']
                         , tupename => "foo"
                         }
                      , #{}
                      }},
    {ok, Model} = lee_model:merge(Model0, lee:base_model()),
    T = fun(A) -> {[foo], #{}, [A]} end,
    ?valid(T(boolean()), true),
    ?valid(T(boolean()), false),
    ?invalid(T(boolean()), 1).

-endif.
