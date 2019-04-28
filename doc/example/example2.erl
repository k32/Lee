%% This module shows how type reflection works
-module(example2).

-export([main/0]).

-include_lib("lee/include/lee_types.hrl").

-type stupid_list(A) :: {cons, A, stupid_list(A)}
                      | nil.

-type mylist(A) :: stupid_list(A)
                 | list(A).

-type mylist() :: mylist(term()).

main() ->
    %% Generate a _model module_ containing mylist types:
    MyModule = lee:type_refl([my, model], [mylist/0, mylist/1]),
    %% Merge it with the Lee base models:
    {ok, Model} = lee_model:compile( [lee:base_metamodel()]
                                   , [lee:base_model(), MyModule]
                                   ),
    {ok, []} = lee:validate_term(Model, mylist(atom()), [foo, bar]),
    {ok, []} = lee:validate_term(Model, mylist(), {cons, foo, {cons, bar, nil}}),
    {error, [Error], _Warnings} = lee:validate_term(Model, mylist(atom()), {cons, foo, {cons, 1, nil}}),
    io:put_chars(Error).
