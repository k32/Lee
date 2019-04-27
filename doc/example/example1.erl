%% This module shows very basic usage of Lee.
-module(example1).

-export([main/0]).

-include_lib("lee/include/lee_types.hrl").

main() ->
    %% First of all we need to create a _data model_ from a few _model
    %% modules_. In this example we use only prepackaged modules that
    %% contain functions necessary to validate Erlang terms. Don't pay
    %% too much attention to this part yet, it will be explained in
    %% more detail later.
    {ok, Model} = lee_model:compile([lee:base_metamodel()], [lee:base_model()]),
    %% 2. Validate Erlang terms against the model:
    {ok, []} = lee:validate_term(Model, boolean(), true),
    {ok, []} = lee:validate_term(Model, boolean(), false),
    %% `true' and `false' values are indeed valid under
    %% `boolean()'... Now let's try something illegal:
    {error, ["Expected true | false, got 42"], _Warnings} =
        lee:validate_term(Model, boolean(), 42),
    %% Now let's validate some strings:
    {ok, []} = lee:validate_term(Model, string(), "Hi! ( ┛O‿‿O)┛彡♥"),
    %% Non-unicode characters are forbidden:
    {error, ["Expected char(), got -1 in list(char())"], []} =
        lee:validate_term(Model, string(), [100, -1]),
    ok.
