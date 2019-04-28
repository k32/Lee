%% Storage backend based on mnesia
-module(lee_mnesia_storage).

-behavior(lee_storage).

-include("lee.hrl").

-export([create/1, get/2, patch/3]).

%%====================================================================
%% lee_storage callbacks
%%====================================================================

create(Options) ->
    TabName = maps:get(table_name, Options, lee_mnesia_storage),
    TabOpts = maps:get(table_options, Options, []),
    ok = ensure_table(TabName, TabOpts),
    TabName.

get(Key, TabName) ->
    case mnesia:read(TabName, Key) of
        [{_, Key, Val}] ->
            {ok, Val};
        [] ->
            undefined
    end.

patch(TabName, Delete, Set) ->
    case mnesia:is_transaction() of
        true ->
            patch_t(TabName, Delete, Set);
        false ->
            {atomic, Result} =
                mnesia:transaction(fun() ->
                                           patch_t(TabName, Delete, Set)
                                   end),
            Result
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% Apply a patch
patch_t(TabName, Delete, Set) ->
    [mnesia:delete({TabName, K}) || K <- Delete],
    [mnesia:write({TabName, K, V}) || {K, V} <- Set],
    TabName.

ensure_table(TabName, TabOpts0) ->
    TabOpts = [{attributes, [key, val]} | TabOpts0],
    case mnesia:create_table(TabName, TabOpts) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _}} ->
            {atomic, ok} = mnesia:clear_table(TabName),
            ok
    end.
