%% @private
-module(lee_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    lee_sup:start_link().

stop(_Pid) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
