-module(edapi_ws_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/_ws", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 3036}],
        [{env, [{dispatch, Dispatch}]}]),
    edapi_ws_sup:start_link().

stop(_State) ->
    ok.
