-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([
    subscribe/1,
    notify/2
]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:debug("WSH INIT",[]),
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, Req, undefined_state}.

websocket_handle({text, <<"SUBSCRIBE ",SBID/binary>>}, Req, State) ->
    lager:debug("WSH SUBSCRIBE: '~p'",[SBID]),
    subscribe(SBID),
    {reply, {text, << "SUBSCRIBED:", SBID/binary >>}, Req, State};
websocket_handle({text, <<"{\"cmd\":",_/binary>>} = {text, Msg}, Req, State) ->
    lager:debug("WSH CMD: '~p'",[Msg]),
    try 
        {JSON} = jiffy:decode(Msg),
        lager:debug("WSH JSON: '~p'",[JSON]),
        case proplists:get_value(<<"cmd">>, JSON) of
            <<"subscribe">> ->
                lager:debug("WSH CMD SUBSCR:",[]),
                ChId = case proplists:get_value(<<"id">>, JSON) of
                    undefined -> get_id_by_session_cookie(Req);
                    ID        -> ID
                end,
                lager:debug("WSH CMD SUBSCR ID:~p",[ChId]),

                subscribe(ChId),
                lager:debug("WSH CMD SUBSCR OK",[]),
                Ret = jiffy:encode({[
                    {<<"status">>,<<"ok">>},
                    {<<"subscriptionid">>, ChId}
                ]}),
                {reply, {text, Ret}, Req, State}
        end
    catch
        _E:_R ->
            lager:error("WSH cmd error: ~p ~p", [_E,_R]),
            {reply, {text, <<"{\"status\":\"failed\"}">>}, Req, State}
    end;
websocket_handle({text, Msg}, Req, State) ->
    lager:debug("WSH TEXT",[]),
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    lager:debug("WSH TIMEOUT",[]),
    erlang:start_timer(20000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info({_Pid, {ws_handler, _Uid}, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    lager:debug("WSH INFO:~p",[_Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

subscribe(EventType) ->
    %% Gproc notation: {p, l, Name} means {(p)roperty, (l)ocal, Name}
    gproc:reg({p, l, {?MODULE, EventType}}).

notify(EventType, Msg) ->
    Key = {?MODULE, EventType},
    gproc:send({p, l, Key}, {self(), Key, Msg}).

get_id_by_session_cookie(Req) ->
    {MKHCookie, _} = cowboy_req:cookie(<<"MIJKSSID">>, Req),
    case rpc:call('edapi@127.0.0.1', mijkweb_session, check_session_data, [ellimcd, MKHCookie]) of
        {ok, [_,_,SData]} -> integer_to_binary(proplists:get_value(<<"accountid">>,SData, 0))
    end.

