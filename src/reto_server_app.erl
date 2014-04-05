-module(reto_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS, 100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes    = router:routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = router:port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    reto_server_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

