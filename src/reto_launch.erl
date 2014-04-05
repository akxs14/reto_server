-module(reto_launch).

-export([start/0]).

start() ->
  ok = application:start(reto_server).
