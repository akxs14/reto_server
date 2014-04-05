-module(router).

-export([routes/0, port/0]).

routes() ->
    [
     {'_', [
            {"/", ad_request_handler, []}
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.