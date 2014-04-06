-module(openrtb2_bid_request_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    BidReq = openrtb2_bid_request_parser:parse(Req),
    SelectedAd = decision_engine:decide(BidReq),    
    BidResponse = prepare_bid_response(SelectedAd),
    {ok, Req2} = cowboy_req:reply(200, [], <<"Hello world from the openrtb2 bid request handler!">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

prepare_bid_response(SelectedAd) ->
  ok.
