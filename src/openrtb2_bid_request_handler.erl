-module(openrtb2_bid_request_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([handle_bid_request/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>,<<"json">>,[]}, handle_bid_request}
  ], Req, State}.

handle_bid_request(Req, State) ->
  %BidReq = openrtb2_bid_request_parser:parse(Req),
  %_SelectedAd = decision_engine:decide(BidReq),    
  %_BidResponse = prepare_bid_response(_SelectedAd),  
  {ok, Req2} = cowboy_req:reply(200, 
    [{<<"content-type">>,<<"application/json">>}], 
    <<"{\"rest\": \"Hello World!\"}">>, Req ),
  {true, Req2, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

% prepare_bid_response(_SelectedAd) ->
%   ok.
