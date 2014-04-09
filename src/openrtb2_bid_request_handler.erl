-module(openrtb2_bid_request_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([handle_bid_request/2]).
-export([terminate/3]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_bid_request}], Req, State}. 

content_types_accepted(Req, State) ->
  {[<<"POST">>, <<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

handle_bid_request(Req, State) ->
  Body = <<"{\"rest\": \"Hello World!\"}">>,
  io:format("handle_bid_request called\n"),
  {Body, Req, State}.

% handle(Req, State) ->
%     BidReq = openrtb2_bid_request_parser:parse(Req),
%     _SelectedAd = decision_engine:decide(BidReq),    
%     _BidResponse = prepare_bid_response(_SelectedAd),
%     {ok, Req2} = cowboy_req:reply(200, [], <<"Hello world from the openrtb2 bid request handler!">>, Req),
%     {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

% prepare_bid_response(_SelectedAd) ->
%   ok.
