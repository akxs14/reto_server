-module(decision_engine).

-export([decide/1]).

decide(BidReq) ->
  Ads = candidateAds(BidReq),
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

candidateAds(BidReq) ->
  [].

calculateCTR(Ad) ->
  {Ad,0}.


