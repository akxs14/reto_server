-module(openrtb2_bid_request_parser).

-export([parse/1]).

parse(BidRequest) ->
  {DecodedBody} = jiffy:decode(BidRequest),
  {_,Id} = proplists:lookup(<<"id">>,DecodedBody),
  io:format("~s~n",[Id]).
