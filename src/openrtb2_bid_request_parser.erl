-module(openrtb2_bid_request_parser).

-export([parse/1]).

%% ===================================================================
%% API functions
%% ===================================================================

parse(JsonBidReq) ->
  {DecodedBidReq} = jiffy:decode(JsonBidReq),
  ParsedBidReq = #{
    id => get_id(DecodedBidReq),
    at => get_auction_type(DecodedBidReq),
    tmax => get_max_time(DecodedBidReq),
    allimps => get_all_impressions(DecodedBidReq),
    imp => get_impressions(DecodedBidReq)
  },
  io:format("~p~n",[ParsedBidReq]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% parse the root object
get_id(DecodedBidReq) ->
  proplists:get_value(<<"id">>,DecodedBidReq, none).

get_auction_type(DecodedBidReq) ->
  proplists:get_value(<<"at">>,DecodedBidReq, none).

get_max_time(DecodedBidReq) ->
  proplists:get_value(<<"tmax">>,DecodedBidReq, none).

get_all_impressions(DecodedBidReq) ->
  proplists:get_value(<<"allimps">>,DecodedBidReq, none).

get_impressions(DecodedBidReq) ->
   JsonImps = proplists:get_value(<<"imp">>,DecodedBidReq, none),
   parse_impressions(JsonImps,[]).

%% parse impression objects
parse_impressions([], ParsedImps) ->
  ParsedImps;
parse_impressions([{HeadImp} | JsonImps], ParsedImps) ->
  parse_impressions(JsonImps, [parse_impression(HeadImp)|ParsedImps]).

parse_impression(DecodedImp) ->
  #{
    id => get_id(DecodedImp),
    banner => get_banner(DecodedImp),
    displaymanager => get_display_manager(DecodedImp),
    displaymanagerserver => get_display_manager_server(DecodedImp),
    instl => get_interstitial(DecodedImp),
    tagid => get_tag_id(DecodedImp),
    bidfloor => get_bid_floor(DecodedImp),
    bidfloorcur => get_bid_floor_currency(DecodedImp),
    ext => get_ext(DecodedImp)
  }.

get_display_manager(DecodedImp) ->
  proplists:get_value(<<"displaymanager">>,DecodedImp, none).

get_display_manager_server(DecodedImp) ->
  proplists:get_value(<<"displaymanagerserver">>,DecodedImp, none).

get_interstitial(DecodedImp) ->
  proplists:get_value(<<"instl">>,DecodedImp, 0).

get_tag_id(DecodedImp) ->
  proplists:get_value(<<"tagid">>,DecodedImp, none).

get_bid_floor(DecodedImp) ->
  proplists:get_value(<<"bidfloor">>,DecodedImp, 0).

get_bid_floor_currency(DecodedImp) ->
  proplists:get_value(<<"bidfloorcur">>,DecodedImp, <<"USD">>).

get_ext(DecodedImp) ->
  proplists:get_value(<<"ext">>,DecodedImp, none).

%% parse banner objects
get_banner(DecodedImp) ->
  {DecodedBanner} = proplists:get_value(<<"banner">>,DecodedImp, none),
  case DecodedBanner of
    none ->
      #{};
    _ ->
      #{
        w => get_width(DecodedBanner),
        h => get_height(DecodedBanner),
        id => get_id(DecodedBanner),
        pos => get_position(DecodedBanner),
        topframe => get_topframe(DecodedBanner),
        btype => get_blocked_creative_types(DecodedBanner),
        battr => get_blocked_creative_attributes(DecodedBanner),
        mimes => get_mime_whitelist(DecodedBanner),
        expdir => get_expandable_ad_properties(DecodedBanner),
        api => get_banner_api(DecodedBanner)
      }
  end.

get_width(DecodedBanner) ->
  proplists:get_value(<<"w">>,DecodedBanner, none).

get_height(DecodedBanner) ->
  proplists:get_value(<<"h">>,DecodedBanner, none).

get_position(DecodedBanner) ->
  proplists:get_value(<<"pos">>,DecodedBanner, none).

get_topframe(DecodedBanner) ->
  proplists:get_value(<<"topframe">>,DecodedBanner, 0).

get_blocked_creative_types(DecodedBanner) ->
  proplists:get_value(<<"btype">>,DecodedBanner, []).

get_blocked_creative_attributes(DecodedBanner) ->
  proplists:get_value(<<"battr">>,DecodedBanner, []).

get_mime_whitelist(DecodedBanner) ->
  proplists:get_value(<<"mimes">>,DecodedBanner, []).

get_expandable_ad_properties(DecodedBanner) ->
  proplists:get_value(<<"expdir">>,DecodedBanner, []).

get_banner_api(DecodedBanner) ->
  proplists:get_value(<<"api">>,DecodedBanner, []).



