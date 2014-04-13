-module(openrtb2_bid_request_parser).

-export([parse/1]).

%% ===================================================================
%% API functions
%% ===================================================================

parse(JsonBidReq) ->
  {DecodedBidReq} = jiffy:decode(JsonBidReq),
  ParsedBidReq = #{
    id => get_id(DecodedBidReq),
    imp => get_impressions(DecodedBidReq),
    at => get_auction_type(DecodedBidReq),
    tmax => get_max_time(DecodedBidReq),
    wseat => get_buyer_seats(DecodedBidReq),
    allimps => get_all_impressions(DecodedBidReq),
    cur => get_allowed_currencies(DecodedBidReq),
    bcat => get_blocked_advertiser_categories(DecodedBidReq),
    badv => get_blocked_domains(DecodedBidReq),    
    ext => get_ext(DecodedBidReq)
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

get_buyer_seats(DecodedBidReq) ->
  proplists:get_value(<<"wseat">>,DecodedBidReq, []).

get_allowed_currencies(DecodedBidReq) ->
  proplists:get_value(<<"cur">>,DecodedBidReq, []).

get_blocked_advertiser_categories(DecodedBidReq) ->
  proplists:get_value(<<"bcat">>,DecodedBidReq, []).

get_blocked_domains(DecodedBidReq) ->
  proplists:get_value(<<"badv">>,DecodedBidReq, []).

get_ext(DecodedBidReq) ->
  proplists:get_value(<<"ext">>,DecodedBidReq, none).

get_all_impressions(DecodedBidReq) ->
  proplists:get_value(<<"allimps">>,DecodedBidReq, 0).

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
    banner => get_banner(<<"banner">>, DecodedImp),
    displaymanager => get_display_manager(DecodedImp),
    displaymanagerserver => get_display_manager_server(DecodedImp),
    instl => get_interstitial(DecodedImp),
    tagid => get_tag_id(DecodedImp),
    bidfloor => get_bid_floor(DecodedImp),
    bidfloorcur => get_bid_floor_currency(DecodedImp),
    ext => get_ext(DecodedImp),
    video => get_video(DecodedImp)
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


%% parse banner objects
get_banner(ElementName, DecodedImp) ->
  case proplists:lookup(ElementName,DecodedImp) of
    none ->
      #{};
    {_, {DecodedBanner}} ->
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
  proplists:get_value(<<"pos">>,DecodedBanner, unknown).

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


%% parse video objects
get_video(DecodedImp) ->
  case proplists:lookup(<<"video">>,DecodedImp) of
    none ->
      #{};
    {_, {DecodedVideo}} ->
      #{
        w => get_width(DecodedVideo),
        h => get_height(DecodedVideo),
        linearity => get_linearity(DecodedVideo),
        minduration => get_min_duration(DecodedVideo),
        maxduration => get_max_duration(DecodedVideo),
        protocol => get_protocol(DecodedVideo),
        mimes => get_mime_whitelist(DecodedVideo),
        startdelay => get_start_delay(DecodedVideo),
        sequence => get_sequence(DecodedVideo),
        battr => get_blocked_creative_attributes(DecodedVideo),
        maxextended => get_max_extended_video_duration(DecodedVideo),
        minbitrate => get_min_bitrate(DecodedVideo),
        maxbitrate => get_max_bitrate(DecodedVideo),
        boxingallowed => get_boxing_allowed(DecodedVideo),
        playbackmethod => get_playback_methods(DecodedVideo),
        delivery => get_delivery_methods(DecodedVideo),
        pos => get_position(DecodedVideo),
        companionad => get_companion_ads(DecodedVideo),
        api => get_banner_api(DecodedVideo)
      }
  end.

get_linearity(DecodedVideo) ->
  proplists:get_value(<<"linearity">>,DecodedVideo, none).

get_min_duration(DecodedVideo) ->
  proplists:get_value(<<"minduration">>,DecodedVideo, none).

get_max_duration(DecodedVideo) ->
  proplists:get_value(<<"maxduration">>,DecodedVideo, none).

get_protocol(DecodedVideo) ->
  proplists:get_value(<<"protocol">>,DecodedVideo, none).

get_start_delay(DecodedVideo) ->
  proplists:get_value(<<"startdelay">>,DecodedVideo, none).

get_sequence(DecodedVideo) ->
  proplists:get_value(<<"sequence">>,DecodedVideo, 1).

get_max_extended_video_duration(DecodedVideo) ->
  proplists:get_value(<<"maxextended">>,DecodedVideo, extension_not_allowed).

get_min_bitrate(DecodedVideo) ->
  proplists:get_value(<<"minbitrate">>,DecodedVideo, none).

get_max_bitrate(DecodedVideo) ->
  proplists:get_value(<<"maxbitrate">>,DecodedVideo, none).

get_boxing_allowed(DecodedVideo) ->
  proplists:get_value(<<"boxingallowed">>,DecodedVideo, 1).

get_playback_methods(DecodedVideo) ->
  proplists:get_value(<<"playbackmethod">>,DecodedVideo, []).

get_delivery_methods(DecodedVideo) ->
  proplists:get_value(<<"delivery">>,DecodedVideo, []).

get_companion_ads(DecodedVideo) ->
  get_banner(<<"companionad">>, DecodedVideo).

