-module(lot_handler).
%-

-export([init/2,
		 content_types_accepted/2,
		 content_types_provided/2,
         forbidden/2,
         is_authorized/2,
		 json_post/2,
		 json_get/2,
         delete_resource/2,
         delete_completed/2,
		 allowed_methods/2]).

%------------------------------------------------------------------------------

init(Req, Opts) ->
	%erlang:display("---- create_handler:init/2 ----"),
	{cowboy_rest, Req, Opts}.

%------------------------------------------------------------------------------

allowed_methods(Req, Opts) ->
	%erlang:display("---- create_handler:allowed_methods/2 ----"),
	{[<<"POST">>,<<"DELETE">>], Req, Opts}.

%------------------------------------------------------------------------------

is_authorized(Req, Opts) ->
    %erlang:display("---- is_authorized/2 ----"),
    case the_bouncer:checks_guestlist(Req, <<"10">>) of
        200 -> {true, Req, Opts};
        _ -> {{false, <<"">>}, Req, Opts}
    end.

%------------------------------------------------------------------------------

%TODO: Check that lot already has record in db (to show that we've already
%      created an auctioneer instance for this lot
forbidden(Req, Opts) ->
    %erlang:display("---- forbidden/2 ----"),

    LotID = cowboy_req:binding(lot_id, Req, false),
    HTTPMethodIsPost = lists:member(cowboy_req:method(Req), [<<"POST">>]),

    BadTing = check_resource_and_method(LotID, HTTPMethodIsPost),

    AuctionID = cowboy_req:binding(auction_id, Req, false),
    XAccessToken = cowboy_req:header(<<"x-access-token">>, Req, ''),

    {StatusCode, AuctionData} = misc:valid_auction_item(AuctionID, 
                                                        LotID, 
                                                        XAccessToken),
    Invalid = case StatusCode of
        200 -> false;
        _ -> true
    end,

    case {BadTing, Invalid} of
        {false, false} -> {false, Req, [AuctionData]};
        {false, true} -> {true, Req, Opts};
        {true, _} -> {true, Req, Opts}
    end.

%------------------------------------------------------------------------------

check_resource_and_method(LotID, HTTPMethodIsPost) ->
    %erlang:display("---- check_resource_and_method/2 ----"),

    RecExists = case gen_server:call(db_server, {db_exists}) of 
        200 -> misc:check_record_exists(LotID);
        404 -> false
    end,

    case {RecExists, HTTPMethodIsPost} of
        {true, true} -> true; % POST to existing record
        {false, false} -> true; % DELETE on non-existent resource
        {false, true} -> false; % record doesn't exist and POST
        {true, false} -> false % DELETE existing record
    end.

%------------------------------------------------------------------------------

delete_resource(Req, Opts) ->
    %erlang:display("---- delete_resource/2 ----"),

    LotID = cowboy_req:binding(lot_id, Req, false),
    case gen_server:call(db_server, {delete_rec, LotID}) of
        true -> {true, Req, Opts};
        _ -> {false, Req, Opts}
    end.

%------------------------------------------------------------------------------

delete_completed(Req, Opts) ->
    %erlang:display("---- delete_completed/2 ----"),
    
    {true, Req, Opts}.

%------------------------------------------------------------------------------

content_types_provided(Req, Opts) ->
    %erlang:display("---- create_handler:content_types_provided/2 ----"),
    {[
            {{<<"application">>, <<"json">>, []}, json_get}
    ], Req, Opts}.

%------------------------------------------------------------------------------

content_types_accepted(Req, Opts) ->
	%erlang:display("---- create_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, json_post}
  	], Req, Opts}.

%------------------------------------------------------------------------------

json_get(Req, Opts) ->
    %erlang:display("---- create_handler:json_get/2 ----"),
	{stop, Req, Opts}.

%------------------------------------------------------------------------------

json_post(Req, Opts) ->
	%erlang:display("---- create_handler:json_post/2 ----"),
    AuctionHouseData = erlang:list_to_binary(Opts),
    JsonDecoded = jsx:decode(AuctionHouseData),
    AuctionType = misc:find_value(<<"auction_type">>, JsonDecoded),
    MinChange = misc:find_value(<<"min_change">>, JsonDecoded),
    StartTimeFloat = misc:find_value(<<"start_time">>, JsonDecoded), 
    EndTimeFloat = misc:find_value(<<"end_time">>, JsonDecoded),    
    {StartTime, _} = string:to_integer(erlang:float_to_list(StartTimeFloat,[{decimals,0}])),
    {EndTime, _} = string:to_integer(erlang:float_to_list(EndTimeFloat,[{decimals,0}])),
    StartPrice = misc:find_value(<<"start_price">>, JsonDecoded),
    ReservePrice = misc:find_value(<<"reserve_price">>, JsonDecoded),
    BidHistoryExists = misc:find_value(<<"bid_history_exists">>, JsonDecoded),

	%{ok, Body, Req2} = cowboy_req:read_body(Req),
	%BodyDecoded = jsx:decode(Body),
	%Username = misc:find_value(<<"username">>, BodyDecoded),
    LotID = cowboy_req:binding(lot_id, Req, false),
	%StartPrice = misc:find_value(<<"start_price">>, BodyDecoded),
    %ReservePrice = misc:find_value(<<"reserve_price">>, BodyDecoded),
	AuctionID = cowboy_req:binding(auction_id, Req, false),
    XAccessToken = cowboy_req:header(<<"x-access-token">>, Req, ''),
    PublicID = misc:get_public_id(XAccessToken),
    Username = misc:get_username(XAccessToken),

    %TODO:
    % here needs to be more data sanitation and testing, yarrrh!

    % generate a uuid for the interaction with this microservice
    BidID = misc:get_new_uuid(),

	% matrix for checking the needed fields exist
    %TODO: Remove this as we get all data directly from the auction ms?
	{RetCode, Message} = case {Username, LotID, ReservePrice, StartPrice, 
                               AuctionID, StartTime, EndTime} of
		{false, _, _, _, _, _, _} -> create_bad_response();
		{_, false, _, _, _, _, _} -> create_bad_response();
		{_, _, false, _, _, _, _} -> create_bad_response();
		{_, _, _, false, _, _, _} -> create_bad_response();
		{_, _, _, _, false, _, _} -> create_bad_response();
        {_, _, _, _, _, false, _} -> create_bad_response();		     
        {_, _, _, _, _, _, false} -> create_bad_response();
		_ -> build_auction_messaging(Username, LotID, ReservePrice, 
								     AuctionID, StartPrice, StartTime, 
                                     EndTime, PublicID, BidID,
                                     AuctionType, MinChange, BidHistoryExists)
	end,

    Req2 = cowboy_req:set_resp_body(Message, Req),
	Req3 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req2),
	cowboy_req:reply(RetCode,Req3),

  	{stop, Req3, Opts}.

%------------------------------------------------------------------------------

create_bad_response() ->
	%erlang:display("---- create_handler:create_bad_response/0 ----"),

	Resp2 = "{ \"messaqe\": \"Could not create auction instance. "
       				 "Missing or incorrect parameter(s)\" }",
	{422, Resp2}.

%------------------------------------------------------------------------------

build_auction_messaging(Username, LotID, ReservePrice, AuctionID,
                        StartPrice, StartTime, EndTime, PublicID,
                        BidID, AuctionType, MinChange, BidHistoryExists) ->
	%erlang:display("---- create_handler:build_auction_messaging/8 ----"),

    {RetCode, Channel} = the_postman:create_exchange_and_queues(Username, LotID),

    ResMess = case ReservePrice > 0 of
        true -> <<"Reserve not met">>;
        _ -> <<"No reserve">>
    end,    

    % Maybe spawn seperate processes for these ops as they can run parallel
	UnixTime = misc:get_milly_time(),
    Payload = [{username, Username},
               {public_id, PublicID},
               {lot_id, LotID},
               {bid_id, BidID},
               {bid_history_exists, BidHistoryExists},
               {auction_type, AuctionType},
               {min_change, MinChange},
               {reserve_price, ReservePrice},
               {reserve_message, ResMess},
			   {auction_id, AuctionID},
			   {exchange, LotID},
			   {start_price, StartPrice},
               {bid_amount, StartPrice},
               {start_time, StartTime}, % should be in milliseconds
			   {end_time, EndTime}, % should be in milliseconds
			   {unixtime, UnixTime},
		   	   {lot_status, <<"created">>},
               {bid_status, <<"none">>},
               {message, <<"Initial lot data">>}],

    JsonPayload = jsx:encode(Payload),

	% publish first message direct to audit and auctionhouse queues
    % and second to exchange
	the_postman:publish_direct_to_queue(Channel, LotID, JsonPayload),
    AuctionHouseQueue = misc:binary_join([LotID, <<"auctionhouse">>], <<"_">>),
    the_postman:publish_direct_to_queue(Channel, AuctionHouseQueue, JsonPayload),

    % create a second bid id as this and prev message have slightly diff info so 
    % we treat them as unique - may chnage this behaviour in the future
    AltBidID = misc:get_new_uuid(),

	MinDetails = [{username, Username},
                  {public, PublicID},
			      {lot_id, LotID},
			      {bid_amount, StartPrice},
                  {bid_id, AltBidID},
                  {start_time, StartTime},
		    	  {end_time, EndTime},
                  {auction_type, AuctionType},
			      {unixtime, UnixTime},
                  {reserve_message, ResMess},
			      {bid_status, <<"none">>},
                  {lot_status, <<"created">>},
		    	  {message, <<"Opening price">>},
		      	  {auction_id, AuctionID}],

	JsonDetails = jsx:encode(MinDetails),
	the_postman:publish_message(Channel, LotID, JsonDetails),

    %erlang:display("Messages published..."),

    % put starting data in the db
    gen_server:call(db_server, {create_db}),
    gen_server:call(db_server, {create_rec, LotID, Payload}),

    %erlang:display("Record created in db_table"),
	
	{RetCode, JsonDetails}.

%------------------------------------------------------------------------------

