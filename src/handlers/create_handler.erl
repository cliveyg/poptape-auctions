-module(create_handler).
%-

-export([init/2,
		 content_types_accepted/2,
		 content_types_provided/2,
         forbidden/2,
         is_authorized/2,
		 json_post/2,
		 json_get/2,
		 allowed_methods/2]).

%------------------------------------------------------------------------------

init(Req, Opts) ->
	erlang:display("---- create_handler:init/2 ----"),
	{cowboy_rest, Req, Opts}.

%------------------------------------------------------------------------------

allowed_methods(Req, Opts) ->
	erlang:display("---- create_handler:allowed_methods/2 ----"),
	{[<<"POST">>], Req, Opts}.

%------------------------------------------------------------------------------

is_authorized(Req, Opts) ->
    erlang:display("---- is_authorized/2 ----"),
    case the_bouncer:checks_guestlist(Req, <<"10">>) of
        200 -> {true, Req, Opts};
        _ -> {{false, <<"">>}, Req, Opts}
    end.

%------------------------------------------------------------------------------

%TODO: Check that lot already has record in db (to show that we've already
%      created an auctioneer instance for this lot
forbidden(Req, Opts) ->
    erlang:display("---- forbidden/2 ----"),

    LotID = cowboy_req:binding(lot_id, Req, false),    
    AuctionID = cowboy_req:binding(auction_id, Req, false),
    XAccessToken = cowboy_req:header(<<"x-access-token">>, Req, ''),

    case misc:valid_auction_item(AuctionID, LotID, XAccessToken) of
        200 -> {false, Req, Opts};
        _ -> {true, Req, Opts} 
    end.

%------------------------------------------------------------------------------

content_types_provided(Req, Opts) ->
    erlang:display("---- create_handler:content_types_provided/2 ----"),
    {[
            {{<<"application">>, <<"json">>, []}, json_get}
    ], Req, Opts}.

%------------------------------------------------------------------------------

content_types_accepted(Req, Opts) ->
	erlang:display("---- create_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, json_post}
  	], Req, Opts}.

%------------------------------------------------------------------------------

json_get(Req, Opts) ->
    erlang:display("---- create_handler:json_get/2 ----"),
	{stop, Req, Opts}.

%------------------------------------------------------------------------------

json_post(Req, Opts) ->
	erlang:display("---- create_handler:json_post/2 ----"),
    
	{ok, Body, Req2} = cowboy_req:read_body(Req),

	BodyDecoded = jsx:decode(Body),
	Username = misc:find_value(<<"username">>, BodyDecoded),
    LotID = cowboy_req:binding(lot_id, Req, false),
	StartPrice = misc:find_value(<<"start_price">>, BodyDecoded),
    StartTime = misc:find_value(<<"start_time">>, BodyDecoded),
	EndTime = misc:find_value(<<"end_time">>, BodyDecoded),
	AuctionID = cowboy_req:binding(auction_id, Req, false),
    XAccessToken = cowboy_req:header(<<"x-access-token">>, Req, ''),

    % get the public id from the jwt
    SplitToken = binary:split(XAccessToken, <<".">>, [global]),
    UserData = lists:nth(2, SplitToken),
    Base64Decoded = bass64url:decode(UserData),
    JWTPayload = jsx:decode(Base64Decoded),
    PublicID = misc:find_value(<<"public_id">>, JWTPayload),

	% matrix for checking the needed fields exist
	{RetCode, Message} = case {Username, LotID, StartPrice, 
                                  AuctionID, StartTime, EndTime} of
		{false, _, _, _, _, _} -> create_bad_response();
		{_, false, _, _, _, _} -> create_bad_response();
		{_, _, false, _, _, _} -> create_bad_response();
		{_, _, _, false, _, _} -> create_bad_response();
		{_, _, _, _, false, _} -> create_bad_response();
        {_, _, _, _, _, false} -> create_bad_response();		     
		_ -> build_auction_messaging(Username, LotID, 
								     AuctionID, StartPrice, StartTime, 
                                     EndTime, PublicID)
	end,

    Req3 = cowboy_req:set_resp_body(Message, Req2),
	Req4 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req3),
	cowboy_req:reply(RetCode,Req4),

  	{stop, Req4, Opts}.

%------------------------------------------------------------------------------

create_bad_response() ->
	erlang:display("---- create_handler:create_bad_response/0 ----"),

	Resp2 = "{ \"messaqe\": \"Could not create auction instance. "
       				 "Missing or incorrect parameter(s)\" }",
	{422, Resp2}.

%------------------------------------------------------------------------------

build_auction_messaging(Username, LotID, AuctionID, StartPrice,
                        StartTime, EndTime, PublicID) ->
	erlang:display("---- create_handler:build_auction_messaging/4 ----"),

    {RetCode, Channel} = the_postman:create_exchange_and_queues(Username, LotID),

	Q2 = misc:binary_join([LotID, Username], <<"_">>),

    %Maybe spawn seperate processes for these ops as they can run parallel
	UnixTime = misc:get_milly_time(),
    Payload = [{username, Username},
               {public_id, PublicID},
               {item_id, LotID},
			   {auction_id, AuctionID},
			   {exchange, LotID},
			   {queues, [Username, Q2]},
			   {price, StartPrice},
               {start_time, StartTime},
			   {end_time, EndTime},
			   {unixtime, UnixTime},
		   	   {status, <<"running">>},
               {message, <<"Your starting price">>}],

    JsonPayload = jsx:encode(Payload),

	% publish first message direct to audit and auctionhouse queues
    % and second to exchange
	the_postman:publish_direct_to_queue(Channel, LotID, JsonPayload),
	
	MinDetails = [{username, Username},
			      {lot_id, LotID},
			      {bid, StartPrice},
                  {start_time, StartTime},
		    	  {end_time, EndTime},
			      {unixtime, UnixTime},
			      {status, <<"open">>},
		    	  {message, <<"Opening price">>},
		      	  {auction_id, AuctionID}],
	JsonDetails = jsx:encode(MinDetails),
	the_postman:publish_message(Channel, LotID, JsonDetails),

    erlang:display("Messages published..."),

    % put starting bid in the db
    gen_server:call(db_server, {create_db}),
    gen_server:call(db_server, {create_rec, LotID, MinDetails}),

    erlang:display("Record created in db_table"),
	
	{RetCode, JsonDetails}.

%------------------------------------------------------------------------------

