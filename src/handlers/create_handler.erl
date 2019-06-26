-module(create_handler).
%-

-export([init/2,
	 content_types_accepted/2,
	 content_types_provided/2,
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

content_types_accepted(Req, Opts) ->
	erlang:display("---- create_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, json_post}
  	], Req, Opts}.

%------------------------------------------------------------------------------

content_types_provided(Req, Opts) ->
        erlang:display("---- create_handler:content_types_provided/2 ----"),
        {[
                {{<<"application">>, <<"json">>, []}, json_get}
        ], Req, Opts}.

%------------------------------------------------------------------------------

json_get(Req, Opts) ->
        erlang:display("---- create_handler:json_get/2 ----"),
	{stop, Req, Opts}.

%------------------------------------------------------------------------------

json_post(Req, Opts) ->
	erlang:display("---- create_handler:json_post/2 ----"),
	erlang:display(os:timestamp()),
        % check for valid x-access-token here. tried to do 
        % this in init but cowboy really didn't like it
        the_bouncer:checks_guestlist(Req, Opts, <<"10">>),

	{ok, Body, Req2} = cowboy_req:read_body(Req),
	BodyDecoded = jsx:decode(Body),
	% not 100% sure if we need public_id - i think the checkaccess
	% call validates user against correct uri. i'll leave it for now
	Username = misc:find_value(<<"username">>, BodyDecoded),
	ItemID = misc:find_value(<<"item_id">>, BodyDecoded),
	StartPrice = misc:find_value(<<"start_price">>, BodyDecoded),
	Endtime = misc:find_value(<<"endtime">>, BodyDecoded),
	AuctionID = cowboy_req:binding(auction_id, Req, false),

	% matrix for checking the needed fields exist
	{RetCode, Message} = case {Username, ItemID, StartPrice, AuctionID, Endtime} of
		{false, _, _, _, _} -> create_bad_response();
		{_, false, _, _, _} -> create_bad_response();
		{_, _, false, _, _} -> create_bad_response();
		{_, _, _, false, _} -> create_bad_response();
		{_, _, _, _, false} -> create_bad_response();		     
		_ -> build_auction_messaging(Username, ItemID, 
					     AuctionID, StartPrice, Endtime)
	end,

        Req3 = cowboy_req:set_resp_body(Message, Req2),
	Req4 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req3),
	cowboy_req:reply(RetCode,Req4),

  	{stop, Req4, Opts}.

%------------------------------------------------------------------------------

create_bad_response() ->
	erlang:display("---- create_handler:create_response/0 ----"),

	Resp2 = "{ \"messaqe\": \"Could not create auction instance. "
       				 "Missing or incorrect parameter(s)\" }",
	{422, Resp2}.

%------------------------------------------------------------------------------

build_auction_messaging(Username, ItemID, AuctionID, StartPrice, Endtime) ->
	erlang:display("---- create_handler:build_auction_messaging/4 ----"),

	%{RetCode, Channel, Connection} = case misc:valid_auction_id(ItemID) of
	{RetCode, Channel, _} = case misc:valid_auction_id(ItemID) of
		200 -> the_postman:create_exchange_and_queues(Username, ItemID);
		_ -> create_bad_response()
	end,

	Q2 = misc:binary_join([ItemID, Username], <<"_">>),
	% don't spawn a listener as it consumes the message and
	% can't do anything with it
	%spawn_link(the_listener, main, [Channel, Q2, self()]),

        %Maybe spawn seperate processes for these ops as they can run parallel
	UnixTime = misc:get_milly_time(),
        Payload = [{username, Username},
                   {item_id, ItemID},
                   {auction_id, AuctionID},
                   {exchange, ItemID},
                   {queues, [Username, Q2]},
                   {price, StartPrice},
                   {endtime, Endtime},
                   {unixtime, UnixTime},
		   {status, <<"running">>},
                   {message, <<"Your starting price">>}],

        JsonPayload = jsx:encode(Payload),

	% publish first message direct to audit queue and second to exchange
	the_postman:publish_direct_to_queue(Channel, ItemID, JsonPayload),
	
	MinDetails = [{username, Username},
		      {item_id, ItemID},
		      {bid, StartPrice},
		      {endtime, Endtime},
		      {unixtime, UnixTime},
		      {status, <<"open">>},
		      {message, <<"Opening price">>},
		      {auction_id, AuctionID}],
	JsonDetails = jsx:encode(MinDetails),
	the_postman:publish_message(Channel, ItemID, JsonDetails),

        % put starting bid in the db
        gen_server:call(db_server, {create_db}),
        gen_server:call(db_server, {create_rec, ItemID, MinDetails}),
	
	{RetCode, JsonDetails}.

%------------------------------------------------------------------------------

