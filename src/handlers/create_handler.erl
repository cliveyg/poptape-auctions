-module(create_handler).
%-

-export([init/2,
	 content_types_accepted/2,
	 json_post/2,
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
	AuctionID = cowboy_req:binding(auction_id, Req, false),

	{RetCode, Message} = case {Username, ItemID, StartPrice, AuctionID} of
		{false, _, _, _} -> create_bad_response();
		{_, false, _, _} -> create_bad_response();
		{_, _, false, _} -> create_bad_response();
		{_, _, _, false} -> create_bad_response();
		_ -> build_auction_messaging(Username, ItemID, AuctionID, StartPrice)
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

build_auction_messaging(Username, ItemID, AuctionID, StartPrice) ->
	erlang:display("---- create_handler:build_auction_messaging/4 ----"),

	%{RetCode, Channel, Connection, Message} = case misc:valid_auction_id(ItemID) of
	{RetCode, Message, Channel} = case misc:valid_auction_id(ItemID) of
		200 -> the_postman:create_exchange_and_queues(Username, ItemID, AuctionID, StartPrice);
		$_ -> create_bad_response()
	end,

	erlang:display(Channel),
	ListenPID = spawn_link(the_listener, main, [Channel, Username, self()]),
	erlang:display("listen pid is..."),
	erlang:display(ListenPID),
	%the_postman:close_all(Channel, Connection),
	
	{RetCode, Message}.

%------------------------------------------------------------------------------

