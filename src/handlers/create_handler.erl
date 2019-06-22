-module(create_handler).
%-

-export([init/2,
	 content_types_accepted/2,
	 json_post/2,
	 allowed_methods/2]).

%------------------------------------------------------------------------------

init(Req, State) ->
	erlang:display("---- create_handler:init/2 ----"),
	{cowboy_rest, Req, State}.

%------------------------------------------------------------------------------

allowed_methods(Req, State) ->
	erlang:display("---- create_handler:allowed_methods/2 ----"),
	{[<<"POST">>], Req, State}.

%------------------------------------------------------------------------------

content_types_accepted(Req, State) ->
	erlang:display("---- create_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, json_post}
  	], Req, State}.

%------------------------------------------------------------------------------

json_post(Req, State) ->
	erlang:display("---- create_handler:json_post/2 ----"),
	erlang:display(os:timestamp()),
        % check for valid x-access-token here. tried to do 
        % this in init but cowboy really didn't like it
        the_bouncer:checks_guestlist(Req, State, <<"10">>),

	{ok, Body, Req2} = cowboy_req:read_body(Req),
	BodyDecoded = jsx:decode(Body),
	% not 100% sure if we need public_id - i think the checkaccess
	% call validates user against correct uri. i'll leave it for now
	Username = misc:find_value(<<"username">>, BodyDecoded),
	ItemID = misc:find_value(<<"item_id">>, BodyDecoded),
	Bid = misc:find_value(<<"bid">>, BodyDecoded),
	AuctionID = cowboy_req:binding(auction_id, Req),

	{RetCode, Resp} = case {Username, ItemID} of
		{false, _} -> create_bad_response();
		{_, false} -> create_bad_response();
		_ -> build_auction_messaging(Username, ItemID, AuctionID, Bid)
	end,

        Req3 = cowboy_req:set_resp_body(Resp, Req2),
	Req4 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req3),
	cowboy_req:reply(RetCode,Req4),

  	{stop, Req4, State}.

%------------------------------------------------------------------------------

create_bad_response() ->
	erlang:display("---- create_handler:create_response/0 ----"),

	Resp2 = "{ \"messaqe\": \"Could not create auction instance. "
       				 "Missing or incorrect parameter(s)\" }",
	{422, Resp2}.

%------------------------------------------------------------------------------

build_auction_messaging(Username, ItemID, AuctionID, Bid) ->
	erlang:display("---- create_handler:create_response/3 ----"),

	%{RetCode, Channel, Connection, Message} = case misc:valid_auction_id(ItemID) of
	{RetCode, Channel, _, Message} = case misc:valid_auction_id(ItemID) of
		200 -> the_postman:create_exchange_and_queue(Username, ItemID, AuctionID);
		$_ -> create_bad_response()
	end,

	%TODO: This is where we need to publish the opening bid and put it in our bag
	% publish opening message
	Payload = <<"{ \"foo\": \"bar\" }">>,
	erlang:display(Bid),
	the_postman:publish_message(Channel, ItemID, Payload),
	P = spawn_link(the_listener, main, [Channel, Username]),
	erlang:display(P),
	erlang:display("=-=-=-=-=-=-=-=-=-=-=-="),


	%the_postman:close_all(Channel, Connection),
	
	{RetCode, Message}.

%------------------------------------------------------------------------------

%------------------------------------------------------------------------------

%------------------------------------------------------------------------------

