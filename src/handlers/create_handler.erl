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
        % check for valid x-access-token here. tried to do 
        % this in init but cowboy really didn't like it
        the_bouncer:checks_guestlist(Req, State, <<"10">>),

	{ok, Body, Req2} = cowboy_req:read_body(Req),
	BodyDecoded = jsx:decode(Body),
	% not 100% sure if we need public_id - i think the checkaccess
	% call validates user against correct uri. i'll leave it for now
	PublicID = misc:find_value(<<"public_id">>, BodyDecoded),
	AuctionID = misc:find_value(<<"auction_id">>, BodyDecoded),

	{RetCode, Resp} = case {PublicID, AuctionID} of
		{false, _} -> create_bad_response();
		{_, false} -> create_bad_response();
		_ -> build_auction(PublicID, AuctionID)
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

build_auction(PublicID, AuctionID) ->
	erlang:display("---- create_handler:create_response/3 ----"),
	erlang:display(PublicID),

	%ValidAuctionCode = misc:valid_auction_id(AuctionID),
	%{RetCode, Resp} = case ValidAuctionCode of
	{RetCode, Resp} = case misc:valid_auction_id(AuctionID) of
		200 -> the_postman:create_exchange_and_queue(AuctionID);
		$_ -> create_bad_response()
	end,
	
	{RetCode, Resp}.

%------------------------------------------------------------------------------

%------------------------------------------------------------------------------

%------------------------------------------------------------------------------

