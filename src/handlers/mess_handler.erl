-module(mess_handler).
%-

-export([init/2,
	 content_types_accepted/2,
	 json_post/2,
	 allowed_methods/2]).

%------------------------------------------------------------------------------

init(Req, State) ->
	erlang:display("---- mess_handler:init/2 ----"),
	{cowboy_rest, Req, State}.

%------------------------------------------------------------------------------

allowed_methods(Req, State) ->
	erlang:display("---- mess_handler:allowed_methods/2 ----"),
	{[<<"POST">>], Req, State}.

%------------------------------------------------------------------------------

content_types_accepted(Req, State) ->
	erlang:display("---- mess_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, json_post}
  	], Req, State}.

%------------------------------------------------------------------------------

json_post(Req, State) ->
	erlang:display("---- mess_handler:json_post/2 ----"),

	Resp = "{ \"messaqe\": \"Ooopsy doo\" }",
        Req2 = cowboy_req:set_resp_body(Resp, Req),
	Req3 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req2),
	cowboy_req:reply(417,Req3),

  	{stop, Req3, State}.

%------------------------------------------------------------------------------

%create_bad_response() ->
%	erlang:display("---- mess_handler:create_response/0 ----"),
%
%	Resp2 = "{ \"messaqe\": \"Could not create auction instance. "
%       				 "Missing or incorrect parameter(s)\" }",
%
%	{422, Resp2}.

%------------------------------------------------------------------------------

%build_auction(PublicID, AuctionID) ->
%	erlang:display("---- mess_handler:create_response/3 ----"),
%	erlang:display(PublicID),

	%ValidAuctionCode = misc:valid_auction_id(AuctionID),
	%{RetCode, Resp} = case ValidAuctionCode of
%	{RetCode, Resp} = case misc:valid_auction_id(AuctionID) of
%		200 -> the_postman:create_exchange_and_queue(AuctionID);
%		$_ -> create_bad_response()
%	end,
%	
%	{RetCode, Resp}.

%------------------------------------------------------------------------------

%------------------------------------------------------------------------------

%------------------------------------------------------------------------------

