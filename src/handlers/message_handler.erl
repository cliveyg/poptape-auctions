-module(message_handler).
%-

-export([init/2,
	 content_types_accepted/2,
	 json_post/2,
	 allowed_methods/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%------------------------------------------------------------------------------

init(Req, State) ->
	erlang:display("---- message_handler:init/2 ----"),
	{cowboy_rest, Req, State}.

%------------------------------------------------------------------------------

allowed_methods(Req, State) ->
	erlang:display("---- message_handler:allowed_methods/2 ----"),
	{[<<"POST">>], Req, State}.

%------------------------------------------------------------------------------

%content_types_provided(Req, State) ->
%	erlang:display("---- message_handler:content_types_provided/2 ----"),
%        {[
%                {{<<"application">>, <<"json">>, []}, create_response}
%        ], Req, State}.

%------------------------------------------------------------------------------

content_types_accepted(Req, State) ->
	erlang:display("---- message_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, json_post}
  	], Req, State}.

%------------------------------------------------------------------------------

json_post(Req, State) ->
	erlang:display("---- message_handler:json_post/2 ----"),

        {ok, Body, Req2} = cowboy_req:read_body(Req),
        BodyDecoded = jsx:decode(Body),
        PublicID = misc:find_value(<<"public_id">>, BodyDecoded),

	{Channel, Connection} = the_postman:open_all(),
	{_, Content} = the_postman:fetch_message(Channel, PublicID),
	
	Payload = case erlang:is_record(Content,'amqp_msg') of
		false -> <<"{ \"message\": \"No more messages in queue\" }">>;
		true -> Content#amqp_msg.payload
	end,
	
	the_postman:close_all(Channel, Connection),
	create_response(Req2, State, Payload).

%------------------------------------------------------------------------------

%create_response(Req, State) ->
%
%        Resp = "{ \"messaqe\": \"Ooopsy doo\" }",
%        Req2 = cowboy_req:set_resp_body(Resp, Req),
%        Req3 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req2),
%        cowboy_req:reply(417,Req3),
%
%        {stop, Req3, State}.

%------------------------------------------------------------------------------

create_response(Req, State, Message) ->
	erlang:display("---- message_handler:create_response/3 ----"),
	%socket_handler:websocket_info("WOO",State),
        Req2 = cowboy_req:set_resp_body(Message, Req),
        Req3 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req2),
        cowboy_req:reply(417,Req3),

        {stop, Req3, State}.

%------------------------------------------------------------------------------



%create_bad_response() ->
%	erlang:display("---- message_handler:create_response/0 ----"),
%
%	Resp2 = "{ \"messaqe\": \"Could not create auction instance. "
%       				 "Missing or incorrect parameter(s)\" }",
%
%	{422, Resp2}.

%------------------------------------------------------------------------------

%build_auction(PublicID, AuctionID) ->
%	erlang:display("---- message_handler:create_response/3 ----"),
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

