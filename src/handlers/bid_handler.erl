-module(bid_handler).
%-behavior(cowboy_handler).

-export([init/2,
	allowed_methods/2,
	content_types_accepted/2,
	content_types_provided/2,
	get_json/2,
	post_json/2]).

init(Req0, State) ->
	erlang:display("---- bid_handler:init/2 ----"),
	%AuctionID = cowboy_req:binding(auction_id,Req0),
	%Message = "{\"message\": \"Bid handler\", \"auction_id\": \""++erlang:binary_to_list(AuctionID)++"\"}",
	%{Message, RetCode} = the_bouncer:on_the_list(Req),

        %case RetCode of
        %        200 -> {cowboy_rest, Req, State};
        %        _ -> cowboy_req:reply(RetCode, #{<<"content-type">> => <<"application/json">>}, Message, Req)
        %end.
	
        %Req1 = cowboy_req:set_resp_body(Message, Req0),
        %Req2 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req1),

	{cowboy_rest, Req0, State}.


allowed_methods(Req, State) ->
        erlang:display("---- bid_handler:allowed_methods/2 ----"),
        {[<<"POST">>, <<"GET">>], Req, State}.


content_types_accepted(Req, State) ->
        %erlang:display("---- bid_handler:content_types_accepted/2 ----"),
        {[
                {{<<"application">>, <<"json">>, []}, post_json}
        ], Req, State}.


content_types_provided(Req, State) ->
       erlang:display("---- bid_handler:content_types_provided/2 ----"),
       {[
               {{<<"application">>, <<"json">>, []}, get_json}
       ], Req, State}.


get_json(Req, State) ->
        erlang:display("---- bid_handler:get_json/2 ----"),
        AuctionID = cowboy_req:binding(auction_id,Req),
        Message = "{\"message\": \"Bid handler\", \"auction_id\": \""++erlang:binary_to_list(AuctionID)++"\"}",

        Req1 = cowboy_req:set_resp_body(Message, Req),
        Req2 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req1),
        cowboy_req:reply(418,Req2),

        {stop, Req2, State}.


post_json(Req, State) ->
        erlang:display("---- bid_handler:post_json/2 ----"),
	%Req2 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",<<"{ \"foo\": \"bar\" }">>),
	{ok, Req, State}.

