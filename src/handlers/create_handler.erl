-module(create_handler).
%-

-export([init/2,
	 content_types_accepted/2,
	 %id_exists_and_is_valid/1,
	 %user_id_exists_and_is_valid/1,
	 %auction_id_exists_and_is_valid/1,
 	 %content_types_provided/2,
	 %get_json/2,
	 post_json/2,
	 %create_response/0,
	 %create_response/2,
	 %they_live/2,
	 allowed_methods/2]).

%------------------------------------------------------------------------------

init(Req, State) ->
	erlang:display("---- create_handler:init/2 ----"),
	{Message, RetCode} = the_bouncer:on_the_list(Req),

        case RetCode of
                200 -> {cowboy_rest, Req, State};
                _ -> cowboy_req:reply(RetCode, #{<<"content-type">> => <<"application/json">>}, Message, Req)
        end.

%------------------------------------------------------------------------------

allowed_methods(Req, State) ->
	%erlang:display("---- create_handler:allowed_methods/2 ----"),
	{[<<"POST">>], Req, State}.

%------------------------------------------------------------------------------

content_types_accepted(Req, State) ->
	erlang:display("---- create_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, post_json}
  	], Req, State}.

%------------------------------------------------------------------------------

%content_types_provided(Req, State) ->
%	erlang:display("---- create_handler:content_types_provided/2 ----"),
%  	{[
%    		{{<<"application">>, <<"json">>, []}, get_json}
%  	], Req, State}.

%get_json(Req, State) ->
%	erlang:display("---- create_handler:get_json/2 ----"),
%  	{<<"{ \"message\": \"registered\" }">>, Req, State}.


post_json(Req, State) ->
	erlang:display("---- create_handler:post_json/2 ----"),

	{ok, Body, Req2} = cowboy_req:read_body(Req),
	BodyDecoded = jsx:decode(Body),
	PublicID = misc:find_value(<<"public_id">>, BodyDecoded),
	AuctionID = misc:find_value(<<"auction_id">>, BodyDecoded),
	%XAccessToken = cowboy_req:parse_header(<<"x-access-token">>, Req).
	XAccessToken = cowboy_req:header(<<"x-access-token">>, Req),

	{RetCode, Resp} = case {PublicID, AuctionID} of
		{false, _} -> create_response();
		{_, false} -> create_response();
		_ -> create_response(PublicID, AuctionID, XAccessToken)
	end,

        Req3 = cowboy_req:set_resp_body(Resp, Req2),
	Req4 = cowboy_req:set_resp_header(<<"content-type">>,"application/json",Req3),
	cowboy_req:reply(RetCode,Req4),

  	{stop, Req4, State}.

%------------------------------------------------------------------------------

create_response() ->
	erlang:display("---- create_handler:create_response/0 ----"),

	Resp2 = "{ \"messaqe\": \"Could not create auction instance. Missing parameter(s)\" }",
	{422, Resp2}.

%------------------------------------------------------------------------------

create_response(PublicID, AuctionID, XAccessToken) ->
	erlang:display("---- create_handler:create_response/2 ----"),

        RetCode1 = user_id_exists_and_is_valid(PublicID, XAccessToken),
        RetCode2 = auction_id_exists_and_is_valid(AuctionID, XAccessToken),
	RetCode = case {RetCode1, RetCode2} of
		{200, 200} -> 201;
		_ -> 404
	end,

        %UUID = uuid:get_v4(),
        UnixTime = os:system_time(second),

        case RetCode of
                201 -> misc:save_auction_instance(PublicID, AuctionID, UnixTime)
        end,

	Resp = case RetCode of
		201 -> "{ \"messaqe\": \"Auction instance created\", \"created\": \""++
                 	erlang:integer_to_list(UnixTime)++"\", \"creator_id\": \""++
                 	erlang:binary_to_list(PublicID)++"\", \"auction_id\": \""++
			erlang:binary_to_list(AuctionID)++"\" }";
                 	%erlang:binary_to_list(AuctionID)++"\", \"bid_instance_id\": \""++
                 	%uuid:uuid_to_string(UUID)++"\" }";
		_ -> "{ \"messaqe\": \"Could not create auction instance. User or auction doesn\'t exist\" }"
	end,

	{RetCode, Resp}.

%------------------------------------------------------------------------------

user_id_exists_and_is_valid(PublicID, XAccessToken) ->
	erlang:display("---- create_handler:id_exists_and_is_valid/1 ----"),

        case PublicID of
                % no PublicID at all
                nothing -> 404;
                % PublicID exists but is empty
                null -> 404;
                % PublicID exists
                PublicID -> misc:valid_id(PublicID, XAccessToken, public_id)
        end.	

%------------------------------------------------------------------------------

auction_id_exists_and_is_valid(AuctionID, XAccessToken) ->
        erlang:display("---- create_handler:id_exists_and_is_valid/1 ----"),

        case AuctionID of
                % no AuctionID at all
                nothing -> 404;
                % AuctionID exists but is empty
                null -> 404;
                % AuctionID exists
                AuctionID -> misc:valid_id(AuctionID, XAccessToken, auction_id)
        end.

%------------------------------------------------------------------------------

