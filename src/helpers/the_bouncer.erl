-module(the_bouncer).

-export([checks_guestlist/3,
	 checks_socket_guestlist/1]).

%------------------------------------------------------------------------------

checks_guestlist(Req, Opts, Level) ->
	erlang:display("---- the_bouncer:checks_guestlist/2 ----"),
        XAccessToken = cowboy_req:header(<<"x-access-token">>, Req, ''),

	case XAccessToken of
		'' -> bad_dog(Req, Opts);
		_ -> good_boye(XAccessToken, Level, Req, Opts)
        end.

%------------------------------------------------------------------------------
% we check the jwt against the authy microservice just once when using 
% websockets. if successfull we store the jwt in ets. 
% all subsequent messages from the client websocket will be checked to see if
% the jwt exists in ets. this saves on repeated calls to authy. should be
% faster.

checks_socket_guestlist(XAccessToken) ->
        erlang:display("---- the_bouncer:checks_socket_guestlist/1 ----"),

	case token_in_ets(XAccessToken) of
		true -> true;
		false -> check_authy(XAccessToken)
	end.
	
%------------------------------------------------------------------------------
% websocket auth call to authy
check_authy(XAccessToken) ->
	erlang:display("---- the_bouncer:check_authy/1 ----"),
        {ok, BaseURL} = application:get_env(auctioneer, authy_url),
        URL = [BaseURL, <<"10">>],
        Headers = [{<<"Content-Type">>, <<"application/json">>},
                   {<<"x-access-token">>, XAccessToken}],
        Payload = <<>>,
        Options = [],
        {ok, StatusCode, _, _} = hackney:request(get, URL,
                                                 Headers, Payload,
                                                 Options),      
        Result = case StatusCode of
                200 -> store_in_ets(XAccessToken);
                401 -> false;
                _ -> false
        end,
	
	Result.	

%------------------------------------------------------------------------------

token_in_ets(XAccessToken) ->
        erlang:display("---- the_bouncer:token_in_ets/1 ----"),
	erlang:display(ets:whereis(jwttable)),
	case ets:whereis(jwttable) of
		undefined -> false;
		_ -> proper_lookup(XAccessToken)
	end.

%------------------------------------------------------------------------------

proper_lookup(XAccessToken) -> 
	erlang:display("---- the_bouncer:proper_lookup/1 ----"),
        List = ets:lookup(jwttable, XAccessToken),
        case erlang:length(List) of
                0 -> false;
                _ -> true
        end.

%------------------------------------------------------------------------------

store_in_ets(XAccessToken) ->
	erlang:display("---- the_bouncer:store_in_ets/1 ----"),
	case ets:whereis(jwttable) of
		undefined -> create_table(XAccessToken);
		_ -> put_in_table(XAccessToken)
	end.
%------------------------------------------------------------------------------

put_in_table(XAccessToken) ->
	erlang:display("---- the_bouncer:put_in_table/1 ----"),
	% use key of XAccessToken value 
	erlang:display(XAccessToken),
	ets:insert(jwttable, {XAccessToken, fab}),
	true.

%------------------------------------------------------------------------------

create_table(XAccessToken) ->
	erlang:display("---- the_bouncer:create_table/1 ----"),
	% store as a set with key being the token - don't really care about the 
	% value so set it to ok
	ets:new(jwttable, [set, public, named_table]),
	put_in_table(XAccessToken).

%------------------------------------------------------------------------------

good_boye(XAccessToken, Level, Req, Opts) ->
	erlang:display("---- the_bouncer:good_boye/3 ----"),
	% get auth url from app config
	{ok, BaseURL} = application:get_env(auctioneer, authy_url),
	URL = [BaseURL, Level],
        Headers = [{<<"Content-Type">>, <<"application/json">>},
                   {<<"x-access-token">>, XAccessToken}],
        Payload = <<>>,
        Options = [],
        {ok, StatusCode, _, _} = hackney:request(get, URL,
                                                 Headers, Payload,
                                                 Options),	
	case StatusCode of
		200 -> ok;
		401 -> bad_dog(Req, Opts);
		_ -> bad_dog(Req, Opts)
	end.

%------------------------------------------------------------------------------

bad_dog(Req, Opts) ->
	erlang:display("---- the_bouncer:bad_dog/2 ----"),
	Message = <<"{\"message\": \"Yer name's not down\"}">>,
	cowboy_req:reply(401,
                        #{<<"content-type">> => <<"application/json">>}, 
			Message, Req),
        {stop, Req, Opts}.	
	
%------------------------------------------------------------------------------
