-module(the_bouncer).

-export([checks_guestlist/3]).

%------------------------------------------------------------------------------

checks_guestlist(Req, State, Level) ->
	erlang:display("---- the_bouncer:checks_guestlist/2 ----"),
        XAccessToken = cowboy_req:header(<<"x-access-token">>, Req, ''),

	case XAccessToken of
		'' -> bad_dog(Req, State);
		_ -> good_boye(XAccessToken, Level, Req, State)
        end.

%------------------------------------------------------------------------------

good_boye(XAccessToken, Level, Req, State) ->
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
		401 -> bad_dog(Req, State);
		_ -> bad_dog(Req, State)
	end.

%------------------------------------------------------------------------------

bad_dog(Req, State) ->
	erlang:display("---- the_bouncer:bad_dog/2 ----"),
	Message = <<"{\"message\": \"Yer name's not down\"}">>,
	cowboy_req:reply(401,
                        #{<<"content-type">> => <<"application/json">>}, 
			Message, Req),
        {stop, Req, State}.	
	
%------------------------------------------------------------------------------
