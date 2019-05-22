-module(the_bouncer).

-export([on_the_list/1]).

on_the_list(Req0) ->

        XAccessToken = cowboy_req:header(<<"x-access-token">>, Req0),
	erlang:display("---- the_bouncer:on_the_list/2 ----"),

        case XAccessToken of
		% no x-access-token header at all
		undefined -> {<<"{\"message\": \"Bad doggy!\"}">>, 401};
		% x-access-token exists but is empty
		<<"null">> -> {<<"{\"message\": \"Bad doggy!\"}">>, 401};
		% x-access-token exists
		XAccessToken -> good_doggy(XAccessToken)
        end.

good_doggy(XAccessToken) ->
	erlang:display("---- the_bouncer:good_doggy/3 ----"),
        %erlang:display(XAccessToken),

        Method = get,
        URL = "https://poptape.club/login/checkaccess/10",
        Headers = [{<<"Content-Type">>, <<"application/json">>},
                   {<<"x-access-token">>, XAccessToken}],
        Payload = <<>>,
        Options = [],
        {ok, StatusCode, _, _} = hackney:request(Method, URL,
                                                         Headers, Payload,
                                                         Options),	

	erlang:display(StatusCode),

	{<<"{\"message\": \"Good boye!\"}">>, 200}.

%replies(Code, Body, Req5) ->
%	cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Body), Req5).
