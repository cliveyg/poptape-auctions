-module(loginms_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
	{ok, _, _, NoDecode} = json_http_request:get_json(<<"https://poptape.club/login/status">>,0,[]),
	erlang:display(NoDecode),
	Listy = erlang:binary_to_list(NoDecode),
	erlang:display(Listy),
    	Req = cowboy_req:reply(200,
        	#{<<"content-type">> => <<"application/json">>},
		NoDecode,
        	Req0),	
	{ok, Req, State}.
