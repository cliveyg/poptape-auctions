-module(status_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, Opts) ->
    	Req = cowboy_req:reply(200,
        	#{<<"content-type">> => <<"application/json">>},
        	<<"{\n\"message\": \"Auctioneer running...\"\n}">>,
        	Req0),	
	{ok, Req, Opts}.
