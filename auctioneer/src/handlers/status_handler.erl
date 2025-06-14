-module(status_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, Opts) ->
	%erlang:display("------ status_handler:init ------"),
    Req = cowboy_req:reply(200,
       	#{<<"content-type">> => <<"application/json">>},
       	<<"{\n\"message\": \"Auctioneer running...\"\n, \"version\": \"0.2.5\"\n }">>,
       	Req0),	
	{ok, Req, Opts}.
