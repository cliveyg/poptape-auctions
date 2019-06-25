-module(status_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, Opts) ->
	erlang:display("------ status_handler:init ------"),
	%Resp = gen_server:call(db_server, {create_db}),
	%erlang:display(Resp),
	ItemID = <<"a1358e72-583a-4c45-8aea-421baf9ff9d2">>,
	Res = gen_server:call(db_server, {get_rec, ItemID}),
	erlang:display(Res),
    	Req = cowboy_req:reply(200,
        	#{<<"content-type">> => <<"application/json">>},
        	<<"{\n\"message\": \"Auctioneer running...\"\n}">>,
        	Req0),	
	{ok, Req, Opts}.
