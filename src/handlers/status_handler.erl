-module(status_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
	erlang:display("---- status_handler:init/2 ----"),	
	erlang:display(whereis(db_server)),
	%DBServerId = whereis(db_server),
	erlang:display("Me, myself and I"),
	erlang:display(self()),
	%erlang:display(DBServerId),
	%Msg = iolist_to_binary
	%DBServerId ! {self(), hello},
	%gen_server:cast(DBServerId, {get_all_recs}), 
	Resp = gen_server:call(db_server, {get_all_recs, <<"SOME_DATA">>}),
	erlang:display("waiting for response..."),
	erlang:display(Resp),

    	Req = cowboy_req:reply(200,
        	#{<<"content-type">> => <<"application/json">>},
        	<<"{\n\"message\": \"Auctioneer running...\"\n}">>,
        	Req0),	
	{ok, Req, State}.
