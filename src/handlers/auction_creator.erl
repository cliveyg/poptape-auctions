-module(auction_creator).

-export([init/0]).

init() ->
	erlang:display("---- auction_creator:init/2 ----"),	
	erlang:display("db server:"),
	erlang:display(whereis(db_server)),
	erlang:display("Me, myself and I"),
	erlang:display(self()),
	%Msg = iolist_to_binary
	%DBServerId ! {self(), hello},
	%gen_server:cast(DBServerId, {get_all_recs}), 
	Resp = gen_server:call(db_server, {get_all_recs, <<"SOME_DATA">>}),
	erlang:display("waiting for response..."),
	erlang:display(Resp),
%    	Req = cowboy_req:reply(200,
%        	#{<<"content-type">> => <<"application/json">>},
%        	<<"{\n\"message\": \"Bid module running...\"\n}">>,
%        	Req0),	
	{ok}.
