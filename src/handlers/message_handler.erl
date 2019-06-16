-module(message_handler).
-behavior(cowboy_handler).

-export([init/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

init(Req0, State) ->
	erlang:display("---- message:init/2 ----"),	
	erlang:display(whereis(db_server)),
	%DBServerId = whereis(db_server),
	erlang:display("Me, myself and I"),
	erlang:display(self()),
	%erlang:display(DBServerId),
	%Msg = iolist_to_binary
	%DBServerId ! {self(), hello},
	%gen_server:cast(DBServerId, {get_all_recs}), 
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	{ok, Channel} = amqp_connection:open_channel(Connection),

	amqp_channel:call(Channel, #'queue.declare'{queue = <<"hello">>}),

	amqp_channel:cast(Channel,
			  #'basic.publish'{
				exchange = <<"">>,
				routing_key = <<"hello">>},
			  #amqp_msg{payload = <<"Hello World!">>}),
	ok = amqp_channel:close(Channel),
	ok = amqp_connection:close(Connection),

    	Req = cowboy_req:reply(418,
        	#{<<"content-type">> => <<"application/json">>},
        	<<"{\n\"message\": \"Run rabbit run...\"\n}">>,
        	Req0),	
	{ok, Req, State}.
