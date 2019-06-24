-module(the_postman).

-export([create_exchange_and_queues/4,
	 create_bidder_queue/4,
	 open_all/0,
	 publish_message/3,
	 fetch_message/2,
	 close_all/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%------------------------------------------------------------------------------

create_exchange_and_queues(Username, ItemID, AuctionID, StartPrice) ->
        erlang:display("---- the_postman:create_exchange_and_queue/4 ----"),
	erlang:display(ItemID),

	{Channel, _} = open_all(),

	ExchangeDeclare = #'exchange.declare'{exchange = ItemID,
		     			      type = <<"fanout">>},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

	% create queue for user who created the auction instance
	#'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Username}),
	Binding1 = #'queue.bind'{queue       = Username,
	       			 exchange    = ItemID,
				 routing_key = <<"">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding1),

        % create queue for the auditor
	% NOTE: Queue and Exchange both use ItemID as name
        #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ItemID}),
        Binding2 = #'queue.bind'{queue       = ItemID,
                                 exchange    = ItemID,
                                 routing_key = <<"">>},
        #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding2),

	%TODO: Maybe spawn seperate processes for these ops as they can run parallel
	UnixTime = erlang:universaltime(), 
        misc:save_my_bag(AuctionID, ItemID, Username, UnixTime, StartPrice), 
	Payload = [{username, Username}, 
		   {item_id, ItemID},
		   {auction_id, AuctionID},
                   {exchange, ItemID},
                   {queues, [Username, ItemID]},
		   {price, StartPrice},
		   {end_time, true},
		   {unix_time, UnixTime},
		   {message, <<"Opening price">>}],
	JsonPayload = jsx:encode(Payload),
	%erlang:display(JsonPayload),
        publish_message(Channel, ItemID, JsonPayload),
	%ListenPID = spawn_link(the_listener, main, [Channel, Username]),

	{201, JsonPayload, Channel}.

%------------------------------------------------------------------------------

create_bidder_queue(Username, ItemID, AuctionID, StartPrice) ->
        erlang:display("---- the_postman:create_bidder_queue/4 ----"),
	erlang:display(ItemID),
        {Channel, _} = open_all(),

        % create queue for bidder
        #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Username}),
        Binding1 = #'queue.bind'{queue       = Username,
                                 exchange    = ItemID,
                                 routing_key = <<"">>},
        #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding1),

        %TODO: Get latest bid from ets
	
	%Maybe spawn seperate processes for these ops as they can run parallel
        UnixTime = erlang:universaltime(),
        %misc:save_my_bag(AuctionID, ItemID, Username, UnixTime, StartPrice),
        Payload = [{username, Username},
                   {item_id, ItemID},
                   {auction_id, AuctionID},
		   {exchange, ItemID},
		   {queues, [Username]},
                   {price, StartPrice},
                   {end_time, true},
                   {unix_time, UnixTime},
                   {message, <<"Your bid">>}],
        
	JsonPayload = jsx:encode(Payload),
	%TODO: Get all old stuff from ets and publish to bidders new queue
        %publish_message(Channel, ItemID, JsonPayload),

        {JsonPayload, Channel}.

%------------------------------------------------------------------------------

publish_message(Channel, Exchange, Payload) ->
	erlang:display("---- the_postman:publish_message/3 ----"),
	Publish = #'basic.publish'{exchange = Exchange, routing_key = <<"">>},
	amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
	ok.

%------------------------------------------------------------------------------

fetch_message(Channel, Queue) ->
	erlang:display("---- the_postman:fetch_message/3 ----"),
	Get = #'basic.get'{queue = Queue, no_ack = true},
	{Record, Content} = amqp_channel:call(Channel, Get),
	{Record, Content}.

%------------------------------------------------------------------------------

open_all() ->
	{ok, RabbitConfig} = application:get_env(auctioneer, rabbitmq),
	
	Host = misc:find_value(host, RabbitConfig),
	User = misc:find_value(user, RabbitConfig),
	Pass = misc:find_value(pass, RabbitConfig),
	VHost = misc:find_value(virtual_host, RabbitConfig),
	erlang:display(VHost),

        {ok, Connection} =
                amqp_connection:start(#amqp_params_network{host = Host,
						   username = User,
						   virtual_host = VHost,
						   password = Pass}),
	{ok, Channel} = amqp_connection:open_channel(Connection),	
        
	{Channel, Connection}.	

%------------------------------------------------------------------------------

close_all(Channel, Connection) ->
        ok = amqp_channel:close(Channel),
        ok = amqp_connection:close(Connection).

%------------------------------------------------------------------------------

