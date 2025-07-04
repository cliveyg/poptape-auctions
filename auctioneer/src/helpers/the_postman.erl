-module(the_postman).

-export([create_exchange_and_queues/2,
		 create_bidder_queue/2,
		 open_all/0,
		 publish_message/3,
		 publish_direct_to_queue/3,
		 fetch_message/2,
		 close_all/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%------------------------------------------------------------------------------

create_exchange_and_queues(Username, LotID) ->
    %erlang:display("---- the_postman:create_exchange_and_queue/2 ----"),

    {Channel, _} = open_all(),

	ExchangeDeclare = #'exchange.declare'{exchange = LotID,
		     			      			  type = <<"fanout">>},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

	% create queue for user who created the auction instance
	% queues name need to be unqiue and a user could have more than one queue
	% so queue name of lot and username should be unqiue
	QueueName = misc:binary_join([LotID, Username], <<"_">>),
	#'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = QueueName}),
	Binding1 = #'queue.bind'{queue       = QueueName,
	       			 		 exchange    = LotID,
				 			 routing_key = <<"">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding1),

    % create queue for the auditor
	% NOTE: Queue and Exchange both use LotID as name
	% auditor queue is durable and persists
    #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = LotID,
									    								durable = true}),
    Binding2 = #'queue.bind'{queue       = LotID,
                             exchange    = LotID,
                             routing_key = <<"">>},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding2),

    % create a third queue for the auctionhouse microservice to consume
    % this ensures that the auction/lot data is up to date
    %AuctionHouseQueue = misc:concat([LotID,<<"_auctionhouse">>]),
    AuctionHouseQueue = misc:binary_join([LotID, <<"auctionhouse">>], <<"_">>),
    #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = AuctionHouseQueue,
                                                                        durable = true}),
    Binding3 = #'queue.bind'{queue       = AuctionHouseQueue,
                             exchange    = LotID,
                             routing_key = <<"">>},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding3),

    {201, Channel}.

%------------------------------------------------------------------------------

create_bidder_queue(Username, LotID) ->
    %erlang:display("---- the_postman:create_bidder_queue/4 ----"),
    {Channel, Connection} = open_all(),

    % redeclare the exchange just in case the server went down
    ExchangeDeclare = #'exchange.declare'{exchange = LotID,
                                          type = <<"fanout">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

    % create queue for bidder
	QueueName = misc:binary_join([LotID, Username], <<"_">>),
    #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = QueueName}),
    Binding1 = #'queue.bind'{queue       = QueueName,
                             exchange    = LotID,
                             routing_key = <<"">>},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding1),

    {Channel, Connection}.

%------------------------------------------------------------------------------

publish_message(Channel, Exchange, Payload) ->
	%erlang:display("---- the_postman:publish_message/3 ----"),
    % all messages are persistent as it doesn't matter if we publish to non 
    % persistent queue/exchange as it won't persist anyway
	Publish = #'basic.publish'{exchange = Exchange, routing_key = <<"">>},
    Props = #'P_basic'{delivery_mode = 2}, %% persistent message
    Msg = #amqp_msg{props = Props, payload = Payload},
	amqp_channel:cast(Channel, Publish, Msg),
	ok.

%------------------------------------------------------------------------------

publish_direct_to_queue(Channel, QueueName, Payload) ->
    %erlang:display("---- the_postman:publish_direct_to_queue/3 ----"),

    Publish = #'basic.publish'{exchange = <<"">>, routing_key = QueueName},
    Props = #'P_basic'{delivery_mode = 2}, %% persistent message
    Msg = #amqp_msg{props = Props, payload = Payload},
    amqp_channel:cast(Channel, Publish, Msg),
    ok.

%------------------------------------------------------------------------------

fetch_message(Channel, Queue) ->
	%erlang:display("---- the_postman:fetch_message/3 ----"),
	Get = #'basic.get'{queue = Queue, no_ack = true},
	{Record, Content} = amqp_channel:call(Channel, Get),
	{Record, Content}.

%------------------------------------------------------------------------------

open_all() ->
	%erlang:display("---- the_postman:open_all ----"),
	{ok, RabbitConfig} = application:get_env(auctioneer, rabbitmq),
	
	Host = misc:find_value(host, RabbitConfig),
	User = misc:find_value(user, RabbitConfig),
	Pass = misc:find_value(pass, RabbitConfig),
	VHost = misc:find_value(virtual_host, RabbitConfig),
	%%erlang:display(VHost),

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

