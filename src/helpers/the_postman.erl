-module(the_postman).

-export([create_exchange_and_queue/1,
	 open_all/0,
	 make_queue_name/1,
	 close_all/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%------------------------------------------------------------------------------

create_exchange_and_queue(AuctionID) ->
        erlang:display("---- the_postman:create_exchange_and_queue/1 ----"),

	{Channel, Connection} = open_all(),

	ExchangeDeclare = #'exchange.declare'{exchange = AuctionID,
		     			      type = <<"fanout">>},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

	create_audit_queue(Channel, AuctionID),

	% create queue for user who created the auction instance
	#'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),
	Binding = #'queue.bind'{queue       = Queue,
				exchange    = AuctionID,
				routing_key = <<"">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

	close_all(Channel, Connection),

	Message = "{ \"messaqe\": \"Message exchange and audit queue created\" }",
	{201, Message}.

%------------------------------------------------------------------------------

open_all() ->
	{ok, RabbitConfig} = application:get_env(auctioneer, rabbitmq),
	
	Host = misc:find_value(host, RabbitConfig),
	User = misc:find_value(user, RabbitConfig),
	Pass = misc:find_value(pass, RabbitConfig),
	
        {ok, Connection} =
                amqp_connection:start(#amqp_params_network{host = Host,
							   username = User,
							   virtual_host = <<"poptape-auctions">>,
							   password = Pass}),
	{ok, Channel} = amqp_connection:open_channel(Connection),	
        
	{Channel, Connection}.	

%------------------------------------------------------------------------------

close_all(Channel, Connection) ->
        ok = amqp_channel:close(Channel),
        ok = amqp_connection:close(Connection).

%------------------------------------------------------------------------------

make_queue_name(OutType) ->
    	[A,B,C] = string:tokens(erlang:pid_to_list(self()),"<>."),
	NameAsList = "consumer-" ++ A ++ "-" ++ B ++ "-" ++ C,
	case OutType of
		s -> NameAsList;
		b -> erlang:list_to_binary(NameAsList)
	end.

%------------------------------------------------------------------------------

create_audit_queue(Channel, AuctionID) ->

        QueueNameString = make_queue_name(s),
        AuditQueueNameStr = QueueNameString++"_audit",
        AuditQueueName = erlang:list_to_binary(AuditQueueNameStr),
        RoutingKey = <<"">>,

        AuditQueueDec = #'queue.declare'{queue = AuditQueueName},
        #'queue.declare_ok'{} = amqp_channel:call(Channel, AuditQueueDec),
        Binding = #'queue.bind'{queue       = AuditQueueName,
                                exchange    = AuctionID,
                                routing_key = RoutingKey},
        #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
	ok.

%------------------------------------------------------------------------------
	
%declare_queue() ->
%	erlang:display("---- the_postman:declare_queue/0 ----"),


