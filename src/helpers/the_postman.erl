-module(the_postman).

-export([create_exchange_and_queue/2,
	 open_all/0,
	 publish_message/3,
	 fetch_message/2,
	 %make_queue_name/1,
	 close_all/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%-record(testrec, {field1, field2}). 

%------------------------------------------------------------------------------

create_exchange_and_queue(PublicID, ItemID) ->
        erlang:display("---- the_postman:create_exchange_and_queue/1 ----"),

	{Channel, Connection} = open_all(),

	ExchangeDeclare = #'exchange.declare'{exchange = ItemID,
		     			      type = <<"fanout">>},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

	%create_audit_queue(Channel, ItemID),

	% create queue for user who created the auction instance
	%#'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),
	#'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = PublicID}),
	Binding1 = #'queue.bind'{queue       = PublicID,
				exchange    = ItemID,
				routing_key = <<"">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding1),

        %QueueString = erlang:binary_to_list(PublicID),
        %NewQueueString = QueueString ++ "_audit",
        %AuditQueue = erlang:list_to_binary(NewQueueString),	
        %#'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = AuditQueue}),
        %Binding2 = #'queue.bind'{queue       = AuditQueue,
        %                        exchange    = ItemID,
        %                        routing_key = <<"">>},
        %#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding2),

	%close_all(Channel, Connection),

	Message = "{ \"messaqe\": \"Message exchange and queue created\" }",
	{201, Channel, Connection, Message}.

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

%make_queue_name(OutType) ->
%    	[A,B,C] = string:tokens(erlang:pid_to_list(self()),"<>."),
%	NameAsList = "consumer-" ++ A ++ "-" ++ B ++ "-" ++ C,
%	case OutType of
%		s -> NameAsList;
%		b -> erlang:list_to_binary(NameAsList)
%	end.

%------------------------------------------------------------------------------

%create_audit_queue(Channel, ItemID, PublicID) ->

        %QueueNameString = make_queue_name(s),
        %AuditQueueNameStr = QueueNameString++"_audit",
        %AuditQueueName = erlang:list_to_binary(AuditQueueNameStr),
%        AuditQueueDec = #'queue.declare'{queue = PublicID},
%        #'queue.declare_ok'{} = amqp_channel:call(Channel, AuditQueueDec),
%        Binding = #'queue.bind'{queue       = AuditQueueName,
%                                exchange    = ItemID,
%                                routing_key = <<"">>},
%        #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
%	ok.

%------------------------------------------------------------------------------
	
%declare_queue() ->
%	erlang:display("---- the_postman:declare_queue/0 ----"),


