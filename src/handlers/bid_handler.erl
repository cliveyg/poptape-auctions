-module(bid_handler).

-export([init/2,
  	 websocket_init/1,
	 websocket_handle/2,
	 websocket_info/2,
	 terminate/3]).

-define(SOCKET_INTERVAL, 50000).

%------------------------------------------------------------------------------

init(Req, _) ->
	erlang:display("------ bid_handler:init/2 ------"),
	%TODO: Sanitize input!
	% we capture the route info here as Req doesn't get 
	% passed on to the websocket methods
	AuctionID = cowboy_req:binding(auction_id, Req),
	ItemID = cowboy_req:binding(item_id, Req),
	Opts = [{auction_id, AuctionID}, {item_id, ItemID}],
    	{cowboy_websocket, Req, Opts, #{idle_timeout => 3000000}}.

%------------------------------------------------------------------------------

websocket_init(Opts) ->
	erlang:display("------ bid_handler:websocket_init/2 ------"),	
	% this timer is an attempt to keep our websocket connection open
	% returns to websocket_info method
	erlang:start_timer(?SOCKET_INTERVAL, self(), {ping, 1}),
    	{reply, {text, <<"{ \"message\": \"Send nudes or JWT, your choice\" }">>}, Opts}.

%------------------------------------------------------------------------------

websocket_handle({text, Json}, Opts) ->
	erlang:display("------ bid_handler:websocket_handle/2 [1] ------"),
	JsonDecoded = jsx:decode(Json),
	%TODO: Sanitize input!
        Username = misc:find_value(<<"username">>, JsonDecoded),
        BidAmount = misc:find_value(<<"bid_amount">>, JsonDecoded),
        XAccessToken = misc:find_value(<<"x-access-token">>, JsonDecoded),
	Created = misc:find_value(<<"created">>, JsonDecoded),
	InputPropList = [{username, Username},
			 {bid_amount, BidAmount},
			 {x_access_token, XAccessToken},
			 {created, Created},
			 {auction_id, proplists:get_value(auction_id, Opts)},
			 {item_id, proplists:get_value(item_id, Opts)}],

	case the_bouncer:checks_socket_guestlist(XAccessToken) of
		true -> accept_connection(InputPropList);
		false -> reject_connection([])
	end;
websocket_handle(_Frame, Opts) ->
	% this handles anything that's not text
        %erlang:display("------ bid_handler:websocket_handle/2 [3] ------"),
        {ok, Opts}.

%------------------------------------------------------------------------------

accept_connection(UserData) ->
	erlang:display("------ bid_handler:accept_connection/1 ------"),
	%TODO: maybe need to check if auction/item is valid and live
	Username = proplists:get_value(username, UserData),
	ItemID = proplists:get_value(item_id, UserData),

	{Channel, Connection} = the_postman:create_bidder_queue(Username, ItemID),

	% seperate process to listen for rabbit messages - returns 
	% any found to websocket_info method
	Queue = misc:binary_join([ItemID, Username], <<"_">>),
	spawn_link(the_listener, main, [Channel, Queue, self()]),

	%TODO read ets here
	erlang:display("WOOOO"),
	erlang:display(ItemID),
	LatestBid = ets:lookup(bidtable, ItemID),
	erlang:display(LatestBid),

	OutData = [{username, Username},
		   {bid, proplists:get_value(bid_amount, UserData)},
		   {item_id, ItemID},
		   {endtime, 12942936428},
		   {status, <<"accepted">>},
		   {message, <<"Bid accepted">>}],

	JsonPayload = jsx:encode(OutData),

	% publish to the queue to return data to user via websocket_info
	the_postman:publish_message(Channel, ItemID, JsonPayload),

	% pass these around so we can shut down 
	% the rabbitmq connection gracefully
	Opts = [{channel, Channel},{connection, Connection}],
	{ok, Opts}.

%------------------------------------------------------------------------------

reject_connection(Opts) ->
	erlang:display("------ bid_handler:reject_connection/1 ------"),
	{stop, Opts}.

%------------------------------------------------------------------------------
% websocket_info like handler_info in a genserver captures all callback outputs

websocket_info(_Info, Opts) ->
	erlang:display("------ bid_handler:websocket_info/2 ------"),

	% ping checker because websockets shuts down too soon by default
	case erlang:element(1, _Info) of
		timeout -> fetch_ping([erlang:element(3, _Info)], Opts);
		ping -> count_ping(erlang:element(2, _Info), Opts);
		rabbit_dropping -> show_me_the_money(_Info, Opts);
		'EXIT' -> cheery_bye(_Info, Opts);
		_ -> {ok, Opts}	
	end.

%------------------------------------------------------------------------------

show_me_the_money(Dropping, Opts) ->
	erlang:display("------ bid_handler:show_me_the_money/2 ------"),
	Mess = erlang:element(2,Dropping),
	{reply, {text, Mess}, Opts}.

%------------------------------------------------------------------------------

cheery_bye(_, Opts) ->
	erlang:display("------ bid_handler:cheery_bye/2 ------"),
        the_postman:close_all(proplists:get_value(channel, Opts),
                              proplists:get_value(connection, Opts)),
	Mess = <<"{ \"message\": \"right, that's it i'm done\" }">>,
	{reply, {text, Mess}, Opts}.

%------------------------------------------------------------------------------

fetch_ping(CountList, Opts) ->
	Count = proplists:get_value(ping, CountList, undefined),
        case Count of
               undefined -> {ok, Opts};
               _ -> count_ping(Count, Opts)
        end.

%------------------------------------------------------------------------------

count_ping(Count, Opts) ->
        NewCount = Count + 1,
        case NewCount of
                7 -> {stop, Opts};
                _ -> send_ping(NewCount, Opts)
        end.	

%------------------------------------------------------------------------------

send_ping(Count, Opts) ->
        erlang:send_after(?SOCKET_INTERVAL, self(), {ping, Count}),
        {reply, {ping, <<>>}, Opts}.	

%------------------------------------------------------------------------------

terminate(_, _, Opts) ->
	erlang:display("------ bid_handler:terminate/3 ------"),
	the_postman:close_all(proplists:get_value(channel, Opts),
			      proplists:get_value(connection, Opts)),
	ok.
