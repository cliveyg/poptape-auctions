-module(sell_handler).

-export([init/2,
		 websocket_init/1,
		 websocket_handle/2,
		 websocket_info/2,
		 terminate/3]).

-define(SOCKET_INTERVAL, 50000).

%------------------------------------------------------------------------------

init(Req, _) ->
	%erlang:display("------ sell_handler:init/2 ------"),
	%TODO: Sanitize input!
	% we capture the route info here as Req doesn't get 
	% passed on to the websocket methods
	AuctionID = cowboy_req:binding(auction_id, Req),
	LotID = cowboy_req:binding(lot_id, Req),
	Opts = [{auction_id, AuctionID}, {lot_id, LotID}],
    {cowboy_websocket, Req, Opts, #{idle_timeout => 3000000}}.

%------------------------------------------------------------------------------

websocket_init(Opts) ->
	%erlang:display("------ sell_handler:websocket_init/2 ------"),	
	% this timer is an attempt to keep our websocket connection open
	% returns to websocket_info method
	erlang:start_timer(?SOCKET_INTERVAL, self(), {ping, 1}),
    {reply, {text, <<"{ \"message\": \"Auctioneer calling\" }">>}, Opts}.

%------------------------------------------------------------------------------

websocket_handle({text, Json}, Opts) ->
	%erlang:display("------ sell_handler:websocket_handle/2 [1] ------"),
	JsonDecoded = jsx:decode(Json),
    XAccessToken = misc:find_value(<<"x-access-token">>, JsonDecoded),
    LotID = proplists:get_value(lot_id, Opts),
    Username = misc:get_username(XAccessToken),

    %TODO: checks_seller_guestlist - needs to check whether incoming
    %      user should have access to this queue
    case the_bouncer:checks_seller_guestlist(XAccessToken, LotID) of
		true -> accept_connection(LotID, Username);
		_ -> reject_connection([{<<"terminated_early">>, true}])
	end;

websocket_handle(_Frame, Opts) ->
	% this handles anything that's not text
    %erlang:display("------ sell_handler:websocket_handle/2 [3] ------"),
    {ok, Opts}.

%------------------------------------------------------------------------------

accept_connection(LotID, Username) ->
	%erlang:display("------ sell_handler:accept_connection/1 ------"),

	{Channel, Connection} = the_postman:open_all(),

	% seperate process to listen for rabbit messages - returns 
	% any found to websocket_info method
	Queue = misc:binary_join([LotID, Username], <<"_">>),
	spawn_link(the_listener, main, [Channel, Queue, self()]),

	% pass these around so we can shut down 
	% the rabbitmq connection gracefully
	Opts = [{channel, Channel},{connection, Connection}],
	{ok, Opts}.

%------------------------------------------------------------------------------

reject_connection(Opts) ->
	%erlang:display("------ sell_handler:reject_connection/1 ------"),
	{stop, Opts}.

%------------------------------------------------------------------------------
% websocket_info like handler_info in a genserver captures all callback outputs

websocket_info(_Info, Opts) ->
	%erlang:display("------ sell_handler:websocket_info/2 ------"),

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
	%erlang:display("------ sell_handler:show_me_the_money/2 ------"),
	Mess = erlang:element(2,Dropping),
	{reply, {text, Mess}, Opts}.

%------------------------------------------------------------------------------

cheery_bye(_, Opts) ->
	%erlang:display("------ sell_handler:cheery_bye/2 ------"),
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
                20 -> {stop, Opts};
                _ -> send_ping(NewCount, Opts)
        end.	

%------------------------------------------------------------------------------

send_ping(Count, Opts) ->
        erlang:send_after(?SOCKET_INTERVAL, self(), {ping, Count}),
        {reply, {ping, <<>>}, Opts}.	

%------------------------------------------------------------------------------

terminate(_, _, Opts) ->
	%erlang:display("------ sell_handler:terminate/3 ------"),

    case misc:find_value(terminated_early, Opts) of
        true -> ok;
        false -> the_postman:close_all(proplists:get_value(channel, Opts),
                 proplists:get_value(connection, Opts)),
                 ok
    end.
