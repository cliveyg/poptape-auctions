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
	LotID = cowboy_req:binding(lot_id, Req),
	Opts = [{auction_id, AuctionID}, {lot_id, LotID}],
    {cowboy_websocket, Req, Opts, #{idle_timeout => 3000000}}.

%------------------------------------------------------------------------------

websocket_init(Opts) ->
	erlang:display("------ bid_handler:websocket_init/2 ------"),	
	% this timer is an attempt to keep our websocket connection open
	% returns to websocket_info method
	erlang:start_timer(?SOCKET_INTERVAL, self(), {ping, 1}),
    {reply, {text, <<"{ \"message\": \"Auctioneer calling\" }">>}, Opts}.

%------------------------------------------------------------------------------

websocket_handle({text, Json}, Opts) ->
	erlang:display("------ bid_handler:websocket_handle/2 [1] ------"),
	JsonDecoded = jsx:decode(Json),
	%TODO: Sanitize input!
    Username = misc:find_value(<<"username">>, JsonDecoded),
    BidAmount = misc:find_value(<<"bid_amount">>, JsonDecoded),
    XAccessToken = misc:find_value(<<"x-access-token">>, JsonDecoded),
    LotID = proplists:get_value(lot_id, Opts),

	InputPropList = [{username, Username},
			 {bid_amount, BidAmount},
			 {x_access_token, XAccessToken},
			 {auction_id, proplists:get_value(auction_id, Opts)},
			 {lot_id, LotID}],

    InputsOK = check_inputs(InputPropList),
	OnTheList = the_bouncer:checks_socket_guestlist(XAccessToken, LotID),

    case {InputsOK, OnTheList} of
		{true, true} -> accept_connection(InputPropList);
		{_, _}-> reject_connection([{<<"terminated_early">>, true}])
	end;

websocket_handle(_Frame, Opts) ->
	% this handles anything that's not text
    %erlang:display("------ bid_handler:websocket_handle/2 [3] ------"),
    {ok, Opts}.

%------------------------------------------------------------------------------

accept_connection(UserData) ->
	erlang:display("------ bid_handler:accept_connection/1 ------"),
	Username = proplists:get_value(username, UserData),
	LotID = proplists:get_value(lot_id, UserData),

	{Channel, Connection} = the_postman:create_bidder_queue(Username, LotID),

	% seperate process to listen for rabbit messages - returns 
	% any found to websocket_info method
	Queue = misc:binary_join([LotID, Username], <<"_">>),
	spawn_link(the_listener, main, [Channel, Queue, self()]),

    % we already know we have data in db at this point so don't worry about
    % checking for if record exists
	{_, DBData} = gen_server:call(db_server, {get_rec, LotID}),
	{_, YourBid} = misc:binary_to_number(
                       proplists:get_value(bid_amount, UserData)
                   ),
    EndTime = proplists:get_value(end_time, DBData),
	UnixTime = misc:get_milly_time(),
    StoredBid = proplists:get_value(current_price, DBData),
    AuctionType = proplists:get_value(auction_type, DBData),
    MinChange = proplists:get_value(min_change, DBData),

	{LotStatus, BidStatus, Message, LatestBid, CurrentWinner} = 
	case check_bids(YourBid, StoredBid, AuctionType, MinChange, UnixTime, EndTime)  of
		{true, true} -> {<<"open">>, 201, <<"Currently winning bid">>, 
			             YourBid, proplists:get_value(username, UserData)};
		{false, true} -> {<<"open">>, 400, <<"Bid failed">>, 	
			              StoredBid, proplists:get_value(username, DBData)};
		{_, false} -> {<<"closed">>, 404, <<"Bidding finished">>, 
			           StoredBid, proplists:get_value(username, DBData)}
	end,

    BidID = misc:get_new_uuid(),
    
    % TODO: check about reserves for other auction types
    ResMess = case proplists:get_value(reserve_price, DBData) == 0 of
        true -> <<"No reserve">>; 
        false -> case proplists:get_value(reserve_price, DBData) > YourBid of
                    true -> <<"Reserve not met">>;
                    false -> <<"Reserve met">>
                 end
    end,

    OutData = [{username, Username},
               {lot_id, LotID},
               {bid_id, BidID},
               {unixtime, UnixTime},
               {bid, YourBid},
               {lot_status, LotStatus},
               {bid_status, BidStatus},
               {message, Message},
               {reserve_message, ResMess},
               {auction_id, proplists:get_value(auction_id, DBData)},
               {end_time, EndTime}],

	case CurrentWinner == proplists:get_value(username, UserData) of
		true -> PublicID = misc:get_public_id(proplists:get_value(x_access_token, UserData)),
                UpdatedDBData = [
                    {current_winner, CurrentWinner},
                    {current_price, LatestBid},
                    {bid_id, BidID},
                    {lot_id, LotID},
                    {auction_type, proplists:get_value(auction_type, DBData)},
                    {min_change, proplists:get_value(min_change, DBData)},
                    {reserve_price, proplists:get_value(reserve_price, DBData)},
                    {auction_id, proplists:get_value(auction_id, DBData)},
                    {exchange, proplists:get_value(exchange, DBData)},
                    {reserve_message, ResMess},
                    {public_id, PublicID},
                    {start_time, proplists:get_value(start_time, DBData)},
                    {end_time, proplists:get_value(end_time, DBData)},
                    {unix_time, proplists:get_value(UnixTime, DBData)},
                    {lot_status, LotStatus},
                    {bid_status, BidStatus},
                    {message, Message}
                ],
                % create record just overwrites a record with the same key if it exists
                gen_server:call(db_server, {create_rec, LotID, UpdatedDBData});
		false -> ok
	end,

	JsonPayload = jsx:encode(OutData),

	% publish to the queue to return data to user via websocket_info
	the_postman:publish_message(Channel, LotID, JsonPayload),

	% pass these around so we can shut down 
	% the rabbitmq connection gracefully
	Opts = [{channel, Channel},{connection, Connection}],
	{ok, Opts}.

%------------------------------------------------------------------------------

check_bids(YourBid, StoredBid, AuctionType, MinChange, UnixTime, EndTime) ->

    erlang:display("----------- check_bids ------------"),
    %erlang:display(YourBid),
    %erlang:display(StoredBid),
    %erlang:display(AuctionType),
    %erlang:display(MinChange),
    %erlang:display(UnixTime),
    %erlang:display(EndTime),

    SuccessBid = case AuctionType of
        <<"EN">> -> YourBid >= (StoredBid + MinChange);
        <<"BN">> -> YourBid >= (StoredBid + MinChange);
        <<"DU">> -> YourBid =< (StoredBid - MinChange)
    end, 
	{SuccessBid, UnixTime < EndTime}.

%------------------------------------------------------------------------------

check_inputs(InputPropList) ->
    erlang:display("----------- check_inputs ------------"),
    OkUsername = length(binary:bin_to_list(
                        proplists:get_value(username, InputPropList))) < 51,
    OkAccessToken = length(binary:bin_to_list(
                        proplists:get_value(username, InputPropList))) < 1001,
    {Status, _} = misc:cash_or_error(proplists:get_value(bid_amount, InputPropList)),

    case {OkUsername, OkAccessToken, Status} of
        {true, true, ok} -> true;
        {_, _, _} -> false
    end.
    
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
                10 -> {stop, Opts};
                _ -> send_ping(NewCount, Opts)
        end.	

%------------------------------------------------------------------------------

send_ping(Count, Opts) ->
        erlang:send_after(?SOCKET_INTERVAL, self(), {ping, Count}),
        {reply, {ping, <<>>}, Opts}.	

%------------------------------------------------------------------------------

terminate(_, _, Opts) ->
	erlang:display("------ bid_handler:terminate/3 ------"),

    case misc:find_value(terminated_early, Opts) of
        true -> ok;
        false -> the_postman:close_all(proplists:get_value(channel, Opts),
                 proplists:get_value(connection, Opts)),
                 ok
    end.
