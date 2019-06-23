-module(bid_handler).

-export([init/2,
  	 websocket_init/1,
	 websocket_handle/2,
	 websocket_info/2,
	 websocket_terminate/3]).

-define(SOCKET_INTERVAL, 50000).

%------------------------------------------------------------------------------

init(Req, _) ->
	erlang:display("------ bid_handler:init/2 ------"),
	%TODO: Sanitize input!
	AuctionID = cowboy_req:binding(auction_id, Req),
	ItemID = cowboy_req:binding(item_id, Req),
	Opts = [{auction_id, AuctionID}, {item_id, ItemID}],
    	{cowboy_websocket, Req, Opts, #{idle_timeout => 3000000}}.

%------------------------------------------------------------------------------

websocket_init(Opts) ->
	erlang:display("------ bid_handler:websocket_init/2 ------"),	
	% this timer is an attempt to keep our websocket connection open
	erlang:start_timer(?SOCKET_INTERVAL, self(), ping),
    	{reply, {text, <<"{ \"message\": \"Send nudes or JWT, your choice\" }">>}, Opts}.

%------------------------------------------------------------------------------

websocket_handle({text, Json}, Opts) ->
	erlang:display("------ bid_handler:websocket_handle/2 [1] ------"),
	JsonDecoded = jsx:decode(Json),
	%TODO: Sanitize input!
        Username = misc:find_value(<<"username">>, JsonDecoded),
        BidAmount = misc:find_value(<<"bid_amount">>, JsonDecoded),
        XAccessToken = misc:find_value(<<"x-access-token">>, JsonDecoded),
	InputPropList = [{username, Username},
			 {bid_amount, BidAmount},
			 {x_access_token, XAccessToken},
			 {auction_id, proplists:get_value(auction_id, Opts)},
			 {item_id, proplists:get_value(auction_id, Opts)}],

	case the_bouncer:checks_socket_guestlist(XAccessToken) of
		true -> accept_connection([], InputPropList);
		false -> reject_connection([])
	end;
websocket_handle(_Frame, Opts) ->
	% this handles anything that's not text
        erlang:display("------ bid_handler:websocket_handle/2 [3] ------"),
        erlang:display(_Frame),
        {ok, Opts}.

%------------------------------------------------------------------------------

accept_connection(Opts, UserData) ->
	erlang:display("------ bid_handler:accept_connection/1 ------"),
        erlang:display("attempting to open connection to rabbit"),
        {Channel, Connection} = the_postman:open_all(),
	erlang:display(Channel),
	erlang:display(Connection),

	% at the mo we just send the incoming data back
	JsonPayload = jsx:encode(UserData),
	erlang:display(JsonPayload),

	{reply, {text, JsonPayload}, Opts}.

%------------------------------------------------------------------------------

reject_connection(Opts) ->
	erlang:display("------ bid_handler:reject_connection/1 ------"),
	{reply, {text, <<"{ \"message\": \"Get outta here\" }">>}, Opts},
	{stop, State}.

%------------------------------------------------------------------------------

websocket_info(_Info, Opts) ->
	erlang:display("------ bid_handler:websocket_info/2 ------"),
	erlang:display(_Info),
	% this restarts the timer 
	erlang:send_after(?SOCKET_INTERVAL, self(), ping),
	{reply, {ping, <<>>}, Opts}.

%------------------------------------------------------------------------------

websocket_terminate(Reason, Req, Opts) -> 
	erlang:display("------ bid_handler:websocket_terminate/3 ------"),
	erlang:display(Reason),
	erlang:display(Req),
	erlang:display(Opts),
	ok.
