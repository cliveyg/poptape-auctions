-module(socket_handler).
%-behaviour(cowboy_websocket_handler).

-export([init/2,
  	 websocket_init/1,
	 websocket_handle/2,
	 websocket_info/2,
	 websocket_terminate/3]).

%------------------------------------------------------------------------------

init(Req, State) ->
	erlang:display("------ socket_handler:init/2 ------"),
	%{cowboy_websocket, Req, State}.
    	{cowboy_websocket, Req, State, #{idle_timeout => 3000000}}.

%------------------------------------------------------------------------------

websocket_init(State) ->
	erlang:display("------ socket_handler:websocket_init/2 ------"),	
	erlang:display(State),

    	{reply, {text, <<"{ \"message\": \"Send nudes or JWT, your choice\" }">>}, State}.

%------------------------------------------------------------------------------

websocket_handle({text, Json}, State) ->
	erlang:display("------ socket_handler:websocket_handle/2 [1] ------"),
	erlang:display(Json),
	JsonDecoded = jsx:decode(Json),
        XAccessToken = misc:find_value(<<"x-access-token">>, JsonDecoded),
	UserData = misc:find_value(<<"user_data">>, JsonDecoded),

	%RetBin = 
	case the_bouncer:checks_socket_guestlist(XAccessToken) of
		true -> accept_connection(State, UserData);
		false -> reject_connection(State)
	end;
	%{reply, {text, RetBin}, State};
websocket_handle(_Frame, State) ->
        erlang:display("------ socket_handler:websocket_handle/2 [2] ------"),
        erlang:display(_Frame),
        {ok, State}.

%------------------------------------------------------------------------------
	
accept_connection(State, UserData) ->
	erlang:display("------ socket_handler:accept_connection/1 ------"),
	erlang:display(UserData),
	RetStr = case UserData of
		false -> "{ \"message\": \"Connection accepted\" }";
		_ -> make_string(UserData)
	end,
	erlang:display("::::::::::::::::::::"),
	erlang:display(RetStr),
	RetBin = erlang:list_to_binary(RetStr),

        erlang:display("attempting to open connection to rabbit"),
        {Channel, Connection} = the_postman:open_all(),
	erlang:display(Channel),
	erlang:display(Connection),

	{reply, {text, RetBin}, State}.

%------------------------------------------------------------------------------

make_string(UserData) ->
	erlang:display("------ socket_handler:make_string/1 ------"),
	RetStr1 = "{ \"message\": \"Connection accepted\", \"user_data\": " ++ 
		  binary:bin_to_list(UserData),
	RetStr2 = RetStr1 ++ " }",
	%erlang:display(RetStr2),
	RetStr2.

%------------------------------------------------------------------------------

reject_connection(State) ->
	erlang:display("------ socket_handler:reject_connection/1 ------"),
	{reply, {text, <<"{ \"message\": \"Get outta here\" }">>}, State}.
	%{stop, State}.

%------------------------------------------------------------------------------

websocket_info(_Info, State) ->
	erlang:display("------ socket_handler:websocket_info/2 ------"),
	erlang:display(_Info),
	%{reply, {text, <<"{ \"message\": \"Stalking you\" }">>}, State}.
    	{ok, State}.

%------------------------------------------------------------------------------

websocket_terminate(Reason, Req, State) -> 
	erlang:display("------ socket_handler:websocket_terminate/3 ------"),
	erlang:display(Reason),
	erlang:display(Req),
	erlang:display(State),
	ok.
