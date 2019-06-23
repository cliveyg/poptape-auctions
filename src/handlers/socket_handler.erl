-module(socket_handler).
%-behaviour(cowboy_websocket_handler).

-export([init/2,
  	 websocket_init/1,
	 websocket_handle/2,
	 websocket_info/2,
	 websocket_terminate/3]).

%------------------------------------------------------------------------------

init(Req, Opts) ->
	erlang:display("------ socket_handler:init/2 ------"),
	%{cowboy_websocket, Req, Opts}.
	erlang:display(Opts),
    	{cowboy_websocket, Req, Opts, #{idle_timeout => 3000000}}.

%------------------------------------------------------------------------------

websocket_init(Opts) ->
	erlang:display("------ socket_handler:websocket_init/2 ------"),	
	erlang:display(Opts),

    	{reply, {text, <<"{ \"message\": \"Send nudes or JWT, your choice\" }">>}, Opts}.

%------------------------------------------------------------------------------

websocket_handle({text, Json}, Opts) ->
	erlang:display("------ socket_handler:websocket_handle/2 [1] ------"),
	erlang:display(Json),
	JsonDecoded = jsx:decode(Json),
        XAccessToken = misc:find_value(<<"x-access-token">>, JsonDecoded),
	UserData = misc:find_value(<<"user_data">>, JsonDecoded),

	case the_bouncer:checks_socket_guestlist(XAccessToken) of
		true -> accept_connection(Opts, UserData);
		false -> reject_connection(Opts)
	end;
	%{reply, {text, RetBin}, Opts};
websocket_handle(_Frame, Opts) ->
        erlang:display("------ socket_handler:websocket_handle/2 [2] ------"),
        erlang:display(_Frame),
        {ok, Opts}.

%------------------------------------------------------------------------------
	
accept_connection(Opts, UserData) ->
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

	{reply, {text, RetBin}, Opts}.

%------------------------------------------------------------------------------

make_string(UserData) ->
	erlang:display("------ socket_handler:make_string/1 ------"),
	RetStr1 = "{ \"message\": \"Connection accepted\", \"user_data\": " ++ 
		  binary:bin_to_list(UserData),
	RetStr2 = RetStr1 ++ " }",
	%erlang:display(RetStr2),
	RetStr2.

%------------------------------------------------------------------------------

reject_connection(Opts) ->
	erlang:display("------ socket_handler:reject_connection/1 ------"),
	{reply, {text, <<"{ \"message\": \"Get outta here\" }">>}, Opts}.
	%{stop, Opts}.

%------------------------------------------------------------------------------

websocket_info(_Info, Opts) ->
	erlang:display("------ socket_handler:websocket_info/2 ------"),
	erlang:display(_Info),
	%{reply, {text, <<"{ \"message\": \"Stalking you\" }">>}, Opts}.
    	{ok, Opts}.

%------------------------------------------------------------------------------

websocket_terminate(Reason, Req, Opts) -> 
	erlang:display("------ socket_handler:websocket_terminate/3 ------"),
	erlang:display(Reason),
	erlang:display(Req),
	erlang:display(Opts),
	ok.
