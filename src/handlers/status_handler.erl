-module(status_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
	erlang:display("---- status_handler:init/2 ----"),	
	erlang:display(whereis(db_server)),
	%DBServerId = whereis(db_server),
	erlang:display("Me, myself and I"),
	erlang:display(self()),
	%erlang:display(DBServerId),
	%Msg = iolist_to_binary
	%DBServerId ! {self(), hello},
	%gen_server:cast(DBServerId, {get_all_recs}), 
	Resp = gen_server:call(db_server, {get_all_recs, <<"SOME_DATA">>}),
	erlang:display("waiting for response..."),
	erlang:display(Resp),

	Method = get,
	URL = "https://poptape.club/login/status",
	JWT = erlang:list_to_binary("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJwdWJsaWNfaWQiOiJhMzg5M2Y4Yi02M2U2LTRiYjctODE0Ny03MTM3Mzg5MTJiZDUiLCJleHAiOjE1NTczOTkzMjN9.iQcft5eJ8lGLgXC8PueU0CwT6B7pLGz3ZCy09hY7pLg"),
	Headers = [{<<"Content-Type">>, <<"application/json">>},
		   {<<"x-access-token">>, JWT}],
	Payload = <<>>,
	Options = [],
	{ok, StatusCode, _, ClientRef} = hackney:request(Method, URL,
        	                                                   Headers, Payload,
                	                                           Options),

	
	{ok, Body} = hackney:body(ClientRef),
	erlang:display("Sent details to login ms..."),
	erlang:display([StatusCode, io:format("~s\n", [Body])]),

    	Req = cowboy_req:reply(200,
        	#{<<"content-type">> => <<"application/json">>},
        	<<"{\n\"message\": \"Bid module running...\"\n}">>,
        	Req0),	
	{ok, Req, State}.
