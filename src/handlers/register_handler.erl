-module(register_handler).
%-behavior(cowboy_handler).

-export([init/2,
	 content_types_accepted/2,
 	 content_types_provided/2,
	 get_json/2,
	 post_json/2,
	 allowed_methods/2]).

init(Req, State) ->
	erlang:display("---- register_handler:init/2 ----"),
	{Message, RetCode} = the_bouncer:on_the_list(Req),

        case RetCode of
                200 -> {cowboy_rest, Req, State};
                _ -> cowboy_req:reply(RetCode, #{<<"content-type">> => <<"application/json">>}, Message, Req)
        end.

allowed_methods(Req, State) ->
	erlang:display("---- register_handler:allowed_methods/2 ----"),
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	erlang:display("---- register_handler:content_types_accepted/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, post_json}
  	], Req, State}.

content_types_provided(Req, State) ->
	erlang:display("---- register_handler:content_types_provided/2 ----"),
  	{[
    		{{<<"application">>, <<"json">>, []}, get_json}
  	], Req, State}.

get_json(Req, State) ->
	erlang:display("---- register_handler:get_json/2 ----"),
  	{<<"{ \"message\": \"registered\" }">>, Req, State}.

post_json(Req, State) ->
	erlang:display("---- register_handler:post_json/2 ----"),

	

  	{true, Req, State}.

