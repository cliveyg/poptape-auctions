-module(json_http_request).

-export([post_json/2,
         post_json/3,
         get_json/2,
         get_json/3]).

post_json(Url, Data) ->
  post_json(Url, Data, []).

post_json(Url, Data, Headers) ->
  request_json(post, Url, null, Data, Headers).

get_json(Url, DecodeFlag) ->
  get_json(Url, DecodeFlag, []).

get_json(Url, DecodeFlag, Headers) ->
  erlang:display(DecodeFlag),
  request_json(get, Url, DecodeFlag, null, Headers).

%% internal
request_json(Method, Url, DecodeFlag, Data, Headers) ->
  Body = auction_json:encode(Data),
  Headers2 = [{<<"Content-Type">>, <<"application/json">>},
              {<<"Accept">>, <<"application/json">>} | Headers],
  {ok, Status, ResHeaders, ResBody} =
    hackney:Method(Url, Headers2, Body, [with_body]),
  Decoded = decode_response(proplists:get_value(<<"Content-Type">>, ResHeaders), ResBody),
  case DecodeFlag of 
     0 -> {ok, Status, ResHeaders, ResBody}; 
     1 -> {ok, Status, ResHeaders, Decoded}
  end.  

decode_response(<<"application/json", _/binary>>, ResBody) ->
  auction_json:decode(ResBody);
decode_response(_, ResBody) ->
  ResBody.

