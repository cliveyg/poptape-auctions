-module(misc).

-export([find_value/2,
	 	 get_milly_time/0,
	 	 binary_join/2,
	 	 valid_auction_item/3]).

%------------------------------------------------------------------------------

find_value(Key, List) ->

	case lists:keyfind(Key, 1, List) of
		{Key, Result} -> Result;
		false -> false
	end.

%------------------------------------------------------------------------------

get_milly_time() ->
	{Mega, Secs, Microsecs} = os:timestamp(),
	TotSecs = (Mega*1000000)+Secs,
	Milly = (TotSecs*1000) + (Microsecs/1000),
	erlang:round(Milly).

%------------------------------------------------------------------------------

valid_auction_item(AuctionID, LotID, XAccessToken) ->
    erlang:display("---- misc:valid_auction_id/2 ----"),
	erlang:display(AuctionID),
	erlang:display(LotID),
    erlang:display(XAccessToken),

    % the auctionhouse microservice returns either 200 or 401
    % part of it's checks are that the auction creator cannot bid on their
    % own auction
	{ok, AuctionURL} = application:get_env(auctioneer, auctionhouse_url),
    URL = [AuctionURL, AuctionID, <<"/">>, LotID],
    erlang:display(URL),
	Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"x-access-token">>, XAccessToken}],
	Payload = <<>>,
	Options = [],
	{ok, StatusCode, _, _} = hackney:request(get, URL,
											 Headers, Payload,
							 				 Options),
	StatusCode.

%------------------------------------------------------------------------------
% copied from https://coderwall.com/p/nmajna/joining-a-list-of-binaries-in-erlang

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).
