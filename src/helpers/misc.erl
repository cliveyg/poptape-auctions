-module(misc).

-export([find_value/2,
	 get_milly_time/0,
	 binary_join/2,
	 valid_auction_id/1]).

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

valid_auction_id(AuctionID) ->
        erlang:display("---- misc:valid_auction_id/2 ----"),
	erlang:display(AuctionID),

	% checking auction id needs no auth (so no extra http call from 
	% auctionhouse to authy. if it needs auth in the future then we
	% scrap the direct call to authy as the call to auctionhouse 
	% would do that call for us
	{ok, AuctionURL} = application:get_env(auctioneer, auctionhouse_url),
        %URL = [AuctionURL, AuctionID],
	%TODO: maybe add x-access-token so we know auction is valid for user
	% to create an auctioneer for?
        Headers = [{<<"Content-Type">>, <<"application/json">>}],
        Payload = <<>>,
        Options = [],
        {ok, StatusCode, _, _} = hackney:request(get, AuctionURL,
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
