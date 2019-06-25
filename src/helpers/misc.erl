-module(misc).

-export([find_value/2,
	 get_root_pid/1,
	 %save_my_bag/5,
	 %put_in_bag/5,
	 load_initial_bid/5,
	 binary_join/2,
	 place_your_bets/5,
	 valid_auction_id/1]).

%------------------------------------------------------------------------------

get_root_pid(CurrentPID) ->
	erlang:display("---- misc:get_root_pid/1 ----"),
	erlang:display(CurrentPID).

%------------------------------------------------------------------------------

find_value(Key, List) ->

        case lists:keyfind(Key, 1, List) of
                {Key, Result} -> Result;
                false -> false
        end.

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

%save_my_bag(AuctionID, ItemID, Username, UnixTime, BidValue) -> 
%	erlang:display("---- misc:save_my_bag/5 ----"),
%	ets:new(bid_table, [bag, public, named_table]),
%	put_in_bag(AuctionID, ItemID, Username, UnixTime, BidValue).

%------------------------------------------------------------------------------

%put_in_bag(AuctionID, ItemID, Username, UnixTime, BidValue) ->
%	erlang:display("---- misc:put_in_bag/5 ----"),
%	% not sure of this due to potential size this table could reach
%	% TODO: delete old entries after auction is finished
%	ets:insert(bid_table, {AuctionID, ItemID, Username, UnixTime, BidValue}).

%------------------------------------------------------------------------------

load_initial_bid(Username, ItemID, AuctionID, StartPrice, Endtime) ->
	erlang:display("---- misc:load_initial_bid/4 ----"),
        case ets:whereis(bidtable) of
                undefined -> create_bid_table(Username, ItemID, 
					      AuctionID, StartPrice, Endtime);
                _ -> place_your_bets(Username, ItemID, 
				     AuctionID, StartPrice, Endtime)
        end.

%------------------------------------------------------------------------------

create_bid_table(Username, ItemID, AuctionID, StartPrice, Endtime) ->
	erlang:display("---- misc:create_bid_table/4 ----"),
	ets:new(bidtable, [set, public, named_table]),
        place_your_bets(Username, ItemID, AuctionID, StartPrice, Endtime).

%------------------------------------------------------------------------------

place_your_bets(Username, ItemID, AuctionID, BidPrice, Endtime) ->
        % not sure of this due to potential size this table could reach
        % TODO: delete old entries after auction is finished
	erlang:display("---- misc:place_your_bets/4 ----"),
	ets:insert(bidtable, {ItemID, Username, BidPrice, Endtime, AuctionID}).

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
