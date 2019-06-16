-module(misc).

-export([find_value/2,
	 save_auction_instance/3,
	 get_root_pid/1,
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
        Headers = [{<<"Content-Type">>, <<"application/json">>}],
        Payload = <<>>,
        Options = [],
        {ok, StatusCode, _, _} = hackney:request(get, AuctionURL,
                                                 Headers, Payload,
        			 	         Options),
	StatusCode.

%------------------------------------------------------------------------------

save_auction_instance(PublicID, AuctionID, UnixTime) ->
	erlang:display("save_auction_instance"),
	case lists:member(auction,ets:all()) of
		true -> save_it(PublicID, AuctionID, UnixTime);
		false -> save_it(PublicID, AuctionID, UnixTime, auction)
	end.

%------------------------------------------------------------------------------

save_it(PublicID, AuctionID, UnixTime) ->
	erlang:display("---- misc:save_it/3 ----"),
	ets:insert(auction, {AuctionID, [PublicID, UnixTime]}),
	ets:delete(auction),
	erlang:display("end of save_it").

%------------------------------------------------------------------------------

save_it(PublicID, AuctionID, UnixTime, TableName) ->
        erlang:display("---- misc:save_it/4 ----"),
	ets:new(TableName, [set, named_table]),
	save_it(PublicID, AuctionID, UnixTime).

	

