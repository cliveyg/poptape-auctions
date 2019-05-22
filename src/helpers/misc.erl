-module(misc).

-export([find_value/2,
	 save_auction_instance/3,
	 %save_it/4,
	 %save_it/5,
	 get_root_pid/1,
	 valid_id/3]).

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

%valid_id(UUID, Type) ->
%        erlang:display("---- misc:valid_id/1 ----"),
%	erlang:display(UUID),
%
%        Method = get,
%        URL = "https://poptape.club/auctioncontrol/"++UUID,
%        Headers = [{<<"Content-Type">>, <<"application/json">>},
%                   {<<"x-access-token">>, XAccessToken}],
%        Payload = <<>>,
%        Options = [],
%        {ok, StatusCode, _, _} = hackney:request(Method, URL,
%                                                         Headers, Payload,
%                                                         Options),
%	
%	erlang:display(StatusCode),
%
%	200.

%------------------------------------------------------------------------------

valid_id(UUID, XAccessToken, Type) ->
	erlang:display("---- misc:valid_id/3 ----"),
	erlang:display(Type),

	% check public_id is same as jwt token	
	
	{ok, PartURL} = case Type of
		auction_id -> application:get_env(auctions, auction_id_url);
		public_id  -> application:get_env(auctions, public_id_url)
	end,

	URL = PartURL++UUID,

        Method = get,
        %URL = "https://poptape.club/login/validate/"++UUID,
        Headers = [{<<"Content-Type">>, <<"application/json">>},
                   {<<"x-access-token">>, XAccessToken}],
        Payload = <<>>,
        Options = [],
        {ok, StatusCode, _, _} = hackney:request(Method, URL,
                                                         Headers, Payload,
                                                         Options),

        %{ok, Body} = hackney:body(ClientRef),
	%erlang:display([StatusCode, io:format("~s\n", [Body])]),
	%erlang:display(StatusCode),
	%Test = case StatusCode of
	%	200 -> jiffy:decode(Body);
	%	_ -> false
	%end,
	%TODO: Complete the auctioncontrol stuff
	%Test = jiffy:decode(Body),
	erlang:display(["Actual status and type: ", StatusCode, Type]),
	%erlang:display(StatusCode),

	200.

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
	%erlang:display(PublicID),
	%erlang:display(AuctionID),
	%erlang:display(UnixTime),
	ets:insert(auction, {AuctionID, [PublicID, UnixTime]}),
	%erlang:display(ets:lookup(auction, AuctionID)),
	%erlang:display("---- deleting data ----"),
	ets:delete(auction),
	erlang:display("end of save_it").

%------------------------------------------------------------------------------

save_it(PublicID, AuctionID, UnixTime, TableName) ->
        erlang:display("---- misc:save_it/4 ----"),

	ets:new(TableName, [set, named_table]),
	save_it(PublicID, AuctionID, UnixTime).

	

