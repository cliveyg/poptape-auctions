-module(misc).

-export([find_value/2,
	 	 get_milly_time/0,
	 	 binary_join/2,
         get_new_uuid/0,
         get_public_id/1,
         check_record_exists/1,
         binary_to_number/1,
         cash_or_error/1,
         to_timestamp/1,
	 	 valid_auction_item/3]).

%------------------------------------------------------------------------------

find_value(Key, List) ->

	case lists:keyfind(Key, 1, List) of
		{Key, Result} -> Result;
		false -> false
	end.

%------------------------------------------------------------------------------
% this tries to convert incoming money fields to numbers and if it fails due
% to letters or other stuff being supplied then we return 0 as a number
cash_or_error(B) ->
    erlang:display("------------ cash_or_error -------------"),
    try binary_to_number(B)
    catch
        error:badarg -> {error, B}
    end.

%------------------------------------------------------------------------------

binary_to_number(B) ->
    erlang:display("------------ binary_to_number -------------"),
    {ok, list_to_number(binary_to_list(B))}.

%------------------------------------------------------------------------------

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.

%------------------------------------------------------------------------------

check_record_exists(LotID) ->
    erlang:display("---- check_record_exists/1 ----"),

    %erlang:display(gen_server:call(db_server, {get_rec, LotID})),
    case gen_server:call(db_server, {get_rec, LotID}) of
        {error,_} -> false;
        {ok,_} -> true
    end.

%------------------------------------------------------------------------------

get_public_id(XAccessToken) ->

    % get the public id from the jwt
    SplitToken = binary:split(XAccessToken, <<".">>, [global]),
    UserData = lists:nth(2, SplitToken),
    Base64Decoded = bass64url:decode(UserData),
    JWTPayload = jsx:decode(Base64Decoded),
    PublicID = misc:find_value(<<"public_id">>, JWTPayload),
    PublicID.

%------------------------------------------------------------------------------

get_new_uuid() ->
    quickrand:seed(),
    UUID = uuid:get_v4_urandom(),
    erlang:list_to_binary(uuid:uuid_to_string(UUID)).

%------------------------------------------------------------------------------

get_milly_time() ->
	{Mega, Secs, Microsecs} = os:timestamp(),
	TotSecs = (Mega*1000000)+Secs,
	Milly = (TotSecs*1000) + (Microsecs/1000),
	erlang:round(Milly).

%------------------------------------------------------------------------------

valid_auction_item(AuctionID, LotID, XAccessToken) ->
    erlang:display("---- misc:valid_auction_id/2 ----"),

    % the auctionhouse microservice returns either 200, 401 or 406 normally
    % part of it's checks are that a non-auction lot owner cannot create an
    % auctioneer instance
	{ok, AuctionURL} = application:get_env(auctioneer, auctionhouse_url),
    URL = [AuctionURL, AuctionID, <<"/">>, LotID, <<"/">>],
	Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"x-access-token">>, XAccessToken}],
	Payload = <<>>,
	Options = [],
	{ok, StatusCode, _, ClientRef} = hackney:request(get, URL,
                                                     Headers, Payload,
                                                     Options),
    {ok, Body} = hackney:body(ClientRef),
	{StatusCode, Body}.

%------------------------------------------------------------------------------

to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}}) ->
    (calendar:datetime_to_gregorian_seconds(
        {{Year,Month,Day},{Hours,Minutes,Seconds}}) 
    - 62167219200)
    * 1000000.

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
