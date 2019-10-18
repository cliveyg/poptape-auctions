-module(the_bouncer).

-export([checks_guestlist/2,
         checks_seller_guestlist/2,
         checks_socket_guestlist/2]).

%------------------------------------------------------------------------------

checks_guestlist(Req, Level) ->
    %erlang:display("---- the_bouncer:checks_guestlist/2 ----"),
    XAccessToken = cowboy_req:header(<<"x-access-token">>, Req, ''),

    case XAccessToken of
        '' -> 401;
        _ -> good_boye(XAccessToken, Level)
    end.

%------------------------------------------------------------------------------
% we check the jwt against the authy microservice just once when using 
% websockets. if successfull we store the jwt in ets. 
% all subsequent messages from the client websocket will be checked to see if
% the jwt exists in ets. this saves on repeated calls to authy. should be
% faster.

checks_socket_guestlist(XAccessToken, LotID) ->
    %erlang:display("---- the_bouncer:checks_socket_guestlist/1 ----"),

    TokenGood = case token_in_ets(XAccessToken, jwttable) of
        true -> true;
        false -> check_authy(XAccessToken)
    end,

    RecordExists = misc:check_record_exists(LotID),

    case {TokenGood, RecordExists} of
        {true, true} -> true;
        {_, _} -> false
    end.

%------------------------------------------------------------------------------

checks_seller_guestlist(XAccessToken, LotID) ->
    %erlang:display("---- the_bouncer:checks_socket_guestlist/1 ----"),
    erlang:display(LotID),
    

    TokenGood = case token_in_ets(XAccessToken, jwttable) of
        true -> true;
        false -> check_authy(XAccessToken)
    end,

    %RecordExists = misc:check_record_exists(LotID),
    RecordExists = true,

    case {TokenGood, RecordExists} of
        {true, true} -> true;
        {_, _} -> false
    end.
  
%------------------------------------------------------------------------------
% websocket auth call to authy
check_authy(XAccessToken) ->
    %erlang:display("---- the_bouncer:check_authy/1 ----"),
    {ok, BaseURL} = application:get_env(auctioneer, authy_url),
    URL = [BaseURL, <<"10">>],
    Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"x-access-token">>, XAccessToken}],
    Payload = <<>>,
    Options = [],
    {ok, StatusCode, _, _} = hackney:request(get, URL,
                                             Headers, Payload,
                                             Options),      
    Result = case StatusCode of
        200 -> store_in_ets(XAccessToken, jwttable);
        401 -> false;
        _ -> false
    end,
      
    Result. 

%------------------------------------------------------------------------------

token_in_ets(XAccessToken, TableName) ->
    %erlang:display("---- the_bouncer:token_in_ets/1 ----"),
        case ets:whereis(TableName) of
            undefined -> false;
            _ -> proper_lookup(XAccessToken, TableName)
        end.

%------------------------------------------------------------------------------

proper_lookup(XAccessToken, TableName) -> 
    %erlang:display("---- the_bouncer:proper_lookup/1 ----"),
    List = ets:lookup(TableName, XAccessToken),
    case erlang:length(List) of
        0 -> false;
        _ -> true
    end.

%------------------------------------------------------------------------------

store_in_ets(XAccessToken, TableName) ->
    %erlang:display("---- the_bouncer:store_in_ets/1 ----"),
    case ets:whereis(TableName) of
        undefined -> create_table(XAccessToken, TableName);
        _ -> put_in_table(XAccessToken, TableName)
    end.
%------------------------------------------------------------------------------

put_in_table(XAccessToken, TableName) ->
    %erlang:display("---- the_bouncer:put_in_table/1 ----"),
    % use key of XAccessToken value 
    ets:insert(TableName, {XAccessToken, fab}),
    true.

%------------------------------------------------------------------------------

create_table(XAccessToken, TableName) ->
    %erlang:display("---- the_bouncer:create_table/1 ----"),
    % store as a set with key being the token - don't really care about the 
    % value so set it to ok
    ets:new(TableName, [set, public, named_table]),
    put_in_table(XAccessToken, TableName).

%------------------------------------------------------------------------------

good_boye(XAccessToken, Level) ->
    %erlang:display("---- the_bouncer:good_boye/3 ----"),
    % get auth url from app config
    {ok, BaseURL} = application:get_env(auctioneer, authy_url),
    URL = [BaseURL, Level],
    Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"x-access-token">>, XAccessToken}],
    Payload = <<>>,
    Options = [],
    {ok, StatusCode, _, _} = hackney:request(get, URL,
                                             Headers, Payload,
                                             Options),  
    StatusCode.

%------------------------------------------------------------------------------

