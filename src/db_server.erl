%%%-------------------------------------------------------------------
%%% Based on db_server.erl by Dave Kuhlman
%%% @copyright (C) 2016, Dave Kuhlman
%%% @end
%%% Created : 2016-12-19 14:06:45.650615
%%%-------------------------------------------------------------------
-module(db_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%------------------------------------------------------------------------------

start_link() ->
	erlang:display("start_link of db_server"),
    	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
	erlang:display("::::: db_server init() ::::::"),
    	{ok, #state{}}.

%------------------------------------------------------------------------------
% the bit's that do the work
%------------------------------------------------------------------------------

% get rec based on LotID
handle_call({get_rec, Key}, _From, Opts) ->
	erlang:display("::::: handle_call:get_rec :::::"),
    	Records = ets:lookup(records_db, Key),
    	Reply = case Records of
        	[{_LotID, Data}] -> {ok, Data};
        	[] -> {error, not_found};
        	_ -> {error, too_many_records}
    	end,
    	{reply, Reply, Opts};

% create the db
handle_call({create_db}, _From, Opts) ->
	erlang:display("::::: handle_call:create_db :::::"),

	% check table exists
	case ets:info(records_db) of
		undefined -> ets:new(records_db, [public, named_table]),
			         {reply, 201, Opts};
		_ -> {reply, 409, Opts}
	end;

% get all recs
handle_call({get_all_recs, []}, _From, Opts) ->
	erlang:display("::::: handle_call:get_all_recs :::::"),

   	F = fun (Item, Acc) -> Acc1 = [Item | Acc], Acc1 end,
    	Items = ets:foldl(F, [], records_db),
    	Reply = {ok, Items},
    	{reply, Reply, Opts};

% delete a record
handle_call({delete_rec, LotID}, _From, Opts) ->
	erlang:display("::::: handle_call:delete_rec :::::"),

    	Reply = case ets:lookup(records_db, LotID) of
        	[] -> {error, not_found};
        	_ -> ets:delete(records_db, LotID)
    	end,
    	{reply, Reply, Opts};

% create a record
handle_call({create_rec, LotID, Content}, _From, Opts) ->
	erlang:display("::::: handle_call:create_rec :::::"),

	case ets:insert(records_db, {LotID, Content}) of
		true -> {reply, 201, Opts};
		false -> {reply, 422, Opts}
	end;

% update a record - although create with the same key has the same effect
handle_call({update_rec, LotID, NewContent}, _From, Opts) ->
	erlang:display("::::: handle_call:update_rec :::::"),
    	DBResponse = ets:lookup(records_db, LotID),
    	Reply = case DBResponse of
        	[_] -> ok = ets:insert(records_db, {LotID, NewContent}),
            	       %ok = ets:sync(records_db),
            	       Response = io_lib:format("/get/~s", [LotID]),
            	       Response1 = list_to_binary(Response),
            	       {ok, Response1};
        	[] -> {error, not_found}
    	end,
    	{reply, Reply, Opts}.

%------------------------------------------------------------------------------

handle_cast(_Msg, Opts) ->
	erlang:display("::::: handle_cast :::::"),
        {noreply, Opts}.

%------------------------------------------------------------------------------

handle_info(_Info, Opts) ->
	erlang:display("::::: handle_info :::::"),
        {noreply, Opts}.

%------------------------------------------------------------------------------

terminate(_Reason, _Opts) ->
	erlang:display("::::: terminate :::::"),
    	%ets:close(state_db),
    	%ets:close(records_db),
    	ok.

%------------------------------------------------------------------------------

code_change(_OldVsn, Opts, _Extra) ->
	erlang:display("::::: code_change :::::"),
        {ok, Opts}.

