%%%-------------------------------------------------------------------
%%% @author Dave Kuhlman
%%% @copyright (C) 2016, Dave Kuhlman
%%% @doc
%%%
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

%% db_server API
-export([
%         get_rec/1,
         get_all_recs/0,
%         delete_rec/1,
         create_rec/1
%         update_rec/2
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	erlang:display("start_link of db_server"),
    	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------

%get_rec(RecordId) ->
%    io:fwrite("(get_rec) RecordId: ~p~n", [RecordId]),
%    gen_server:call(?SERVER, {get_rec, RecordId}).

get_all_recs() ->
	erlang:display("** Create recs la"),
    	%io:fwrite("(get_all_recs)~n"),
    	gen_server:call(?SERVER, {get_all_recs}).

%delete_rec(RecordId) ->
%    io:fwrite("(delete_rec) RecordId: ~p~n", [RecordId]),
%    gen_server:call(?SERVER, {delete_rec, RecordId}).

create_rec(Content) ->
	erlang:display("Create recs la"),
	%io:fwrite("(create_rec) ~n", []),
    	gen_server:call(?SERVER, {create_rec, Content}).

%update_rec(RecordId, Content) ->
%    io:fwrite("(update_rec) RecordId: ~p  Content: ~p~n", [RecordId, Content]),
%    gen_server:call(?SERVER, {update_rec, RecordId, Content}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	erlang:display("db_server init"),
    	%{ok, Recordfilename} = application:get_env(auction_main_db, auctions_file_name),
	%erlang:display(Recordfilename),
    %dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    %{ok, Statefilename} = application:get_env(rest_update, state_file_name),
    %dets:open_file(state_db, [{file, Statefilename}, {type, set}]),
    	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%handle_call({get_rec, Key}, _From, State) ->
%    Records = dets:lookup(records_db, Key),
%    io:fwrite("(call get_rec) Records: ~p~n", [Records]),
%    Reply = case Records of
%        [{_RecordId, Data}] ->
%            {ok, Data};
%        [] ->
%            {error, not_found};
%        _ ->
%            {error, too_many_records}
%    end,
%    {reply, Reply, State};

%handle_call({get_all_recs, Data}, _From, State) ->
handle_call({get_all_recs, Data}, _From, State) ->
	erlang:display("getting all the recs la"),
	erlang:display(Data),
	%erlang:display(_From),
	%erlang:display("-----------------------"),
%    F = fun (Item, Acc) -> Acc1 = [Item | Acc], Acc1 end,
%    Items = dets:foldl(F, [], records_db),
%    Reply = {ok, Items},
    {reply, ok, State}.

%handle_call({delete_rec, RecordId}, _From, State) ->
%    Reply = case dets:lookup(records_db, RecordId) of
%        [] ->
%            {error, not_found};
%        _ ->
%            dets:delete(records_db, RecordId)
%    end,
%    {reply, Reply, State};

%handle_call({create_rec, Content}, _From, State) ->
%    RecordId = generate_id(),
%    ok = dets:insert(records_db, {RecordId, Content}),
%    ok = dets:sync(records_db),
%    Reply = {ok, RecordId},
%    {reply, Reply, State};

%handle_call({update_rec, RecordId, NewContent}, _From, State) ->
%    DBResponse = dets:lookup(records_db, RecordId),
%    Reply = case DBResponse of
%        [_] ->
%            ok = dets:insert(records_db, {RecordId, NewContent}),
%            ok = dets:sync(records_db),
%            Response = io_lib:format("/get/~s", [RecordId]),
%            Response1 = list_to_binary(Response),
%            {ok, Response1};
%        [] ->
%            {error, not_found}
%    end,
%    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	erlang:display("we are handling this cast message"),
	%erlang:display(_Msg),
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	erlang:display("we are handling this info"),
	%erlang:display(_Info),
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
%    dets:close(state_db),
%    dets:close(records_db),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%generate_id() ->
%    Records = dets:lookup(state_db, current_id),
%    Response = case Records of
%        [{current_id, CurrentId}] ->
%            NextId = CurrentId + 1,
%            dets:insert(state_db, {current_id, NextId}),
%            Id = lists:flatten(io_lib:format("id_~4..0B", [CurrentId])),
%            Id;
%        [] ->
%            error
%    end,
%    Response.
