-module(auctions_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
            {'_', [{"/auctions/status", status_handler, []},
                   {"/auctions", create_handler, []},
                   {"/auctions/loginms", loginms_handler, []},
                   {"/auctions/register", register_handler, []},
                   {"/auctions/:auction_id", auction_handler, []}]}
        ]),
        {ok, _} = cowboy:start_clear(my_http_listener,
            [{ip,{0,0,0,0}},{port, 9000}],
            #{env => #{dispatch => Dispatch},
              middlewares => [cowboy_router, cowboy_handler]}
        ),
        %erlang:display("before auctions sup"),
        db_sup:start_link(),
        auctions_sup:start_link().

stop(_State) ->
        ok.
