-module(auctioneer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
            {'_', [{"/auction/status", status_handler, []},
		   %{"/auction/join", join_handler, []},
                   {"/auction", create_handler, []},
		   {"/auction/mess", mess_handler, []},
                   %{"/auction/bid", bid_handler, []},
                   {"/auction/:auction_id/:public_id", auction_handler, []}]}
        ]),
        {ok, _} = cowboy:start_clear(my_http_listener,
            [{ip,{0,0,0,0}},{port, 9000}],
            #{env => #{dispatch => Dispatch},
              middlewares => [cowboy_router, cowboy_handler]}
        ),
        db_sup:start_link(),
        auctioneer_sup:start_link().

stop(_State) ->
        ok.
