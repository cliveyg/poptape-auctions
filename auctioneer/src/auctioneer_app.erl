-module(auctioneer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	%TODO: Add constraints here?
        Dispatch = cowboy_router:compile([
           {'_', [{"/auction/bid/:auction_id/:lot_id", bid_handler, []}, % websocket
           {"/auction/seller/:lot_id", sell_handler, []}, % websocket
		   {"/auction/status", status_handler, []}, % http
           {"/auction/:auction_id/:lot_id", lot_handler, []}]} % http
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
