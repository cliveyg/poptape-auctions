-module(the_listener).
  
-export([main/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

main(Channel, Queue, CallingPID) ->
    erlang:display("------ the_listener:main/2 ------"),
    erlang:display(Queue),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel, CallingPID).

loop(Channel, PID) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
	    erlang:display("------ the_listener:loop/1 ------"),
	    PID ! {rabbit_dropping, Body},
            loop(Channel, PID)
    end.
