-module(the_listener).
  
-export([main/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

main(Channel, Queue) ->
    erlang:display("------ the_listener:main/2 ------"),
    %io:format(" [*] Waiting for logs. To exit press CTRL+C~n"),
    erlang:display(Queue),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
	    erlang:display("------ the_listener:loop/1 ------"),
	    erlang:display(Body),
            %io:format(" [x] ~p~n", [Body]),
            loop(Channel)
end.
