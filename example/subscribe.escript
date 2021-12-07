#!/usr/bin/env escript
%%! -pa _build/default/lib/nats/ebin -pa _build/default/lib/jsx/ebin -Wall

main([Host, Port, Subject, SID]) ->
    P = list_to_integer(Port),
    nats:start_link(#{remote_address => Host,
                      remote_port => P,
                      ping_interval => 1000}),
    {ok, Info} = nats:connect(),
    io:format("~p~n", [Info]),
    ok = nats:subscribe(list_to_binary(Subject), list_to_integer(SID)),
    loop().

loop() ->
    receive
        Msg ->
            io:format("Received: ~p~n", [Msg]),
            loop()
    after
        60000 ->
            done
    end.
