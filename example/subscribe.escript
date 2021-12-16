#!/usr/bin/env escript
%%! -pa _build/default/lib/natserl/ebin -pa _build/default/lib/jsx/ebin -Wall

main([Host, Port, Subject, SID]) ->
    P = list_to_integer(Port),
    natserl:start_link(#{remote_address => Host,
                         remote_port => P,
                         ping_interval => 1000}),
    {ok, Info} = natserl:connect(),
    io:format("Connected to server: ~p~n", [Info]),
    ok = natserl:subscribe(list_to_binary(Subject), list_to_binary(SID)),
    loop().

loop() ->
    io:format("Waiting for messages...~n", []),
    receive
        Msg ->
            io:format("Received a message: ~p~n", [Msg]),
            loop()
    after
        60000 ->
            done
    end.
