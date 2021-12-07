#!/usr/bin/env escript
%%! -pa _build/default/lib/nats/ebin -Wall -pa _build/default/lib/jsx/ebin

main([Host, Port, Subject, Message]) ->
    P = list_to_integer(Port),
    nats:start_link(#{remote_address => Host, remote_port => P}),
    {ok, Info} = nats:connect(),
    io:format("~p~n", [Info]),
    ok = nats:publish(list_to_binary(Subject), list_to_binary(Message)).
