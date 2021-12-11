#!/usr/bin/env escript
%%! -pa _build/default/lib/natserl/ebin -Wall -pa _build/default/lib/jsx/ebin

main([Host, Port, Subject, Message]) ->
    P = list_to_integer(Port),
    natserl:start_link(#{remote_address => Host, remote_port => P}),
    {ok, Info} = natserl:connect(),
    io:format("~p~n", [Info]),
    ok = natserl:publish(list_to_binary(Subject), list_to_binary(Message)).
