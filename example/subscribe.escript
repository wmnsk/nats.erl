#!/usr/bin/env escript
%%! -pa _build/default/lib/natserl/ebin -pa _build/default/lib/jsx/ebin -Wall

main([Host, Port, Subject, ID]) ->
    P = list_to_integer(Port),
    {ok, Pid} = natserl:start_link(#{name => <<"natserl1">>,
                                     remote_address => Host,
                                     remote_port => P,
                                     ping_interval => 1000}),
    {ok, Info} = natserl:connect(Pid),
    io:format("Connected to server: ~p~n", [Info]),
    SID = list_to_binary(ID),
    ok = natserl:subscribe(Pid, list_to_binary(Subject), SID),
    Receiver = spawn(fun() -> handle_message() end),
    natserl:receive_on(Pid, SID, Receiver),
    timer:sleep(infinity).

handle_message() ->
    io:format("Waiting for messages...~n", []),
    receive
        Msg ->
            io:format("Received a message: ~p~n", [Msg]),
            handle_message()
    end.
