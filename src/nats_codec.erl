-module(nats_codec).

-export([encode/1,
         decode/1]).

-include("include/nats.hrl").

encode(#{operation := Op} = Params) ->
    encode(Op, maps:without([Op], Params)).

encode('CONNECT', Opts) ->
    O = jsx:encode(Opts),
    <<"CONNECT ", O/binary, ?CRLF/binary>>;
encode('PUB', #{subject := Sbj, message := Msg, reply_to := RepTo}) ->
    Bytes = integer_to_binary(byte_size(Msg)),
    <<"PUB ", Sbj/binary, " ", RepTo/binary, " ", Bytes/binary, ?CRLF/binary, Msg/binary, ?CRLF/binary>>;
encode('PUB', #{subject := Sbj, message := Msg}) ->
    Bytes = integer_to_binary(byte_size(Msg)),
    <<"PUB ", Sbj/binary, " ", Bytes/binary, ?CRLF/binary, Msg/binary, ?CRLF/binary>>;
encode('SUB', #{subject := Sbj, sid := SID, queue_group := QG}) ->
    ID = integer_to_binary(SID),
    <<"SUB ", Sbj/binary, " ", QG/binary, " ", ID/binary, ?CRLF/binary>>;
encode('SUB', #{subject := Sbj, sid := SID}) ->
    ID = integer_to_binary(SID),
    <<"SUB ", Sbj/binary, " ", ID/binary, ?CRLF/binary>>;
encode('PING', _) ->
    ?PING;
encode('PONG', _) ->
    ?PONG.

decode(?PING) ->
    #{operation => 'PING'};
decode(?PONG) ->
    #{operation => 'PONG'};
decode(?OK) ->
    #{operation => 'OK'};
decode(Msg) ->
    [Op|Body] = string:split(Msg, " "),
    decode(upper_atom(Op), Body).

decode('INFO' = Op, [Body]) ->
    Info = jsx:decode(Body, [{labels, atom}, return_maps]),
    Info#{operation => Op};
decode('MSG' = Op, Body) ->
    [M, P, <<>>] = string:split(Body, ?CRLF, all),
    {S, I, R, B} = case string:split(M, " ", all) of
        [Sbj, ID, Rep, Bytes] ->
            {Sbj, binary_to_integer(ID), Rep, binary_to_integer(Bytes)};
        [Sbj, ID, Bytes] ->
            {Sbj, binary_to_integer(ID), undefined, binary_to_integer(Bytes)}
    end,
    #{operation => Op, subject => S, sid => I, reply_to => R, num_bytes => B, payload => P}.

upper_atom(In) ->
    to_atom([if L >= $a, L =< $z -> L - $a + $A; true -> L end || <<L>> <= In]).

to_atom("INFO")    -> 'INFO';
to_atom("CONNECT") -> 'CONNECT';
to_atom("PUB")     -> 'PUB';
to_atom("SUB")     -> 'SUB';
to_atom("UNSUB")   -> 'UNSUB';
to_atom("MSG")     -> 'MSG';
to_atom("PING")    -> 'PING';
to_atom("PONG")    -> 'INFO';
to_atom("+OK")     -> 'OK';
to_atom("-ERR")    -> 'ERR'.
