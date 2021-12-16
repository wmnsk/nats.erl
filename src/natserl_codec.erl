-module(natserl_codec).

-export([encode/1,
         decode/1]).

-include_lib("include/natserl.hrl").

encode(#{operation := Op} = Params) ->
    encode(Op, maps:without([operation], Params)).

encode('INFO', Info) ->
    I = jsx:encode(Info),
    ?INFO(I);
encode('CONNECT', Opts) ->
    O = jsx:encode(Opts),
    ?CONNECT(O);
encode('PUB', #{subject := Sbj, payload := Msg} = Body) ->
    Bytes = integer_to_binary(byte_size(Msg)),
    case maps:get(reply_to, Body, undefined) of
        undefined ->
            ?PUB(Sbj, Bytes, Msg);
        RepTo ->
            ?PUB(Sbj, RepTo, Bytes, Msg)
    end;
encode('SUB', #{subject := Sbj, sid := SID} = Body) ->
    case maps:get(queue_group, Body, undefined) of
        undefined ->
            ?SUB(Sbj, SID);
        QG ->
            %% If I remove this variable, compiler will say
            %% "src/natserl_codec.erl:31:23: variable 'ID' is unbound."
            %% `rebar3 clean` and removing _build/ dir didn't work.
            %% So weird ¯\_(ツ)_/¯
            ID = SID,
            ?SUB(Sbj, QG, SID)
    end;
encode('UNSUB', #{sid := SID} = Body) ->
    case maps:get(max_msgs, Body, undefined) of
        undefined ->
            ?UNSUB(SID);
        Max ->
            M = integer_to_binary(Max),
            ?UNSUB(SID, M)
    end;
encode('MSG', #{subject := Sbj, sid := SID, payload := Msg} = Body) ->
    Bytes = integer_to_binary(byte_size(Msg)),
    case maps:get(reply_to, Body, undefined) of
        undefined ->
            ?MSG(Sbj, SID, Bytes, Msg);
        RepTo ->
            ?MSG(Sbj, SID, RepTo, Bytes, Msg)
    end;
encode('PING', _) ->
    ?PING;
encode('PONG', _) ->
    ?PONG;
encode('OK', _) ->
    ?OK;
encode('ERR', #{message := Msg}) ->
    ?ERR(Msg).

decode(Msg) ->
    [Op|Body] = string:split(Msg, " "),
    decode(upper_atom(Op), Body).

decode('INFO' = Op, [Body]) ->
    Info = jsx:decode(Body, [{labels, atom}, return_maps]),
    Info#{operation => Op};
decode('CONNECT' = Op, [Body]) ->
    Opts = jsx:decode(Body, [{labels, atom}, return_maps]),
    Opts#{operation => Op};
decode('PUB' = Op, Body) ->
    [M, P, <<>>] = string:split(Body, ?CRLF, all),
    {S, R, B} = case string:split(M, " ", all) of
        [Sbj, Rep, Bytes] ->
            {Sbj, Rep, binary_to_integer(Bytes)};
        [Sbj, Bytes] ->
            {Sbj, undefined, binary_to_integer(Bytes)}
    end,
    #{operation => Op, subject => S, reply_to => R, num_bytes => B, payload => P};
decode('SUB' = Op, [Body]) ->
    {S, Q, I} = case string:split(remove_crlf(Body), " ", all) of
        [Sbj, QG, SID] ->
            {Sbj, QG, SID};
        [Sbj, SID] ->
            {Sbj, undefined, SID}
    end,
    #{operation => Op, subject => S, queue_group => Q, sid => I};
decode('UNSUB' = Op, [Body]) ->
    {I, M} = case string:split(remove_crlf(Body), " ", all) of
        [SID, Max] ->
            {SID, binary_to_integer(Max)};
        [SID] ->
            {SID, undefined}
    end,
    #{operation => Op, sid => I, max_msgs => M};
decode('MSG' = Op, Body) ->
    [M, P, <<>>] = string:split(Body, ?CRLF, all),
    {S, I, R, B} = case string:split(M, " ", all) of
        [Sbj, ID, Rep, Bytes] ->
            {Sbj, ID, Rep, binary_to_integer(Bytes)};
        [Sbj, ID, Bytes] ->
            {Sbj, ID, undefined, binary_to_integer(Bytes)}
    end,
    #{operation => Op, subject => S, sid => I, reply_to => R, num_bytes => B, payload => P};
decode('PING', _) ->
    #{operation => 'PING'};
decode('PONG', _) ->
    #{operation => 'PONG'};
decode('OK', _) ->
    #{operation => 'OK'};
decode('ERR' = Op, [Body]) ->
    #{operation => Op, message => remove_crlf(Body)}.

upper_atom(Bin) ->
    to_atom([if L >= $a, L =< $z -> L - $a + $A; true -> L end
             || <<L>> <= remove_crlf(Bin)]).

to_atom("INFO")    -> 'INFO';
to_atom("CONNECT") -> 'CONNECT';
to_atom("PUB")     -> 'PUB';
to_atom("SUB")     -> 'SUB';
to_atom("UNSUB")   -> 'UNSUB';
to_atom("MSG")     -> 'MSG';
to_atom("PING")    -> 'PING';
to_atom("PONG")    -> 'PONG';
to_atom("+OK")     -> 'OK';
to_atom("-ERR")    -> 'ERR'.

remove_crlf(Bin) ->
    %% for some reason string:trim(Bin, trailing, "\r\n") doesn't work.
    [B|_] = string:replace(Bin, "\r\n", ""),
    B.
