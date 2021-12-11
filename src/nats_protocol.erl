-module(nats_protocol).

-export([connect/2,
         pub/3, pub/4,
         sub/3, sub/4,
         unsub/2, unsub/3,
         ping/1, pong/1,
         unmarshal/1]).

%% for servers. we don't need them?
%-export([info/2,
%         msg/3,
%         ok/1,
%         err/1])

%% move to .hrl?
-define(OK, <<"+OK\r\n">>).
-define(PING, <<"PING\r\n">>).
-define(PONG, <<"PONG\r\n">>).
-define(CRLF, <<"\r\n">>).

%% TODO: add more specs.

-spec send(pid(), binary(), binary() | none) -> ok.
send(Conn, Msg, Expected) ->
    Conn ! {send, Msg, Expected},
    ok.

%% Sent to server to specify connection information.
%% Sent by: Client
connect(Conn, Opts) ->
    O = marshal_opts(Opts),
    send(Conn, <<"CONNECT ", O/binary, ?CRLF/binary>>, ?OK).

%% Publish a message to a subject, with optional reply subject.
%% Sent by: Client
pub(Conn, Subject, Message) ->
    Bytes = integer_to_binary(byte_size(Message)),
    Data = <<"PUB ", Subject/binary, " ", Bytes/binary, ?CRLF/binary, Message/binary, ?CRLF/binary>>,
    send(Conn, Data, ?OK).
pub(_Conn, _Subject, _ReplyTo, _Message) ->
    unimplemented.

%% Subscribe to a subject (or subject wildcard).
%% Sent by: Client
sub(Conn, Subject, SID) ->
    ID = integer_to_binary(SID),
    Data = <<"SUB ", Subject/binary, " ", ID/binary, ?CRLF/binary>>,
    send(Conn, Data, ?OK).
sub(_Conn, _Subject, _QueueGroup, _SID) ->
    unimplemented.

%% Unsubscribe (or auto-unsubscribe) from subject.
%% Sent by: Client
unsub(_Conn, _SID) -> unimplemented.
unsub(_Conn, _SID, _MaxMsgs) -> unimplemented.

%% PING keep-alive message.
%% Sent by: Both
ping(Conn) ->
    send(Conn, ?PING, ?PONG).

%% PONG keep-alive response.
%% Sent by: Both
pong(Conn) ->
    send(Conn, ?PONG, none).

%% Sent to client after initial TCP/IP connection.
%% Sent by: Server
%info(Conn, Opts) ->
%    O = marshal_opts(Opts),
%    send(Conn, <<"INFO ", O/binary, ?CRLF>>, none).

%% Delivers a message payload to a subscriber.
%% Sent by: Server
%msg(_Conn, _Subject, _Opts) -> unimplemented.

%% Acknowledges well-formed protocol message in verbose mode.
%% Sent by: Server
%ok(_Conn) -> unimplemented.

%% Indicates a protocol error. May cause client disconnect.
%% Sent by: Server
%err(_Conn, _Opts) -> unimplemented.


marshal_opts(Opts) when is_map(Opts); is_list(Opts) ->
    jsx:encode(Opts);
marshal_opts(Opts) ->
    Opts.

unmarshal(?PING) ->
    #{operation => 'PING'};
unmarshal(Msg) ->
    [Op|Body] = string:split(Msg, " "),
    unmarshal(uppercase(Op), Body).

unmarshal(<<"INFO">> = Op, [Body]) ->
    Info = jsx:decode(Body, [{labels, atom}, return_maps]),
    Info#{operation => binary_to_atom(Op)};
unmarshal(<<"MSG">> = Op, Body) ->
    [M, P, <<>>] = string:split(Body, ?CRLF, all),
    {S, I, R, B} = case string:split(M, " ", all) of
        [Sbj, ID, Rep, Bytes] ->
            {Sbj, binary_to_integer(ID), Rep, binary_to_integer(Bytes)};
        [Sbj, ID, Bytes] ->
            {Sbj, binary_to_integer(ID), undefined, binary_to_integer(Bytes)}
    end,
    #{operation => binary_to_atom(Op), subject => S, sid => I, reply_to => R,
      num_bytes => B, payload => P}.

uppercase(In) ->
    [if L >= $a, L =< $z -> L - $a + $A; true -> L end || <<L>> <= In].
