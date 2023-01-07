-define(CRLF, <<"\r\n">>).

%% Protocol Messages
%% https://docs.nats.io/reference/reference-protocols/nats-protocol#protocol-messages
-define(INFO(Vals),
    <<"INFO ", Vals/binary, "\r\n">>
).
-define(CONNECT(Opts),
    <<"CONNECT ", Opts/binary, "\r\n">>
).
-define(PUB(Sbj, Bytes, Msg),
    <<"PUB ", Sbj/binary, " ", Bytes/binary, "\r\n", Msg/binary, "\r\n">>
).
-define(PUB(Sbj, RepTo, Bytes, Msg),
    <<"PUB ", Sbj/binary, " ", RepTo/binary, " ", Bytes/binary, "\r\n", Msg/binary, "\r\n">>
).
-define(SUB(Sbj, SID),
    <<"SUB ", Sbj/binary, " ", SID/binary, "\r\n">>
).
-define(SUB(Sbj, QG, SID),
    <<"SUB ", Sbj/binary, " ", QG/binary, " ", ID/binary, "\r\n">>
).
-define(UNSUB(SID),
    <<"UNSUB ", SID/binary, "\r\n">>
).
-define(UNSUB(SID, Max),
    <<"UNSUB ", SID/binary, " ", Max/binary, "\r\n">>
).
-define(MSG(Sbj, SID, Bytes, Msg),
    <<"MSG ", Sbj/binary, " ", SID/binary, " ", Bytes/binary, "\r\n", Msg/binary, "\r\n">>
).
-define(MSG(Sbj, SID, RepTo, Bytes, Msg),
    <<"MSG ", Sbj/binary, " ", SID/binary, " ", RepTo/binary, " ", Bytes/binary, "\r\n", Msg/binary,
        "\r\n">>
).
-define(PING,
    <<"PING\r\n">>
).
-define(PONG,
    <<"PONG\r\n">>
).
-define(OK,
    <<"+OK\r\n">>
).
-define(ERR(Msg),
    <<"-ERR ", Msg/binary, "\r\n">>
).
-define(ERR_UNKNOWN_PROTOCOL_OPERATION,
    ?ERR(<<"'Unknown Protocol Operation'">>)
).
-define(ERR_ATTEMPTED_TO_CONNECT_TO_ROUTE_PORT,
    ?ERR(<<"'Attempted To Connect To Route Port'">>)
).
-define(ERR_AUTHORIZATION_VIOLATION,
    ?ERR(<<"'Authorization Violation'">>)
).
-define(ERR_AUTHORIZATION_TIMEOUT,
    ?ERR(<<"'Authorization Timeout'">>)
).
-define(ERR_INVALID_CLIENT_PROTOCOL,
    ?ERR(<<"'Invalid Client Protocol'">>)
).
-define(ERR_MAXIMUM_CONTROL_LINE_EXCEEDED,
    ?ERR(<<"'Maximum Control Line Exceeded'">>)
).
-define(ERR_PARSER_ERROR,
    ?ERR(<<"'Parser Error'">>)
).
-define(ERR_SECURE_CONNECTION_TLS_REQUIRED,
    ?ERR(<<"'Secure Connection - TLS Required'">>)
).
-define(ERR_STALE_CONNECTION,
    ?ERR(<<"'Stale Connection'">>)
).
-define(ERR_MAXIMUM_CONNECTIONS_EXCEEDED,
    ?ERR(<<"'Maximum Connections Exceeded'">>)
).
-define(ERR_SLOW_CONSUMER,
    ?ERR(<<"'Slow Consumer'">>)
).
-define(ERR_MAXIMUM_PAYLOAD_VIOLATION,
    ?ERR(<<"'Maximum Payload Violation'">>)
).
-define(ERR_INVALID_SUBJECT,
    ?ERR(<<"'Invalid Subject'">>)
).
-define(ERR_PERMISSIONS_VIOLATION_FOR_SUBSCRIPTION_TO(Sbj),
    ?ERR(<<"'Permissions Violation for Subscription to ", Sbj/binary, "'">>)
).
-define(ERR_PERMISSIONS_VIOLATION_FOR_PUBLISH_TO(Sbj),
    ?ERR(<<"'Permissions Violation for Publish to ", Sbj/binary, "'">>)
).
