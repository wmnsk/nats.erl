-module(natserl_codec_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() ->
    Bin = <<"INFO {\"version\":\"0.1.0\"}\r\n">>,
    Map = #{operation => 'INFO', version => <<"0.1.0">>},
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

connect_test_() ->
    Bin = <<"CONNECT {\"verbose\":true}\r\n">>,
    Map = #{operation => 'CONNECT', verbose => true},
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

pub_test_() ->
    Bin = <<"PUB foo.bar 21\r\nHi this is publisher.\r\n">>,
    Map = #{
        operation => 'PUB',
        subject => <<"foo.bar">>,
        % can be omitted in encode
        reply_to => undefined,
        % can be omitted in encode
        num_bytes => 21,
        payload => <<"Hi this is publisher.">>
    },
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

pub_rep_test_() ->
    Bin = <<"PUB foo.bar my.inbox 21\r\nHi this is publisher.\r\n">>,
    Map = #{
        operation => 'PUB',
        subject => <<"foo.bar">>,
        reply_to => <<"my.inbox">>,
        % can be omitted in encode
        num_bytes => 21,
        payload => <<"Hi this is publisher.">>
    },
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

sub_test_() ->
    Bin = <<"SUB foo.bar abc123\r\n">>,
    Map = #{
        operation => 'SUB',
        subject => <<"foo.bar">>,
        sid => <<"abc123">>,
        % can be omitted in encode
        queue_group => undefined
    },
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

sub_qg_test_() ->
    Bin = <<"SUB foo.bar qg1 abc123\r\n">>,
    Map = #{
        operation => 'SUB',
        subject => <<"foo.bar">>,
        sid => <<"abc123">>,
        % can be omitted in encode
        queue_group => <<"qg1">>
    },
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

unsub_test_() ->
    Bin = <<"UNSUB abc123\r\n">>,
    Map = #{
        operation => 'UNSUB',
        sid => <<"abc123">>,
        % can be omitted in encode
        max_msgs => undefined
    },
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

unsub_msgs_test_() ->
    Bin = <<"UNSUB abc123 255\r\n">>,
    Map = #{
        operation => 'UNSUB',
        sid => <<"abc123">>,
        max_msgs => 255
    },
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

msg_test_() ->
    Bin = <<"MSG foo.bar abc123 18\r\nYou got a message.\r\n">>,
    Map = #{
        operation => 'MSG',
        subject => <<"foo.bar">>,
        sid => <<"abc123">>,
        % can be omitted in encode
        reply_to => undefined,
        % can be omitted in encode
        num_bytes => 18,
        payload => <<"You got a message.">>
    },
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

ping_test_() ->
    Bin = <<"PING\r\n">>,
    Map = #{operation => 'PING'},
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

pong_test_() ->
    Bin = <<"PONG\r\n">>,
    Map = #{operation => 'PONG'},
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

ok_test_() ->
    Bin = <<"+OK\r\n">>,
    Map = #{operation => 'OK'},
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].

err_test_() ->
    Bin = <<"-ERR 'Unknown Protocol Operation'\r\n">>,
    Map = #{operation => 'ERR', message => <<"'Unknown Protocol Operation'">>},
    [
        ?_assertEqual(Map, natserl_codec:decode(Bin)),
        ?_assertEqual(Bin, natserl_codec:encode(Map))
    ].
