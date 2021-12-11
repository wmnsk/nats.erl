-module(nats_codec_tests).

-include_lib("eunit/include/eunit.hrl").

ping_test_() ->
    Bin = <<"PING\r\n">>,
    Map = #{operation => 'PING'},
    [?_assertEqual(Map, nats_codec:decode(Bin)),
     ?_assertEqual(Bin, nats_codec:encode(Map))].

pong_test_() ->
    Bin = <<"PONG\r\n">>,
    Map = #{operation => 'PONG'},
    [?_assertEqual(Map, nats_codec:decode(Bin)),
     ?_assertEqual(Bin, nats_codec:encode(Map))].
