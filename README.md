# nats.erl

A simple [NATS](https://nats.io/) client library for Erlang/OTP.

[![Hex.pm version](https://img.shields.io/hexpm/v/natserl.svg)](https://hex.pm/packages/natserl) 
![CI status](https://github.com/wmnsk/nats.erl/actions/workflows/test.yml/badge.svg)

_This project is still WIP. Any of the implementations, including exported APIs, may change drastically and frequently._

# Features

- [ ] Connecting to a NATS server
  - [x] A subscriber receiving messages (publish/subscribe)
  - [x] A subscriber receiving  messages (request/reply)
  - [x] A publisher sending messages
  - [x] Keep-alive (PING/PONG) handling
  - [x] Unsubscribing
  - [ ] Using TLS
- [ ] Authentications
  - [ ] Token Authentication
  - [ ] Username/Password credentials
  - [ ] TLS Certificate
  - [ ] NKEY with Challenge
  - [ ] Decentralized JWT Authentication/Authorization

# Usage

See [Subscriber/Publisher examples](./example) for details.

```erlang
%% Start gen_server with network info.
{ok, Pid} = natserl:start_link(#{
    name => <<"natserl">>,  % any name unique per connection
    remote_address => Host, % server address
    remote_port => P,       % server port
    ping_interval => 1000   % interval to send PING
}),

%% Connect to your NATS server.
%% Info has the contents of INFO message from the server.
{ok, Info} = natserl:connect(Pid),

%% Subscribe or publish with required parameters.
ok = natserl:subscribe(Pid, Subject, SID),
ok = natserl:publish(Pid, Subject, Message),

%% After subscribing, received messages are sent to your Pid.
receive
    Msg ->
        io:format("Received: ~p~n", [Msg]),
end

%% Or you can receive messages on the Pid you specify.
Receiver = spawn(fun() -> your_message_handler() end),
natserl:receive_on(Pid, SID, Receiver),
```

# Running examples

Compile everything first in the project root.

```shell-session
rebar3 compile
```

Run the subscriber example.  
Params: `<server address> <server port> <subject> <sid>`.

```shell-session
$ example/subscribe.escript 127.0.0.2 4222 test.abc "SID"
Connected to server: #{client_id => 4,client_ip => <<"127.0.0.1">>,
                       go => <<"go1.17.1">>,headers => true,
                       host => <<"0.0.0.0">>,max_payload => 1048576,
                       operation => 'INFO',port => 4222,proto => 1,
                       server_id =>
                           <<"NAQYDOL7KUQNF2LUS46JWIC6L6OA65MOHC2H4DNCMUV3TNRXSFV5KE47">>,
                       server_name =>
                           <<"NAQYDOL7KUQNF2LUS46JWIC6L6OA65MOHC2H4DNCMUV3TNRXSFV5KE47">>,
                       version => <<"2.6.2">>}
Waiting for messages...
Received a message: #{num_bytes => 9,operation => 'MSG',
                      payload => <<"Hi there!">>,reply_to => undefined,
                      sid => <<"SID">>,subject => <<"test.abc">>}
Waiting for messages...
```

Then run the publisher example on another terminal session.  
Params: `<server address> <server port> <subject> <message>`.

```shell-session
$ ./example/publish.escript 127.0.0.2 4222 test.abc "Hi there!"
Connected to server: #{client_id => 5,client_ip => <<"127.0.0.1">>,
                       go => <<"go1.17.1">>,headers => true,
                       host => <<"0.0.0.0">>,max_payload => 1048576,
                       operation => 'INFO',port => 4222,proto => 1,
                       server_id =>
                           <<"NAQYDOL7KUQNF2LUS46JWIC6L6OA65MOHC2H4DNCMUV3TNRXSFV5KE47">>,
                       server_name =>
                           <<"NAQYDOL7KUQNF2LUS46JWIC6L6OA65MOHC2H4DNCMUV3TNRXSFV5KE47">>,
                       version => <<"2.6.2">>}
Published a message "Hi there!" with subject "test.abc"
```

You'll see the received message on the subscriber's session. It is a map that consists of all the fields of a MSG operation.

```shell-session
Received a message: #{num_bytes => 9,operation => 'MSG',
                      payload => <<"Hi there!">>,reply_to => undefined,
                      sid => <<"SID">>,subject => <<"test.abc">>}
Waiting for messages...
```

## Author(s)

Yoshiyuki Kurauchi ([Website](https://wmnsk.com/) / [Twitter](https://twitter.com/wmnskdmms)), with a lot of help from my teammates in [Working Group Two](https://wgtwo.com).

_We're always open to welcome co-authors! Please feel free to talk to us._

# LICENSE

[Apache License 2.0](./LICENSE)
