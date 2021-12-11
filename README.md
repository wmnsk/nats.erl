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
  - [ ] Unsubscribing
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
natserl:start_link(#{remote_address => Host, % server address
                     remote_port => P,       % server port
                     ping_interval => 1000   % interval to send PING
                    }),

%% Connect to your NATS server.
%% Info has the contents of INFO message from the server.
{ok, Info} = natserl:connect(),

%% Subscribe or publish with required parameters.
ok = natserl:subscribe(Subject, SID),
ok = natserl:publish(Subject, Message),

%% After subscribing, received messages are sent to your Pid.
receive
    Msg ->
        io:format("Received: ~p~n", [Msg]),
end
```

# Running examples

Compile everything first in the project root.

```shell-session
rebar3 compile
```

Run the subscriber example.  
Params: `<server address> <server port> <subject> <sid>`.

```shell-session
./example/subscribe.escript 127.0.0.2 4222 foo.bar 11
```

Then run the publisher example on another terminal session.  
Params: `<server address> <server port> <subject> <message>`.

```shell-session
./example/publish.escript 127.0.0.2 4222 foo.bar "msg to publish"
```

## Author(s)

Yoshiyuki Kurauchi ([Website](https://wmnsk.com/) / [Twitter](https://twitter.com/wmnskdmms)), with a lot of help from my teammates in [Working Group Two](https://wgtwo.com).

_We're always open to welcome co-authors! Please feel free to talk to us._

# LICENSE

[Apache License 2.0](./LICENSE)
