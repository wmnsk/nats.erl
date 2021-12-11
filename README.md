# nats.erl

A simple [NATS](https://nats.io/) client library for Erlang/OTP.

[![Hex.pm version](https://img.shields.io/hexpm/v/natserl.svg)](https://hex.pm/packages/natserl) 
![CI status](https://github.com/wmnsk/nats.erl/actions/workflows/test.yml/badge.svg)

_This project is still WIP. Any of the implementations, including exported APIs, may change drastically and frequently._

# Features

- [ ] Connect to a NATS server
  - [x] A subscriber receiving messages (publish/subscribe)
  - [ ] A subscriber receiving  messages (request/reply)
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
natserl:start_link(#{remote_address => Host,
                     remote_port => P,
                     ping_interval => 1000}),

%% Connect to your NATS server.
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

Compile and run the sample subscriber.

```shell-session
rebar3 compile
./example/subscribe.escript 127.0.0.2 4222 foo.bar 11
```

Then run the sample publisher on another terminal session.

```shell-session
./example/publish.escript 127.0.0.2 4222 foo.bar "msg to publish"
```

## Author(s)

Yoshiyuki Kurauchi ([Website](https://wmnsk.com/) / [Twitter](https://twitter.com/wmnskdmms)), with a lot of help from my teammates in [Working Group Two](https://wgtwo.com).

_We're always open to welcome co-authors! Please feel free to talk to us._

# LICENSE

[Apache License 2.0](./LICENSE)
