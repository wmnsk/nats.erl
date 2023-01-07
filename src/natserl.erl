-module(natserl).

-behaviour(gen_server).

-export([
    start_link/1,
    connect/0, connect/1, connect/2,
    publish/2, publish/3, publish/4,
    subscribe/2, subscribe/3, subscribe/4,
    unsubscribe/1, unsubscribe/2, unsubscribe/3,
    receive_on/2, receive_on/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("include/natserl.hrl").

-record(state, {
    name :: atom(),
    remote_address :: inet:hostname(),
    remote_port :: inet:portno(),
    socket :: gen_tcp:socket(),
    conn :: pid(),
    conn_info :: map(),
    decoder :: pid(),
    subscribers :: map()
}).

%% APIs for users
%% TODO: add specs.

start_link(Config) ->
    Name = binary_to_atom(maps:get(name, Config, <<"natserl">>)),
    gen_server:start_link({local, Name}, ?MODULE, [Config#{name => Name}], []).

connect() ->
    connect(?MODULE, #{}).
connect(Pid) when is_pid(Pid) ->
    connect(Pid, #{});
connect(Opts) when is_map(Opts) ->
    connect(?MODULE, Opts).
connect(Pid, Opts) ->
    Info = gen_server:call(Pid, {connect, Opts}),
    {ok, Info}.

publish(Subject, Message) ->
    publish(?MODULE, Subject, undefined, Message).
publish(Pid, Subject, Message) when is_pid(Pid) ->
    publish(Pid, Subject, undefined, Message);
publish(Subject, ReplyTo, Message) ->
    publish(?MODULE, Subject, ReplyTo, Message).
publish(Pid, Subject, ReplyTo, Message) ->
    gen_server:call(Pid, {publish, Subject, ReplyTo, Message}).

subscribe(Subject, SID) ->
    subscribe(?MODULE, Subject, undefined, SID).
subscribe(Pid, Subject, SID) when is_pid(Pid) ->
    subscribe(Pid, Subject, undefined, SID);
subscribe(Subject, QueueGroup, SID) ->
    subscribe(?MODULE, Subject, QueueGroup, SID).
subscribe(Pid, Subject, QueueGroup, SID) ->
    gen_server:call(Pid, {subscribe, Subject, QueueGroup, SID}).

unsubscribe(SID) ->
    unsubscribe(?MODULE, SID, undefined).
unsubscribe(Pid, SID) when is_pid(Pid) ->
    unsubscribe(Pid, SID, undefined);
unsubscribe(SID, MaxMsgs) ->
    unsubscribe(?MODULE, SID, MaxMsgs).
unsubscribe(Pid, SID, MaxMsgs) ->
    gen_server:call(Pid, {unsubscribe, SID, MaxMsgs}).

receive_on(SID, Receiver) ->
    receive_on(?MODULE, SID, Receiver).
receive_on(Pid, SID, Receiver) ->
    gen_server:call(Pid, {receive_on, SID, Receiver}).

%% gen_server callbacks
%% TODO: use gen_statem instead.

init([Config]) ->
    Name = maps:get(name, Config),
    Raddr = maps:get(remote_address, Config, undefined),
    Rport = maps:get(remote_port, Config, 0),
    {ok, Socket} = gen_tcp:connect(Raddr, Rport, [binary, {active, false}]),
    {ok, Msg} = gen_tcp:recv(Socket, 0, 1000),
    Info = natserl_codec:decode(Msg),

    Conn = spawn_link(fun() -> sender_loop(Socket) end),
    Decoder = spawn_link(fun() -> decoder_loop(<<>>) end),
    Interval = maps:get(ping_interval, Config, -1),
    timer:send_interval(Interval, ping_interval),

    State = #state{
        name = Name,
        remote_address = Raddr,
        remote_port = Rport,
        socket = Socket,
        conn = Conn,
        conn_info = Info,
        decoder = Decoder,
        subscribers = #{}
    },
    {ok, State}.

handle_call({connect, Opts}, _From, State) ->
    Conn = State#state.conn,
    P = natserl_codec:encode(Opts#{operation => 'CONNECT'}),
    ok = send(Conn, P, ?OK),
    {reply, State#state.conn_info, State};
handle_call({publish, Subject, ReplyTo, Message}, _From, State) ->
    Conn = State#state.conn,
    P = natserl_codec:encode(#{
        operation => 'PUB',
        subject => Subject,
        reply_to => ReplyTo,
        payload => Message
    }),
    ok = send(Conn, P, ?OK),
    {reply, ok, State};
handle_call({subscribe, Subject, QueueGroup, SID}, {Sub, _Tag} = _From, State) ->
    Conn = State#state.conn,
    P = natserl_codec:encode(#{
        operation => 'SUB',
        subject => Subject,
        queue_group => QueueGroup,
        sid => SID
    }),
    ok = send(Conn, P, ?OK),
    Subs = maps:merge(State#state.subscribers, #{SID => Sub}),
    {reply, ok, State#state{subscribers = Subs}};
handle_call({unsubscribe, SID, MaxMsgs}, _From, State) ->
    Conn = State#state.conn,
    P = natserl_codec:encode(#{
        operation => 'UNSUB',
        sid => SID,
        max_msgs => MaxMsgs
    }),
    ok = send(Conn, P, ?OK),
    Subs = maps:remove(SID, State#state.subscribers),
    {reply, ok, State#state{subscribers = Subs}};
handle_call({receive_on, SID, Receiver}, _From, State) ->
    Subs = maps:merge(State#state.subscribers, #{SID => Receiver}),
    {reply, ok, State#state{subscribers = Subs}};
handle_call(_Request, _From, State) ->
    {reply, nonexisting, State}.

handle_cast(Msg, State) ->
    ?LOG_ERROR("Got unknown cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info({tcp, _Socket, Msg}, State) ->
    State#state.decoder ! {Msg, State},
    {noreply, State};
handle_info(ping_interval, State) ->
    P = natserl_codec:encode(#{operation => 'PING'}),
    ok = send(State#state.conn, P, ?PONG),
    ?LOG_DEBUG("Exchanged PING/PONG successfully~n", []),
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_ERROR("Got unknown info: ~p~n", [Info]),
    {noreply, State}.

handle_message(#{operation := 'PING'}, State) ->
    P = natserl_codec:encode(#{operation => 'PONG'}),
    ok = send(State#state.conn, P, none),
    ?LOG_DEBUG("Responded to PING successfully~n", []),
    ok;
handle_message(#{operation := 'MSG', sid := I} = U, State) ->
    Subscriber = maps:get(I, State#state.subscribers),
    Subscriber ! U,
    ok.

decoder_loop(Previous) ->
    receive
        {Msg, State} ->
            M = <<Previous/binary, Msg/binary>>,
            case natserl_codec:decode(M) of
                {need_more_data, _Len} ->
                    decoder_loop(M);
                Decoded ->
                    ok = handle_message(Decoded, State)
            end
    end,
    decoder_loop(<<>>).

terminate(_Reason, State) ->
    Socket = State#state.socket,
    gen_tcp:close(Socket).

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.

-spec send(pid(), binary(), binary() | none) -> ok.
send(Conn, Msg, Expected) ->
    Conn ! {send, Msg, Expected},
    ok.

sender_loop(Socket) ->
    inet:setopts(Socket, [{active, true}]),
    receive
        {send, Bin, none} ->
            gen_tcp:send(Socket, Bin);
        {send, Bin, Rsp} ->
            inet:setopts(Socket, [{active, false}]),
            gen_tcp:send(Socket, Bin),
            verify_response(Socket, Rsp)
    end,
    sender_loop(Socket).

verify_response(Socket, Rsp) ->
    {ok, R} = gen_tcp:recv(Socket, byte_size(Rsp)),
    case R of
        Rsp ->
            ok;
        ?PING ->
            P = natserl_codec:encode(#{operation => 'PONG'}),
            gen_tcp:send(Socket, P),
            ?LOG_DEBUG("Responded to PING successfully~n", []),
            verify_response(Socket, Rsp);
        Unknown ->
            ?LOG_ERROR("Got unexpected response: ~p~n", [Unknown])
    end.
