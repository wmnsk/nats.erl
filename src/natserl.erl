-module(natserl).

-behaviour(gen_server).

-export([start_link/1,
         connect/0, connect/1,
         publish/2, publish/3,
         subscribe/2, subscribe/3,
         unsubscribe/1, unsubscribe/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include_lib("include/natserl.hrl").

-record(state,
        {remote_address :: inet:hostname(),
         remote_port    :: inet:portno(),
         socket         :: gen_tcp:socket(),
         conn           :: pid(),
         conn_info      :: map(),
         subscribers    :: map()
        }).

%% APIs for users
%% TODO: add specs.

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

connect() ->
    connect(#{}).
connect(Opts) ->
    Socket = gen_server:call(?MODULE, {connect, Opts}),
    {ok, Socket}.

publish(Subject, Message) ->
    publish(Subject, undefined, Message).
publish(Subject, ReplyTo, Message) ->
    gen_server:call(?MODULE, {publish, Subject, ReplyTo, Message}).

subscribe(Subject, SID) ->
    subscribe(Subject, undefined, SID).
subscribe(Subject, QueueGroup, SID) ->
    gen_server:call(?MODULE, {subscribe, Subject, QueueGroup, SID}).

%% not implemented yet.
unsubscribe(SID) ->
    unsubscribe(SID, undefined).
unsubscribe(SID, MaxMsgs) ->
    gen_server:call(?MODULE, {unsubscribe, SID, MaxMsgs}).

%% gen_server callbacks

init([Config]) ->
    Raddr = maps:get(remote_address, Config, undefined),
    Rport = maps:get(remote_port, Config, 0),
    {ok, Socket} = gen_tcp:connect(Raddr, Rport, [binary, {active, false}]),
    {ok, Msg} = gen_tcp:recv(Socket, 0, 1000),
    Info = natserl_codec:decode(Msg),

    Conn = spawn(fun() -> sender_loop(Socket) end),
    Interval = maps:get(ping_interval, Config, -1),
    timer:send_interval(Interval, ping_interval),

    {ok, #state{remote_address = Raddr,
                remote_port = Rport,
                socket = Socket,
                conn = Conn,
                conn_info = Info,
                subscribers = #{}}}.

handle_call({connect, Opts}, _From, State) ->
    Conn = State#state.conn,
    P = natserl_codec:encode(Opts#{operation => 'CONNECT'}),
    ok = send(Conn, P, ?OK),
    {reply, State#state.conn_info, State};
handle_call({publish, Subject, ReplyTo, Message}, _From, State) ->
    Conn = State#state.conn,
    P = natserl_codec:encode(#{operation => 'PUB',
                               subject => Subject,
                               reply_to => ReplyTo,
                               message => Message}),
    ok = send(Conn, P, ?OK),
    {reply, ok, State};
handle_call({subscribe, Subject, QueueGroup, SID}, {Sub, _Tag} = _From, State) ->
    Conn = State#state.conn,
    P = natserl_codec:encode(#{operation => 'SUB',
                               subject => Subject,
                               queue_group => QueueGroup,
                               sid => SID}),
    ok = send(Conn, P, ?OK),
    Subs = maps:merge(State#state.subscribers, #{SID => Sub}),
    {reply, ok, State#state{subscribers = Subs}};
handle_call(_Request, _From, State) ->
    {reply, nonexisting, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Msg}, State) ->
    NewState = handle_message(natserl_codec:decode(Msg), State),
    {noreply, NewState};
handle_info(ping_interval, State) ->
    P = natserl_codec:encode(#{operation => 'PING'}),
    ok = send(State#state.conn, P, ?PONG),
    ?LOG_DEBUG("Exchanged PING/PONG successfully~n", []),
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_ERROR("Meh: ~p~n", [Info]),
    {noreply, State}.

handle_message(#{operation := 'PING'}, State) ->
    P = natserl_codec:encode(#{operation => 'PONG'}),
    ok = send(State#state.conn, P, none),
    ?LOG_DEBUG("Responded to PING successfully~n", []),
    State;
handle_message(#{operation := 'MSG', sid := I} = U, State) ->
    Subscriber = maps:get(I, State#state.subscribers),
    Subscriber ! U,
    State.

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
            {ok, Rsp} = gen_tcp:recv(Socket, byte_size(Rsp));
        CatchAll ->
            ?LOG_ERROR("Huh?: ~p~n", [CatchAll])
    end,
    sender_loop(Socket).
