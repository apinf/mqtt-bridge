-module(mqtt_bridge).

-export([start/0]).

-export([init/1, handle_info/2, terminate/2]).

-type topic()  :: binary().
% -type qos() :: qos0 | qos1 | qos2 | 0 | 1 | 2.
-type qos() :: emqttc:mqttc_qosopt().
-type topicandqos() ::{topic(), qos()}.
-type topicrewritespec() :: [{topicandqos(), topicandqos()}].
-type mqttc_opt() :: emqttc:mqttc_opt().

-record(state, {source :: pid(),
                sink :: pid(),
                transforms :: topicrewritespec(),
                source_opts :: [emqttc:mqttc_opt()],
                sink_opts :: [emqttc:mqttc_opt()]
               }).
start() ->
  {ok, TopicRewriteSpec} = application:get_env(topics),
  {ok, SourceConn} = application:get_env(source_connection),
  {ok, SinkConn} = application:get_env(sink_connection),
  Config = {SourceConn, SinkConn, TopicRewriteSpec},
  start_link(Config).

start_link(Args) ->
  start_link({local, ?MODULE}, Args).

start_link(Name, Args) ->
  gen_server:start_link(Name, ?MODULE, Args, []).

-spec init(Args) -> {ok, Client} | no_return() when
    Args      :: {MqttOpts, topicrewritespec()},
    MqttOpts  :: [mqttc_opt()],
    Client    :: pid().
init({SourceOpts, SinkOpts, TopicRewriteSpec}) ->
  process_flag(trap_exit, true),
  Source = connect_and_subscribe(SourceOpts, source, TopicRewriteSpec),
  Sink = connect_and_subscribe(SinkOpts, sink, TopicRewriteSpec),
  State = #state{source = Source,
                 sink = Sink,
                 transforms = TopicRewriteSpec,
                 source_opts = SourceOpts,
                 sink_opts = SinkOpts},
  {ok, State}.


handle_info({publish, C, Topic, Payload}, State = #state{source = C}) ->
  case transform(Topic, State#state.transforms) of
    undefined -> ok;
    NewTopic -> emqttc:publish(State#state.sink, NewTopic, Payload)
  end,
  {noreply, State};

handle_info({mqttc, C, connected}, State = #state{source = C}) ->
  io:format("Source (~p) connected~n", [C]),
  {noreply, State};
handle_info({mqttc, C, connected}, State = #state{sink = C}) ->
  io:format("Sink (~p) connected~n", [C]),
  {noreply, State};

handle_info({mqttc, C, disconnected}, S = #state{sink = C}) ->
  io:format("Sink (~p) disconnected~n", [C]),
  {noreply, S};
handle_info({mqttc, C, disconnected}, S = #state{source = C}) ->
  io:format("Source (~p) disconnected~n", [C]),
  {noreply, S};
handle_info({'EXIT', C, Reason}, S = #state{source = C}) ->
  io:format("Source ~p exited for reason ~p~n", [C, Reason]),
  C1 = connect_and_subscribe(S#state.source_opts, source, S#state.transforms),
  {noreply, S#state{source = C1}};
handle_info({'EXIT', C, Reason}, S = #state{sink = C}) ->
  io:format("Sink ~p exited for reason ~p~n", [C, Reason]),
  C1 = connect_and_subscribe(S#state.sink_opts, sink, S#state.transforms),
  {noreply, S#state{sink = C1}};
handle_info(Event, State) ->
  io:format("UNEXPECTED EVENT: ~p~n", [Event]),
  {noreply, State}.


terminate(Reason, _State) ->
 io:format("Terminating for reason ~p~n", [Reason]),
 ok.

-spec connect_and_subscribe([mqttc_opt()],
                            sink | source,
                            topicrewritespec()) -> pid() | no_return().
connect_and_subscribe(ConnOpts, sink, _TopicRewriteSpec) ->
  {ok, Sink} = emqttc:start_link(ConnOpts),
  Sink;
connect_and_subscribe(ConnOpts, source, TopicRewriteSpec) ->
  {ok, Source} = emqttc:start_link(ConnOpts),
  Topics = lists:map(fun ({R, _L}) -> R end, TopicRewriteSpec),
  ok = emqttc:subscribe(Source, Topics),
  Source.

transform(_Topic, []) -> undefined;
transform(Topic, [{{From, _}, {To, _}} | Tail]) ->
  case emqttc_topic:match(Topic, From) of
    true -> replace(Topic, From, To);
    false -> transform(Topic, Tail)
  end.

%% Currently only entire topic trees can be bridged.
%% Source and sink topics must not have `+` and may end in `#`.
replace(Topic, From, To) when From == To -> Topic;
replace(Topic, From, To) ->
  [From0, <<>>] = binary:split(From, <<"#">>, []),
  [To0, <<>>] = binary:split(To, <<"#">>, []),
  Len = erlang:byte_size(From0),
  Len1 = erlang:byte_size(Topic),
  Rest = erlang:binary_part(Topic, {Len, Len1 - Len}),
  normalize_topic(To0, Rest).

normalize_topic(Prefix, Suffix) ->
  case {binary:last(Prefix), binary:first(Suffix)} of
    {$/, $/} -> <<$/, S0>> = Suffix, <<Prefix, S0>>;
    {$/, _} -> <<Prefix/binary, Suffix/binary>>;
    {_, $/} -> <<Prefix, Suffix>>;
    {_, _} -> <<Prefix, $/, Suffix>>
  end.
