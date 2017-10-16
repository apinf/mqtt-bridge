-module(mqtt_bridge_app).
-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([start_link/0, init/1, terminate/1]).

start(_Type, _Args) ->
  start_link().
stop(_State) ->
  ok.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  Opts = {one_for_one, 1, 5},
  Listener = {listener,
              {mqtt_bridge, start, []},
              permanent,
              5000,
              worker,
              [emq_plugin_bridge_remotelistener]},
  Children = [Listener],
  {ok, {Opts, Children}}.

terminate(_Reason) -> ok.
