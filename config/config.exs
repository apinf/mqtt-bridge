use Mix.Config

config :mqtt_bridge, :topics, [
  {{"/source/topic/tree/to/be/remounted/#", 0}, {"/rewritten/sink/topic/tree/#", 0}},
  {{"/another/source/tree/#", 1}, {"/sink/tree/#", 1}},
]

config :mqtt_bridge, :sink_connection, [
  {:host, 'localhost'},
  {:port, 1883},
  {:client_id, "sink-client-id"},
  {:logger, :info}
]

config :mqtt_bridge, :source_connection, [
  {:host, 't.emqtt.io'},
  {:port, 1883},
  {:client_id, "source-client-id"},
  {:logger, :info}
]
