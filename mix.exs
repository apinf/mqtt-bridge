defmodule EMQ.Plugins.Bridge.Mixfile do
  use Mix.Project
  def project do
    [
      app: :mqtt_bridge,
      version: "0.1.0",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      language: :erlang,
      deps: deps(),
    ]
  end

  defp deps do
    [
      {:emqttc, github: "phanimahesh/emqttc"},
      {:goldrush, github: "basho/goldrush", tag: "0.1.9", override: true, runtime: false, compile: "make"}
    ]
  end

  defp env() do
    # Refer to emqttc connect options for mqtt_opts
    [{:mqtt_opts, [{:logger, :info}]}]
  end

  def application do
    [
      mod: {:mqtt_bridge_app, []},
      env: env(),
      applications: [
        :kernel,
        :stdlib,
        :emqttc
      ],
    ]
  end
end
