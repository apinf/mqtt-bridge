mqtt-bridge
===========

A one-way mqtt bridge to republish topics from one mqtt broker to another. Can change QoS and rewrite topics (prefix rewriting only).

Please see `config/config.exs` for sample configuration.

Usage
=====

Include this in any erlang based project's release by adding `mqtt_bridge` to the list of started applications.

Alternatively, run ad-hoc with `mix do deps.get, compile, run --no-halt` after installing erlang and elixir.
