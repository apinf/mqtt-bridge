@moduledoc """
A schema is a keyword list which represents how to map, transform, and validate
configuration values parsed from the .conf file. The following is an explanation of
each key in the schema definition in order of appearance, and how to use them.

## Import

A list of application names (as atoms), which represent apps to load modules from
which you can then reference in your schema definition. This is how you import your
own custom Validator/Transform modules, or general utility modules for use in
validator/transform functions in the schema. For example, if you have an application
`:foo` which contains a custom Transform module, you would add it to your schema like so:

`[ import: [:foo], ..., transforms: ["myapp.some.setting": MyApp.SomeTransform]]`

## Extends

A list of application names (as atoms), which contain schemas that you want to extend
with this schema. By extending a schema, you effectively re-use definitions in the
extended schema. You may also override definitions from the extended schema by redefining them
in the extending schema. You use `:extends` like so:

`[ extends: [:foo], ... ]`

## Mappings

Mappings define how to interpret settings in the .conf when they are translated to
runtime configuration. They also define how the .conf will be generated, things like
documention, @see references, example values, etc.

See the moduledoc for `Conform.Schema.Mapping` for more details.

## Transforms

Transforms are custom functions which are executed to build the value which will be
stored at the path defined by the key. Transforms have access to the current config
state via the `Conform.Conf` module, and can use that to build complex configuration
from a combination of other config values.

See the moduledoc for `Conform.Schema.Transform` for more details and examples.

## Validators

Validators are simple functions which take two arguments, the value to be validated,
and arguments provided to the validator (used only by custom validators). A validator
checks the value, and returns `:ok` if it is valid, `{:warn, message}` if it is valid,
but should be brought to the users attention, or `{:error, message}` if it is invalid.

See the moduledoc for `Conform.Schema.Validator` for more details and examples.
"""
[
  extends: [],
  import: [],
  mappings: [
    "mqtt_bridge.topics": [
      to: "mqtt_bridge.topics",
      datatype: [list: :complex],
      default: [],
      doc: """
      Topic trees to be mirrored from source to sink.
      For each tree, choose an arbitrary descriptive name, ex: foo,
      and provide both source and sink trees as follows:
       
          mqtt_bridge.topics.foo.source.tree = "/source/tree/#"
          mqtt_bridge.topics.foo.sink.tree = "/rewritten/sink/tree/#"
       
      Optionally, qos can also be specified as
       
          mqtt_bridge.topics.foo.source.qos = 1
          mqtt_bridge.topics.foo.sink.qos = 0
       
      Where unspecified, qos is assumed to be 0.
      Isolated source or sink trees (without a matching counterpart)
      will be ignored.
      """,
      required: true,
      commented: true,
    ],
    "mqtt_bridge.topics.*.source.tree": [
      commented: true,
      hidden: true,
      datatype: :binary,
      required: true,
    ],
    "mqtt_bridge.topics.*.source.qos": [
      commented: true,
      hidden: true,
      datatype: :integer,
      default: 0
    ],
    "mqtt_bridge.topics.*.sink.tree": [
      commented: true,
      hidden: true,
      datatype: :binary,
      required: true
    ],
    "mqtt_bridge.topics.*.sink.qos": [
      commented: true,
      hidden: true,
      datatype: :integer,
      default: 0
    ],
    "mqtt_bridge.sink.host": [
      commented: false,
      datatype: :charlist,
      default: 'localhost',
      doc: "Provide documentation for mqtt_bridge.sink_connection.host here.",
      hidden: false,
      to: "mqtt_bridge.sink_connection.host"
    ],
    "mqtt_bridge.sink.port": [
      commented: false,
      datatype: :integer,
      default: 1883,
      doc: "Provide documentation for mqtt_bridge.sink_connection.port here.",
      hidden: false,
      to: "mqtt_bridge.sink_connection.port"
    ],
    "mqtt_bridge.sink.client_id": [
      commented: false,
      datatype: :binary,
      default: "client1",
      doc: "Provide documentation for mqtt_bridge.sink_connection.client_id here.",
      hidden: false,
      to: "mqtt_bridge.sink_connection.client_id"
    ],
    "mqtt_bridge.sink.logger": [
      commented: false,
      datatype: :atom,
      default: :info,
      doc: "Provide documentation for mqtt_bridge.sink_connection.logger here.",
      hidden: false,
      to: "mqtt_bridge.sink_connection.logger"
    ],
    "mqtt_bridge.sink.username": [
      commented: false,
      hidden: true,
      default: "username",
      to: "mqtt_bridge.sink_connection.username"
    ],
    "mqtt_bridge.sink.password": [
      commented: false,
      hidden: true,
      default: "password",
      to: "mqtt_bridge.sink_connection.password"
    ],
    "mqtt_bridge.source.host": [
      commented: false,
      datatype: :charlist,
      default: 'mqtt.hsl.fi',
      doc: "Provide documentation for mqtt_bridge.source_connection.host here.",
      hidden: false,
      to: "mqtt_bridge.source_connection.host"
    ],
    "mqtt_bridge.source.port": [
      commented: false,
      datatype: :integer,
      default: 1883,
      doc: "Provide documentation for mqtt_bridge.source_connection.port here.",
      hidden: false,
      to: "mqtt_bridge.source_connection.port"
    ],
    "mqtt_bridge.source.client_id": [
      commented: false,
      datatype: :binary,
      default: "source-client-id",
      doc: "Provide documentation for mqtt_bridge.source_connection.client_id here.",
      hidden: false,
      to: "mqtt_bridge.source_connection.client_id"
    ],
    "mqtt_bridge.source.logger": [
      commented: false,
      datatype: :atom,
      default: :info,
      doc: "Provide documentation for mqtt_bridge.source_connection.logger here.",
      hidden: false,
      to: "mqtt_bridge.source_connection.logger"
    ]
  ],
  transforms: [
    "mqtt_bridge.topics": fn conf ->
      topics = Conform.Conf.find(conf, "mqtt_bridge.topics.$n")
      |> Enum.group_by(
        fn {[_, _, n, _, _], _} -> n end,
        fn {[_, _, _, s, t], v} -> {s, t, v} end
        # s - 'source' or 'sink'
        # t - 'tree' or 'qos'
        # v - tree (string) or qos (0|1|2)
      )
      |> Map.values()
      # List of lists with {s, t, v}
      |> Enum.map(fn l ->
        # l is a list of {s, t, v}
        Enum.group_by(l,
                      fn {s, _t, _v} -> s end,
                      fn {_s, t, v} -> {t, v} end)
        # Map with s as key, list of {t, v} as value
        |> Enum.map(fn {s, tvs} -> {s, Map.new(tvs)} end)
        |> Map.new()
        # Map with s as key, map of t => v as value
      end)
      # List of maps with source and sink fields
      # values are maps with tree and qos fileds
      |> Enum.map(fn m ->
        {
          {m['source']['tree'], m['source']['qos']},
          {m['sink']['tree'], m['sink']['qos']}
        }
      end)
      |> Enum.filter(
        fn {{src, sqos}, {snk, skqos}} -> src != nil and snk != nil end
      )
      Conform.Conf.remove(conf, "mqtt_bridge.topics.$n")
      topics
    end,
  ],
  validators: []
]
