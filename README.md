# RabbitMQ Message Timestamp Plugin #

This plugin fills the `timestamp` property of a message as it enters
RabbitMQ with the current (server node) timestamp value.

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installing ##

Clone the repo and then build it with `make`:

```
git clone git@github.com:rabbitmq/rabbitmq-message-timestamp.git
# [snip]
cd rabbitmq-message-timestamp
make
# [snip]
make dist
# [snip]
ls plugins/*
```

Build artefacts then can be found under the `plugins` directory.

Finally copy all the `*.ez` files inside the `dist` folder to the
`$RABBITMQ_HOME/plugins` folder. Don't copy the file
`rabbit_common-x.y.z` since it's already part of RabbitMQ distribution.

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_message_timestamp
```

The plugin will then hook into the `basic.publish` process in order to
add the current timestamp as seen by the broker.

## Limitations ##

The plugin hooks into the `basic.publish` path, so expect a small
throughput reduction when using this plugin, since it has to modify
every message that crosses RabbitMQ.

If there's enough demand, we could add in the future a way for only
time-stamping messages that crosses certain exchanges, say by applying
policies.

## LICENSE ##

See the LICENSE file
