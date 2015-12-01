# RabbitMQ Message Timestamp Plugin #

This plugin fills the `timestamp` property of a message as it enters
RabbitMQ with the current (server node) timestamp value.

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installing ##

To use this plugin you will have to build RabbitMQ from master and
then build the plugin against it, since it depends on a new
_interceptors_ API that's not yet available on the 3.5.x series.

Clone the repository, then `cd` into the umbrella folder and type:

    make dist VERSION=1.0.0

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
