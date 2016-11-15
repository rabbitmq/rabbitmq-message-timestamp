# RabbitMQ Message Timestamp Plugin #

This plugin fills the `timestamp` property of a message as it enters
RabbitMQ with the current (server node) timestamp value.

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installation

Binary builds of this plugin from
the [Community Plugins page](http://www.rabbitmq.com/community-plugins.html).

See [Plugin Installation](http://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Building from Source

You can build and install it like any other plugin (see
[the plugin development guide](http://www.rabbitmq.com/plugin-development.html)).

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

This plugin should not be enabled at the same time as any other 
interceptors  that hook into the `basic.publish` process, such as 
the  `rabbitmq-routing-node-stamp` plugin. Enabling more than one 
interceptor that is registered to the `basic.publish` process will 
cause all AMQP 0-9-1 connections to fail when creating a new channel.

If there's enough demand, we could add in the future a way for only
time-stamping messages that crosses certain exchanges, say by applying
policies.

## LICENSE ##

See the LICENSE file
