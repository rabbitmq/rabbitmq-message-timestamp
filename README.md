# RabbitMQ Message Timestamp Plugin #

This plugin fills the `timestamp` property and `timestamp_in_ms` header of a message as it enters
RabbitMQ with the current (server node) timestamp value.

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 until 3.11.

:warning: Starting in RabbitMQ 3.12, this plugin is **deprecated** :warning:

In RabbitMQ 3.12 and later versions, instead of using this plugin, use the following `rabbitmq.conf` snippet:
```ini
message_interceptors.incoming.set_header_timestamp.overwrite = false
```
To allow timestamps being [overwritten](#always-overwrite-timestamps) use:
```ini
message_interceptors.incoming.set_header_timestamp.overwrite = true
```

## Limitations

This plugin cannot be used together with [rabbitmq-routing-node-stamp](https://github.com/rabbitmq/rabbitmq-routing-node-stamp)
as they override the same extension point.

## Installation

Binary builds of this plugin can be obtained from
the [Community Plugins page](https://www.rabbitmq.com/community-plugins.html).

See [Plugin Installation](https://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Building from Source

You can build and install it like any other plugin (see
[the plugin development guide](https://www.rabbitmq.com/plugin-development.html)).

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_message_timestamp
```

The plugin will then hook into the `basic.publish` process in order to
add the current timestamp as seen by the broker.

## Always overwrite timestamps ##

This plugin will not overwrite an existing timestamp on a message. To always
overwrite, create an `advanced.config` file for RabbitMQ with the following
content, or add the `rabbitmq_message_timestamp` term to your existing file:

```
[
    {rabbitmq_message_timestamp, [
        {overwrite_timestamps, true}
    ]}
].
```

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
