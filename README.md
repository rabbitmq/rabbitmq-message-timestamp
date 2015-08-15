# RabbitMQ Message Timestamp Plugin #

This plugin fills the `timestamp` property of a message as it enters
RabbitMQ with the current timestamp value.

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installing ##

To use this plugin you will have to build RabbitMQ from master and
then build the plugin against it, since it depends on a new
_interceptors_ API that's not yet available on the 3.5.x series.

Install and setup the RabbitMQ Public Umbrella as explained here:
[http://www.rabbitmq.com/plugin-development.html#getting-started](http://www.rabbitmq.com/plugin-development.html#getting-started).

Then `cd` into the umbrella folder and type:

    $ git clone git://github.com/rabbitmq/rabbitmq-message-timestamp.git
    $ cd rabbitmq-message-timestamp
    $ make

Finally copy all the `*.ez` files inside the `dist` folder to the
`$RABBITMQ_HOME/plugins` folder. Don't copy the file
`rabbit_common-x.y.z` since it's not needed inside the broker
installation.

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
