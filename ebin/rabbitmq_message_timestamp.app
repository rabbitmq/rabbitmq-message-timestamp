{application, rabbitmq_message_timestamp,
 [{description, "RabbitMQ Message Timestamp"},
  {vsn, "%%VSN%%"},
  {modules, ['rabbit_timestamp_interceptor']},
  {registered, []},
  {applications, [kernel, stdlib, rabbit]}]}.
