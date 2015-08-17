%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Message Timestamp.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_message_timestamp_test).

-export([test/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

test() ->
    ok = eunit:test(tests(?MODULE, 60), [verbose]).

timestamp_test() ->
    {ok, Conn} = amqp_connection:start(#amqp_params_network{}),
    {ok, Chan} = amqp_connection:open_channel(Conn),

    Ex = <<"e1">>,
    Q = <<"q">>,

    setup_fabric(Chan, make_exchange(Ex, <<"direct">>), make_queue(Q)),

    Msgs = [1, 2, 3, 4, 5],

    amqp_channel:call(Chan, #'confirm.select'{}),

    publish_messages(Chan, Ex, Msgs),

    amqp_channel:wait_for_confirms_or_die(Chan),

    {ok, Result} = consume(Chan, Q, Msgs, 5000),

    [begin
         ?assertNotEqual(get_timestamp(Msg), undefined),
         ?assert(is_integer(get_timestamp(Msg))),
         ?assert(get_timestamp(Msg) > 0)
     end|| Msg <- Result],

    ok.

get_timestamp(#amqp_msg{props = #'P_basic'{timestamp = T}}) ->
    T.

setup_fabric(Chan, ExDeclare, QueueDeclare) ->
    setup_fabric(Chan, ExDeclare, QueueDeclare, <<>>).

setup_fabric(Chan,
             ExDeclare = #'exchange.declare'{exchange = Ex},
             QueueDeclare,
             RK) ->
    declare_exchange(Chan, ExDeclare),

    #'queue.declare_ok'{queue = Q} =
        amqp_channel:call(Chan, QueueDeclare),

    #'queue.bind_ok'{} =
        amqp_channel:call(Chan, #'queue.bind' {
                                   queue       = Q,
                                   exchange    = Ex,
                                   routing_key = RK
                                  }).

declare_exchange(Chan, ExDeclare) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(Chan, ExDeclare).

publish_messages(Chan, Ex, Msgs) ->
    publish_messages(Chan, Ex, <<>>, Msgs).

publish_messages(Chan, Ex, RK, Msgs) ->
    [amqp_channel:call(Chan,
                       #'basic.publish'{exchange = Ex,
                                        routing_key = RK},
                       make_msg(V)) || V <- Msgs].

consume(Chan, Q, Msgs, Timeout) ->
    #'basic.consume_ok'{} =
        amqp_channel:subscribe(Chan, #'basic.consume'{queue  = Q,
                                                      no_ack = true}, self()),
    collect(length(Msgs), Timeout).


collect(N, Timeout) ->
    collect(0, N, Timeout, []).

collect(N, N, _Timeout, Acc) ->
    {ok, lists:reverse(Acc)};
collect(Curr, N, Timeout, Acc) ->
    receive {#'basic.deliver'{},
             Msg = #amqp_msg{}} ->
            collect(Curr+1, N, Timeout, [Msg | Acc])
    after Timeout ->
            {error, {timeout, Acc}}
    end.

make_queue(Q) ->
    #'queue.declare' {
       queue       = Q
      }.

make_exchange(Ex, Type) ->
    #'exchange.declare'{
       exchange    = Ex,
       type        = Type
      }.

make_msg(V) ->
    #amqp_msg{payload = term_to_binary(V)}.

tests(Module, Timeout) ->
    {foreach, fun() -> ok end,
     [{timeout, Timeout, fun () -> Module:F() end} ||
         {F, _Arity} <- proplists:get_value(exports, Module:module_info()),
         string:right(atom_to_list(F), 5) =:= "_test"]}.
