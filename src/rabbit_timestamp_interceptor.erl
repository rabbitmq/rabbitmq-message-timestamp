%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_timestamp_interceptor).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("rabbit_message_timestamp.hrl").

-import(rabbit_basic, [header/2]).

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "timestamp interceptor"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"timestamp interceptor">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"timestamp interceptor">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

init(_Ch) ->
    undefined.

description() ->
    [{description,
      <<"Adds current timestamp to messages as they enter RabbitMQ">>}].

intercept(#'basic.publish'{} = Method, Content, _IState) ->
    OverwriteTimestamps = application:get_env(rabbitmq_message_timestamp, overwrite_timestamps, false),
    DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
    Timestamp = os:system_time(seconds),
    TimestampMs = os:system_time(milli_seconds),
    Content2 = set_content_timestamp(DecodedContent, Timestamp, OverwriteTimestamps),
    Content3 = set_content_timestamp_millis(Content2, TimestampMs, OverwriteTimestamps),
    {Method, Content3};

intercept(Method, Content, _VHost) ->
    {Method, Content}.

applies_to() ->
    ['basic.publish'].

%%----------------------------------------------------------------------------
% OverwriteTimestamps = false, do not overwrite an existing timestamp
set_content_timestamp(#content{properties = Props} = Content, _, false)
  when is_integer(Props#'P_basic'.timestamp) ->
    Content;
% OverwriteTimestamps = true, overwrite an existing timestamp
set_content_timestamp(#content{properties = Props0} = Content, Timestamp, true)
  when is_integer(Props0#'P_basic'.timestamp) ->
    set_content_timestamp(Content, Timestamp);
set_content_timestamp(#content{properties = Props} = Content, Timestamp, _)
  when Props#'P_basic'.timestamp == undefined ->
    set_content_timestamp(Content, Timestamp).

set_content_timestamp_millis(#content{properties = #'P_basic'{headers = Headers}} = Content, TimestampMs, OverwriteTimestamps) ->
    case {OverwriteTimestamps, header(?TIMESTAMP_IN_MS, Headers)} of
        {true, _} ->
            %% Overwrite or set a timestamp_in_ms header
            set_content_timestamp_header(Content, TimestampMs);
        {_, undefined} ->
            %% No timestamp_in_ms header, so add one
            set_content_timestamp_header(Content, TimestampMs);
        _ ->
            %% Do not overwrite an existing timestamp_in_ms header
            Content
    end.

add_header(undefined, Header) -> [Header];
add_header(Headers, Header) ->
  lists:keystore(element(1, Header), 1, Headers, Header).

set_content_timestamp(#content{properties = Props0} = Content, Timestamp) ->
    %% we need to reset properties_bin = none so the new properties
    %% get serialized when delivering the message.
    Props1 = Props0#'P_basic'{timestamp = Timestamp},
    Content#content{properties = Props1, properties_bin = none}.

set_content_timestamp_header(#content{properties = #'P_basic'{headers = Headers0} = Props0} = Content, TimestampMs) ->
    Headers1 = add_header(Headers0, {?TIMESTAMP_IN_MS, long, TimestampMs}),
    Props1 = Props0#'P_basic'{headers = Headers1},
    Content#content{properties = Props1, properties_bin = none}.
