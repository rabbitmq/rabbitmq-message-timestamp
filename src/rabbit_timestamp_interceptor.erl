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
    DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
    Timestamp = os:system_time(seconds),
    TimestampMs = os:system_time(milli_seconds),
    Content2 = set_content_timestamp(DecodedContent, Timestamp),
    Content3 = set_content_timestamp_millis(Content2, TimestampMs),
    {Method, Content3};

intercept(Method, Content, _VHost) ->
    {Method, Content}.

applies_to() ->
    ['basic.publish'].

%%----------------------------------------------------------------------------

                                                % Do not overwrite an existing timestamp
set_content_timestamp(#content{properties = Props} = Content, _)
  when is_integer(Props#'P_basic'.timestamp) ->
    Content;

set_content_timestamp(#content{properties = Props} = Content, Timestamp)
  when Props#'P_basic'.timestamp == undefined ->
    %% we need to reset properties_bin = none so the new properties
    %% get serialized when deliverying the message.
    Content#content{properties = Props#'P_basic'{timestamp = Timestamp},
                    properties_bin = none}.

set_content_timestamp_millis(#content{properties = #'P_basic'{headers = Headers} = Props} = Content, TimestampMs) ->
  case header(?TIMESTAMP_IN_MS, Headers) of
    undefined ->
      Content#content{
        properties = Props#'P_basic'{headers = add_header(Headers, {?TIMESTAMP_IN_MS, long, TimestampMs})},
        properties_bin = none
       };
    %% Do not overwrite an existing TIMESTAMP_IN_MS.
    _ -> Content
  end.

add_header(undefined, Header) -> [Header];
add_header(Headers, Header) ->
  lists:keystore(element(1, Header), 1, Headers, Header).
