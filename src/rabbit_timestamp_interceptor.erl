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
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_timestamp_interceptor).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

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
    Timestamp = time_compat:os_system_time(seconds),
    Content2 = set_content_timestamp(DecodedContent, Timestamp),
    {Method, Content2};

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
