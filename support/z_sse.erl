% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2014 Maas-Maarten Zeeman
%%
%% @doc Zotonic Server Sent Events
%%
%% Copyright 2014 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_sse).

-export([
    get_last_seen_id/1,

    init_stream/2,
    event/1, event/2, event/3,
    retry/1,

    send_retry/2,
    send_event/2, send_event/3, send_event/4,

    stop/1
]).

%%
%% Exports
%%

init_stream(Handler, Context) ->
    case Handler:sse_init(Context) of
        {ok, SSEState} ->
            {<<>>, push_events_fun(Handler, SSEState)}; 
        {ok, Response, SSEState} ->
            {encode_msg(Response), push_events_fun(Handler, SSEState)};
        {stop, SSEState} ->
            handle_stop(Handler, SSEState)
    end.

% @doc Create an sse event
event(Data) ->
    event(undefined, undefined, Data).

event(Type, Data) ->
    event(undefined, Type, Data).

event(Id, Type, Data) when is_binary(Data) ->
    {sse_event, Id, Type, Data}.

% @doc Create a retry message
retry(Time) when is_integer(Time) andalso Time > 0 ->
    {sse_retry, Time}.

% @doc Send a fresh retry time to the client
send_retry(Pid, Time) when is_integer(Time) andalso Time > 0 ->
    Pid ! {sse_retry, Time}.

% @doc Send an event to the client
send_event(Pid, Data) when is_binary(Data) ->
    send_event(Pid, event(Data));
send_event(Pid, {sse_event, _, _, _}=Event) ->
    Pid ! Event.

send_event(Pid, Type, Data) ->
    send_event(Pid, event(Type, Data)).

send_event(Pid, Id, Type, Data) ->
    send_event(Pid, event(Id, Type, Data)).

% @doc Stop the sse.
%
stop(Pid) ->
    Pid ! stop.

% @doc Get the last seen id of the client.
%
get_last_seen_id(Context) ->
    z_context:get_req_header("Last-Seen-Id", Context).

%%
%% Helpers
%%

%% @doc Receive incoming events, and push it to the client.
%%
push_events(Handler, Context) ->
    receive
        {sse_event, _, _, _}=Event ->
            {encode_msg(Event), push_events_fun(Handler, Context)};
        {sse_retry, _}=Retry ->
            {encode_msg(Retry), push_events_fun(Handler, Context)}; 
        Message ->
            %% Unknown message, let the handler take care of it.
            handle_info(Handler, Message, Context)
    end.

handle_stop(Handler, SSEState) ->
    handle_stop(Handler, undefined, SSEState).

handle_stop(Handler, Response, SSEState) ->
    Msg = encode_msg(Response),
    case Handler:sse_terminate(SSEState) of
        Atom when is_atom(Atom) ->
            {Msg, done};
        {stop, Response2} ->
            Msg2 = encode_msg(Response2),
            {<<Msg/binary, Msg2/binary>>, done}
    end.

handle_info(Handler, Info, SSEState) ->
    case Handler:sse_info(Info, SSEState) of
        {ok, SSEState1} ->
            {<<>>, push_events_fun(Handler, SSEState1)};
        {ok, Response, SSEState1} ->
            {encode_msg(Response), push_events_fun(Handler, SSEState1)};
        {stop, SSEState1} ->
            handle_stop(Handler, SSEState1);
        {stop, Response, SSEState1} ->
            handle_stop(Handler, Response, SSEState1)
    end.

push_events_fun(Handler, State) ->
    fun() -> push_events(Handler, State) end.

%% @doc Encode a sse event.
encode_msg(undefined) ->
    <<>>;
encode_msg({sse_event, EventId, Type, Data}) ->
    <<(event_id(EventId))/binary, (type(Type))/binary, (data(Data))/binary>>;
encode_msg({sse_retry, Time}) when is_integer(Time) andalso Time > 0 ->
    BTime = z_convert:to_binary(Time),
    <<"retry: ", BTime/binary, "\n">>.


% Encode an id.
event_id(undefined) ->
    <<>>;
event_id(EventId) when is_binary(EventId) ->
    <<"id: ", EventId/binary, "\n">>;
event_id(EventId) ->
    event_id(z_convert:to_binary(EventId)).

%% Encode an event type.
type(undefined) ->
    <<>>;
type(Type) when is_atom(Type) ->
    BType = z_convert:to_binary(Type),
    <<"event: ", BType/binary, "\n">>.

% Encode event data.
data(Data) ->
    D = binary:replace(Data, <<"\n">>, <<"\ndata:">>, [global]),
    <<"data:", D/binary, "\n\n">>.

