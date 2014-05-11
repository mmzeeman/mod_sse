%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
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

-module(controller_stream).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% Webmachine exports
-export([
    init/1,
    forbidden/2,
    content_types_provided/2,

    event_stream/2
]).

%% SSE handler exports
-export([
    sse_init/1,
    sse_info/2,
    sse_terminate/1
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

-record(state, {
    page_pid,
    monitor_ref,
    timer_ref
}).

% Timeout to flush the SSE connection and allow the client to reconnect.
-define(SSE_FLUSH_TIMEOUT, 55000).

%%
init(DispatchArgs) ->
    {ok, DispatchArgs}.

%% @doc The request must have a valid session cookie.
forbidden(ReqData, DispatchArgs) ->
    Context = z_context:new(ReqData),
    Context1 = z_context:set(DispatchArgs, Context),

    Context2 = z_context:continue_session(Context1),
    ?WM_REPLY(not z_context:has_session(Context2), Context2).

%% @doc We only handle event streams
%%
content_types_provided(ReqData, Context) ->
    {[{"text/event-stream", event_stream}], ReqData, Context}.


%% @doc Handle a push event stream to the client.
%%
event_stream(ReqData, Context) ->
    ContextReq = ?WM_REQ(ReqData, Context),
    ContextNoCache = z_context:set_nocache_headers(ContextReq),
    Context1 = z_context:ensure_all(ContextNoCache),

    %% Initialize the sse handler.
    Stream = z_sse:init_stream(?MODULE, Context1),

    %% Start the loop.
    ?WM_REPLY({stream, Stream}, Context1). 
    
%%
%% SSE Handler Exports
%%

%% Called when the event stream starts
sse_init(#context{page_pid=PagePid}) ->
    MonitorRef = erlang:monitor(process, PagePid),
    z_session_page:comet_attach(self(), PagePid),

    TimerRef = erlang:send_after(?SSE_FLUSH_TIMEOUT, self(), flush),
    
    %% Retry after one second if there was an error.
    {ok, z_sse:retry(1000), #state{page_pid=PagePid, timer_ref=TimerRef, monitor_ref=MonitorRef}}.

%% Called when a script is queued.
sse_info(script_queued, #state{page_pid=Pid}=State) ->
    case z_session_page:get_scripts(Pid) of
        [] -> 
            {ok, State};
        Data ->
            {ok, z_sse:event(script, erlang:iolist_to_binary(Data)), State}
    end;

%% The page stopped..
sse_info({'DOWN', _MonitorRef, process, Pid, _Info}, #state{page_pid=Pid}=State) ->
    {stop, State#state{page_pid=undefined, monitor_ref=undefined}};

%% Received a flush message.
sse_info(flush, State) ->
    %% Close the sse connection to see if the client is alive.
    {stop, z_sse:retry(50), State#state{timer_ref=undefined}};

%% Unknown message
sse_info(_Info, State) ->
    {ok, State}.

%% Called when the sse-stream stops.
sse_terminate(#state{monitor_ref=MRef, timer_ref=TRef}) ->
    %% Unmonitor if we are still connected to a page.
    case MRef of
        undefined -> ok;
        _ ->
            erlang:demonitor(MRef, [flush])
    end,

    % And cancel the timer and flush possible flush messages.
    case TRef of
        undefined -> ok;
        _ ->
            erlang:cancel_timer(TRef),
            z_utils:flush_message(flush)
    end.

