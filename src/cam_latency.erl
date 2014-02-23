-module(cam_latency).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        { reqid=none,
	  timerid=none,
	  frame_counter=0,
	  buffer= <<>>
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:cast(?SERVER, start).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start, #state{reqid=none} = State) ->
    {ok, RequestId} = httpc:request(get,
				    {"http://nerves/video", []},
				    [],
				    [{sync, false}, {stream, self}]),
    {ok, TimerId} = timer:send_interval(10, timeout),
    io:format("~n"),
    {noreply, State#state{reqid=RequestId,timerid=TimerId}};
handle_cast(start, State) ->
    % Already started
    {noreply, State};
handle_cast(stop, #state{reqid=RequestId, timerid=TimerId} = State) ->
    httpc:cancel_request(RequestId),
    {ok, cancel} = timer:cancel(TimerId),
    {noreply, State#state{reqid=none,timerid=none}}.


handle_info({http, {_RequestId, stream_start, _Headers}}, State) ->
    io:format("~nGot stream_start!!~n"),
    {noreply, State};
handle_info({http, {_RequestId, stream, Body}},
	    #state{buffer=Buffer, frame_counter=FrameCounter} = State) ->
    Timestamp = get_timestamp(),
    {NewBuffer, Frames} = frame_decoder:decode(<<Buffer/binary, Body/binary>>),
    NewFrameCounter = write_frames(FrameCounter, Timestamp, Frames),
    {noreply, State#state{buffer=NewBuffer, frame_counter=NewFrameCounter}};
handle_info({http, {_RequestId, stream_end, _Headers}}, State) ->
    io:format("~nGot stream_end!!~n"),
    {noreply, State};
handle_info(timeout, State) ->
    io:format("\r~p", [get_timestamp()]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_timestamp() ->
    {_, S, US} = now(),
    S + (US/1000000).

%% Write the list of frames to disk
write_frames(FrameCounter, _Timestamp, []) ->
    FrameCounter;
write_frames(FrameCounter, Timestamp, [Frame|T]) ->
    Filename = io_lib:format("frame_~p_~p.jpg", [FrameCounter, Timestamp]),
    ok = file:write_file(Filename, Frame),
    write_frames(FrameCounter + 1, Timestamp, T).
