-module(cam_latency).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        { reqid=none,
	  timerid=none,
	  frame_counter=0,
	  buffer= <<>>,
	  serial=none
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    start([false]).

start([UseSparkfun7SegmentLed]) ->
    gen_server:cast(?SERVER, {start, UseSparkfun7SegmentLed}).

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

handle_cast({start, UseSparkfun7SegmentLed}, #state{reqid=none} = State) ->
    Serial = init_output(UseSparkfun7SegmentLed),
    {ok, RequestId} = httpc:request(get,
				    {"http://nerves/video", []},
				    [],
				    [{sync, false}, {stream, self}]),
    {ok, TimerId} = timer:send_interval(10, timeout),
    {noreply, State#state{reqid=RequestId,timerid=TimerId,serial=Serial}};
handle_cast({start, _UseSparkfun7SegmentLed}, State) ->
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
    {stop, connection_closed, State};
handle_info(timeout, #state{serial=none} = State) ->
    io:format("\r~p", [get_timestamp()]),
    {noreply, State};
handle_info(timeout, #state{serial=Serial} = State) ->
    SecMillis = round(get_timestamp() * 1000) rem 10000,
    Str = list_to_binary(io_lib:format("~4.10.0b", [SecMillis])),
    %io:format("~p~n", [Str]),
    % Send it down. The 16#79,0 forces the cursor to the left
    Serial ! {send, <<16#79, 0, Str/binary>>},
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

init_output(UseSparkfun7SegmentLed) ->
    case UseSparkfun7SegmentLed of
	true ->
	    Serial = serial:start([{speed, 115200}, {open, "/dev/ttyUSB0"}]),

	    % Wait for 7 segment display to wake up
	    timer:sleep(2000),

	    % Dim appears to work better than bright.
	    Serial ! { send, <<16#7a,1>> },

	    % Put the decimal on in the seconds place
	    Serial ! { send, <<16#77,2#0001>> },
	    Serial;

	false ->
	    % Start on a fresh line
	    io:format("~n"),
	    none
    end.
