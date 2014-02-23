-module(cam_latency).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        { reqid,
	  frame_counter=0,
	  buffer= <<>>
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, RequestId} = httpc:request(get,
				    {"http://nerves/video", []},
				    [],
				    [{sync, false}, {stream, self}]),
    {ok, _} = timer:send_interval(10, timeout),
    io:format("~n"),
    {ok, #state{reqid=RequestId}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_RequestId, stream_start, _Headers}}, State) ->
    io:format("~nGot stream_start!!~n"),
    {noreply, State};
handle_info({http, {_RequestId, stream, Body}},
	    #state{buffer=Buffer, frame_counter=FrameCounter} = State) ->
    Timestamp = get_timestamp(),
    {NewBuffer, Frames} = decode_frames(<<Buffer/binary, Body/binary>>),
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
    file:write_file(Filename, Frame),
    write_frames(FrameCounter + 1, Timestamp, T).

%% Parse the mime and assemble a list of frames
decode_frames(Buffer) ->
    {NewBuffer, FramesInReverse} = decode_frames(Buffer, []),
    {NewBuffer, lists:reverse(FramesInReverse)}.

decode_frames(Buffer, PreviousFrames) ->
    Boundary = <<"foofoo">>,
    case cow_multipart:parse_headers(Buffer, Boundary) of
	{ok, _Headers, Rest} ->
	    case cow_multipart:parse_body(Rest, Boundary) of
		{done, Frame, NextPart} ->
		    decode_frames(NextPart, [Frame|PreviousFrames]);
		_ ->
		    {Buffer, PreviousFrames}
	    end;
	_ ->
	    {Buffer, PreviousFrames}
    end.
