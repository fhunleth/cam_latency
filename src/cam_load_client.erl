-module(cam_load_client).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/1]).

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
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Does httpc only support one connection at a time???
    Url = "http://nerves/video?" ++ integer_to_list(erlang:phash2(self())),
    io:format("Starting load client: ~p~n", [Url]),
    {ok, RequestId} = httpc:request(get,
				    {Url, []},
				    [],
				    [{sync, false}, {stream, self}]),
    {ok, TimerId} = timer:send_interval(5000, timeout),
    {ok, #state{reqid=RequestId,timerid=TimerId}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, #state{reqid=RequestId, timerid=TimerId} = State) ->
    io:format("Stopping load client...~n"),
    httpc:cancel_request(RequestId),
    {ok, cancel} = timer:cancel(TimerId),
    {stop, normal, State#state{reqid=none,timerid=none}}.


handle_info({http, {_RequestId, stream_start, _Headers}}, State) ->
    io:format("~nGot stream_start!!~n"),
    {noreply, State};
handle_info({http, {_RequestId, stream, Body}},
	    #state{buffer=Buffer, frame_counter=FrameCounter} = State) ->
    {NewBuffer, Frames} = frame_decoder:decode(<<Buffer/binary, Body/binary>>),
    NewFrameCounter = FrameCounter + length(Frames),
    {noreply, State#state{buffer=NewBuffer, frame_counter=NewFrameCounter}};
handle_info({http, {_RequestId, stream_end, _Headers}}, State) ->
    io:format("~nGot stream_end!!~n"),
    {stop, connection_closed, State};
handle_info(timeout, #state{reqid=RequestId, timerid=TimerId, frame_counter=FrameCounter} = State) ->
    Fps = FrameCounter / 5.0,
    io:format("~p fps~n", [Fps]),
    % Temp
    httpc:cancel_request(RequestId),
    timer:cancel(TimerId),
    {stop, letsstop, State}.
    %{noreply, State#state{frame_counter=0}}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
