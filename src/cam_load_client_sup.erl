-module(cam_load_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0, start_children/1, stop_all/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?SERVER, []).

start_children(Count) when Count > 0 ->
    {ok,_} = start_child(),
    start_children(Count - 1);
start_children(_) ->
    ok.

stop_all() ->
    [ cam_load_client:stop(Pid) || {_,Pid,_,_} <- supervisor:which_children(?SERVER) ].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {cam_load_client, {cam_load_client, start_link, []},
	      Restart, Shutdown, Type, [cam_load_client]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
