%%%-------------------------------------------------------------------
%% @doc poke top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(poke_room_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	PokeRoom = #{id => poke_room,  start => {poke_room, start_link, []}, restart => permanent, shutdown => 5000, type => worker, modules => [poke_room]},
    {ok, {{simple_one_for_one, 0, 1}, [PokeRoom]}}.

%%====================================================================
%% Internal functions
%%====================================================================
