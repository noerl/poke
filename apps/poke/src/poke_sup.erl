%%%-------------------------------------------------------------------
%% @doc poke top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(poke_sup).

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
	PokeRoomSup = #{id => poke_room_sup,  start => {poke_room_sup, start_link, []}, restart => permanent, shutdown => 5000, type => supervisor, modules => [poke_room_sup]},
    {ok, {{one_for_all, 0, 1}, [PokeRoomSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
