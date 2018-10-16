%%%-------------------------------------------------------------------
%% @doc poke public API
%% @end
%%%-------------------------------------------------------------------

-module(poke_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(normal, []) ->
	ets:new(room, [named_table, public]),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, poke, "index.html"}},
                        {"/websocket", poke_ws, []},
                        {"/static/[...]", cowboy_static, {priv_dir, poke, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 80}], #{
		env => #{dispatch => Dispatch}
	}),
    poke_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
