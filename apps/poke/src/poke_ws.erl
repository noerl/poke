-module(poke_ws).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	{ok, State}.

websocket_handle({text, <<"create_room:", DataBin/binary>>}, State) ->
	{ok, Pid} = supervisor:start_child(poke_room_sup, []),
	DataList = jsx:decode(DataBin),
	Name = proplists:get_value(<<"name">>, DataList),
	Pid ! {join_room, Name, self()},
	{ok, State};
websocket_handle({text, <<"join_room:", DataBin/binary>>}, State) ->
	DataList = jsx:decode(DataBin),
	RoomId = proplists:get_value(<<"roomId">>, DataList),
	Name = proplists:get_value(<<"name">>, DataList),
	case ets:lookup(room, RoomId) of
		[{RoomId, Pid}] ->  
			Pid ! {join_room, Name, self()},
			{ok, State};
		_ ->
			{reply, {text, <<"{\"error\":1}">>}, State}
	end;
websocket_handle(_Data, State) ->
	{ok, State}.


websocket_info({cmd, Msg}, State) ->
        {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
	{ok, State}.
