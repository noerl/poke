-module(poke_ws).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {
	roomPid,
	pos,
	isBanker,
	handCards
}).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init([]) ->
	{ok, #state{}}.




websocket_handle({text, <<"send_card:", DataBin/binary>>}, State) ->
	DataList = jsx:decode(DataBin),
	CardList = proplists:get_value(<<"cards">>, DataList),
	case valid(CardList, State) of
		 true -> 
			State#state.roomPid ! {send_card, CardList},
			{ok, State};
		false ->
			{reply, {text, <<"{\"error\":\"出牌不允许\"}">>}, State}
	end;
websocket_handle({text, <<"join_room:", DataBin/binary>>}, State) ->
	DataList = jsx:decode(DataBin),
	RoomId = proplists:get_value(<<"roomId">>, DataList),
	Name = proplists:get_value(<<"name">>, DataList),
	case ets:lookup(room, RoomId) of
		[{RoomId, Pid}] ->  
			Pid ! {join_room, Name, self()},
			{ok, State#state{roomPid = Pid}};
		_ ->
			{reply, {text, <<"{\"error\":\"房间不存在\"}">>}, State}
	end;
websocket_handle({text, <<"create_room:", DataBin/binary>>}, State) ->
	{ok, Pid} = supervisor:start_child(poke_room_sup, []),
	DataList = jsx:decode(DataBin),
	Name = proplists:get_value(<<"name">>, DataList),
	Pid ! {join_room, Name, self()},
	{ok, State#state{roomPid = Pid}};
websocket_handle({text, <<"exit_room:", _DataBin/binary>>}, State) ->
	State#state.roomPid ! {exit_room, self()},
	{ok, State};
websocket_handle(_Data, State) ->
	{ok, State}.



websocket_info({deal_card, HandCards}, State) ->
	CardList = encode(HandCards),
	DataBin = jsx:encode([{<<"handCards">>, CardList}]),
    {reply, {text, <<<<"deal_card:">>/binary, DataBin/binary>>}, State#state{handCards = HandCards}};
websocket_info({cmd, Cmd, []}, State) ->
	{ok, State};
websocket_info({cmd, Cmd, Data}, State) ->
	DataBin = jsx:encode(Data),
    {reply, {text, <<Cmd/binary, DataBin/binary>>}, State};
websocket_info(_Info, State) ->
	{ok, State}.


valid(SendPoke, #state{handCards = HandPoke, isBanker = IsBanker}) ->
	poke_logic:check(SendPoke, HandPoke, IsBanker).


encode(ColorCards) ->
	encode(ColorCards, []).


encode([{Value, Color} | ColorCards], List) when Value < ?POKE_LARGE_KING ->
	Poke = (Value-1)*4 + Color - 1,
	encode(ColorCards, [Poke|List]);
encode([{15, 1} | ColorCards], List) ->
	encode(ColorCards, [53|List]);
encode([], List) -> List.


decode([53|List], ColorCards) -> 
	decode(List, [{15,1}|ColorCards]);
decode([V|List], ColorCards) when V < 53 ->
	Value = (V div 4) + 1,
	Color = (V rem 4) + 1,
	decode(List, [{Value, Color}|ColorCards]);
decode([], ColorCards) -> ColorCards;







