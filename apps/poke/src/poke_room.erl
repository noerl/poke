-module(poke_room).
-behaviour(gen_server).

-record(user, {
    pid,    %% 玩家进程pid
    name,   %% 玩家名称
    pos,    %% 玩家座位
    state,  %% 玩家状态
    handcards = [], %% 玩家手牌
    playcards = []  %% 玩家打出的牌
}).

-record(room, {
    roomId, %% roomId
    banker, %% 庄家 pid
    player, %% 当前出牌的玩家pid  
    timerRef, %% 倒计时ref
    lastcards = [], %% 前手牌
    posList = [1,2,3], %% 
    userList = [] %% 玩家列表
}).


-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



%%%=========================================================================
%%%  API
%%%=========================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).



init([]) ->
    RoomId = room_id(),
    {ok, #room{roomId = RoomId, banker = 1}}.


handle_call(_Request, _From, Room) ->
    {reply, ignore, Room}.


handle_cast(_Request, Room) ->
    {noreply, Room}.


handle_info({join_room, Name, Pid}, Room) ->
    NewRoom = 
    case lists:keyfind(Pid, #user.pid, Room#room.userList) of
	false ->
	    case Room#room.posList of
		[Pos] ->
	    	    User = #user{pid = Pid, name = Name, pos = Pos},
		    JoinRoom = jsx:encode([[{<<"name">>, Name}, {<<"pos">>, Pos}]]),
		    Msg = <<<<"join_room:">>/binary, JoinRoom/binary>>,
		    [UserTmp#user.pid ! {cmd, Msg} || UserTmp <- Room#room.userList],
		    NewUserList = [User|Room#room.userList],
		    RoomUser = jsx:encode([[{<<"name">>, UserTmp#user.name}, {<<"pos">>, UserTmp#user.pos}] || UserTmp <- NewUserList]),
		    Pid ! {cmd, <<<<"join_room:">>/binary, RoomUser/binary>>},
		    RoomInfo = jsx:encode([{<<"roomId">>, Room#room.roomId}, {<<"banker">>, Room#room.banker}]),
		    Pid ! {cmd, <<<<"room_info:">>/binary, RoomInfo/binary>>},
		    self() ! deal_card,
		    Room#room{posList = [], userList = NewUserList};
		[Pos|List] ->
		    User = #user{pid = Pid, name = Name, pos = Pos},
                    JoinRoom = jsx:encode([[{<<"name">>, Name}, {<<"pos">>, Pos}]]),
                    Msg = <<<<"join_room:">>/binary, JoinRoom/binary>>,
                    [UserTmp#user.pid ! {cmd, Msg} || UserTmp <- Room#room.userList],
                    NewUserList = [User|Room#room.userList],
		    RoomUser = jsx:encode([[{<<"name">>, UserTmp#user.name}, {<<"pos">>, UserTmp#user.pos}] || UserTmp <- NewUserList]),
                    Pid ! {cmd, <<<<"join_room:">>/binary, RoomUser/binary>>},
		    RoomInfo = jsx:encode([{<<"roomId">>, Room#room.roomId}, {<<"banker">>, Room#room.banker}]),
                    Pid ! {cmd, <<<<"room_info:">>/binary, RoomInfo/binary>>},
		    Room#room{posList = List, userList = NewUserList}
	    end;
	{Pid, User} ->
    	    RoomUser = jsx:encode([[{<<"name">>, UserTmp#user.name}, {<<"pos">>, UserTmp#user.pos}] || UserTmp <- Room#room.userList]),
	    Pid ! {cmd, <<<<"join_room:">>/binary, RoomUser/binary>>},
	    HandCards = jsx:encode([{<<"handCards">>, User#user.handcards}]),
	    Pid ! {cmd, <<<<"hand_cards:">>/binary, HandCards/binary>>},
	    TableCards = jsx:encode([[{<<"pos">>, UserTmp#user.pos}, {<<"playCards">>, UserTmp#user.playcards}] || UserTmp <- Room#room.userList]),
	    Pid ! {cmd, <<<<"table_cards:">>/binary, TableCards/binary>>},
	    Time = erlang:read_timer(Room#room.timerRef, []),
	    RoomInfo = jsx:encode([{<<"banker">>, Room#room.banker}, {<<"player">>, Room#room.player}, {<<"time">>, Time}]),	    
	    Pid ! {cmd, <<<<"room_info:">>/binary, RoomInfo/binary>>},
	    Room
    end,
    {noreply, NewRoom};
handle_info(deal_card, Room) ->
    UserList = Room#room.userList,
    [UserTmp#user.pid ! {cmd, jsx:encode([{<<"pos">>, UserTmp#user.pos},{<<"handCards">>, [1,2,3]}])} || UserTmp <- UserList],
    {noreply, Room};
handle_info(_Request, Room) ->
    {noreply, Room}.


terminate(_Reason, Room) ->
    ets:delete(room, Room#room.roomId).

code_change(_OldVsn, Room, _Extra)->
    {ok, Room}.


room_id() ->
    rand:seed(exrop),
    RoomId = iolist_to_binary(io_lib:format("~6.10.0B", [rand:uniform(999999)])),
    case ets:lookup(room, RoomId) of
	[] -> 
	    ets:insert(room, {RoomId, self()}),
	    RoomId;
	_ -> 
	    room_id()
    end.
