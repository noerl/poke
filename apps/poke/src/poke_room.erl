-module(poke_room).
-behaviour(gen_server).

-record(user, {
    pid,    %% 玩家进程pid
    name,   %% 玩家名称
    pos,    %% 玩家座位
    state = 1,  %% 玩家状态 %% 0 预备 1 开始 2 挂机
    handcards = [], %% 玩家手牌
    playcards = []  %% 玩家打出的牌
}).

-record(room, {
    roomId, %% roomId
    banker, %% 庄家 位置
    player, %% 当前出牌的玩家的位置 
    timerRef, %% 倒计时ref
    lastcards = [], %% 前手牌
    posList = [1,2,3], %% 玩家位置信息
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
        	    join_once(Pid, Name, Room);
        	{Pid, User} ->
            	join_again(Pid, User, Room)
        end,
    {noreply, NewRoom};
handle_info({exit_room, Pid}, Room) ->
    NewRoom = exit_room(Pid, Room),
	{noreply, NewRoom};
handle_info(deal_card, Room) ->
	CardList = poke_logic:deal_card(),
    UserList = Room#room.userList,
    Fun = fun(UserTmp) ->
    		#user{pid = Pid, pos = Pos} = UserTmp,
    		PosCard = lists:nth(Pos, CardList),
    		Pid ! {cmd, jsx:encode([{<<"pos">>, Pos},{<<"handCards">>, PosCard}])},
    		UserTmp#user{handcards = PosCard}
    	end,
    NewUserList = lists:map(Fun, UserList),
    {noreply, Room#room{userList = NewUserList}};
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


join_once(Pid, Name, Room) ->
    case Room#room.posList of
        [Pos] ->
            NewUserList = join_notice(Pid, Name, Pos, Room),
            self() ! deal_card,
            Room#room{posList = [], userList = NewUserList};
        [Pos|List] ->
            NewUserList = join_notice(Pid, Name, Pos, Room),
            Room#room{posList = List, userList = NewUserList}
    end.


join_notice(Pid, Name, Pos, Room) ->
    UserList = Room#room.userList,
    User = #user{pid = Pid, name = Name, pos = Pos},
    JoinRoom = jsx:encode([[{<<"name">>, Name}, {<<"pos">>, Pos}]]),
    Msg = <<<<"join_room:">>/binary, JoinRoom/binary>>,
    [UserTmp#user.pid ! {cmd, Msg} || UserTmp <- UserList],
    NewUserList = [User|UserList],
    RoomUser = jsx:encode([[{<<"name">>, UserTmp#user.name}, {<<"pos">>, UserTmp#user.pos}] || UserTmp <- NewUserList]),
    Pid ! {cmd, <<<<"join_room:">>/binary, RoomUser/binary>>},
    RoomInfo = jsx:encode([{<<"roomId">>, Room#room.roomId}, {<<"banker">>, Room#room.banker}]),
    Pid ! {cmd, <<<<"room_info:">>/binary, RoomInfo/binary>>},
    NewUserList.


join_again(Pid, User, Room) ->
    UserList = Room#room.userList,
    User1 = User#user{state = 1},
    NewUserList = lists:keystore(Pid, #user.pid, UserList, User1),

    RoomUser = jsx:encode([[{<<"name">>, UserTmp#user.name}, {<<"pos">>, UserTmp#user.pos}] || UserTmp <- NewUserList]),
    Pid ! {cmd, <<<<"join_room:">>/binary, RoomUser/binary>>},

    HandCards = jsx:encode([{<<"handCards">>, User#user.handcards}]),
    Pid ! {cmd, <<<<"hand_cards:">>/binary, HandCards/binary>>},

    TableCards = jsx:encode([[{<<"pos">>, UserTmp#user.pos}, {<<"playCards">>, UserTmp#user.playcards}] || UserTmp <- NewUserList]),
    Pid ! {cmd, <<<<"table_cards:">>/binary, TableCards/binary>>},

    Time = erlang:read_timer(Room#room.timerRef, []),
    RoomInfo = jsx:encode([{<<"banker">>, Room#room.banker}, {<<"player">>, Room#room.player}, {<<"time">>, Time}]),        
    Pid ! {cmd, <<<<"room_info:">>/binary, RoomInfo/binary>>},

    Room#room{userList = NewUserList}.


exit_room(Pid, Room) ->
    UserList = Room#room.userList,
    case lists:keyfind(Pid, #user.pid, UserList) of
        {Pid, User} ->
            Len = length(UserList),
            case Len of
                1 -> 
                    {stop, normal, Room};
                2 -> 
                    exit_unplaying(Pid, UserList, Room);
                3 ->
                    case playing(UserList) of
                        true -> 
                            exit_playing(Pid, User, UserList, Room);
                        false ->
                            exit_unplaying(Pid, UserList, Room)
                    end
            end;
        _ ->
            Pid ! exit
    end.


%% 未开始时退出 庄家位置不变
exit_unplaying(Pid, UserList, Room) ->
    NewUserList = lists:keydelete(Pid, #user.pid, UserList),
    [User#user.pid ! {cmd, [<<"exit:">>, [{<<"pos">>, User#user.pos}]]} || User <- UserList],
    {noreply, Room#room{userList = NewUserList}};


%% 正在玩时退出
exit_playing(Pid, User, UserList, Room) ->
    User1 = User#user{state = 2},
    NewUserList = lists:keystore(Pid, #user.pid, UserList, User1),
    NewRoom = Room#room{userList = NewUserList},
    [User#user.pid ! {cmd, [<<"exit:">>, [{<<"pos">>, User#user.pos}]]} || User <- UserList],
    {noreply, NewRoom};
            



playing(UserList) ->
    Fun = fun(User) -> User#user.state =:= 1 end,
    lists:all(Fun, UserList).
     


