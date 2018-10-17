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
	NewUserList = deal_card(Room),
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
    JRNoticeOther = join_room_notice(Name, Pos, 1),
    [UTmp#user.pid ! JRNoticeOther || UTmp <- UserList],

    NewUserList = [User|UserList],
    JRNoticeSelf = join_room_notice(NewUserList),
    Pid ! JRNoticeSelf,

    RoomInfo = [{<<"roomId">>, Room#room.roomId}, {<<"banker">>, Room#room.banker}],
    Pid ! {cmd, <<"room_info:">>, RoomInfo},
    NewUserList.


join_again(Pid, User, Room) ->
    UserList = Room#room.userList,
    User1 = User#user{state = 1},
    NewUserList = lists:keystore(Pid, #user.pid, UserList, User1),

    StateNotice = state_notice(User1),
    [UTmp#user.pid ! StateNotice || UTmp <- UserList],

    JRNoticeSelf = join_room_notice(NewUserList),
    Pid ! JRNoticeSelf,

    HandCards = [{<<"handCards">>, User#user.handcards}],
    Pid ! {cmd, <<"hand_cards:">>, HandCards},

    TableCards = [[{<<"pos">>, UTmp#user.pos}, {<<"playCards">>, UTmp#user.playcards}] || UTmp <- NewUserList],
    Pid ! {cmd, <<"table_cards:">>, TableCards},

    Time = erlang:read_timer(Room#room.timerRef, []),
    RoomInfo = [{<<"banker">>, Room#room.banker}, {<<"player">>, Room#room.player}, {<<"time">>, Time}],        
    Pid ! {cmd, <<"room_info:">>, RoomInfo},

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
                    exit_unplaying(Pid, User, UserList, Room);
                3 ->
                    case playing(UserList) of
                        true -> 
                            exit_playing(Pid, User, UserList, Room);
                        false ->
                            exit_unplaying(Pid, User, UserList, Room)
                    end
            end;
        _ ->
            Pid ! exit
    end.


%% 未开始时退出 庄家位置不变
exit_unplaying(Pid, User, UserList, Room) ->
    NewUserList = lists:keydelete(Pid, #user.pid, UserList),
    ExitInfo = exit_pack(User),
    [UTmp#user.pid ! ExitInfo || UTmp <- UserList],
    {noreply, Room#room{userList = NewUserList}}.


%% 正在玩时退出
exit_playing(Pid, User, UserList, Room) ->
    User1 = User#user{state = 2},
    NewUserList = lists:keystore(Pid, #user.pid, UserList, User1),
    NewRoom = Room#room{userList = NewUserList},

    StateNotice = state_notice(User1),
    [UTmp#user.pid ! StateNotice || UTmp <- UserList],

    ExitInfo = exit_pack(User),
    Pid ! ExitInfo,
    {noreply, NewRoom}.
            



playing(UserList) ->
    Fun = fun(User) -> User#user.state =:= 1 end,
    lists:all(Fun, UserList).
     


deal_card(Room) ->
    CardList = poke_logic:shuffle(),
    Banker = Room#room.banker,
    Fun = fun(UserTmp) ->
        #user{pid = Pid, pos = Pos} = UserTmp,
        HandCards = lists:nth(Pos, CardList),
        NewHandCards = 
            case Banker =:= Pos of
                true -> lists:sort([41,41,43] ++ HandCards);
                false -> HandCards
            end,
        PosCard = [{<<"pos">>, Pos},{<<"handCards">>, NewHandCards}],
        Pid ! {cmd, <<"deal_card:">>, PosCard},
        UserTmp#user{handcards = PosCard}
    end,
    lists:map(Fun, Room#room.userList).


join_room_notice(UserList) ->
    RoomUser = [join_room_pack(User) || User <- UserList],
    {cmd, <<"join_room:">>, RoomUser}.

join_room_notice(Name, Pos, State) ->
   User = join_room_pack(Name, Pos, State),
   {cmd, <<"join_room:">>, [User]}.


join_room_pack(#user{name = Name, pos = Pos, state = State}) ->
    join_room_pack(Name, Pos, State).

join_room_pack(Name, Pos, State) ->
    [{<<"name">>, Name}, {<<"pos">>, Pos}, {<<"state">>, State}].


state_notice(User) ->
    StateInfo = state_pack(User),
    {cmd, <<"state_info:">>, StateInfo}.


state_pack(#user{pos = Pos, state = State}) ->
    state_pack(Pos, State).

state_pack(Pos, State) ->
    [{<<"pos">>, Pos}, {<<"state">>, State}].


exit_pack(User) ->
    {cmd, <<"exit:">>, [{<<"pos">>, User#user.pos}]}.
