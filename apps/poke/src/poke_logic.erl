-module(poke_logic).

-include("poke.hrl").

-export([
	shuffle/0,
	shuffle/3
]).

-export([
	check/3,
	check/4,
	check_type/3,
	poke_check/3,
	poke_check/4,

	calc_same/1,

	same_1/1,
	same_2/1,
	same_3/2,
	same_zha/3,
	same_wangzha/1,
	same_wushik/2
]).



check(SendPoke, HandPoke, IsBanker) ->
	LastPoke = {0, 0, 0},
	check(LastPoke, SendPoke, HandPoke, IsBanker).

check(LastPoke, SendPoke, HandPoke, IsBanker) ->
	SortFun = fun({V1,C1},{V2,C2}) -> V1 < V2 orelse (V1 =:= V2 andalso C1 < C2) end,
	PokeList = lists:sort(SortFun, SendPoke),
	SortHandPoke = lists:sort(SortFun, HandPoke),
	case hand_poke(PokeList, SortHandPoke) of
		{ok, NewHandPoke} ->
			case check_type(PokeList, NewHandPoke, IsBanker) of
				{ok, Type, Len, Num, Reward} -> 
					case check_max(LastPoke, {Type, Len, Num}) of
						true -> {ok, NewHandPoke, Reward};
						false -> false
					end;
				false -> 
					false
			end;
		_ ->
			false
	end.
	

hand_poke(SSPoke, SHPoke) ->
	hand_poke(SSPoke, SHPoke, []).


hand_poke([P|SendList], [P|HandList], NewHand) ->
	hand_poke(SendList, HandList, NewHand);
hand_poke([{V1,C1}|SendList], [{V2,C2}|HandList], NewHand) when V1 > V2 orelse (V1 =:= V2 andalso C1 > C2) ->
	hand_poke([{V1,C1}|SendList], HandList, [{V2, C2}|NewHand]);
hand_poke([], HandList, NewHand) -> HandList ++ NewHand;
hand_poke(_, _, _) -> false.




check_max({Type, Len, Num1}, {Type, Len, Num2}) -> Num1 < Num2;
check_max({Type, Len1, _}, {Type, Len2, _}) -> Len1 < Len2;
check_max({Type1, _, _}, {Type2, _, _}) -> Type1 < Type2;
check_max(_, _) -> false.


%% 3个老K 的游戏规则
%% ♠️ ❤️ ♣️ ♦️
%% 副五十K < 正五十K < 4个头 < 5个头 < 3个王 < 3个五十K < 6个头 < 7个头 < 8个头 < 4个王 < 4个五十K < 5个五十K
%% 4 		5		 6		7	    8	   9		 10		11		12	   13	   14       15

%% 打的牌  手上的牌  是否是庄家
check_type(PokeList, [], IsBanker) ->
	poke_check(PokeList, 0, IsBanker);
check_type(PokeList, _HandList, IsBanker) ->
	poke_check(PokeList, 1, IsBanker).




poke_check(PokeList, HandAlso, IsBanker) ->
	MaxSame = calc_same(PokeList),
	poke_check(MaxSame, PokeList, HandAlso, IsBanker).

%% {ok, 5, Len, 0}   	王炸
%% {ok, 6, Len, Son}	  5, 10, K  
%% {ok, Type, Len, Num}  Type 1, 2, 3, 4

poke_check(_Same, [], _, _) -> 
	false;
poke_check(1, PokeList, _, IsBanker) ->
	case same_1(PokeList) of
		{ok, Type, Len, Num, Reward} -> {ok, Type, Len, Num, Reward};
		false -> same_wushik(PokeList, IsBanker)
	end;
poke_check(2, PokeList, _, _) ->
	case same_2(PokeList) of
		{ok, Type, Len, Num, Reward} -> {ok, Type, Len, Num, Reward};
		false -> same_wangzha(PokeList)
	end;
poke_check(3, PokeList, HandAlso, IsBanker) ->
	case same_3(PokeList, HandAlso) of
		{ok, Type, Len, Num, Reward} -> {ok, Type, Len, Num, Reward};
		false -> same_wushik(PokeList, IsBanker)
	end;
poke_check(Same, PokeList, HandAlso, IsBanker) when Same >= 4 andalso Same =< 8 ->
	case same_zha(PokeList, Same, IsBanker) of
		{ok, Type, Len, Num, Reward} -> 
			{ok, Type, Len, Num, Reward};
		false -> 
			poke_check(3, PokeList, HandAlso, IsBanker)
	end;
poke_check(_, _, _, _) -> false.


%% poke值转换为数学数值
%% [{1,0},{11,1},{12,1},{13,3},{14,0},{15,0}]
%% 3*13+4-2
%% ♠️ ❤️ ♣️ ♦️





calc_same([Poke|List]) -> 
	calc_same(List, 1, Poke, 0).


calc_same([{V, _}|List], Len, {V, _}, Max) ->
	calc_same(List, Len+1, V, Max);
calc_same([{V, _}|List], Len, _OldV, Max) ->
	calc_same(List, 1, V, max(Max, Len));
calc_same([], Len, _V, Max) ->
	max(Max, Len).


%% 单张 或者 龙
same_1([Poke|List]) ->
	same_1(List, 1, Poke).


same_1([{NewV, _}|List], Len, {V, _}) when V + 1 =:= NewV andalso NewV < ?POKE_2 -> 
	same_1(List, Len+1, NewV);
same_1([], Len, {V, _}) when Len =:= 1 orelse Len >= 5 -> {ok, 1, Len, V, 0};
same_1(_, _, _) -> false.
	

%% 一对 或者 连队
same_2([{V, _}, {V, _}|List]) ->
	same_2(List, 1, V).

same_2([{NewV, _}, {NewV, _}|List], Len, V) when V + 1 =:= NewV andalso NewV < ?POKE_2 ->
	same_2(List, Len+1, NewV);
same_2([], 1, ?POKE_SMALL_KING) -> {ok, 2, 1, ?POKE_SMALL_KING, 1};
same_2([], 1, ?POKE_LARGE_KING) -> {ok, 2, 1, ?POKE_LARGE_KING, 1};
same_2([], Len, V) -> {ok, 2, Len, V, 0};
same_2(_, _, _) -> false.


%% 三带二 或者 六带四
same_3(NumList, HandAlso) ->
	same_3(NumList, 0, 0, 0, HandAlso).



same_3([{V, _}, {V, _}, {V, _}|List], 0, 0, Salves, HandAlso) ->
	same_3(List, 1, V, Salves, HandAlso);
same_3([{NewV, _}, {NewV, _}, {NewV, _}|List], Len, V, Salves, HandAlso) when V + 1 =:= NewV andalso NewV < ?POKE_2 ->
	same_3(List, Len+1, NewV, Salves, HandAlso);
same_3([_Poke|List], Len, V, Salves, HandAlso) ->
	same_3(List, Len, V, Salves+1, HandAlso);
same_3([], Len, V, Salves, HandAlso) when Len > 0 andalso (Len * 2 =:= Salves orelse (HandAlso =:= 0 andalso Len * 2 > Salves)) ->
	{ok, 3, Len, V, 0};
same_3(_, _, _, _, _) -> false.


%% 炸弹
same_zha([{V, Color}|List], Len, IsBanker) ->
	BR = color(Color, {0, 0}),
	case same_zha(List, Len-1, V, BR) of
		{ok, Reward} ->
			LenReward = len_reward(Len),
			XJReward = xianjia_reward(IsBanker, V, Len),
			AllReward = Reward + LenReward + XJReward,
			Type = zha_type(Len),
			{ok, Type, Len, V,  AllReward};
		false ->
			false
	end.


xianjia_reward(0, ?POKE_K, Len) ->
	k_reward(Len);
xianjia_reward(_, _, _) -> 0.
	



same_zha([{V, C}|List], Len, V, BR) ->
	BR1 = color(C, BR),
	same_zha(List, Len - 1, V, BR1);
same_zha([], 0, ?POKE_2, BR) ->
	Reward = color_reward(BR) + 1,
	{ok, Reward};
same_zha([], 0, _V, BR) -> 
	{ok, color_reward(BR)};
same_zha(_, _, _, _) -> false.


color(0, {Black, Red}) -> {Black+1, Red};
color(1, {Black, Red}) -> {Black, Red+1};
color(2, {Black, Red}) -> {Black+1, Red};
color(3, {Black, Red}) -> {Black, Red+1};
color(_, {Black, Red}) -> {Black, Red}.

color_reward({4, 4}) -> 2;
color_reward({4, _}) -> 1;
color_reward({_, 4}) -> 1;
color_reward({_, _}) -> 0.

len_reward(4) -> 0;
len_reward(Len) -> Len - 4.


%% 王炸
same_wangzha(PokeList) ->
	same_wangzha(PokeList, 0).

same_wangzha([{V, _}|List], Len) when V >= ?POKE_SMALL_KING ->
	same_wangzha(List, Len + 1);
same_wangzha([], 3) ->
	{ok, 8, 3, 0, 2};
same_wangzha([], 4) ->
	{ok, 13, 4, 0, 6};
same_wangzha(_, _) -> false.
	

%% 5 10 K 或者 555 101010 KKK
same_wushik([{?POKE_5, C}, {?POKE_10, C}, {?POKE_K, C}], _) -> {ok, 5, 1, 0, 0};
same_wushik([{?POKE_5, _}, {?POKE_10, _}, {?POKE_K, _}], _) -> {ok, 4, 1, 0, 0};
same_wushik(PokeList, 0) -> same_wu(PokeList, 0, {0, 0});
same_wushik(_, _) -> false.


same_wu([{?POKE_5, C}|List], Len, BR) ->
	BR1 = color(C, BR),
	same_wu(List, Len+1, BR1);
same_wu([{?POKE_10, C}|List], Len, BR) when Len > 2 ->
	Reward = color_reward(BR),
	BR1 = color(C, {0, 0}),
	same_shi(List, 1, Len, BR1, Reward);
same_wu(_, _, _) -> false.

same_shi([{?POKE_10, C}|List], Len, Min, BR, Reward) ->
	BR1 = color(C, BR),
	same_shi(List, Len+1, Min, BR1, Reward);
same_shi([{?POKE_K, C}|List], Len, Min, BR, Reward) when Len > 2 ->
	ShiColorReward = color_reward(BR),
	BR1 = color(C, {0, 0}),
	same_k(List, 1, min(Len, Min), BR1, Reward + ShiColorReward, abs(Len-Min));
same_shi(_, _, _, _, _) -> false.


same_k([{?POKE_K, C}|List], Len, Min, BR, Reward, Son) ->
	BR1 = color(C, BR),
	same_k(List, Len+1, Min, BR1, Reward, Son);
same_k([], Len, Min, BR, Reward, Son) when Len > 2 ->
	KColorReward = color_reward(BR),
	KReward = k_reward(Len),
	Type = wushik_type(min(Len, Min)),
	AllReward = Reward + KColorReward + KReward + Son + abs(Len-Min),
	{ok, 6, Type, 0, AllReward};
same_k(_, _, _, _, _, _) -> false.


k_reward(3) -> 0;
k_reward(4) -> 3;
k_reward(5) -> 5.


%% 副五十K < 正五十K < 4个头 < 5个头 < 3个王 < 3个五十K < 6个头 < 7个头 < 8个头 < 4个王 < 4个五十K < 5个五十K
%% 4 		5		 6		7	    8	   9		 10		11		12	   13	   14       15
zha_type(4) -> 6;
zha_type(5) -> 7;
zha_type(6) -> 10;
zha_type(7) -> 11;
zha_type(8) -> 12.


wushik_type(3) -> 9;
wushik_type(4) -> 14;
wushik_type(5) -> 15.


shuffle() ->
	CardMap = shuffle(?ALL_POKE, #{1 => {0, []}, 2 => {0, []}, 3 => {0, []}}, #{}),
	bucket(CardMap, 3, 1).



shuffle([Poke|List], UnFinMap, FinMap) ->
	Len = maps:size(UnFinMap),
	case Len of
		1 -> 
			#{1 := {_, OwnList}} = UnFinMap,
			FinMap#{1 => lists:reverse(List) ++ [Poke|OwnList]};
		_ ->
			Index = rand:uniform(Len),
			#{Index := {IndexLen, IndexList}} = UnFinMap,
			case IndexLen < ?POKE_NUM of
				true -> 
					UnFinMap1 = UnFinMap#{Index => {IndexLen+1, [Poke|IndexList]}},
					shuffle(List, UnFinMap1, FinMap);
				false ->
					Fun = fun(Key, Value, Map) ->
							if  Key > Index -> Map#{Key - 1 => Value};
								Key < Index -> Map#{Key => Value};
								true -> Map
							end
						end,
					UnFinMap1 = maps:fold(Fun, #{}, UnFinMap),
					shuffle(List, UnFinMap1, FinMap#{Len => [Poke|IndexList]})
			end
	end.


bucket(CardMap, Max, Max) ->
	swap(Max, Max, CardMap);
bucket(CardMap, Max, Min) ->
	CardMap1 = swap(Min, Max, CardMap),
	bucket(CardMap1, Max, Min+1).


swap(K, Max, Map) ->
	Idx = rand:uniform(Max),
	case Idx =/= K of
		true -> 
			#{K := V1, Idx := V2} = Map,
			Map#{K => V2, Idx => V1};
		false ->
			Map
	end.

	




