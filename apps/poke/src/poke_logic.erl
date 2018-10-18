-module(poke_logic).

%% 2张梅花K 1张黑桃K [41,41,43]
%% lists:reverse(lists:flatten([[Num, Num] || Num <- lists:seq(0, 53)]) -- [41,41,43]) 
-define(POKE_LIST, [53,53,52,52,51,51,50,50,49,49,48,48,47,47,46,46,45,45,44,44,43,42,42,40,40,39,39,38,38,37,37,36,36,35,35,34,34,33,33,32,32,31,31,30,30,29,29,28,28,27,27,26,26,25,25,24,24,23,23,22,22,21,21,20,20,19,19,18,18,17,17,16,16,15,15,14,14,13,13,12,12,11,11,10,10,9,9,8,8,7,7,6,6,5,5,4,4,3,3,2,2,1,1,0,0]).

-define(POKE_NUM, 34).


-export([
	shuffle/0,
	shuffle/3
]).



shuffle() ->
	CardMap = shuffle(?POKE_LIST, #{1 => {0, []}, 2 => {0, []}, 3 => {0, []}}, #{}),
	bucket(CardMap, 3, 1).



shuffle([Poke|List], UnFinMap, FinMap) ->
	Len = maps:size(UnFinMap),
	case Len of
		1 -> FinMap#{1 => lists:reverse(List) ++ [Poke|OwnList]};
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

	

% shuffle([Poke|List], UnFinList, FinList) ->
% 	case UnFinList of
% 		[{L1, PL1}, {L2, PL2}] ->
% 			case rand:uniform(2) of
% 				1 -> 
% 					case L1 < ?POKE_NUM of
% 						true -> 
% 							shuffle(List, [{L1+1, [Poke|PL1]}, {L2, PL2}], FinList);
% 						false ->
% 							[[Poke|PL1], List ++ PL2, FinList]
% 					end;
% 				2 -> 
% 					case L2 < ?POKE_NUM of
% 						true ->
% 							shuffle(List, [{L1, PL1}, {L2+1, [Poke|PL2]}], FinList);
% 						false ->
% 							[lists:reverse(List) ++ PL1, [Poke|PL2], FinList]
% 					end
% 			end;
% 		[{L1, PL1}, {L2, PL2}, {L3, PL3}] ->
% 			case rand:uniform(3) of
% 				1 -> 
% 					case L1 < ?POKE_NUM of
% 						true -> 
% 							shuffle(List, [{L1+1, [Poke|PL1]}, {L2, PL2}, {L3, PL3}], FinList);
% 						false ->
% 							shuffle(List, [{L2, PL2}, {L3, PL3}], [Poke|PL1])
% 					end;
% 				2 ->
% 					case L2 < ?POKE_NUM of
% 						true -> 
% 							shuffle(List, [{L1, PL1}, {L2+1, [Poke|PL2]}, {L3, PL3}], FinList);
% 						false ->
% 							shuffle(List, [{L1, PL1}, {L3, PL3}], [Poke|PL2])
% 					end;
% 				3 ->
% 					case L3 < ?POKE_NUM of
% 						true -> 
% 							shuffle(List, [{L1, PL1}, {L2, PL2}, {L3+1, [Poke|PL3]}], FinList);
% 						false ->
% 							shuffle(List, [{L1, PL1}, {L2, PL2}], [Poke|PL3])
% 					end
% 			end
% 	end.



