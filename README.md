poke
=====

send command:
cmd:create_room data:{name}
cmd:join_room data:{name, roomId}
cmd:game_ready data:{ready}
cmd:play_cards data:{cards:{}}} 

recv command
cmd:room_info data:{player:{name, pos}}
cmd:deal_cards data:{cards:{}}
cmd:play_cards data:{pos, cards:{}}
