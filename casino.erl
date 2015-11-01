-module(casino).
-export([go/0]).

go() ->
	{ok, DPid} = dealer:start_link(),
	[player:join_game(DPid) || _X  <- lists:seq(1,4)].
