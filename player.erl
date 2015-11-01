-module(player).

-export([join_game/1, start_player/1]).

join_game(Dealer) ->
   spawn( ?MODULE, start_player, [Dealer] ).

%% client startup code
start_player(Dealer) ->
	Hand = [],
	Community = [],
	State = {Hand, Community ,Dealer},
	dealer:new_player(Dealer, self()),
	player_loop(State).

player_loop({Hand, Community, Dealer}) ->
	io:format("Hand ~p Community ~p Dealer ~p~n", [Hand, Community, Dealer]),
	receive
		{hand, NewHand} ->
			%io:format("Hand: ~p~n", [NewHand]),
			dealer:deal(Dealer, self()),
			player_loop({NewHand, Community, Dealer});
		{table, Cards} ->
			%io:format("Table: ~p~n", [Cards]),
			NewCommunity = lists:flatten([Cards]++Community),
			case length(NewCommunity) of
				5 ->
					BestHand = poker:get_best_hand(Hand, NewCommunity),
					io:format("Best hand ~p from ~p~n", [Hand, BestHand]),
					dealer:my_best_hand(Dealer, self(), BestHand);
				_ ->
					dealer:thanks(Dealer, self())
			end,
			NewState = {Hand, NewCommunity, Dealer},
			player_loop( NewState );
		{result, Result} ->
			io:format("Result: I~p~n", [Result]),
			dealer:leave_table(Dealer, self());
		X ->
			io:format("received ~p~n", [X])
	end.
