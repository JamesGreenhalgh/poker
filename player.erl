-module(player).
-export([join_game/1, start_player/1]).
-record(player,	{dealer,
		hand=[],
		community=[],
		bet=0,
		stack=100} ).

join_game(Dealer) -> spawn( ?MODULE, start_player, [Dealer] ).

start_player(Dealer) ->
	State = #player{dealer=Dealer},
	dealer:new_player(Dealer, self()),
	player_loop(State).

player_loop(State) ->
	io:format("Hand ~p Community ~p~n", [poker:format_cards(State#player.hand), poker:format_cards(State#player.community)]),
	receive
		{preflop_bet} ->
			betting_loop(State);
		{hand, Hand} ->
			dealer:deal(State#player.dealer, self()),
			player_loop(State#player{hand=Hand});
		{preturn_bet} ->
			betting_loop(State);		
		{table, Cards} ->
			io:format("Table: ~p~n", [Cards]),
			NewCommunity = lists:flatten([Cards]++State#player.community),
			case length(NewCommunity) of
				5 ->
					BestHand = poker:get_best_hand(State#player.hand, NewCommunity),
					io:format("Best hand ~p from ~p~n", [poker:format_evaluated_hand(BestHand), poker:format_cards(State#player.hand)]),
					dealer:my_best_hand(State#player.dealer, self(), BestHand);
				_ ->
					dealer:thanks(State#player.dealer, self())
			end,
			player_loop( State#player{community=NewCommunity} );
		{preriver_bet} ->
			betting_loop(State);		
		{river_bet} ->
			betting_loop(State);		
		{result, Result} ->
			io:format("Result: I~p~n", [Result]),
			dealer:leave_table(State#player.dealer, self());
		X ->
			io:format("received ~p~n", [X]),
			player_loop(State)
	end.
% Bet, check, or fold
% If someone else bets, you get to call it, re-raise, or fold
% If noone elsebets, next phase
betting_loop(State) ->
	receive
		{raise, Player, RaiseAmount} ->
			io:format("~p has raised to ~p~n", [Player, RaiseAmount]),
			betting_loop(State);
		{bet, Player, BetAmount} ->
			io:format("~p has bet~p~n", [Player, BetAmount]),
			betting_loop(State);
		{call, Player} ->
			io:format("~p has called~n", [Player]),
			betting_loop(State);
		{check, Player} ->
			io:format("~p has checked~n", [Player]),
			betting_loop(State);
		{ended} ->
			io:format("end of betting loop~p~n", ["BLA"]),
			dealer:tell_table(State#player.dealer, self(), {ready_for_flop, self()}),
			player_loop(State);
		{fold, Player} ->
			io:format("~p has folded~n", [Player]),
			betting_loop(State);
		{yourturn} ->
			%dealer:tell_table(raise, State#player.dealer, self(), 100), 
			dealer:tell_table(State#player.dealer, self(), {bet, self(), 100}),
			%dealer:tell_table(call, State#player.dealer, self(), 100), 
			%dealer:tell_table(check, State#player.dealer, self()), 
			%dealer:tell_table(fold,  State#player.dealer, self()), 
			betting_loop(State#player{bet=100});
		X ->
			io:format("Player ~p received ~p", [self(), X]),
			betting_loop(State)
	end.
