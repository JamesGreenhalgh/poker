-module(poker).
-export([get_best_hand/2, deal_n_cards/2, shuffled_deck/0, jim/0, test/0, play_hands/1]).

-define(S, 	[{2,h}, {3,h}, {4,h}, {5,h}, {6,h}, {7,h}, {8,h}, {9,h}, {t,h}, {j,h}, {q,h}, {k,h}, {a,h},
		{2,c}, {3,c}, {4,c}, {5,c}, {6,c}, {7,c}, {8,c}, {9,c}, {t,c}, {j,c}, {q,c}, {k,c}, {a,c},
		{2,s}, {3,s}, {4,s}, {5,s}, {6,s}, {7,s}, {8,s}, {9,s}, {t,s}, {j,s}, {q,s}, {k,s}, {a,s},
		{2,d}, {3,d}, {4,d}, {5,d}, {6,d}, {7,d}, {8,d}, {9,d}, {t,d}, {j,d}, {q,d}, {k,d}, {a,d}]).

-define(START_DECK_INTEGER,
		[{2,h}, {3,h}, {4,h}, {5,h}, {6,h}, {7,h}, {8,h}, {9,h}, {10,h}, {11,h}, {12,h}, {13,h}, {14,h},
		{2,c}, {3,c}, {4,c}, {5,c}, {6,c}, {7,c}, {8,c}, {9,c}, {10,c}, {11,c}, {12,c}, {13,c}, {14,c},
		{2,s}, {3,s}, {4,s}, {5,s}, {6,s}, {7,s}, {8,s}, {9,s}, {10,s}, {11,s}, {12,s}, {13,s}, {14,s},
		{2,d}, {3,d}, {4,d}, {5,d}, {6,d}, {7,d}, {8,d}, {9,d}, {10,d}, {11,d}, {12,d}, {13,d}, {14,d}]).

shuffled_deck() ->
	shuffle(?START_DECK_INTEGER).

get_best_hand(Hand, Community) ->
	Combinations = comb(5, lists:flatten(Hand++Community)),
	%io:format("Comb hand: ~p~n", [Combinations]),
	SortableHands = [evaluate_hand(lists:sort(X)) || X <- Combinations],
	%io:format("Sortable hand: ~p~n", [SortableHands]),
	EvaluatedSortedCombos = lists:reverse(lists:sort(SortableHands)),
	hd(EvaluatedSortedCombos).
	%io:format("Winner hand: ~p~n", [Winner]),
	%Winner.

test() ->
	Deck = shuffle(?S),
	{Flop, Deck1} = deal_n_cards(3, Deck),
	{Hand, Deck2} = deal_n_cards(2, Deck1),
	{Turn, Deck3} = deal_n_cards(1, Deck2),
	{River, _Deck4} = deal_n_cards(1, Deck3),
	Length = 5,
	Combinations = comb(Length, Hand++Turn++River++Flop),
	EvaluatedSortedCombos = sort_hands([{player1, evaluate_hand(sort(X))} || X <- Combinations]),
	io:format("combos ~p~n", [EvaluatedSortedCombos]),
	%SortedCombos = sort_hands(Combinations),
	%io:format("SortedCombos: ~p~n", [SortedCombos]),
	Winner = hd(EvaluatedSortedCombos),
	io:format("Winner ~p~n", [Winner]).


%list_permutations(_List1, _List2, _Length) ->
%	ok.	

jim() ->
	Deck = shuffle(?S),
	{Flop, Deck1} = deal_n_cards(3, Deck),
	{Hand, Deck2} = deal_n_cards(2, Deck1),
	{Turn, Deck3} = deal_n_cards(1, Deck2),
	{River, _Deck4} = deal_n_cards(1, Deck3),
	Length = 5,
	%Perms = perms(Flop),
	Combs = comb(Length, Flop++Hand++Turn++River),
	%io:format("perms ~p~n", [Perms]).
	io:format("combos ~p~n", [Combs]).


%perms([]) -> [[]];
%perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].


comb(0,_) ->
    [[]];
comb(_,[]) ->
    [];
comb(N,[H|T]) ->
    [[H|L] || L <- comb(N-1,T)]++comb(N,T).


play_hands(NumHands) ->
	play_hands(NumHands, []).
play_hands(0, Stats) ->
	io:format("Statistics: ~p~n", [Stats]);
play_hands(NumHands, Stats) ->
	HandResults = play(),
	%io:format("Sorted hands ~p~n", [sort_hands(HandResults)]),
	SortedHandResults = sort_hands(HandResults),
	Winner = winner(SortedHandResults),
	%io:format("Winner ~p", [Winner]),
	TidyHandResults = [Re || {_Pl, {Re, _Ki}} <- HandResults],
	length(Winner) > 1 andalso io:format("Winner ~p TidyHand~p~n", [Winner,SortedHandResults]),
	%io:format("Tidy Hand Results ~p~n", [TidyHandResults]),
	NewStats = update_stats(TidyHandResults++Winner, Stats),
	play_hands(NumHands-1, NewStats).


winner([{Player, Hand} | Rest]) ->
	lists:flatten([Player, [X || {X, _} <- lists:takewhile(fun({_,D}) -> Hand =:= D end, Rest)]]).


update_stats([], Stats) ->
	Stats;
update_stats([Result | Rest], Stats) ->
	NewStats = case lists:keysearch(Result, 1, Stats) of
		false ->
			[{Result, 1}|Stats];
		{value, {Result, Val}} ->
			lists:keyreplace(Result, 1, Stats, {Result, 1+Val});
		_ ->
			io:format("Error")
	end,
	update_stats(Rest, NewStats).


play() ->
	StartingDeck = ?S,
	ShuffledDeck = shuffle(StartingDeck),
	% Deal Cards
	{Hand, NewDeck} = deal_n_cards(5, ShuffledDeck),
	{Hand2, NewDeck1} = deal_n_cards(5, NewDeck),
	{Hand3, NewDeck2} = deal_n_cards(5, NewDeck1),
	{Hand4, NewDeck3} = deal_n_cards(5, NewDeck2),
	{Hand5, _NewDeck4} = deal_n_cards(5, NewDeck3),
	% Check Results
	Result = evaluate_hand(sort(Hand)),
	Result2 = evaluate_hand(sort(Hand2)),
	Result3 = evaluate_hand(sort(Hand3)),
	Result4 = evaluate_hand(sort(Hand4)),
	Result5 = evaluate_hand(sort(Hand5)),
	[{player1, Result}, {player2, Result2}, {player3, Result3}, {player4, Result4}, {player5, Result5}].

sort_hands(Hands) -> 
	%lists:sort(fun(A,B) -> hand_order(A,B) end, Hands).
	lists:sort(Hands).

shuffle(Deck) ->
	random:seed(erlang:now()),
	[X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Deck])].

deal_n_cards(N, Deck) ->
	{lists:sublist(Deck,N), lists:nthtail(N,Deck)}.

sort(Hand) ->
	%lists:sort(fun(A,B) -> rank_order(A,B) end, Hand).
	lists:keysort(1, Hand).

has_flush([{_,X},{_,X},{_,X},{_,X},{_,X}]) ->
		alush;
has_flush(_Hand) ->
	noflush.
%% This system allows for community cards like in hold em.
%% That's why we might compare the kicker with quads,eg.
%% Return {hand, Highest, To, Lowest}

has([ {X,_}, {X,_}, {X,_}, {Y,_}, {Y,_}]) ->
	[hand_to_int(full_house), X, Y];
has([ {Y,_}, {Y,_}, {X,_}, {X,_}, {X,_}]) ->
	[hand_to_int(full_house), X, Y];
has([ {X,_}, {X,_}, {X,_}, {X,_}, {Y,_}]) ->
	[hand_to_int(quads), X, Y];
has([ {Y,_}, {X,_}, {X,_}, {X,_}, {X,_}]) ->
	[hand_to_int(quads), X, Y];
has([ {X,_}, {X,_}, {X,_}, {Y,_}, {Z,_}]) ->
	[hand_to_int(three_of_a_kind), X, Y, Z];
has([ {Z,_}, {X,_}, {X,_}, {X,_}, {Y,_}]) ->
	[hand_to_int(three_of_a_kind), X, Y, Z];
has([ {Z,_}, {Y,_}, {X,_}, {X,_}, {X,_}]) ->
	[hand_to_int(three_of_a_kind), X, Y, Z];
has([ {Y,_}, {Y,_}, {Z,_}, {X,_}, {X,_}]) ->
	[hand_to_int(two_pair), X, Y, Z];
has([ {Y,_}, {Y,_}, {X,_}, {X,_}, {Z,_}]) ->
	[hand_to_int(two_pair), X, Y, Z];
has([ {Z,_}, {Y,_}, {Y,_}, {X,_}, {X,_}]) ->
	[hand_to_int(two_pair), X, Y, Z];
has([ {X,_}, {X,_}, {W,_}, {Z,_}, {Y,_}]) ->
	[hand_to_int(pair), X, Y, Z, W];
has([ {W,_}, {X,_}, {X,_}, {Z,_}, {Y,_}]) ->
	[hand_to_int(pair), X, Y, Z, W];
has([ {W,_}, {Z,_}, {X,_}, {X,_}, {Y,_}]) ->
	[hand_to_int(pair), X, Y, Z, W];
has([ {W,_}, {Z,_}, {Y,_}, {X,_}, {X,_}]) ->
	[hand_to_int(pair), X, Y, Z, W];
has([ {A,_}, {B,_}, {C,_}, {D,_}, {E,_}]) ->
	Straight = ((A+1 =:= B) andalso
	(B+1 =:= C) andalso
	(C+1 =:= D) andalso
	(D+1 =:= E)),
	case Straight of
		true -> [hand_to_int(straight), E];
		false -> [hand_to_int(high), E, D, C, B, A]
	end.

evaluate_hand(Hand) ->
	case {has_flush(Hand), has(Hand)} of
		{flush, [9| Kicker]} ->
			[hand_to_int(straight_flush)]++Kicker;
		{flush, [1| Kicker]} ->
			[hand_to_int(flush)]++Kicker;
		{_, X} ->
			X
	end.	

%just_hand_order(SameHand, SameHand) ->
%	false;
%just_hand_order({Hand, Kicker}, {Hand, Kicker2}) ->
%	compare_kicker(Kicker, Kicker2);
%just_hand_order({Hand, _}, {Hand2, _}) ->
%	hand_to_int(Hand) < hand_to_int(Hand2).
%
%
%%% TODO sort this out...same as above
%hand_order({_, SameHand}, {_, SameHand}) ->
%	%io:format("Equivalent hand! WOW!! ~p~n", [SameHand]),
%	false;
%hand_order({_, {Hand, Kicker}}, {_, {Hand, Kicker2}}) ->
%	compare_kicker(Kicker, Kicker2);
%hand_order({_, {Hand, _}}, {_, {Hand2, _}}) ->
%	hand_to_int(Hand) < hand_to_int(Hand2).
%
%
%compare_kicker([Kicker | Rest], [Kicker2 | Rest2]) ->
%	case rank_to_int(Kicker) =:= rank_to_int(Kicker2) of
%		false -> 
%			rank_to_int(Kicker) > rank_to_int(Kicker2);
%		true ->
%			compare_kicker(Rest, Rest2)
%	end;
%compare_kicker([], []) ->
%	false.
%
hand_to_int(Hand) ->
	Hands = [{straight_flush,9},
		{quads,8},
		{full_house,7},
		{flush,6},
		{straight,5},
		{three_of_a_kind,4},
		{two_pair,3},
		{pair,2},
		{high,1}],
	{value, {Hand, Rank}} = lists:keysearch(Hand, 1, Hands),
	Rank.

%rank_order({A,_}, {B,_}) ->
%	rank_to_int(A) < rank_to_int(B).
%rank_to_int(A) ->
%	case A of
%		a -> 14;
%		k -> 13;
%		q -> 12;
%		j -> 11;
%		t -> 10;
%		X -> X
%	end.



% slacker

% Single player straight flush? Winner
% Multiple straight flush? Winner is the one with the highest card. If multiple people share the highest card (obviously in a different suit) they split the pot. (Note: Royal flush is excluded because it's just a special straight flush that no one else can beat.)
%    Does any single player have 4 of a kind? If yes, he is the winner.
%    Do multiple players have 4 of a kind? If yes, the one with the highest 'set of 4' is the winner. If multiple players have the highest set of 4 (which is not achievable with a standard poker deck, but is with a double deck or community cards), the one with the highest kicker (highest card not in the set of 4) is the winner. If this card is the same, they split the pot.
%    Does any single player have a full house? If yes, he is the winner.
%    Do multiple players have full houses? If yes, then keeping in mind that a full house is a 3-set and a 2-set, the player with the highest 3-set wins the pot. If multiple players share the highest 3-set (which isn't possible without community cards like in hold 'em, or a double deck) then the player with the highest 2-set is the winner. If the 2-set and 3-set is the same, those players split the pot.
%    Does any single player have a flush? If yes, he is the winner.
%    Do multiple players have a flush? If yes, the player with a flush with the highest unique card is the winner. This hand is similar to 'high card' resolution, where each card is effectively a kicker. Note that a flush requires the same suit, not just color. While the colors used on the suit are red and black, two each, there's nothing to that connection. A club is no more similar to a spade than it is to a heart - only suit matters. The colors are red and black for historical purposes and so the same deck can be played for other games where that might matter.
%    Does any single player have a straight? If yes, he wins the pot.
%    Do multiple players have straights? If so, the player with the highest straight wins. (a-2-3-4-5 is the lowest straight, while 10-j-q-k-a is the highest straight.) If multiple players share the highest straight, they split the pot.
%    Does any single player have a 3 of a kind? If yes, he wins the pot.
%    Do multiple players have 3 of a kind? If yes, the player with the highest 3-set wins the pot. If multiple players have the highest 3-set, the player with the highest kicker wins the pot. If multiple players tie for highest 3-set and highest kicker, the player with the next highest kicker wins the pot. If the players tie for the highest 3-set, highest kicker, and highest second kicker, the players split the pot.
%    Does any single player have 2-pair? If yes, he wins the pot.
%    Do multiple players have 2-pair? If yes, the player with the highest pair wins the pot. If multiple players tie for the highest pair, the player with the second highest pair wins the pot. If multiple players tie for both pairs, the player with the highest kicker wins the pot. If multiple players tie for both pairs and the kicker, the players split the pot.
%    Does any single player have a pair? If yes, he wins the pot.
%    Do multiple players have a pair? If yes, the player with the highest pair win. If multiple players have the highest pair, the player with the highest kicker wins. Compare second and third kickers as expected to resolve conflicts, or split if all three kickers tie.
%    At this point, all cards are kickers, so compare the first, second, third, fourth, and if necessary, fifth highest cards in order until a winner is resolved, or split the pot if the hands are identical.
%

