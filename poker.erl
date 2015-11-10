-module(poker).
-export([format_evaluated_hand/1, format_cards/1, get_best_hand/2, deal_n_cards/2, shuffled_deck/0, jim/0, test/0, play_hands/1]).

-define(S, 	[{2,h}, {3,h}, {4,h}, {5,h}, {6,h}, {7,h}, {8,h}, {9,h}, {t,h}, {j,h}, {q,h}, {k,h}, {a,h},
		{2,c}, {3,c}, {4,c}, {5,c}, {6,c}, {7,c}, {8,c}, {9,c}, {t,c}, {j,c}, {q,c}, {k,c}, {a,c},
		{2,s}, {3,s}, {4,s}, {5,s}, {6,s}, {7,s}, {8,s}, {9,s}, {t,s}, {j,s}, {q,s}, {k,s}, {a,s},
		{2,d}, {3,d}, {4,d}, {5,d}, {6,d}, {7,d}, {8,d}, {9,d}, {t,d}, {j,d}, {q,d}, {k,d}, {a,d}]).

-define(START_DECK_INTEGER,
		[{2,h}, {3,h}, {4,h}, {5,h}, {6,h}, {7,h}, {8,h}, {9,h}, {10,h}, {11,h}, {12,h}, {13,h}, {14,h},
		{2,c}, {3,c}, {4,c}, {5,c}, {6,c}, {7,c}, {8,c}, {9,c}, {10,c}, {11,c}, {12,c}, {13,c}, {14,c},
		{2,s}, {3,s}, {4,s}, {5,s}, {6,s}, {7,s}, {8,s}, {9,s}, {10,s}, {11,s}, {12,s}, {13,s}, {14,s},
		{2,d}, {3,d}, {4,d}, {5,d}, {6,d}, {7,d}, {8,d}, {9,d}, {10,d}, {11,d}, {12,d}, {13,d}, {14,d}]).


format_cards(Cards) ->
	lists:flatten([rank_to_string(Rank)++suit_to_string(Suit)++", " || {Rank, Suit} <- Cards]).

suit_to_string(Suit) ->
	string:to_upper(atom_to_list(Suit)).

rank_to_string(Rank) ->
	case Rank of 
		14 -> "A";
		13 -> "K";
		12 -> "Q";
		11 -> "J";
		10 -> "T";
		X -> integer_to_list(X)
	end.

format_evaluated_hand(Hand) ->
	[Type | Kicker] = Hand,
	String = case(Type) of
		9 ->
			rank_to_string(Kicker) ++ " high straight flush";
		8 ->
			[FourOf | Rest] = Kicker,
			"Four " ++ rank_to_string(FourOf) ++ "s + " ++ [rank_to_string(X)++", " || X <- Rest];
		7 ->
			[ThreeOf | [TwoOf|Rest]] = Kicker,
			"Full House: 3 " ++ rank_to_string(ThreeOf) ++ ". Two of " ++ rank_to_string(TwoOf) ++ " + " ++ [rank_to_string(X)++", " || X <- Rest];
		6 ->
			"Flush + " ++ [rank_to_string(X)++", " || X <- Kicker];
		5 ->
			rank_to_string(hd(Kicker)) ++ " high straight";
		4 ->
			[Threes | Rest] = Kicker,
			"Three " ++ rank_to_string(Threes) ++ "s + " ++ [rank_to_string(X)++", " || X <- Rest];
		3 ->
			[PairOne | [PairTwo|Rest]] = Kicker,
			"Two Pairs: " ++ rank_to_string(PairOne) ++ " and " ++ rank_to_string(PairTwo) ++ " + " ++ [rank_to_string(X)++", " || X <- Rest];
		2 ->
			[Pair | Rest] = Kicker,
			"Pair of " ++ rank_to_string(Pair) ++ "s + " ++ [rank_to_string(X)++", " || X <- Rest];
		1 ->
			"High: " ++ [rank_to_string(X)++", " || X <- Kicker];
		X ->
			"Error: " ++ X
	end,
	lists:flatten(String).

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
	flush;
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
%int_to_hand(Int) ->
%	case Int of
%		9 -> "Straight flush";
%               8 -> "Quads";
%                7 -> "Full House";
%                6 -> "Flush";
%                5 -> "Straight";
%                4 -> "Three Of A Kind";
%                3 -> "Two Pair";
%                2 -> "Pair";
%                1 -> "High";
%		_ -> undefined
%	end.


%rank_order({A,_}, {B,_}) ->
%	rank_to_int(A) < rank_to_int(B).
%rank_to_int(A) ->
%%	case A of
%		a -> 14;
%		k -> 13;
%		q -> 12;
%		j -> 11;
%		t -> 10;
%		X -> X
%	end.
