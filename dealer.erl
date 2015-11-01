-module(dealer).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([new_player/2, deal/2, thanks/2, my_best_hand/3, leave_table/2]).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> 
	Players = [],
	ReadyPlayers =[],
	PlayerHands =[],
	Deck = poker:shuffled_deck(),
	{ok, {Players, PlayerHands, ReadyPlayers, Deck}}.

new_player(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {new_player, PlayerPid}).
deal(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {deal, PlayerPid}).
thanks(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {thanks, PlayerPid}).
my_best_hand(Dealer, PlayerPid, Hand) ->
	gen_server:call(Dealer, {players_hand, PlayerPid, Hand}).
leave_table(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {departing_player, PlayerPid}).

handle_call({players_hand, PlayerPid, Hand}, _From, State) ->
	{Players, PlayerHands, ReadyPlayers, Deck} = State,
	NewPlayerHands = [{PlayerPid, Hand}]++PlayerHands,
	NewReadyPlayers = ReadyPlayers++[PlayerPid],
	case length(NewReadyPlayers) =:= length(Players) of
		true ->
			%figger out hands
			Winners = lists:reverse(lists:keysort(2, NewPlayerHands)),
			io:format("Winner!!!: ~p ~n",[hd(Winners)]);
			% start new hand somehow
		false ->
			ok	
			%keep waiting
	end,
	NewState = {Players, NewPlayerHands, NewReadyPlayers, Deck},
	{reply, ok, NewState};
handle_call({thanks, PlayerPid}, _From, State) ->
	{Players, PlayerHands, ReadyPlayers, Deck} = State,
	NewReadyPlayers = ReadyPlayers++[PlayerPid],
	{NewDeck, NewReady} = case (length(NewReadyPlayers) == length(Players)) of
		true -> 
			%io:format("DEALINg to taBLE!!!: ~p ~n",[PlayerHands]),
			{deal_n_cards_to_table(2, Players, Deck), []}
		; false ->
			{Deck, ReadyPlayers++[PlayerPid]}
	end,
 	NewState = {Players, PlayerHands, NewReady, NewDeck},
	{reply, ok, NewState};

handle_call({deal, PlayerPid}, _From, State) ->
	{Players, PlayerHands, ReadyPlayers, Deck} = State,
	{NewDeck, NewReady} = case (length(ReadyPlayers++[PlayerPid]) =:= length(Players)) of
		true -> 
			{deal_n_cards_to_table(3, Players, Deck), []}
		; false ->
			{Deck, ReadyPlayers++[PlayerPid]}
	end,
 	NewState = {Players, PlayerHands, NewReady, NewDeck},
	{reply, ok, NewState};
handle_call({new_player, PlayerPid}, _From, State) ->
	io:format("New Player: ~p~n",[PlayerPid]),
	{Players, PlayerHands, ReadyPlayers, Deck} = State,
	NewDeck = case length(Players++[PlayerPid]) of
		4 ->
			io:format("Dealing cards!!!: ~p ~n",[PlayerPid]),
			deal_n_cards_to_players(2, Players++[PlayerPid], Deck);
		_ ->
			Deck
	end,
	NewState = {Players++[PlayerPid], PlayerHands, ReadyPlayers, NewDeck},
	{reply, ok, NewState};
handle_call({departing_player, _PlayerPid}, _From, _State) ->
	ok;
handle_call(terminate, _From, State) ->
	{stop, normal, ok, State};
handle_call({X, Y}, _From, State) ->
	io:format("Unexpected handle call: ~p ~p ~p ~n",[X,Y,State]),
	{reply, State}.
 
handle_cast({return, Cat }, Cats) ->
	{noreply, [Cat|Cats]}.
handle_info(Msg, WTF) ->
	io:format("Unexpected message: ~p WTF: ~p~n",[Msg, WTF]),
	{noreply, WTF}.
terminate(normal, WTF) ->
	[io:format("Dealer terminate~p was set free.~n",[C]) || C <- WTF],
	ok;
terminate(Abnormal, WTF) ->
	io:format("Abnormal dealer terminate ~p. ~p~n",[Abnormal, WTF]),
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%send_messages(Message, Players) ->
%	[send_message(Message, Player) || Player <- Players].
send_message(Message, Player) ->
	%Message ! Player.
	%io:format("Sending Message ~p to Player ~p~n",[Message, Player]),
	Player ! Message.

deal_n_cards_to_players(N, [Player|Players], Deck) ->
	{Hand, NewDeck} = poker:deal_n_cards(N, Deck),
	send_message({hand, Hand}, Player),
	deal_n_cards_to_players(N, Players, NewDeck);
deal_n_cards_to_players(_, [], Deck) ->
	Deck.

deal_n_cards_to_table(N, Players, Deck) ->
	{Hand, NewDeck} = poker:deal_n_cards(N, Deck),
	[send_message({table, Hand}, Player) || Player <- Players],
	NewDeck.

