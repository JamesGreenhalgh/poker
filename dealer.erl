-module(dealer).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([tell_table/3, new_player/2, deal/2, thanks/2, my_best_hand/3, leave_table/2]).
-record(dealer, {players=[],
		deck=[],
		readyplayers=[],
		playerhands=[],
		pot=0,
		playerbets=[] }).

start_link() -> gen_server:start_link(?MODULE, [], []).
init([]) -> 
	State = #dealer{deck=poker:shuffled_deck()},
	{ok, State}.

new_player(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {new_player, PlayerPid}).
%preflop_bet(Dealer, PlayerPid, Bet) ->
%	gen_server:call(Dealer, {preflop_bet, PlayerPid, Bet}).
deal(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {deal, PlayerPid}).
thanks(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {thanks, PlayerPid}).
my_best_hand(Dealer, PlayerPid, Hand) ->
	gen_server:call(Dealer, {players_hand, PlayerPid, Hand}).
leave_table(Dealer, PlayerPid) ->
	gen_server:call(Dealer, {departing_player, PlayerPid}).
tell_table(Dealer, PlayerPid, Message) ->
	gen_server:call(Dealer, {tell_table, PlayerPid, Message}),
	gen_server:call(Dealer, Message).

handle_call({new_player, PlayerPid}, _From, State) ->
	io:format("New Player: ~p~n",[PlayerPid]),
	Players = State#dealer.players,
	case length(Players++[PlayerPid]) of
		4 ->
			send_messages({preflop_bet}, Players++[PlayerPid]),
			send_messages({yourturn}, Players++[PlayerPid]);
		_ ->
			State#dealer.deck
	end,
	{reply, ok, State#dealer{players = Players ++ [PlayerPid]} };
handle_call({bet, PlayerPid, BetAmount}, _From, State) ->
	Players = State#dealer.players,
	NewBets = State#dealer.playerbets ++ [{PlayerPid, BetAmount}],
	NewReady = case length(State#dealer.readyplayers ++ [PlayerPid]) of
		4 ->
			% Everyone has bet. Tell them so stop
			send_messages({ended}, Players),
			[];
		_ ->
			State#dealer.readyplayers++[PlayerPid]
	end,
	{reply, ok, State#dealer{readyplayers=NewReady, playerbets=NewBets} };
handle_call({ready_for_flop, PlayerPid}, _From, State) ->
	NumPlayers = length(State#dealer.players),
	{NewDeck, NewReady} = case length(State#dealer.readyplayers ++ [PlayerPid]) of
		NumPlayers ->
			io:format("Dealing hole cards", []),
			{deal_n_cards_to_players(2, State#dealer.players, State#dealer.deck), []};
		_ ->
			{State#dealer.deck, State#dealer.readyplayers++[PlayerPid]}
	end,
	{reply, ok, State#dealer{deck=NewDeck, readyplayers=NewReady} };

%handle_call({ready, PlayerPid, Bet}, _From, State) ->
%	{Players, PlayerHands, ReadyPlayers, Deck} = State,
%	{NewDeck, NewReady} = case (length(ReadyPlayers++[PlayerPid]) =:= length(Players)) of
%		true -> 
%			{deal_n_cards_to_table(3, Players, Deck), []}
%		; false ->
%			{Deck, ReadyPlayers++[PlayerPid]}
%	end,
% 	NewState = {Players, PlayerHands, NewReady, NewDeck},
%	{reply, ok, State#dealer;
%
handle_call({thanks, PlayerPid}, _From, State) ->
	NewReadyPlayers = State#dealer.readyplayers ++ [PlayerPid],
	{NewDeck, NewReady} = case (length(NewReadyPlayers) =:= length(State#dealer.players)) of
		true -> 
			%io:format("DEALINg to taBLE!!!: ~p ~n",[PlayerHands]),
			{deal_n_cards_to_table(2, State#dealer.players, State#dealer.deck), []}
		; false ->
			{State#dealer.deck, State#dealer.readyplayers++[PlayerPid]}
	end,
 	NewState = State#dealer{readyplayers=NewReady,deck=NewDeck},
	{reply, ok, NewState};
handle_call({deal, PlayerPid}, _From, State) ->
	ReadyPlayers = State#dealer.readyplayers,	
	Players = State#dealer.players,
	{NewDeck, NewReady} = case (length(ReadyPlayers++[PlayerPid]) =:= length(Players)) of
		true -> 
			{deal_n_cards_to_table(3, Players, State#dealer.deck), []}
		; false ->
			{State#dealer.deck, ReadyPlayers++[PlayerPid]}
	end,
 	NewState = State#dealer{readyplayers=NewReady,deck=NewDeck},
	{reply, ok, NewState};
handle_call({tell_table, PlayerPid, Message}, _From, State) ->
	send_messages({Message, PlayerPid}, State#dealer.players),
	{reply, ok, State};
handle_call({fold, _PlayerPid}, _From, State) ->
	{reply, ok, State#dealer{players=[]}};
handle_call({raise, _PlayerPid, _RaiseAmount}, _From, State) ->
	{reply, ok, State#dealer{players=[]}};
handle_call({check, _PlayerPid}, _From, State) ->
	{reply, ok, State#dealer{players=[]}};

handle_call({players_hand, PlayerPid, Hand}, _From, State) ->
	NewPlayerHands = [{PlayerPid, Hand}]++State#dealer.playerhands,
	NewReadyPlayers = State#dealer.readyplayers++[PlayerPid],
	case length(NewReadyPlayers) =:= length(State#dealer.players) of
		true ->
			Winners = lists:reverse(lists:keysort(2, NewPlayerHands)),
			{Winner, WinningHand} = hd(Winners),
			io:format("Winner is ~p! Hand: ~p~n",[Winner, poker:format_evaluated_hand(WinningHand)]);
			% start new hand somehow
		false ->
			ok	
			%keep waiting
	end,
	NewState = State#dealer{playerhands=NewPlayerHands, readyplayers=NewReadyPlayers},
	{reply, ok, NewState};




handle_call({departing_player, _PlayerPid}, _From, _State) ->
	ok;
handle_call(terminate, _From, State) ->
	{stop, normal, ok, State};
handle_call({X, Y}, _From, State) ->
	io:format("Unexpected handle call: ~p ~p ~p ~n",[X,Y,State]),
	{reply, State}.
 
handle_cast(X, State) ->
	io:format("Handle cast :~p", [X]),
	{noreply, State}.
handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, State}.
terminate(normal, WTF) ->
	[io:format("Dealer terminate~p was set free.~n",[C]) || C <- WTF],
	ok;
terminate(Abnormal, WTF) ->
	io:format("Abnormal dealer terminate ~p. ~p~n",[Abnormal, WTF]),
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

send_messages(Message, Players) ->
	[send_message(Message, Player) || Player <- Players].
send_message(Message, Player) ->
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

