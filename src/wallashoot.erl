%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2016, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created : 13 Mar 2016 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(wallashoot).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_server/3, start_server/4, stop_server/1, start_link/3]).
-export([join/2, leave/1, move/2, shoot/2, stat/1, setopt/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	  width :: pos_integer(),
	  height :: pos_integer(),
	  max_players :: pos_integer(),
	  players :: ets:tab(),
	  field :: array:array(),
	  action_timeout = 100 :: non_neg_integer()
}).

-record(player, {
	  pid :: pid(),
	  monitor :: reference(),
	  name :: string(),
	  x :: undefined | non_neg_integer(),
	  y :: undefined | non_neg_integer(),
	  state = alive :: alive | dead | left,
	  kills = 0 :: non_neg_integer(),
	  timer :: undefined | timer:tref()
}).

-type direction() :: up
		   | up_right
		   | right
		   | down_right
		   | down
		   | down_left
		   | left
		   | up_left.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_server(Width, Height, MaxPlayers) -> {ok, Pid, Id} | {error, Reason} when
      Width :: pos_integer(),
      Height :: pos_integer(),
      MaxPlayers :: pos_integer(),
      Pid :: pid(),
      Id :: term(),
      Reason :: any().

start_server(Width, Height, MaxPlayers) ->
    Id = make_ref(),
    start_server(Id, Width, Height, MaxPlayers).

-spec start_server(Id, Width, Height, MaxPlayers) -> {ok, Pid, Id} | {error, Reason} when
      Id :: term(),
      Width :: pos_integer(),
      Height :: pos_integer(),
      MaxPlayers :: pos_integer(),
      Pid :: pid(),
      Id :: term(),
      Reason :: any().

start_server(Id, Width, Height, MaxPlayers) ->
    case wallashoot_sup:start_server(Id, Width, Height, MaxPlayers) of
	{ok, Pid} ->
	    {ok, Pid, Id};
	Error ->
	    Error
    end.

-spec stop_server(Id) -> ok when
      Id :: term().

stop_server(Id) ->
    wallashoot_sup:stop_server(Id).

-spec start_link(Width, Height, MaxPlayers) -> {ok, Server} | {error, Reason} when
      Width :: pos_integer(),
      Height :: pos_integer(),
      MaxPlayers :: pos_integer(),
      Server :: pid(),
      Reason :: any().

start_link(Width, Height, MaxPlayers) ->
    gen_server:start_link(?MODULE, [Width, Height, MaxPlayers], []).

-spec join(Server, Name) -> {ok, {X, Y}, Stat, Field} | {error, Reason} when
      Server :: pid(),
      Name :: term(),
      X :: non_neg_integer(),
      Y :: non_neg_integer(),
      Stat :: [{PlayerPid, PlayerName, PlayerState, PlayerKills}],
      PlayerPid :: pid(),
      PlayerName :: term(),
      PlayerState :: alive | dead | left,
      PlayerKills :: non_neg_integer(),
      Field :: array:array(array:array(undfined | pid())),
      Reason :: any().

join(Server, Name) ->
    gen_server:call(Server, {join, Name}).

-spec leave(Server) -> ok when
      Server :: pid().

leave(Server) ->
    gen_server:cast(Server, {leave, self()}).

-spec move(Server, Direction) -> {ok, {moved, X, Y}} | {error, Reason} when
      Server :: pid(),
      Direction :: direction(),
      X :: non_neg_integer(),
      Y :: non_neg_integer(),
      Reason :: any().

move(Server, Direction) ->
    gen_server:call(Server, {move, Direction}).

-spec shoot(Server, Direction) -> {ok, {killed, {X, Y, Player}}} | {error, Reason} when
      Server :: pid(),
      Direction :: direction(),
      X :: non_neg_integer(),
      Y :: non_neg_integer(),
      Player :: pid(),
      Reason :: any().

shoot(Server, Direction) ->
    gen_server:call(Server, {shoot, Direction}).

-spec stat(Server) -> {ok, Stat} when
      Server :: pid(),
      Stat :: [{PlayerPid, PlayerName, PlayerState, PlayerKills}],
      PlayerPid :: pid(),
      PlayerName :: term(),
      PlayerState :: alive | dead | left,
      PlayerKills :: non_neg_integer().
      
stat(Server) ->
    gen_server:call(Server, stat).

setopt(Server, Opts) ->
    gen_server:call(Server, {setopt, Opts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Width, Height, MaxPlayers]) ->
    State = #state{
	       width = Width,
	       height = Height,
	       max_players = MaxPlayers,
	       players = ets:new(players, [private, {keypos, #player.pid}]),
	       field = field_new(Width, Height)
	      },
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({join, Name}, {Pid, _}, State) ->
    case player_join(Pid, Name, State) of
	{ok, {X, Y}, #state{field = Field} = State1} ->
	    lager:info("Player joined: ~p '~p': {~p, ~p}", [Pid, Name, X, Y]),

	    notify({player_joined, Pid, Name, {X, Y}}, [Pid], State1),

	    Stat = get_stat(State1),
	    {reply, {ok, {X, Y}, Stat, Field}, State1};
	Error ->
	    lager:error("Join player: ~p '~p': ~p", [Pid, Name, Error]),
	    {reply, Error, State}
    end;

handle_call({move, Direction}, {Pid, _}, State) ->
    case player_move(Pid, Direction, State) of
	{ok, {moved, X, Y} = Result, State1} ->
	    lager:info("Player moved: ~p: {~p, ~p}", [Pid, X, Y]),

	    notify({player_moved, Pid, {X, Y}}, [Pid], State1),
	    {reply, {ok, Result}, State1};
	Error ->
	    lager:error("Player move: ~p: ~p", [Pid, Error]),
	    {reply, Error, State}
    end;

handle_call({shoot, Direction}, {Pid, _}, State) ->
    case player_shoot(Pid, Direction, State) of
	{ok, State1} ->
	    lager:info("Player shooted: ~p: missed", [Pid]),

	    notify({player_shot, Pid, Direction}, [Pid], State1),
	    {reply, ok, State1};

	{ok, {killed, {X, Y, Pid1}} = Result, State1} ->
	    lager:info("Player shooted: ~p: kill: ~p(~p, ~p)", [Pid, Pid1, X, Y]),

	    notify({player_shot, Pid, Direction}, [Pid], State1),
	    notify({player_killed, Pid, Pid1}, [Pid], State1),

	    case is_game_finished(State1) of
		true ->
		    Stat = get_stat(State1),
		    lager:info("Game finished: ~p", [Stat]),

		    notify({finished, Stat}, [], State1),
		    {stop, normal, {ok, Result}, State1};
		false ->
		    {reply, {ok, Result}, State1}
	    end;
	Error ->
	    {reply, Error, State}
    end;

handle_call(stat, _From, State) ->
    Stat = get_stat(State),
    {reply, {ok, Stat}, State};

handle_call({setopt, Opts}, _From, State) ->
    State1 = server_setopt(Opts, State),
    {reply, ok, State1};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({leave, Pid}, State) ->
    case player_leave(Pid, State) of
	{ok, State1} ->
	    lager:info("Player left the game: ~p: exited", [Pid]),
	    notify({player_left, Pid}, [Pid], State1),
	    case is_game_finished(State1) of
		true ->
		    Stat = get_stat(State1),
		    lager:info("Game finished: ~p", [Stat]),

		    notify({finished, Stat}, [], State1),
		    {stop, normal, State1};
		false ->
		    {noreply, State1}
	    end;
	_Else ->
	    {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({player_timer, Pid}, State) ->
    case player_timer(Pid, State) of
	{ok, State1} ->
	    {noreply, State1};
	_ ->
	    {noreply, State}
    end;

handle_info({'DOWN', _Monitor, process, Pid, Reason}, State) ->
    case player_leave(Pid, State) of
	{ok, State1} ->
	    lager:info("Player left the game: ~p: ~p", [Pid, Reason]),
	    notify({player_left, Pid}, [Pid], State1),
	    case is_game_finished(State1) of
		true ->
		    Stat = get_stat(State1),
		    lager:info("Game finished: ~p", [Stat]),
		    
		    notify({finished, Stat}, [], State1),
		    {stop, normal, State1};
		false ->
		    {noreply, State1}
	    end;
	_Else ->
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

server_setopt([{action_timeout, T} | Opts], State) ->
    server_setopt(Opts, State#state{action_timeout = T});
server_setopt([], State) ->
    State.

notify(Msg, Exclude, #state{players = Tab} = _State) ->
    Msg1 = {wallashoot, self(), Msg},
    Players = ets:foldl(
		fun (#player{pid = Pid}, Acc) ->
			case not lists:member(Pid, Exclude) of
			    true ->
				[Pid | Acc];
			    false ->
				Acc
			end
		end,
		[], Tab),
    
    lists:foreach(
      fun (Pid) ->
	      try
		  Pid ! Msg1
	      catch
		  _:_ ->
		      ok
	      end
      end,
      Players),
    
    ok.

player_join(Pid, Name, #state{players = Players, max_players = MaxPlayers} = State) ->
    case get_player(Pid, Players) of
	{ok, _} ->
	    {error, already_in_game};
	false ->
	    case players_number(Players) < MaxPlayers of
		true ->
		    case player_name_exists(Name, Players) of
			true ->
			    {error, name_exists};
			false ->
			    case create_player(Pid, Name, State) of
				{ok, Player, State1} ->
				    {ok, {Player#player.x, Player#player.y}, State1};
				Error ->
				    Error
			    end
		    end;
		false ->
		    {error, max_players}
	    end
    end.

player_leave(Pid, #state{players = Tab, field = Field} = State) ->
    case get_player(Pid, Tab) of
	{ok, #player{state = alive} = Player} ->
	    demonitor(Player#player.monitor),
	    case Player#player.timer of
		undefined ->
		    ok;
		_ ->
		    timer:cancel(Player#player.timer)
	    end,
	    {ok, Field1} = field_set(Field, Player#player.x, Player#player.y, undefined),
	    Player1 = Player#player{
			state = left,
			x = undefined,
			y = undefined,
			monitor = undefined,
			timer = undefined
		       },
	    put_player(Player1, Tab),

	    {ok, State#state{field = Field1}};

	{ok, _} ->
	    {ok, State};
	Error ->
	    Error
    end.

player_move(Pid, Direction, #state{players = Tab, field = Field} = State) ->
    case get_alive_player(Pid, Tab) of
	{ok, #player{timer = undefined, x = X, y = Y} = Player} ->
	    case field_move(Field, X, Y, Direction) of
		{ok, {X1, Y1}, Field1} ->
		    {ok, TimerRef} = timer:send_after(State#state.action_timeout, self(), {player_timer, Pid}),
		    
		    put_player(Player#player{x = X1, y = Y1, timer = TimerRef}, Tab),
		    {ok, {moved, X1, Y1}, State#state{field = Field1}};
		Error ->
		    Error
	    end;
	{ok, _Player} ->
	    {error, not_ready};
	Error ->
	    Error
    end.

player_shoot(Pid, Direction, #state{players = Tab, field = Field} = State) ->
    case get_alive_player(Pid, Tab) of
	{ok, #player{timer = undefined, x = X, y = Y} = Player} ->
	    case field_shoot(Field, X, Y, Direction) of
		{ok, {X1, Y1, Pid1}, Field1} ->
		    {ok, State1} = player_die(Pid1, State#state{field = Field1}),

		    {ok, TimerRef} = timer:send_after(State1#state.action_timeout, self(), {player_timer, Pid}),
		    put_player(Player#player{kills = Player#player.kills + 1, timer = TimerRef}, Tab),

		    {ok, {killed, {X1, Y1, Pid1}}, State1};
		false ->
		    {ok, TimerRef} = timer:send_after(State#state.action_timeout, self(), {player_timer, Pid}),
		    put_player(Player#player{timer = TimerRef}, Tab),
		    {ok, State};
		Error ->
		    Error
	    end;
	{ok, _Player} ->
	    {error, not_ready};
	Error ->
	    Error
    end.

player_die(Pid, #state{players = Tab, field = Field} = State) ->
    case get_player(Pid, Tab) of
	{ok, #player{state = alive} = Player} ->
	    demonitor(Player#player.monitor),
	    case Player#player.timer of
		undefined ->
		    ok;
		_ ->
		    timer:cancel(Player#player.timer)
	    end,
	    {ok, Field1} = field_set(Field, Player#player.x, Player#player.y, undefined),
	    Player1 = Player#player{
			state = dead,
			x = undefined,
			y = undefined,
			monitor = undefined,
			timer = undefined
		       },
	    put_player(Player1, Tab),
	    
	    {ok, State#state{field = Field1}};
	{ok, _} ->
	    {ok, State};
	Error ->
	    Error
    end.

player_timer(Pid, #state{players = Tab} = State) ->
    case get_player(Pid, Tab) of
	{ok, Player} ->
	    put_player(Player#player{timer = undefined}, Tab),
	    {ok, State};
	Error ->
	    Error
    end.

is_game_finished(#state{players = Tab, max_players = MaxPlayers} = _State) ->
    Alive = ets:match_object(Tab, #player{_ = '_', state = alive}),
    (ets:info(Tab, size) == MaxPlayers) and (length(Alive) =< 1).

create_player(Pid, Name, State) ->
    case field_get_free(State#state.field) of
	{ok, {X, Y}} ->
	    Monitor = monitor(process, Pid),
	    Player = #player{
			pid = Pid,
			name = Name,
			monitor = Monitor,
			state = alive,
			x = X,
			y = Y
		       },

	    ok = put_player(Player, State#state.players),
	    {ok, Field1} = field_set(State#state.field, X, Y, Pid),
	    {ok, Player, State#state{field = Field1}};
	Error ->
	    Error
    end.

get_player(Pid, Tab) ->
    case ets:lookup(Tab, Pid) of
	[Player] ->
	    {ok, Player};
	[] ->
	    false
    end.

get_alive_player(Pid, Tab) ->
    case get_player(Pid, Tab) of
	{ok, #player{state = alive} = Player} ->
	    {ok, Player};
	{ok, _} ->
	    {error, not_alive};
	Error ->
	    Error
    end.

put_player(Player, Tab) ->
    true = ets:insert(Tab, Player),
    ok.

player_name_exists(Name, Tab) ->
    case ets:match_object(Tab, #player{_ = '_', name = Name}) of
	[_ | _] ->
	    true;
	[] ->
	    false
    end.

players_number(Players) ->
    ets:info(Players, size).

field_new(W, H) ->
    array:new(W, [{default, array:new(H, [{default, undefined}])}]).

field_get(Field, X, Y) ->
    field_access_pos(Field, X, Y,
		     fun (_, Array, _, _) ->
			     {ok, array:get(Y, Array)}
		     end).

field_set(Field, X, Y, Value) ->
    field_access_pos(Field, X, Y,
		     fun (_, Array, _, _) ->
			     Array1 = array:set(Y, Value, Array),
			     Field1 = array:set(X, Array1, Field),
			     {ok, Field1}
		     end).

field_access_pos(Field, X, Y, F) ->
    case (X >= 0) and (X < array:size(Field)) of
	true ->
	    Array = array:get(X, Field),
	    case (Y >= 0) and (Y < array:size(Array)) of
		true ->
		    F(Field, Array, X, Y);
		_ ->
		    {error, badarg}
	    end;
	false ->
	    {error, badarg}
    end.

field_get_free(Field) ->
    field_get_free(Field, 0, 0).

field_get_free(Field, I, J) ->
    case field_get(Field, I, J) of
	{ok, undefined} ->
	    {ok, {I, J}};
	{ok, _} ->
	    field_get_free(Field, I, J+1);
	{error, badarg} when J > 0 ->
	    field_get_free(Field, I+1, 0);
	{error, badarg} ->
	    {error, no_free_pos}
    end.

field_move(Field, X1, Y1, Direction) ->
    {X2, Y2} = direction(X1, Y1, Direction),
    case field_get(Field, X1, Y1) of
	{ok, V} ->
	    case field_get(Field, X2, Y2) of
		{ok, undefined} ->
		    {ok, Field1} = field_set(Field, X1, Y1, undefined),
		    {ok, Field2} = field_set(Field1, X2, Y2, V),
		    {ok, {X2, Y2}, Field2};
		{ok, _} -> 
		    {error, position_is_busy};
		{error, badarg} ->
		    {error, no_way};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

field_shoot(Field, X, Y, Direction) ->
    {X1, Y1} = direction(X, Y, Direction),
    case field_get(Field, X1, Y1) of
	{ok, undefined} ->
	    field_shoot(Field, X1, Y1, Direction);
	{ok, Pid} ->
	    {ok, Field1} = field_set(Field, X1, Y1, undefined),
	    {ok, {X1, Y1, Pid}, Field1};
	%% Out of the field.
	_Error ->
	    false
    end.

-ifdef(TEST).
field_basic_test() ->
    Field = field_new(5, 5),
    
    ?assertEqual({error, badarg}, field_set(Field, 5, 5, 5)),
    {ok, Field1} = field_set(Field, 2, 2, 2),
    {ok, Field2} = field_set(Field1, 3, 3, 3),
    
    ?assertEqual({error, badarg}, field_get(Field2, 5, 5)),
    ?assertEqual({ok, undefined}, field_get(Field2, 1, 1)),
    ?assertEqual({ok, 2}, field_get(Field2, 2, 2)),
    ?assertEqual({ok, 3}, field_get(Field2, 3, 3)),

    ok.

field_get_free_test() ->
    Field = field_new(2, 2),

    ?assertEqual({ok, {0, 0}}, field_get_free(Field)),
    
    {ok, Field1} = field_set(Field, 0, 0, 1),
    {ok, Field2} = field_set(Field1, 0, 1, 1),
    {ok, Field3} = field_set(Field2, 1, 0, 1),
    ?assertEqual({ok, {1, 1}}, field_get_free(Field3)),

    {ok, Field4} = field_set(Field3, 1, 1, 1),
    ?assertEqual({error, no_free_pos}, field_get_free(Field4)),
    
    ok.

field_move_test() ->
    Field = field_new(2, 2),
    {ok, Field1} = field_set(Field, 0, 0, 1),
    {ok, Field2} = field_set(Field1, 0, 1, 1),

    {ok, {1, 0}, _} = field_move(Field2, 0, 0, right),
    {ok, {1, 1}, _} = field_move(Field2, 0, 0, up_right),
    ?assertEqual({error, position_is_busy}, field_move(Field2, 0, 0, up)),
    ?assertEqual({error, no_way}, field_move(Field2, 0, 0, left)),

    ok.

field_shoot_test() ->
    Field = field_new(2, 2),
    {ok, Field1} = field_set(Field, 0, 0, 1),
    {ok, Field2} = field_set(Field1, 0, 1, 1),

    ?assertEqual(false, field_shoot(Field2, 0, 0, right)),
    ?assertEqual(false, field_shoot(Field2, 0, 0, up_right)),
    {ok, {0, 1, 1}, _} = field_shoot(Field2, 0, 0, up),

    ok.
-endif.

direction(X, Y, up) ->
    {X, Y+1};
direction(X, Y, up_right) ->
    {X+1, Y+1};
direction(X, Y, right) ->
    {X+1, Y};
direction(X, Y, down_right) ->
    {X+1, Y-1};
direction(X, Y, down) ->
    {X, Y-1};
direction(X, Y, down_left) ->
    {X-1, Y-1};
direction(X, Y, left) ->
    {X-1, Y};
direction(X, Y, up_left) ->
    {X-1, Y+1}.

-ifdef(TEST).
direction_test_() ->
    [?_test({1, 2} = direction(1, 1, up)),
     ?_test({2, 2} = direction(1, 1, up_right)),
     ?_test({2, 1} = direction(1, 1, right)),
     ?_test({2, 0} = direction(1, 1, down_right)),
     ?_test({1, 0} = direction(1, 1, down)),
     ?_test({0, 0} = direction(1, 1, down_left)),
     ?_test({0, 1} = direction(1, 1, left)),
     ?_test({0, 2} = direction(1, 1, up_left))].
-endif.

get_stat(#state{players = Tab}) ->
    ets:foldl(
      fun (#player{pid = Pid, name = Name, state = PlayerState, kills = Kills}, Acc) ->
	      [{Pid, Name, PlayerState, Kills} | Acc]
      end,
      [], Tab).


