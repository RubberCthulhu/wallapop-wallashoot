%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2016, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2016 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(wallashoot_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, groups/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([wallashoot_client_basic/1]).
-export([wallashoot_client_move/1]).
-export([wallashoot_client_shoot/1]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes,10}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    %%ok = application:start(syntax_tools),
    %%ok = application:start(goldrush),
    %%ok = application:start(lager),
    ok = application:start(wallashoot),
    {ok, Pid, Id} = wallashoot:start_server(10, 10, 2),
    [{server, {Pid, Id}} | Config].

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    {_, Id} = ?config(server, Config),
    ok = wallashoot:stop_server(Id),
    ok = application:stop(wallashoot),
    %%ok = application:stop(lager),
    %%ok = application:stop(goldrush),
    %%ok = application:stop(syntax_tools),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() -> 
    [wallashoot_client_basic,
     wallashoot_client_move,
     wallashoot_client_shoot].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
wallashoot_client_basic(Config) -> 
    {Server, _} = ?config(server, Config),

    Name = "Player 1",
    {ok, _, _, _} = wallashoot:join(Server, Name),

    Self = self(),
    {ok, [{Self, Name, alive, 0}]} = wallashoot:stat(Server),

    ok = wallashoot:leave(Server),

    {ok, [{Self, Name, left, 0}]} = wallashoot:stat(Server),

    ok.

wallashoot_client_move(Config) -> 
    {Server, _} = ?config(server, Config),

    wallashoot:setopt(Server, [{action_timeout, 0}]),

    Name = "Player 1",
    {ok, {0, 0}, _, _} = wallashoot:join(Server, Name),
    
    {error, no_way} = wallashoot:move(Server, down),
    {error, no_way} = wallashoot:move(Server, down_right),
    {error, no_way} = wallashoot:move(Server, down_left),
    {error, no_way} = wallashoot:move(Server, left),
    {ok, {moved, 1, 0}} = wallashoot:move(Server, right),
    {ok, {moved, 2, 1}} = wallashoot:move(Server, up_right),
    {ok, {moved, 2, 2}} = wallashoot:move(Server, up),
    {ok, {moved, 1, 3}} = wallashoot:move(Server, up_left),
    {ok, {moved, 0, 3}} = wallashoot:move(Server, left),

    ok.

wallashoot_client_shoot(Config) -> 
    {Server, _} = ?config(server, Config),

    wallashoot:setopt(Server, [{action_timeout, 0}]),

    {Name1, Name2} = {"Player 1", "Player 2"},
    {ok, {0, 0}, _, _} = wallashoot:join(Server, Name1),

    Self = self(),
    Pid2 = spawn(
	     fun () ->
		     {ok, {0, 1}, _, _} = wallashoot:join(Server, Name2),
		     Result = wallashoot:shoot(Server, down),
		     Self ! {shoot_result, self(), Result}
	     end),
    
    receive
	{shoot_result, Pid2, {ok, {killed, {0, 0, Self}}}} ->
	    ok
    after 3000 ->
	    ct:fail(timeout)
    end,
    
    ok.





