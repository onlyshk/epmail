%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(popd_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Socket) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Socket).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================-
init(Socket) ->
    io:format("Supervisor start"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {popd, {popd, start_link, [Socket]},
	      Restart, Shutdown, Type, [popd]},

    {ok, {SupFlags, [AChild]}}.

stop() ->
    io:format("The pop3 server shutdown !!! \n"),
    exit(whereis(?MODULE), shutdown).
