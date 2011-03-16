%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 14 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(logger_sup).

-behaviour(supervisor).

-vsn('0.3').
-author('kuleshovmail@gmail.com').

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_Args) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Child = {logger, {logger, start_link, []},
	      Restart, Shutdown, Type, [logger]},

    {ok, {SupFlags, [Child]}}.

stop() ->
    io:format("The logger server shutdown !!! \n"),
    logger:db_backup(),
    
    io:format("The log file backuped \n"),
    {ok, Config} = config:read(config),
    LogPath = config:get_key(error_logger_path, Config),
    file:delete(LogPath),

    io:format("Temp log storage deleted \n"),
    exit(whereis(?MODULE), shutdown).
