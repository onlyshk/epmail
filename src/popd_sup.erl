%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(popd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = {one_for_one, 5, 600},
    ListenerSup = {popd_listener_sup,
		   {popd_listener_sup, start_link, []},
		   permanent, 2000, supervisor, [popd_listener_sup]},
 
    Children = [ListenerSup],

    {ok, {RestartStrategy, Children}}.

    
stop() ->
    exit(whereis(?MODULE), shutdown).
