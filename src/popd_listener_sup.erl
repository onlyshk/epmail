%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(popd_listener_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, stop/0]).
-export([start_child/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Child) ->
    supervisor:start_child(?MODULE, [Child]).

init(_Args) ->
    RestartStrategy = {one_for_one, 10, 60},
       
    Listener = {popd_listener, {popd_listener, start_link, []},
            permanent, brutal_kill, worker, [popd_listener]},

    FSM_sup = {pop_fsm_sup,
    	      {pop_fsm_sup, start_link, []},
    	       permanent, 2000, supervisor, [pop_fsm_sup]},
     
    Children = [Listener, FSM_sup],
    
    {ok, {RestartStrategy, Children}}.   

stop() ->
    exit(whereis(?MODULE), shutdown).
