%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(pop_fsm_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, stop/0]).
-export([start_child/3]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Socket, UserName, PassWord) ->
    supervisor:start_child(?MODULE, [Socket, UserName, PassWord]).

init(_Args) ->
    RestartStrategy = {simple_one_for_one, 10, 60},
    
    Fsm = {underfined, {popd_fsm, start_link, []},
            permanent, brutal_kill, worker, [popd_fsm]},
    
    Children = [Fsm],
    
    {ok, {RestartStrategy, Children}}.   

stop() ->
    exit(whereis(?MODULE), shutdown).
