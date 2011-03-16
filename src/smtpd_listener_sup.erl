%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 7 Feb 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(smtpd_listener_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, stop/0]).
-export([start_child/1]).

-vsn('0.3').
-author('kuleshovmail@gmail.com').

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Child) ->
    supervisor:start_child(?MODULE, [Child]).

init(_Args) ->
    RestartStrategy = {one_for_one, 10, 60},
        
    Listener = {smtpd_listener, {smtpd_listener, start_link, []},
            permanent, brutal_kill, worker, [smtpd_listener]},

    FSM_sup = {smtp_fsm_sup,
    	      {smtp_fsm_sup, start_link, []},
    	       permanent, 2000, supervisor, [smtp_fsm_sup]},
     
    Children = [Listener, FSM_sup],
    
    {ok, {RestartStrategy, Children}}.   

stop() ->
    exit(whereis(?MODULE), shutdown).
