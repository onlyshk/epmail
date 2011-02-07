%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 7 Feb 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(smtp_fsm_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, stop/0]).
-export([start_child/2]).

-vsn('0.1').
-author('kuleshovmail@gmail.com').

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Socket, Client) ->
    supervisor:start_child(?MODULE, [Socket, Client]).

init(_Args) ->
    RestartStrategy = {simple_one_for_one, 10, 60},
            
    Fsm = {underfined, {smtp_fsm, start_link, []},
            permanent, brutal_kill, worker, [smtp_fsm]},

    Children = [Fsm],
    
    {ok, {RestartStrategy, Children}}.   

stop() ->
    exit(whereis(?MODULE), shutdown).
