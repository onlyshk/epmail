%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(epmail_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    case epmail_sup:start_link(StartArgs) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.


stop(_State) ->
  ok.

