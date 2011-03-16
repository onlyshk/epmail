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

-vsn('0.3').

%% Application callbacks
-export([start/2, stop/1]).

start(normal, _StartArgs) ->
    Sup = epmail_sup:start_link(),
    Sup.

stop(_State) ->
  ok.


%%
%% Internal API
%%
