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

-include_lib("epmail.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(normal, _StartArgs) ->
    ?INFO_MSG("Starting application ~p~n", [?MODULE]),
    Sup = epmail_sup:start_link(),
    Sup.

stop(_State) ->
  ok.


%%
%% Internal API
%%
