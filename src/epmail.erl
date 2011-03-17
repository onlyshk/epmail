%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(epmail).

-behaviour(application).

-vsn('0.3').

-include_lib("epmail.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(normal, _StartArgs) ->
    ?INFO_MSG("Starting application ~p~n", [?MODULE]),
    ok = init_mnesia(),
    ok = config:start(),
    Sup = epmail_sup:start_link(),
    Sup.

stop(_State) ->
  ok.


%%
%% Internal API
%%

init_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).
