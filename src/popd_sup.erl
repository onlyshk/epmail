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

-vsn('0.1').
-author('kuleshovmail@gmail.com').

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

    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),

    case UserStorage of
	mnesia ->
	    mnesia:create_schema([node()]),
	    mnesia:start(),
	    mnesia:create_table(users, []);
	_ ->
	    dets
    end,
    
    RestartStrategy = {one_for_one, 5, 600},
 
    ListenerSup = {popd_listener_sup,
		  {popd_listener_sup, start_link, []},
		  permanent, 2000, supervisor, [popd_listener_sup]},

    SmtpListenerSup = {smtpd_listener_sup,
		      {smtpd_listener_sup, start_link, []},
		      permanent, 2000, supervisor, [smtpd_listener_sup]},
   
    Children = [ListenerSup, SmtpListenerSup],

    {ok, {RestartStrategy, Children}}.

    
stop() ->
    exit(whereis(?MODULE), shutdown).
