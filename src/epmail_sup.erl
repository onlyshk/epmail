%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(epmail_sup).

-behaviour(supervisor).

-vsn('0.2').
-author('kuleshovmail@gmail.com').

%% API
-export([start_link/1]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%% API functions
start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),
    Pop3ServerStart = config:get_key(pop3_server_start, Config),
    SmtpServerStart = config:get_key(smtp_server_start, Config),
    
    case UserStorage of
	mnesia ->
	    mnesia:create_schema([node()]),
	    mnesia:start(),
	    mnesia:create_table(users, []);
	dets ->
	    dets;
	ets ->
	    ets;
	sqlite3 ->
	    sqlite3:open(user_db, [in_memory]),
	    TableInfo = [{user, text, [not_null]}, {password, text, [not_null]}, {domain, text, [not_null]}],
	    ok = sqlite3:create_table(user_db, users, TableInfo)
    end,
    
    RestartStrategy = {one_for_one, 5, 600},
 
    ListenerSup = {popd_listener_sup,
		  {popd_listener_sup, start_link, []},
		  permanent, 2000, supervisor, [popd_listener_sup]},
    
    SmtpListenerSup = {smtpd_listener_sup,
		      {smtpd_listener_sup, start_link, []},
		      permanent, 2000, supervisor, [smtpd_listener_sup]},
    if
	(Pop3ServerStart == start) and (SmtpServerStart == start) ->
	    Children = [ListenerSup, SmtpListenerSup];
	(Pop3ServerStart == start) and (SmtpServerStart /= start) ->
	    Children = [ListenerSup];
	(Pop3ServerStart /= start) and (SmtpServerStart == start) ->
	    Children = [SmtpServerStart]
    end,
	
    {ok, {RestartStrategy, Children}}.

    
stop() ->
    exit(whereis(?MODULE), shutdown).
