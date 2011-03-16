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

-vsn('0.3').
-author('kuleshovmail@gmail.com').

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    UserStorage = config:get_option(user_storage),
    Pop3ServerStart = config:get_option(pop3_server_start),
    SmtpServerStart = config:get_option(smtp_server_start),
    Sqlite3DataBase = config:get_option(sqlite3_database),
    
    case UserStorage of
        mnesia ->
            mnesia:create_table(users, []);
        dets ->
            dets;
        ets ->
            ets;
        sqlite3 ->
            sqlite3:open(Sqlite3DataBase),
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
	    Children = [SmtpListenerSup]
    end,
	
    {ok, {RestartStrategy, Children}}.

