%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(popd_listener).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([accept/1]).

-define(SERVER, ?MODULE). 

start_link()  ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    io:format("Pop3 server stop! \n "),
    gen_server:cast(?MODULE, stop).

accept(Socket) ->
    case gen_tcp:accept(Socket) of
	{ok, S} ->
	    gen_tcp:send(S, "Connection from POP3 server OK+ \n"),
        {error, Reason} ->
	    Reason
        end.
% 
% Callback functions
%
init([]) ->
    Port = 110,
    Opts = [binary, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    
    case gen_tcp:listen(Port, Opts) of
	 {ok, ListenSocket} ->
	    spawn(?MODULE, accept, [ListenSocket]);
	 {error, Reason} ->
	    Reason
    end,
    {ok, null}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.
%

%
% terminate server
%
terminate(_Reason, _State) ->
    ok.

%
% Hot code change
%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%

%
% Reply
%
reply_line(ok, "") ->
    "+OK\r\n";

reply_line(ok, Text) ->
    ["+OK ", Text, "\r\n"];

reply_line(err, "") ->
    "-ERR\r\n";

reply_line(err, Text) ->
    ["-ERR ", Text, "\r\n"].
%

