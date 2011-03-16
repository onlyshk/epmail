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

-vsn('0.3').
-author('kuleshovmail@gmail.com').

-include_lib("pop.hrl").

-define(SERVER, ?MODULE).

start_link()  ->
    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),

    case UserStorage of
	dets ->
	    maildir:create_key_value_user_pass_db("User");
	ets ->
	    maildir:create_key_value_user_pass_db("User");
	_ ->
	    mnesia
    end,

    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

accept(Socket) ->
    case gen_tcp:accept(Socket) of
	{ok, Sock} ->
	     {ok, Pid} = pop_fsm_sup:start_child(Sock, [], []),
	     popd_fsm:set_socket(Pid),
            gen_tcp:send(Sock, "+OK POP3 server ready \r\n"),
  	     accept(Socket);
	{error, Reason} ->
	    Reason
    end.    
    
stop() ->
    io:format("Pop3 server stop! \n "),
    gen_server:cast(?MODULE, stop).

% 
% Callback functions
%
init([]) ->
    process_flag(trap_exit, true),
    {ok, Config} = config:read(config),
 
    Port = config:get_key(pop3_port, Config),
    
    Opts = [list, {reuseaddr, true}, 
            {keepalive, false}, {ip,{0,0,0,0}}, {active, false}],
        
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            spawn(fun() -> accept(ListenSocket) end),
            {ok, #state{listener = ListenSocket}};
        {error, Reason} ->
            {stop, Reason}
    end.

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
terminate(_Reason, State) ->
    maildir:destroy(),
    gen_tcp:close(State#state.listener),
    ok.

%
% Hot code change
%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%
