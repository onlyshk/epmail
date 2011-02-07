%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 7 Feb 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(smtp_fsm).

-behaviour(gen_fsm).

-vsn('0.1').
-author('kuleshovmail@gmail.com').

%% API
-export([stop/0]).
-export([set_socket/1]).
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, autorization/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {socket
	        }).

%%% API
start_link(Socket) ->
    gen_fsm:start_link(?MODULE, [Socket], []).

stop() ->
   gen_fsm:send_all_state_event(?SERVER, stop).

set_socket(Pid) ->
    gen_fsm:send_event(Pid, {autorization}).

%%% gen_fsm callbacks
init([Socket]) ->
    {ok, autorization, #state{socket = Socket}}.

autorization(Event, State) ->
    case gen_tcp:recv(State#state.socket, 0) of
	{ok, Data} ->
	    ReParseData = string:to_lower(utils:trim(Data)),

	    try
		case ReParseData of
		    "quit" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ " SMTP server signing off\r\n"),
			gen_tcp:close(State#state.socket);
		    "noop" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
			autorization(Event, State);
		    _ ->
			gen_tcp:send(State#state.socket, pop_messages:err_message()),
			autorization(Event, State )
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end;

	{error, closed} ->
	    ok
    end,
    
    {next_state, autorization, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

handle_event(stop, _StateName, StateData) ->
   {stop, normal, StateData};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%% Internal functions
