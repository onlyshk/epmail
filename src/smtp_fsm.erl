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
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, autorization/2, mail_transaction/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {socket,
		client,
		rcpts = []
	        }).

%%% API
start_link(Socket, Client) ->
    gen_fsm:start_link(?MODULE, [Socket, Client], []).

stop() ->
   gen_fsm:send_all_state_event(?SERVER, stop).

set_socket(Pid) ->
    gen_fsm:send_event(Pid, {autorization}).

%%% gen_fsm callbacks
init([Socket, Client]) ->
    {ok, autorization, #state{socket = Socket, client = Client}}.

autorization(Event, State) ->
    case gen_tcp:recv(State#state.socket, 0) of
	{ok, Data} ->
	    ReParseData = string:to_lower(utils:trim(Data)),

	    {ok, Config} = config:read(config),
	    SmtpServerName = config:get_key(smtp_server_name, Config),

	    %% HELO command
	    try
		case smtp_messages:is_helo_message(ReParseData) of 
		    { _ , Helo } ->
			if
			    (length(Helo) == 1) ->
				gen_tcp:send(State#state.socket, integer_to_list(250) ++ " " ++ SmtpServerName ++  "\r\n"),
				mail_transaction(Event, State);
			    true ->
				autorization(Event, State)
			end;  
		    error ->
			error  
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end,

	    %% EHLO command
	    try
		case smtp_messages:is_ehlo_message(ReParseData) of 
		    { _ , Ehlo } ->
			if
			    (length(Ehlo) == 1) ->
				gen_tcp:send(State#state.socket, "250-EPmail smtp server is pleased to meet you" ++  "\r\n"),
				gen_tcp:send(State#state.socket, "250-HELP" ++ "\r\n"),
				gen_tcp:send(State#state.socket, "250 EHLO" ++ "\r\n"),
				mail_transaction(Event, State);
			    true ->
				autorization(Event, State)
			end;  
		    error ->
			error  
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end,
	    
	    try
		case ReParseData of
		    "quit" ->
			gen_tcp:send(State#state.socket, integer_to_list(221) ++ " " ++ "2.0.0" ++ " Bye" ++ "\r\n"),
			gen_tcp:close(State#state.socket);
		    "noop" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
			autorization(Event, State);
		    "rset" ->
			gen_tcp:send(State#state.socket, "250 OK \r\n"),
			autorization(Event, State#state{client = underfined, rcpts = []});
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

mail_transaction(Event, State) ->
    case gen_tcp:recv(State#state.socket, 0) of
	{ok, Data} ->
	    ReParseData = string:to_lower(utils:trim(Data)),

	    %% MAIL FROM command
	    try
		case smtp_messages:is_mail_message(ReParseData) of 
		    { _ , Mail } ->
			if
			    (length(Mail) > 0) ->
				gen_tcp:send(State#state.socket, "250 OK \r\n"),
				mail_transaction(Event, State#state{client = Mail});
			    true ->
				mail_transaction(Event, State)
			end;  
		    error ->
			error  
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end,

	    %% RCPT TO: command
	    try
		case smtp_messages:is_rcpt_message(ReParseData) of 
		    { _ , Rcpt } ->
			if
			    (length(Rcpt) > 0) ->
				gen_tcp:send(State#state.socket, "250 OK \r\n"),
				mail_transaction(Event, State#state{rcpts = [ Rcpt | rcpts]});
			    true ->
				mail_transaction(Event, State)
			end;  
		    error ->
			error  
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end,

	    try
		case ReParseData of
		    "quit" ->
			gen_tcp:send(State#state.socket, integer_to_list(221) ++ " " ++ "2.0.0" ++ " Bye" ++ "\r\n"),
			gen_tcp:close(State#state.socket);
		    "noop" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
			mail_transaction(Event, State);
		    "rset" ->
			gen_tcp:send(State#state.socket, "250 OK \r\n"),
			autorization(Event, State#state{client = underfined, rcpts = []});
		    _ ->
			gen_tcp:send(State#state.socket, pop_messages:err_message()),
			autorization(Event, State )
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end;
	    
	{error, closed} ->
	    ok
    end,
    {next_state, mail_transaction, State}.

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
