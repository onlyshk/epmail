%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(popd_fsm).

-behaviour(gen_fsm).

%% API
-export([stop/0]).
-export([set_socket/0]).
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1, autorization/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {socket,
	        username,
	        password}).

%%% API
start_link(Socket, UserName, Password) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Socket, UserName, Password], []).

stop() ->
   gen_fsm:send_all_state_event(?SERVER, stop).

set_socket() ->
    gen_fsm:send_event(?SERVER, {autorization}).

%%% gen_fsm callbacks
init([Socket, UserName, Password]) ->
    {ok, autorization, #state{socket = Socket, username = UserName, password = Password}}.

autorization(Event, State) ->
    case gen_tcp:recv(State#state.socket, 0) of
	{ok, Data} ->
	    ReParseData = string:to_lower(utils:trim(Data)),
	  
	  %% User login command
          try
	      case pop_messages:is_message_user(ReParseData) of 
		  { _ , Name } ->
		      if
			  (length(Name) == 1) ->
			      gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
			      autorization(Event, State#state{username = Name});
			  true ->
			      autorization(Event, State)
		      end;  
		  error ->
		      error  
	      end
	  catch _:_ -> gen_tcp:close(State#state.socket)
          end,

	  %% password getting
	  %% At first here we check that password must be input only
	  %% after user name. Then we check that passs command has 1 parameter.
	  %% and at last we check that user pass == user pass in dets db
	  try
	      case pop_messages:is_message_pass(ReParseData) of
		  { _ , Pass } ->
		      if
			  (State#state.username == []) ->
			      gen_tcp:send(State#state.socket, pop_messages:err_message()),
			      autorization(Event, State#state{username = [], password = []});
		          true ->
			      
			      if
				  (length(Pass) == 1) ->
				      case maildir:check_pass(lists:concat(State#state.username), lists:concat(State#state.password)) of
					  ok ->
					      gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
					      autorization(Event, State#state{password = Pass});
					  error ->
					      gen_tcp:send(State#state.socket, pop_messages:err_message() ++ 
                                                                   "Your password wrong, pleasy try user/pass again" ++ "\r\n"),
					      autorization(Event, State#state{username = [], password = []})
				      end;
				  true ->
				      autorization(Event, State#state{username = [], password = []})
			      end
		      end;
		  error ->
		      error
	      end
	  catch _:_ -> gen_tcp:close(State#state.socket)
	  end,
	    try
	  case ReParseData of
	      "quit" ->
		  gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ " POP3 server signing off\r\n"),
		  gen_tcp:close(State#state.socket);
	      _ ->
		 gen_tcp:close(State#state.socket)
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
