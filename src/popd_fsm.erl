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

-vsn('0.3').
-author('kuleshovmail@gmail.com').

%% API
-export([stop/0]).
-export([set_socket/1]).
-export([start_link/3]).
-export([loop_for_list/2]).

%% gen_fsm callbacks
-export([init/1, autorization/2, transaction/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {socket,
	        username,
	        password}).

%%% API
start_link(Socket, UserName, Password) ->
    gen_fsm:start_link(?MODULE, [Socket, UserName, Password], []).

stop() ->
   gen_fsm:send_all_state_event(?SERVER, stop).

set_socket(Pid) ->
    gen_fsm:send_event(Pid, {autorization}).

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
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
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
					case maildir:check_pass(lists:concat(State#state.username), lists:concat(Pass)) of
					    ok ->
						gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
						transaction(Event, State);
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
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)	     
	    end,
	          
	    try
		case ReParseData of
		    "quit" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ " POP3 server signing off\r\n"),
			gen_tcp:close(State#state.socket);
		    "noop" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
			autorization(Event, State);
		    _ ->
			gen_tcp:send(State#state.socket, pop_messages:err_message()),
			autorization(Event, State)
		end
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
	    end;

	{error, closed} ->
	    ok
    end,
    
    {next_state, transaction, State}.

loop_for_list(State, FileCount)  ->
    Domain = maildir:find_domain(lists:concat(State#state.username)),
    OctetList = utils:get_list_octets(Domain ++ "/" ++ State#state.username ++ "/new"),
    lists:zipwith(fun (X1, X2) -> gen_tcp:send(State#state.socket, [integer_to_list(X1), " " ++ integer_to_list(X2)]
					       ++ "\r\n") end, lists:seq(1, FileCount), OctetList).

loop_for_uidl(State, FileCount)  ->
    Domain = maildir:find_domain(lists:concat(State#state.username)),
    {ok, UIDLList} = file:list_dir(Domain ++ "/" ++ State#state.username ++ "/new/"),
    lists:zipwith(fun (X1, X2) -> gen_tcp:send(State#state.socket, [integer_to_list(X1), " " ++ X2]
    					       ++ "\r\n") end, lists:seq(1, FileCount), UIDLList).

transaction(Event, State) ->    
    case gen_tcp:recv(State#state.socket, 0) of
	{ok, Data} ->
	    ReParseData = string:to_lower(utils:trim(Data)),
	    Domain = maildir:find_domain(lists:concat(State#state.username)),

	    try
	    	case pop_messages:is_message_uidl(ReParseData) of
		     { _ , [U | _] } ->
			FilesCount = utils:files_count(Domain ++ State#state.username ++ "/new"),
			MessageNumUIDL = list_to_integer(U),
			
			if
			    ((MessageNumUIDL == 0) or (MessageNumUIDL > FilesCount))->
				gen_tcp:send(State#state.socket, "-ERR \r\n"),
				transaction(Event, State); 
			    true ->
				UIDLList = utils:get_file_name_by_num(Domain ++ "/" ++ State#state.username ++ "/new/", list_to_integer(U)),
				gen_tcp:send(State#state.socket, "+OK " ++ U ++ " " ++ UIDLList ++ "\r\n"),
				gen_tcp:send(State#state.socket, ".\r\n"),
				transaction(Event, State)
			end;
		    {_} ->
			FilesCount = utils:files_count(Domain ++ "/" ++ State#state.username ++ "/new"),

			gen_tcp:send(State#state.socket, "+OK\r\n"),

			loop_for_uidl(State, FilesCount),
			gen_tcp:send(State#state.socket, ".\r\n"),
	    		transaction(Event, State);
			
			error ->
				error
	    	    end
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
	    end,
	    
	    try
		case pop_messages:is_message_list(ReParseData) of
		    { _ , [H | _] } ->
			FileCount = utils:files_count(Domain ++ State#state.username ++ "/new"),
			MessageNum = list_to_integer(H),
			
			if
			    ((MessageNum == 0) or (MessageNum > FileCount))->
				gen_tcp:send(State#state.socket, "-ERR \r\n"),
				transaction(Event, State); 
			    true ->
				Octets = utils:get_octet_from_file(Domain ++ "/" ++ State#state.username ++ "/new", list_to_integer(H)),
				gen_tcp:send(State#state.socket, "+OK " ++ H ++ " " ++ integer_to_list(Octets) ++ "\r\n"),
				gen_tcp:send(State#state.socket, ".\r\n"),
				transaction(Event, State)
			    end;
		    {_} ->
			%% +OK 2 messages (320 octets)
			%% 1 120
			%% 2 200
			%% .
			OctetSumm = utils:octets_summ(Domain ++ "/" ++ State#state.username ++ "/new"),
			FileCount = utils:files_count(Domain ++ "/" ++ State#state.username ++ "/new"),
			gen_tcp:send(State#state.socket, "+OK " ++ integer_to_list(FileCount) ++ " message (" ++ integer_to_list(OctetSumm)
					     ++ " octets)\r\n"),
			loop_for_list(State, FileCount),
			gen_tcp:send(State#state.socket, ".\r\n"),
			transaction(Event, State);
		    error ->
			error
		end
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
	    end,   

            %% TOP command
	    %% TOP msg num
	    %% +OK Top of message follows
	    %% .
	    try
		case pop_messages:is_message_top(ReParseData) of
		    {_, [MS, NUM]} ->
			Mail = utils:get_file_path_by_num(Domain ++ "/" ++ State#state.username ++ "/new",
			      							     list_to_integer(MS)),
			Lines = utils:read_n_lines(Mail, list_to_integer(NUM)),
			gen_tcp:send(State#state.socket, "+OK Top of message follows \r\n"),
			gen_tcp:send(State#state.socket, Lines ++ "\r\n"),
			gen_tcp:send(State#state.socket, "." ++ "\r\n"),
		        transaction(Event, State);
		    error ->
			error
		end
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
	    end,
	    
	    %% RETR command
	    %% +OK || -ERR n message (m octets)
	    %% < body message>
	    %% .
	    try
		case pop_messages:is_message_retr(ReParseData) of
		    {_, [Retr | _ ]} ->			      
			Message = utils:get_file_path_by_num(Domain ++ "/" ++ State#state.username ++ "/new",
			      							     list_to_integer(Retr)),
			{ok, Text} = file:read_file(Message),
			FileSize = filelib:file_size(Message),

			gen_tcp:send(State#state.socket, "+OK" ++ " "),
			gen_tcp:send(State#state.socket, integer_to_list(FileSize) ++ "octets\r\n"),
			gen_tcp:send(State#state.socket, binary_to_list(Text) ++ "\r\n"),
			gen_tcp:send(State#state.socket, "." ++ "\r\n"),
			transaction(Event, State); 
		    error ->
			error
		end
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
	    end,

	    try
		case pop_messages:is_message_dele(ReParseData) of
		    {_, [Dele| _]} ->
			gen_tcp:send(State#state.socket, "+OK message" ++ " "),
			gen_tcp:send(State#state.socket, Dele ++ " "),
			gen_tcp:send(State#state.socket, "deleted \r\n"),
			utils:delete_messages(Domain ++ "/" ++ State#state.username, list_to_integer(Dele)),
			transaction(Event, State);
		    error ->
			error
		end
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
	    end,
	    
	    try
		case ReParseData of
		    "quit" ->
		      gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ " POP3 server signing off\r\n"),
		      gen_tcp:close(State#state.socket);
		    "noop" ->
		      gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
		      transaction(Event, State);
		    "stat" ->
		       DomainForSTAT = maildir:find_domain(lists:concat(State#state.username)),
		       MessageCount = utils:files_count(DomainForSTAT ++ "/" ++ State#state.username ++ "/new"),
		       Octet = utils:octets_summ(DomainForSTAT ++ "/" ++ State#state.username ++ "/new"),
		       gen_tcp:send(State#state.socket, "+OK "),
		       gen_tcp:send(State#state.socket, integer_to_list(MessageCount) ++ " "),
		       gen_tcp:send(State#state.socket, integer_to_list(Octet) ++ "\r\n"),
		       transaction(Event, State);
		    "rset" ->
			DomainForRSET = maildir:find_domain(lists:concat(State#state.username)),
			MessCount = utils:files_count(DomainForRSET ++ "/" ++ State#state.username ++ "/tmp"),
			Octet = utils:octets_summ(DomainForRSET ++ "/" ++ State#state.username ++ "/tmp"),
			gen_tcp:send(State#state.socket, "+OK maildrop has " ++ integer_to_list(MessCount) ++ " messages"),
			gen_tcp:send(State#state.socket, " (" ++ integer_to_list(Octet) ++ " octets)" ++ "\r\n"),
			utils:copy_files_for_rset(DomainForRSET ++ "/" ++ State#state.username ++ "/tmp" ++ "/",
						  DomainForRSET ++ "/" ++ State#state.username ++ "/new"),
			transaction(Event, State);
		    _ ->
			gen_tcp:send(State#state.socket, pop_messages:err_message()),
			transaction(Event, State )
		end
	    catch _:_ ->
		    gen_tcp:close(State#state.socket),
		    supervisor:delete_child(pop_fsm_sup, popd_fsm)
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
