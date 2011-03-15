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

-vsn('0.2').
-author('kuleshovmail@gmail.com').

-include_lib("smtp.hrl").

%% API
-export([stop/0]).
-export([set_socket/1]).
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, autorization/2, mail_transaction/2, recv_rcpt_transaction/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

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
				mail_transaction(Event, State#state{rcpt = []});
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
				gen_tcp:send(State#state.socket, "250-VRFY" ++ "\r\n"),
				gen_tcp:send(State#state.socket, "250-HELP" ++ "\r\n"),
				gen_tcp:send(State#state.socket, "250 EHLO" ++ "\r\n"),
				mail_transaction(Event, State#state{rcpt = []});
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
			autorization(Event, State#state{client = underfined});
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

	    try
		case smtp_messages:is_vrfy_message(ReParseData) of
		    {_, [VRFY]} ->
			case dets:lookup(upDisk, VRFY) of
			    [{Name,DomainName,_}] ->
				gen_tcp:send(State#state.socket, "250 " ++ Name ++ "@" ++ DomainName ++ "\r\n"),
				mail_transaction(Event, State);
			    [] ->
				mail_transaction(Event, State)
			end;    
		    error ->
			error
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end,

	    %% MAIL FROM command
	    try
		case smtp_messages:is_mail_message(ReParseData) of 
		    { _ , Mail } ->
			if
			    (length(Mail) > 0) ->
				gen_tcp:send(State#state.socket, "250 OK \r\n"),
				recv_rcpt_transaction(Event, State#state{client = utils:split_mail_address(Mail)});
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
			gen_tcp:send(State#state.socket, "221 EPmail Service closing transmission channel" ++ "\r\n"),
			gen_tcp:close(State#state.socket);
		    "noop" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
			mail_transaction(Event, State);
		    "rset" ->
			gen_tcp:send(State#state.socket, "250 OK \r\n"),
			mail_transaction(Event, State#state{client = underfined})
		    end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end;
	{error, closed} ->
	    ok
    end,
    {next_state, mail_transaction, State}.

%
% RCPT and DATA transaction
%
recv_rcpt_transaction(Event, State) ->    
    case gen_tcp:recv(State#state.socket, 0) of
	{ok, Data} ->
	    ReParseData = string:to_lower(utils:trim(Data)),

	    %% RCPT TO: command
	    try
		case smtp_messages:is_rcpt_message(ReParseData) of 
		    { _ , Rcpt } ->
		        ListParse = lists:map(fun(X) -> utils:split_mail_address(X) end, Rcpt),
			gen_tcp:send(State#state.socket, "250 OK \r\n"),
			recv_rcpt_transaction(Event, State#state{rcpt = ListParse});  
		    error ->
			error  
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end,

	    try
		case ReParseData of
		    "quit" ->
			gen_tcp:send(State#state.socket, "221 EPmail Service closing transmission channel" ++ "\r\n"),
			gen_tcp:close(State#state.socket);
		    "noop" ->
			gen_tcp:send(State#state.socket, pop_messages:ok_message() ++ "\r\n"),
			recv_rcpt_transaction(Event, State);
		    "data" ->
			gen_tcp:send(State#state.socket, "354 Enter mail, end with . on a line by itself \r\n"),
			
			{ok, Packet} = do_recv(State#state.socket, []),
		        {ok, Config} = config:read(config),
			Domain = config:get_key(domain, Config),
			Port   = config:get_key(smtp_port, Config),
			SmtpServerName = config:get_key(smtp_server_name, Config),
			SplitAddressList = [string:tokens(S, "@") || S <- State#state.rcpt],  % utils:parse_to(lists:flatten(Packet))],

			LocalList = [X || X <- SplitAddressList, Y <- Domain,   lists:last(X) == Y],
			RemoteList =  [utils:get_head(X) ++ "@" ++ lists:last(X) || X <- SplitAddressList,
										    Y <- Domain, not (string:equal(lists:last(X), Y))],
			%
			% Send mail to local server
			% First of all check domain
			% If domain name in config and domain from LocalList =:=
			% Send mail  to local server
			%
			case LocalList of
			    [] ->
				[];
			    _ ->
				% TODO
				% Need normal random generator
				{H, M, S} = now(),
				Sum = lists:map(fun(_) -> {A, B, C} = erlang:now(),
							   random:seed(A, B, C), random:uniform(1000) end, lists:seq(1,10)),
				
				DoSum = lists:sum(Sum) + H + M + S,

				lists:map(fun(X) ->
						  {ok, WD} = file:open(lists:last(X) ++ "/" ++
									   utils:get_head(X) ++ "/new/" ++ integer_to_list(DoSum),
								       [raw, append]),
						  file:write(WD, Packet),
						  file:close(WD)
					  end,
					  LocalList)
			end,

			case RemoteList of
			    [] ->
				[];
			    _ ->
				MXWithSpace = lists:map(fun(X) -> string:tokens(X, " ") end, RemoteList),
				MXWithS = lists:map(fun(X) -> string:tokens(X, "@") end, lists:nth(1,MXWithSpace)),

				% lists:last(lists:nth(1, MXWithS - Domain
			        % utils:get_head(lists:nth(1, MXWithS - Name

				MX = lists:map(fun(X) ->
						       utils:get_mx(lists:last(X))
					       end,
					       MXWithS),

				MxAddresses = lists:map(fun(X) ->
								{_, Last} = lists:nth(1, X),
								Last
							end,
							MX),

				Opts = [list, {reuseaddr, true}, 
					{keepalive, false}, {ip,{0,0,0,0}}, {active, false}],

				lists:map(fun(MxAd) ->
						  case gen_tcp:connect(MxAd, Port, Opts) of
						      {ok, Socket} ->
							  case gen_tcp:recv(Socket, 0) of
							      {ok, Starting} ->
								  io:format(Starting),
								  gen_tcp:send(Socket, "ehlo " ++ SmtpServerName  ++ "\r\n"),
								  gen_tcp:send(Socket, "starttls" ++ "\r\n"),
								  case gen_tcp:recv(Socket, 0) of
								      {ok, Ehlo} ->
									  io:format(Ehlo),
									  io:format(State#state.client),
									  gen_tcp:send(Socket,
										       "mail from: " ++ State#state.client ++ "\r\n"),
									  case gen_tcp:recv(Socket, 0) of
									      {ok, MailFrom} ->
										  io:format(MailFrom),
										  lists:map(fun(X) ->
												    io:format(string:tokens(X, " ")),
												    gen_tcp:send(Socket,
														 "rcpt to: "  ++
														     X
														 ++ "\r\n")
											    end, MXWithSpace),
										  case gen_tcp:recv(Socket, 0) of
										      {ok, RC} ->
											  io:format(RC),
											  gen_tcp:send(Socket, "data" ++ "\r\n"),
											  case gen_tcp:recv(Socket, 0) of
											      {ok, Ans} ->
												  io:format(Ans),
													      gen_tcp:send(Socket,
													      		   Packet  ++ "\r\n"),
													      gen_tcp:send(Socket, ".\r\n"),
													      	  case gen_tcp:recv(Socket, 0) of
													      	      {ok, Q} ->
													      		  io:format(Q),
													      		  gen_tcp:send("quit\r\n");
													      	      {error, Reason} ->
													      		  io:format(Reason)
													      	  end;
													  {error, Reason} ->
													      Reason
												      end;     
												  {error, Reason} ->
												      io:format(Reason)
											      end;
										      {error, Reason} ->
											  io:format(Reason)
										  end;
									      {error, Reason} ->
										  io:format(Reason)
									  end;
								      {error, Reason} ->
									  io:format(Reason)
								  end;
							      {error, Reason} ->
								  Reason	    
							  end
						  end,
						  MxAddresses)	    
			end;
		    
		     "rset" ->
			gen_tcp:send(State#state.socket, "250 OK \r\n"),
			autorization(Event, State#state{client = underfined});
		    _ ->
			gen_tcp:send(State#state.socket, pop_messages:err_message()),
			mail_transaction(Event, State )
		end
	    catch _:_ -> gen_tcp:close(State#state.socket)
	    end;

	
	{error,closed} ->
	    ok
    end, 
    {next_state, mail_transaction, State}.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
	    S = string:tokens(B, "\r\n"),
	    case lists:last(S) of
		"." ->
		    gen_tcp:send(Sock, "250 OK\r\n"),
		    case gen_tcp:recv(Sock, 0) of
			{ok, Packet} ->
			    case Packet of
				"QUIT\r\n" ->
				    gen_tcp:send(Sock, "221 foo.com Service closing transmission channel\r\n"),
				    {ok, B}
			    end
		    end;

		_ ->
		    other
	    end,
	    do_recv(Sock, [Bs | B]);
        {error, closed} ->
	    {ok, Bs}
    end.

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
