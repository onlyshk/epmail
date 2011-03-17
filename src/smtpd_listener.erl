%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 7 Feb 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(smtpd_listener).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-vsn('0.3').
-author('kuleshovmail@gmail.com').

-include_lib("epmail.hrl").

-record(state, {listener}).

start_link()  ->    
    UserStorage = config:get_option(user_storage),

    case UserStorage of
        dets ->
            maildir:create_key_value_user_pass_db("User");
        ets ->
            maildir:create_key_value_user_pass_db("User");
        _ ->
            mnesia
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept(Socket) ->
    SmtpServerName = config:get_option(smtp_server_name),
   
    case gen_tcp:accept(Socket) of
        {ok, Sock} ->
            {ok, Pid} = smtp_fsm_sup:start_child(Sock, []),
            smtp_fsm:set_socket(Pid),
            gen_tcp:send(Sock, integer_to_list(220) ++ " " ++ SmtpServerName ++ " " ++ "Simple Mail Transfer Service Ready\r\n"),
            accept(Socket);
        {error, Reason} ->
            Reason
    end.    
    
stop() ->
    ?INFO_MSG("Stopping SMTP server~n", []),
    gen_server:cast(?MODULE, stop).

% 
% Callback functions
%
init([]) ->
    process_flag(trap_exit, true),

    IP = {0, 0, 0, 0}, % TODO: should be defined in configuration file
    Port = config:get_option(smtp_port),
    
    Opts = [list, {reuseaddr, true}, {packet, 0},
            {keepalive, false}, {ip, IP}, {active, false}],
    StrIP = inet_parse:ntoa(IP),

    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            ?INFO_MSG("Starting SMTP server on ~s:~p~n", [StrIP, Port]),
            spawn(fun() -> accept(ListenSocket) end),
            {ok, #state{listener = ListenSocket}};
        {error, Reason} ->
            ?ERROR_MSG("Cannot open tcp socket due to: ~p~n", [Reason]),
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
