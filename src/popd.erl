%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(popd).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Socket], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Socket]) ->
    spawn(?MODULE, data, [Socket]),
    {ok, null}.

data(Socket) ->
    io:format("ASDASDASD", []),
    gen_server:cast(?SERVER, {create_socket, Socket}).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({create_socket, Socket}, State) ->
    gen_tcp:send(Socket, 'h'),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



