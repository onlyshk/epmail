%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(logger).

-vsn('0.3').
-author('kuleshovmail@gmail.com').

-behavior(gen_server).

-export([init/1]).
-export([start_link/0, stop/0]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([add_log/1, db_backup/0]).
-export([terminate/2]).

%
% Client API
%
start_link() ->
    {ok, Config} = config:get(logger,config),
    LoggerPath = Config,
    case LoggerPath of
	 '_' ->
	    stop;
          _  ->
	    gen_server:start_link({local, ?MODULE}, ?MODULE, LoggerPath, [])
    end.

stop() ->
    io:format("Logger server stop ~n "),
    gen_server:cast(?MODULE, stop).
%

%
% Log db api
%
create_log(LoggerPath) ->
    case dets:open_file(loggerDisk,[{file,LoggerPath}]) of
	{ok, Name} ->
	    Name;
	{error, Reason} ->
	    Reason
    end.

db_backup() ->
    Fun = fun( Object, Acc ) -> [Object | Acc] end,
    List = dets:foldl( Fun, [],  loggerDisk),

    {ok, F} = file:open("log.txt", [append]),
    file:write(F, [io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B ~s~n",
    			    [YYYY,M,D, HH,MM,SS, Comment]) || {{HH,MM,SS},{YYYY,M,D},Comment} <- List ]),
    file:close(F).
    
destroy_log() ->
    dets:close(loggerDisk).    
%

%
% Log w/r API
%
add_log(Text) ->
    Time = time(),
    Date = date(),
    gen_server:call(?MODULE, {add_log, {Time, Date, Text}}).
%

%
% Callback functions
%
init(LoggerPath) ->
    io:format("Logger server start ~n "),
    process_flag(trap_exit, true),
    create_log(LoggerPath),

    {ok, F} = file:open("log.txt", [append]),
    file:write(F, " Time        Date   Error \n"),
    file:close(F),

    {ok, null}.

terminate(_Reason, _LoopData) ->
    destroy_log().

handle_call({add_log, {Time, Date, Text} }, _From, LoopData) ->
    Reply = dets:insert(loggerDisk, {Time, Date, Text}),
    {reply, Reply, LoopData}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

% no useble func
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

% no useble func
handle_info(_Msg, LoopData) ->
    {noreply, LoopData}.
%
