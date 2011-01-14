-module(logger).

-author('kuleshovmail@gmail.com').
-vsn('0.1').

-behavior(gen_server).

-export([init/1]).
-export([start_link/0, stop/0]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([add_log/1]).
-export([terminate/2]).

%
% Client API
%
start_link() ->
    LoggerPath = config:get_log_path(config),
    gen_server:start_link({local, ?MODULE}, ?MODULE, LoggerPath, []).

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

destroy_log() ->
    dets:close(loggerDisk).    
%

%
% Log w/r API
%
add_log(Text) ->
    Time = time(),
    Date = date(),
    Space = " ",
    gen_server:call(?MODULE, {add_log, {Time, Date, Space, Text}}).
%

%
% Callback functions
%
init(LoggerPath) ->
    io:format("Logger server start ~n "),
    create_log(LoggerPath),
    {ok, null}.

terminate(_Reason, _LoopData) ->
    destroy_log().

handle_call({add_log, {Time, Date, Space, Text} }, _From, LoopData) ->
    Reply = dets:insert(loggerDisk, {Time, Date, Space, Text}),
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
