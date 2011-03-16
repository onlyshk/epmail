%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(config).

-export([start/0, get_option/1]).

-vsn('0.3').
-author('kuleshovmail@gmail.com').

-include_lib("epmail.hrl").

-define(EPMAIL_CONFIG, "./epmail.conf").

-record(config, {key, value}).

start() ->
    mnesia:create_table(config, [{disc_copies, [node()]},
                                 {record_name, config},
                                 {local_content, true},
                                 {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    Config = whereis_config(),
    load_file(Config).

whereis_config() ->
    case os:getenv("EPMAIL_CONFIG") of
        false ->
            case application:get_env(config) of
                {ok, Path} -> Path;
                undefined -> ?EPMAIL_CONFIG
            end;
        Path -> Path
    end.

load_file(File) ->
    ?INFO_MSG("Reading configuration file ~s~n", [File]),
    case file:consult(File) of
        {ok, Terms} ->
            F = fun({Key, Value}) ->
                    Record = #config{key = Key, value = Value},
                    mnesia:dirty_write(Record)
            end,
            lists:foreach(F, Terms);
        {error, Reason} ->
            Msg = file:format_error(Reason),
            ?ERROR_MSG("Can't load config file ~s: ~s~n", [File, Msg]),
            exit(File ++ ": " ++ Msg)
    end.

get_option(Opt) ->
    case ets:lookup(config, Opt) of
        [#config{value = Val}] -> Val;
        _ -> undefined
    end.
