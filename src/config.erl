%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(config).

-export([read_config/1]).
-export([get_smtp_server_name/1, get_pop3_server_name/1]).
-export([get_smtp_port/1, get_pop3_port/1]).
-export([get_starting_smtp/1, get_starting_pop3/1]).
-export([get_log_path/1]).
-export([get_domain_dir_path/1]).
-export([get_domain_from_dir_path/1]).

-author('kuleshovmail@gmail.com').
-vsn('0.1').

%
% Read .config file
%
read_config( [] ) ->
    error;
read_config( ConfigPath ) ->
    Config = file:read_file(ConfigPath),
    case Config of
	{ok, ConfigText} ->
	    List = binary_to_list(ConfigText),
	    split_config_text(List);
        _ ->
	    error
    end.

%
% split text in configuration file by /n
%
split_config_text([]) ->
    [];
split_config_text(List) ->
    string:tokens(List, "\n").

%
% Function return smtp server name from configuration file
%
get_smtp_server_name( [] ) ->
    [];
get_smtp_server_name(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token = string:tokens(lists:nth(1,ConfigText), " : "),
    [_ | T ] = Token,
    list_to_atom(lists:nth(1,T)).

%
%  Function return pop3 server name from configuration file
%
get_pop3_server_name([]) ->
    [];
get_pop3_server_name(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token = string:tokens(lists:nth(2,ConfigText), " : "),
    [_ | T ] = Token,
    list_to_atom(lists:nth(1,T)).

%
% Check port from configuration file for smtp
%    
get_smtp_port([]) ->
    [];
get_smtp_port(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token = string:tokens(lists:nth(3,ConfigText), " : "),
    [_ | T ] = Token,
    try
	list_to_integer(lists:nth(1,T))
	    catch
		_:_ -> 'Error, smtp port must be integer!'
    end.

%
% Check port from configuration file for pop3
%
get_pop3_port([]) ->
    [];
get_pop3_port(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token = string:tokens(lists:nth(4,ConfigText), " : "),
    [_ | T ] = Token,
    try
	list_to_integer(lists:nth(1,T))
	    catch
		_:_ -> 'Error, pop3 port must be integer!'
    end.

%
% Function for getting true/false,
% start smtp server at the begin or not
%
get_starting_smtp([]) ->
    [];
get_starting_smtp(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token = string:tokens(lists:nth(5,ConfigText), " : "),
    [_ | T ] = Token,
    list_to_atom(lists:nth(1,T)).

%
% Function for getting true/false,
% start pop3 server at the begin or not
%
get_starting_pop3([]) ->
    [];
get_starting_pop3(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token = string:tokens(lists:nth(6,ConfigText), " : "),
    [_ | T ] = Token,
    list_to_atom(lists:nth(1,T)).

%
% Function get log file path
%
get_log_path([]) ->
    [];
get_log_path(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token =  string:tokens(lists:nth(7,ConfigText), " : "),
    [_ | T ] = Token,
    list_to_atom(lists:nth(1,T)).

get_domain_dir_path([]) ->
    [];
get_domain_dir_path(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    Token = string:tokens(lists:nth(8, ConfigText), " : "),
    [_ | T] = Token,
    string:tokens(lists:nth(1,T), ",").

get_domain_from_dir_path([]) ->
    [];
get_domain_from_dir_path(ConfigPath) ->
    Slash = utils:get_os1(),
    Path = get_domain_dir_path(ConfigPath),
    Token = string:tokens(Path, Slash),
    lists:last(Token).
