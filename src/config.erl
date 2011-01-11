-module(config).

-export([get_smtp_server_name/1, get_pop3_server_name/1]).
-export([get_smtp_port/1, get_pop3_port/1]).
-export([get_starting_smtp/1, get_starting_pop3/1]).

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
    ServerName = string:tokens(lists:nth(1,ConfigText), " : "),
    [_ | T ] = ServerName,
    list_to_atom(lists:nth(1,T)).

%
%  Function return pop3 server name from configuration file
%
get_pop3_server_name([]) ->
    [];
get_pop3_server_name(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    ServerName = string:tokens(lists:nth(2,ConfigText), " : "),
    [_ | T ] = ServerName,
    list_to_atom(lists:nth(1,T)).

%
% Check port from configuration file for smtp
%    
get_smtp_port([]) ->
    [];
get_smtp_port(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    ServerName = string:tokens(lists:nth(3,ConfigText), " : "),
    [_ | T ] = ServerName,
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
    ServerName = string:tokens(lists:nth(4,ConfigText), " : "),
    [_ | T ] = ServerName,
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
    ServerName = string:tokens(lists:nth(5,ConfigText), " : "),
    [_ | T ] = ServerName,
    list_to_atom(lists:nth(1,T)).

%
% Function for getting true/false,
% start pop3 server at the begin or not
%
get_starting_pop3([]) ->
    [];
get_starting_pop3(ConfigPath) ->
    ConfigText = read_config(ConfigPath),
    ServerName = string:tokens(lists:nth(6,ConfigText), " : "),
    [_ | T ] = ServerName,
    list_to_atom(lists:nth(1,T)).


