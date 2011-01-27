%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 16 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(utils).

-export([trim/1]).
-export([files_count/1]).
-export([octets_summ/1]).
-export([octets_count/1]).
-export([get_list_octets/1]).
-export([trim_whitespace/1]).
-export([delete_messages/2]).
-export([get_octet_from_file/2]).
-export([get_file_path_by_num/2]).


%
% Trim string
%
trim_whitespace(Input) ->
       re:replace(Input, "\\s+", "", [global]).

trim(String)  ->
    String2 = lists:dropwhile(fun is_whitespace/1, String),
    lists:reverse(lists:dropwhile(fun is_whitespace/1, lists:reverse(String2))).

is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_Else) -> false.

%
% Get files count in directory
%
files_count(Dir) ->
    case file:list_dir(Dir) of  
         {ok, FileNames} ->
            length(FileNames);
        {error, Reason} ->
            Reason
    end.

%
% Count of chars in file
%
octets_count(Mail) ->
    case file:read_file(Mail) of
	{ok, File} ->
	    CharCount = lists:flatten(string:tokens(binary_to_list(File), "\n")),
	    length(CharCount);
	{error, Reason} ->
	    Reason
    end.

%
% Get sum of files size in direcotory
%
octets_summ(Dir) ->
    {ok, List} = file:list_dir(Dir),
    lists:sum([filelib:file_size(Dir ++ "/" ++ X) || X <- List]).

%
% Get list of files size in directory
%
get_list_octets(Dir) ->
    {ok, List} = file:list_dir(Dir),
    [filelib:file_size(Dir ++ "/" ++ X) || X <- List].

%
% Get file size from file
%
get_octet_from_file(Dir, Mail) ->
    MessageList = get_list_octets(Dir),
    lists:nth(Mail, MessageList).

%
% Get file path by num from directory
%
get_file_path_by_num(Dir, Num) ->
    {ok, List} = file:list_dir(Dir),
    Dir ++ "/" ++ lists:nth(Num, List).

%
% Move to tmp directory
%
delete_messages(Dir, Num) ->
    {ok, List} = file:list_dir(Dir),
    file:delete(Dir ++ "/" ++ lists:nth(Num, List)).
