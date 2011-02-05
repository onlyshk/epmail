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
-export([get_os/0]).
-export([get_os1/0]).
-export([get_head/1]).
-export([files_count/1]).
-export([octets_summ/1]).
-export([octets_count/1]). 
-export([get_list_octets/1]).
-export([get_os_for_tmp/0]).
-export([trim_whitespace/1]).
-export([delete_messages/2]).
-export([get_octet_from_file/2]).
-export([copy_files_for_rset/2]).
-export([get_file_path_by_num/2]).

-vsn('0.1').
-author('kuleshovmail@gmail.com').

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
    Slash = get_os1(),
    {ok, List} = file:list_dir(Dir),
    lists:sum([filelib:file_size(Dir ++ Slash ++ X) || X <- List]).

%
% Get list of files size in directory
%
get_list_octets(Dir) ->
    Slash = get_os1(),
    {ok, List} = file:list_dir(Dir),
    [filelib:file_size(Dir ++ Slash ++ X) || X <- List].

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
    Slash = get_os1(),
    {ok, List} = file:list_dir(Dir),
    Dir ++ Slash ++ lists:nth(Num, List).

%
% Move to tmp directory
%
delete_messages(Dir, Num) ->
    Slash = get_os1(),
    {ok, List} = file:list_dir(Dir),
  
    {ok, ID1} = file:open(Dir ++ lists:nth(Num, List), [read]),
    {ok, ID2} = file:open(Dir ++ Slash ++ "tmp" ++ Slash , [write]),

    file:copy(ID1, ID2),

    ok = file:close(ID1),
    ok = file:close(ID2),

    file:delete(Dir ++ Slash ++ lists:nth(Num, List)).

%
% Copy files when RSET command received
%
copy_files_for_rset(Dir, NewDir) ->
    Slash = get_os1(),
    {ok, List} = file:list_dir(Dir),
   
    lists:foreach(fun(X) ->
			  {ok, ID1} = file:open(Dir ++ Slash ++ X, [read]),
			  {ok, ID2} = file:open(NewDir ++ Slash ++ X, [write]),
			  io:format(NewDir),
			  file:copy(ID1, ID2),
			  ok = file:close(ID1),
			  ok = file:close(ID2),
			  file:delete(Dir ++ Slash ++ X)
		  end, List).
    

%
% Get operation system version
%
get_os()->
    {Osfamily, _} = os:type(),
    case Osfamily of
	unix ->
	    "/new";
	win32 ->
	    "\new"
    end.

%
% Get os for tmp dir
%
get_os_for_tmp()->
    {Osfamily, _} = os:type(),
    case Osfamily of
	unix ->
	    "/tmp";
	win32 ->
	    "\tmp"
    end.

%
% Slash \ || / depend operating systen
%
get_os1() ->
    {Osfamily, _} = os:type(),
    case Osfamily of
	unix ->
	    "/";
	win32 ->
	    "\\"
    end.
	
%
% -| Get head of list
%
get_head([]) ->
    [];
get_head(List) ->
    lists:nth(1, List).
