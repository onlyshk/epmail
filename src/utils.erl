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
-export([get_head/1]).
-export([get_tail/1]).
-export([files_count/1]).
-export([octets_summ/1]).
-export([octets_count/1]).
-export([get_list_octets/1]).
-export([trim_whitespace/1]).
-export([delete_messages/2]).
-export([get_random_string/2]).
-export([get_file_name_by_num/2]).
-export([split_mail_address/1]).
-export([get_octet_from_file/2]).
-export([copy_files_for_rset/2]).
-export([get_file_path_by_num/2]).

-export([get_mx/1]).
-export([parse_to/1]).

-include_lib("kernel/src/inet_dns.hrl").

-vsn('0.2').
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
% Get file name from dir by num
%
get_file_name_by_num(Dir, Num) ->
    {ok, MessageList} = file:list_dir(Dir),
    lists:nth(Num, MessageList).

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
    {ok, List} = file:list_dir(Dir ++ "/new/"),
    io:format(Dir),
    {ok, ID1} = file:open(Dir ++ "/new/" ++ lists:nth(Num, List), [read]),
    {ok, ID2} = file:open(Dir ++ "/tmp/" ++ lists:nth(Num, List), [append]),
    
    file:copy(ID1, ID2),

    ok = file:close(ID1),
    ok = file:close(ID2),
	      
    file:delete(Dir ++ "/new/" ++ lists:nth(Num, List)).

%
% Copy files when RSET command received
%
copy_files_for_rset(Dir, NewDir) ->
    {ok, List} = file:list_dir(Dir),
   
    lists:foreach(fun(X) ->
			  {ok, ID1} = file:open(Dir ++ "/" ++ X, [read]),
			  {ok, ID2} = file:open(NewDir ++ "/" ++ X, [write]),
			  file:copy(ID1, ID2),
			  ok = file:close(ID1),
			  ok = file:close(ID2),
			  file:delete(Dir ++ "/" ++ X)
		  end, List).
    
	
%
% -| Get head of list
%
get_head([]) ->
    [];
get_head([H | _]) ->
    H.

%
% -| Get tail of list
%
get_tail([]) ->
    [];
get_tail([_ | T]) ->
    T.

%
% Split mail addres by @
%
split_mail_address([]) ->
    [];
split_mail_address(MailAddress) ->
    Add1 = string:strip(MailAddress, both, $<),
    Add2 = string:strip(Add1, both, $>),
    [_ | T] = string:tokens(Add2, "@"),
    T.

parse_to(Data) ->
    List = string:tokens(Data, "\r\n"),
    Sep1 = lists:map(fun(H) ->string:tokens(H, ":") end, List),
    Sep2 = lists:filter(fun ([K | _]) -> K =:= "To" end, Sep1),
    ListAddress = lists:append(Sep2),
    [_ | Tail] = ListAddress,
    lists:map(fun(Address) -> string:tokens(Address, ",") end, Tail).

%
% get mx record
%
get_mx(Domain) ->
    {ok, {hostent, Domain, _, _, _Len, List}} = inet_res:getbyname(Domain, mx),
    List.

%
% Generate random string
%
get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).
