%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(maildir).

-export([destroy/0]).
-export([add_user/3]).
-export([delete_user/2]).
-export([create_key_value_user_pass_db/1]).
-export([check_pass/2]).

-export([get_message/2]).

add_user([], _, _) ->
    error;

add_user(_, [], _) ->
    error;

add_user(_, _, []) ->
    error;

add_user(Domain, UserName, Password) ->
    filelib:ensure_dir(Domain ++ UserName),
    filelib:ensure_dir(Domain ++ UserName),
    filelib:ensure_dir(Domain ++ UserName ++ "tmp/"),
    filelib:ensure_dir(Domain ++ UserName ++ "new/"),

    dets:insert(upDisk, {UserName, Domain , Password}).

delete_user(Domain, UserName) ->
    dets:delete(upDisk, {Domain, {UserName, '_'}}).

create_key_value_user_pass_db(UsersPath) ->
    case dets:open_file(upDisk,[{file,UsersPath}]) of
	{ok, Name} ->
	    Name;
	{error, Reason} ->
	    Reason
    end.

destroy() ->
    dets:close(upDisk).

get_message(Domain, UserName) ->
    file:read_file(Domain ++ UserName ++ "new/").

check_pass(UserName, Password) ->
    case dets:lookup(upDisk, UserName) of
	[{_,_,P}]->
	     if
		 (Password =:= P) ->
		     ok;
		 true ->
		     error
	     end;
	[] ->
	    error
    end.
	    
	    
    
