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

add_user([], _, _) ->
    error;

add_user(_, [], _) ->
    error;

add_user(_, _, []) ->
    error;

add_user(Domain, UserName, Password) ->
    filelib:ensure_dir(Domain ++ UserName),
    filelib:ensure_dir(Domain ++ UserName ++ "MailDir/"),
    filelib:ensure_dir(Domain ++ UserName ++ "MailDir/" ++ "tmp/"),
    filelib:ensure_dir(Domain ++ UserName ++ "MailDir/" ++ "new/"),
    filelib:ensure_dir(Domain ++ UserName ++ "MailDir/" ++ "cur/"),

    dets:insert(upDisk, {Domain, UserName, Password}).

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
    

    
