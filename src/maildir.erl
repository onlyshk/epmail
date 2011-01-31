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
-export([delete_user/1]).
-export([create_key_value_user_pass_db/1]).
-export([check_pass/2]).
-export([find_domain/1]).

%
% Add user in dets db
%
add_user([], _, _) ->
    error;

add_user(_, [], _) ->
    error;

add_user(_, _, []) ->
    error;

%
% Add new user mailbox
% Domain - directory mailbox
% UserName - User Name of new user
% Password - password for access to user mailbox
%
add_user(Domain, UserName, Password) ->
    Slash = utils:get_os1(),
    filelib:ensure_dir(Domain ++ UserName ++ Slash),
    filelib:ensure_dir(Domain ++ UserName ++ Slash ++ "new" ++ Slash),

    dets:insert(upDisk, {UserName, Domain , Password}).

%
% Deleting user from dets db
%
delete_user(UserName) ->
    dets:delete(upDisk, {UserName, '_', '_'}).

%
% Find domain by User Name
%
find_domain(UserName) ->
    [{_, Domain, _}] = dets:lookup(upDisk, UserName),
    Domain.

%
% Create user | password dets database
%
create_key_value_user_pass_db(UsersPath) ->
    case dets:open_file(upDisk,[{file,UsersPath}]) of
	{ok, Name} ->
	    Name;
	{error, Reason} ->
	    Reason
    end.

%
% Destroy upDisk dets db
%
destroy() ->
    dets:close(upDisk).

%
% Check username's password
%
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
	    
	    
    
