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

-vsn('0.2').
-author('kuleshovmail@gmail.com').

-record(users, {username,
	        password}).

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
    filelib:ensure_dir(Domain ++ UserName ++ "/"),
    filelib:ensure_dir(Domain ++ UserName ++ "/" ++ "new" ++ "/"),
    filelib:ensure_dir(Domain ++ UserName ++ "/" ++ "tmp" ++ "/"),

    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),

    %% | Postgresql data
    Host = config:get_key(host, Config),
    Database = config:get_key(database, Config), 
    User = config:get_key(user, Config),
    Login = config:get_key(login, Config),

    case UserStorage of
	mnesia ->
	    User = #users{username = UserName, password = Password},
	    mnesia:transaction(fun() -> mnesia:write(User) end);
	dets ->
	    dets:insert(upDisk, {UserName, Domain , Password});
	ets ->
	    ets:insert(usersTable, {UserName, Domain, Password});
	sqlite3 ->
	    sqlite3:write(user_db, users, [{user, UserName}, {domain, Domain}, {password, Password}]);
	postgresql ->
	    {ok, Db} = pgsql:connect(Host, Database, User, Login),
	    pgsql:prepare(Db, insert, "INSERT INTO mail_user (name, domain, password) VALUES ($1 ,$2 ,$3)"),
	    ABs = [{UserName, Domain, Password}],
	    [pgsql:execute(Db, insert, [A, B, C]) || {A,B,C} <- ABs]
    end.

%
% Deleting user from dets db
%
delete_user(UserName) ->
    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),

    Host = config:get_key(host, Config),
    Database = config:get_key(database, Config), 
    User = config:get_key(user, Config),
    Login = config:get_key(login, Config),

    case UserStorage of
	mnesia ->
	    mnesia:transaction(fun() -> mnesia:delete({users, UserName}) end);
	dets ->
	    dets:delete(upDisk, {UserName, '_', '_'});
	ets ->
	    ets:delete(usersTable, {UserName, '_', '_'});
	sqlite3 ->
	    sqlite3:delete(user_db, users, {user, UserName});
	postgresql ->
	    {ok, Db} = pgsql:connect(Host, Database, User, Login),
	    pgsql:pquery(Db, "DELETE FROM  mail_user  WHERE name = $1", [UserName])
    end.
%
% Find domain by User Name
%
find_domain(UserName) ->
    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),

    %% | Postgresql data
    Host = config:get_key(host, Config),
    Database = config:get_key(database, Config), 
    User = config:get_key(user, Config),
    Login = config:get_key(login, Config),


    case UserStorage of
	mnesia ->
	   {_, [{_,Domain,_}]} = mnesia:transaction(fun() -> mnesia:read({users, UserName}) end),
	    Domain;
	dets ->
	    [{_, Domain, _}] = dets:lookup(upDisk, UserName),
	    Domain;
	ets ->
	    [{_, Domain, _}] = ets:lookup(usersTable, UserName),
	    Domain;
	sqlite3 ->
	    [_, T] = sqlite3:sql_exec(user_db, "SELECT domain FROM users WHERE user = ?", [{1, UserName}]),
	    {rows, Domain_in_tupple} = T,
	    [{Domain_in_bin}] = Domain_in_tupple,
	    binary_to_list(Domain_in_bin);
	 postgresql ->
	    {ok, Db} = pgsql:connect(Host, Database, User, Login),
	    {ok, _, _, _, Rows} = pgsql:pquery(Db, "SELECT domain FROM  mail_user  WHERE name = $1", [UserName]),
	    [Row] = Rows,
	    {D} = Row,
	    binary_to_list(D)
    end.

%
% Create user | password dets ot ets database
%
create_key_value_user_pass_db(UsersPath) ->
    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),
    
    case UserStorage of
	dets ->
	    case dets:open_file(upDisk,[{file,UsersPath}]) of
		{ok, Name} ->
		    Name;
		{error, Reason} ->
		    Reason
	    end;
	ets ->
	    ets:new(usersTable, [bag]);
	sqlite3 ->
	    sqlite3;
	postgresql ->
	    postgresql;
	_ ->
	    error
    end.

%
% Destroy upDisk dets db
%
destroy() ->
    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),

    case UserStorage of
	dets ->
	    dets:close(upDisk);
	ets ->
	    ets:delete(usersTab);
	sqlite3 ->
	    sqlite3:close(user_db)
    end.

%
% Check username's password
%
check_pass( UserName, Password) ->
    {ok, Config} = config:read(config),
    UserStorage = config:get_key(user_storage, Config),

    %% | Postgresql data
    Host = config:get_key(host, Config),
    Database = config:get_key(database, Config), 
    User = config:get_key(user, Config),
    Login = config:get_key(login, Config),

    case UserStorage of
	mnesia ->
	    {_, [{_,_, MnesiaPass}]} = mnesia:transaction(fun() -> mnesia:read({users, UserName}) end),
	    if
		(Password =:= MnesiaPass) ->
		    ok;
		true ->
		    error
	    end;

	sqlite3 ->
	    [_, T] = sqlite3:sql_exec(user_db, "SELECT password FROM users WHERE user = ?", [{1, UserName}]),
	    {rows, Domain_in_tupple} = T,
	    [{Domain_in_bin}] = Domain_in_tupple,
	    PasswordForSqlite3 = binary_to_list(Domain_in_bin),
	    if
		(Password =:= PasswordForSqlite3) ->
		    ok;
		true ->
		    error
	    end;
	
	ets ->
	    case ets:lookup(usersTable, UserName) of
		[{_,_, EtsPass}] ->
		    if
			(Password =:= EtsPass) ->
			    ok;
			true ->
			    error
		    end;
		[] ->
		    error
	    end;
	
	dets->
	    case dets:lookup(upDisk, UserName) of
		[{_, _, DetsPass}]->
		    if
			(Password =:= DetsPass) ->
			    ok;
			true ->
			    error
		    end;
		[] ->
		    error
	    end;
	
	postgresql ->
	    {ok, Db} = pgsql:connect(Host, Database, User, Login),
	    {ok, _, _, _, Rows} = pgsql:pquery(Db, "SELECT password FROM  mail_user  WHERE name = $1", [UserName]),
	    [Row] = Rows,
	    {PostgrePassBin} = Row,
	    PostgrePass = binary_to_list(PostgrePassBin),

	     if
		 (Password =:= PostgrePass) ->
			    ok;
		 true ->
		     error
	     end
    end.
	    
	    
    
