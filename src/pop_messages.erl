%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 19 Jan 2011 by  <kuleshovmail@gmail.com>
%%%------------------------------------------------------------------
-module(pop_messages).

-export([err_message/0, ok_message/0]).

-export([is_message_user/1]).
-export([is_message_pass/1]).
-export([is_message_list/1]).
-export([is_message_retr/1]).
-export([is_message_dele/1]).

err_message() ->
    ["-ERR\r\n"].

ok_message() ->
    ["+OK"].

is_message_user([]) ->
    error;
is_message_user(UserName) when is_list(UserName) ->
   [H | T] = string:tokens(UserName, " "),
 
   case [H | T] of
      ["user", _] ->
	   {H, T};
       
      
      [_, _] ->
	  error;
      
      [_] ->
	   error;
 
      "user" ->
	   error;
       
      [] ->
	   error;
       
       _ ->
	   error
   end.

is_message_pass([]) ->
    error;
is_message_pass(Password) when is_list(Password) ->
   [H | T] = string:tokens(Password, " "),
   case [H | T] of
      ["pass", _] ->
	  {H, T};
       
      [_, _] ->
	  error;
       
      [_] ->
          error;

      "pass" ->
	   error;

      [] ->
	   error;
       
      _ ->
	   error
   end.

is_message_list([]) ->
    error;
is_message_list(Message)  ->
   [H | T] = string:tokens(Message, " "),
   case [H | T] of
      ["list", _]  ->
	  {H, T};
       
      ["list"] ->
      	   {H};
       
      [_, _] ->
	  error;
      
      [_] ->
          error;

      list ->
          error;
       
      [] ->
	   error;
      _ ->
	   error
   end.

is_message_retr([]) ->
    error;
is_message_retr(Message)  ->
   [H | T] = string:tokens(Message, " "),
   case [H | T] of
      ["retr", _] ->
	  {H, T};
       
      [_, _] ->
	  error;
       
      ["retr"] ->
	   error;
      
      [_] ->
          error;

      "retr" ->
	  error;
       
      [] ->
	   error;
      _ ->
	   error
	      
   end.

is_message_dele([]) ->
    error;
is_message_dele(Message) ->
   [H | T] = string:tokens(Message, " "),
   case [H | T] of
      ["dele", _] ->
	  {H, T};
       
      [_, _] ->
	  error;
       
      [_] ->
	   error;
       
      "dele" ->
	   error;
       
      [] ->
	   error;
       
       _ ->
	   error
   end.
