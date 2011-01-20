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

err_message() ->
    ["-ERR\r\n"].

ok_message() ->
    ["+OK"].

is_message_user([]) ->
    error;
is_message_user(UserName) ->
   [H | T] = string:tokens(UserName, " "),
   case [H | T] of
      ["user", _] ->
	  {H, T};
       
      [_, _] ->
	  error;
       
      [_] ->
	   error;
      [] ->
	   error
   end.

is_message_pass(UserName) when is_list(UserName) ->
   [H | T] = string:tokens(UserName, " "),
   case [H | T] of
      ["pass", _] ->
	  {H, T};
       
      [_, _] ->
	  error;
       
      [[], []] ->
	   error;

      [H] ->
	   error
   end.
