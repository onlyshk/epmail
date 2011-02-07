%%%-------------------------------------------------------------------
%%% @author  Kuleshov Alexander
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 7 Feb 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-module(smtp_messages).

-export([is_helo_message/1]).
-export([is_ehlo_message/1]).

is_helo_message([]) ->
    error;
is_helo_message(Message) ->
    [H | T] = string:tokens(Message, " "),
 
   case [H | T] of
       ["helo", _] ->
	   {H, T};
       
       [_, _] ->
	   error;
      
       [_] ->
	   error;
 
       "helo" ->
	   error;
       
       [] ->
	   error;
       
       _ ->
	   error
   end.

is_ehlo_message([]) ->
    error;
is_ehlo_message(Message) ->
    [H | T] = string:tokens(Message, " "),
 
   case [H | T] of
       ["ehlo", _] ->
	   {H, T};
       
       [_, _] ->
	   error;
      
       [_] ->
	   error;
 
       "ehlo" ->
	   error;
       
       [] ->
	   error;
       
       _ ->
	   error
   end.
