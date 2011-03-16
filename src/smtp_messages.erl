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
-export([is_mail_message/1]).
-export([is_rcpt_message/1]).
-export([is_vrfy_message/1]).

-vsn('0.3').
-author('kuleshovmail@gmail.com').

%
% HELO smtp message
% SMTP SEND: HELO (for supporting old message)
%
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

%
% EHLO smtp message
% SMTP SEND: EHLO [user@localhhost]
%
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

%
% MAIL FROM smtp message
% SMTP SEND: MAIL FROM: <user@localhhost>
%
is_mail_message([]) ->
    error;
is_mail_message(MailMessage) ->
    [H | T] = string:tokens(MailMessage, ":"),
    case [H | T] of
	["mail from", _] ->
	    {H, T};
	        [_, _] ->
	   error;
      
       [_] ->
	   error;
 
       "mail from" ->
	   error;
       
       [] ->
	   error;
       
       _ ->
	   error
   end.

%
% RCPT to smtp message
% SMTP SEND: RCPT TO: <user@localhhost>
%
is_rcpt_message([]) ->
    error;
is_rcpt_message(RcptMessage) ->
        [H | T] = string:tokens(RcptMessage, ":"),
    case [H | T] of
	["rcpt to", _] ->
	    {H, T};
	        [_, _] ->
	   error;
      
       [_] ->
	   error;
 
       "rcpt to" ->
	   error;
       
       [] ->
	   error;
       
       _ ->
	   error
   end.

%
% VRFY to smtp message
% SMTP SEND: VRFY: NAME
%
is_vrfy_message([]) ->
    error;
is_vrfy_message(VrfyMessage) ->
        [H | T] = string:tokens(VrfyMessage, " "),
    case [H | T] of
	["vrfy", _] ->
	    {H, T};
	        [_, _] ->
	   error;
      
       [_] ->
	   error;
 
       "vrfy" ->
	   error;
       
       [] ->
	   error;
       
       _ ->
	   error
   end.
