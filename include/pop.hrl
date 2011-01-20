%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.

-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.

-record(state, {
                listener 
               }).
