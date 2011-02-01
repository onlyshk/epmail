%%%-------------------------------------------------------------------
%%% @author  <Kuleshov Alexander>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2011 by  <kuleshovmail@gmail.com>
%%%-------------------------------------------------------------------
-module(config).

-export([get_key/2]).
-export([read/1]).

-author('kuleshovmail@gmail.com').
-vsn('0.1').

get_key(_Key, []) ->
  {error, not_found};
get_key(Key, [{Key, Value} | _Config]) ->
  Value;
get_key(Key, [{_Other, _Value} | Config]) ->
  get_key(Key, Config).

read(FileName) -> 
	{ok, Terms} = file:consult(FileName), 
	read_includes(Terms). 

read_includes(Terms) ->
  read_includes(Terms, []).

read_includes([{include_config, File} | Terms], Acc) ->
  case file:consult(File) of
    {ok, NewTerms} ->
      read_includes(Terms ++ NewTerms, Acc);
    {error,enoent} ->
      {error, {bad_include, File}};
    Other ->
      {error, Other}
  end;
read_includes([Other | Terms], Acc) ->
  read_includes(Terms, [Other | Acc]);
read_includes([], Result) ->
  {ok, Result}.
