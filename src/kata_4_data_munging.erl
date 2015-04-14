%%%-------------------------------------------------------------------
%%% @author mhurd
%%% @copyright (C) 2015
%%% @doc http://codekata.com/kata/kata04-data-munging/
%%% @end
%%% Created : 14. Apr 2015 12:26
%%%-------------------------------------------------------------------
-module(kata_4_data_munging).
-author("mhurd").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

read(Filename) ->
  process(file:open(Filename, read)).

process({ok, IoDevice}) ->
  ?debugMsg("Opened file OK"),
  process_line(IoDevice);
process({error, Reason}) ->
  ?debugFmt("Read file failed: ~p~n", [Reason]).

process_line(IoDevice) ->
  case file:read_line(IoDevice) of
    {ok, Data} ->
      ?debugFmt("~p~n", [Data]),
      process_line(IoDevice);
    {error, Reason} -> ?debugFmt("Read line failed: ~p~n", [Reason]);
    eof -> ?debugMsg("End of file reached.")
  end.

-ifdef(EUNIT).
read_test() ->
  Res = read("../resources/weather.dat"),
  ?_assert(Res =:= "").
-endif.
