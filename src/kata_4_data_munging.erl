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

min_max([H|T]) ->
  min_max(T, H , H).
min_max([H|T], Min, Max) when H =< Min -> min_max(T, H, Max);
min_max([H|T], Min, Max) when H >= Max -> min_max(T, Min, H);
min_max([H|T], Min, Max) when (H > Min) and (H < Max) -> min_max(T, Min, Max);
min_max([], Min, Max) -> {Min, Max}.

-ifdef(EUNIT).
min_max_test() ->
  [?assertEqual({1,7}, min_max([1,3,6,7])),
  ?assertEqual({1,1}, min_max([1])),
  ?assertEqual({-9,100}, min_max([-4,2,-9,100])),
  ?assertEqual({3,7}, min_max([7,3,6,6]))].
-endif.

range(Numbers) ->
  case min_max(Numbers) of
    {Min, Max} -> Max - Min
  end.

-ifdef(EUNIT).
range_test() ->
  [?assertEqual(6, range([1,3,6,7])),
  ?assertEqual(0, range([1])),
  ?assertEqual(109,  range([-4,2,-9,100])),
  ?assertEqual(4, range([7,3,6,6]))].
-endif.

process_line(IoDevice) ->
  case file:read_line(IoDevice) of
    {ok, Data} ->
      ?debugFmt("~p", [string:tokens(Data, " ")]),
      process_line(IoDevice);
    {error, Reason} -> ?debugFmt("Read line failed: ~p~n", [Reason]);
    eof -> ?debugMsg("End of file reached.")
  end.

-ifdef(EUNIT).
read_test() ->
  Res = read("../resources/weather.dat"),
  ?_assert(Res =:= "").
-endif.
