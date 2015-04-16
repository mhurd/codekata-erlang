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
  % discard the header line
  case file:read_line(IoDevice) of
    {ok, _Data} ->
      process_data(IoDevice);
    {error, Reason} -> ?debugFmt("Read line failed: ~p~n", [Reason]);
    eof -> ?debugMsg("End of file reached.")
  end;
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

get_col_and_range(ColIndex, RangeIndexes, Data) when (ColIndex > 0) and (ColIndex =< length(Data)) ->
  ColVal = lists:nth(ColIndex, Data),
  Range = range(lists:map(
    fun(N) ->
      case N of
        0 -> erlang:error(invalid_index);
        _ -> lists:nth(N, Data)
      end
    end
    , RangeIndexes)),
  {ColVal, Range};
get_col_and_range(ColIndex, _RangeIndexes, _Data) when ColIndex =< 0 ->
  erlang:error(invalid_index);
get_col_and_range(ColIndex, _RangeIndexes, Data) when ColIndex > length(Data) ->
  erlang:error(invalid_index).

  -ifdef(EUNIT).
get_col_and_range_test() ->
  [?assertEqual({1, 6}, get_col_and_range(1, [1,4], [1,3,6,7])),
    ?assertEqual({6, 60}, get_col_and_range(3, [5,6], [1,3,6,7,-4,56,10])),
    ?assertException(error, invalid_index, get_col_and_range(0, [1,4], [1,3,6,7])),
    ?assertException(error, invalid_index, get_col_and_range(1, [0,4], [1,3,6,7]))].
-endif.

process_data(IoDevice) ->
  case file:read_line(IoDevice) of
    {ok, Data} ->
      Tokens = string:tokens(Data, " "),
      ?debugFmt("Tokens: ~p", [Tokens]),
      process_data(IoDevice);
    {error, Reason} -> ?debugFmt("Read line failed: ~p~n", [Reason]);
    eof -> ?debugMsg("End of file reached.")
  end.

-ifdef(EUNIT).
read_test() ->
  Res = read("../resources/weather.dat"),
  ?_assert(Res =:= "").
-endif.
