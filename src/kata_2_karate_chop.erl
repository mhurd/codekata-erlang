%%%-------------------------------------------------------------------
%%% @author mhurd
%%% @copyright (C) 2015
%%% @doc http://codekata.com/kata/kata02-karate-chop/
%%% @end
%%% Created : 09. Apr 2015 12:27
%%%-------------------------------------------------------------------
-module(kata_2_karate_chop).
-author("mhurd").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([iterate/2]).

% NOTE: only implentaing one technique here as its apparent that Erlang is not perfectly suited to Array manipulation.

% simple iterative solution before moving onto binary chop (add performance metrics?).
iterate(Target, Array) -> iterate(Target, array:to_list(Array), 0).
iterate(Target, [Target|_], Index) -> Index;
iterate(Target, [_|T], Index) -> iterate(Target, T, Index + 1);
iterate(_Target, [], _) -> -1.

iterate_test() -> assert(fun iterate/2).

% binary chop technique 1
floor(X) ->
  T = erlang:trunc(X),
  case (X - T) of
    Neg when Neg < 0 -> T - 1;
    Pos when Pos > 0 -> T;
    _ -> T
  end.

halfway_between(N1, N2) ->
  if
    N1 == N2 -> N1;
    N1 < N2 -> N1 + floor((N2 - N1)/2);
    N1 > N2 -> N2 + floor((N1 - N2)/2)
  end.

-ifdef(EUNIT).
halfway_between_test() ->
  [
    ?assert(1 == halfway_between(1, 2)),
    ?assert(0 == halfway_between(0, 0)),
    ?assert(5 == halfway_between(1, 10)),
    ?assert(4 == halfway_between(3, 5)),
    ?assert(1 == halfway_between(1, 2)),
    ?assert(1 == halfway_between(0, 3)),
    ?assert(2 == halfway_between(1, 3)),
    ?assert(50== halfway_between(1, 100))
  ].
-endif.

chop1(Target, Array) ->
  case array:size(Array) of
    0 -> -1;
    _ -> chop1(Target, Array, 0, array:size(Array), -1, halfway_between(0, array:size(Array)))
  end.

chop1(_Target, _Array, _LowerBound, _UpperBound, _LastIndex, _LastIndex) ->
  -1;
chop1(Target, Array, LowerBound, UpperBound, _LastIndex, Index) ->
  case array:get(Index, Array) of
    Target -> Index;
    Other -> if
               Other > Target -> chop1(Target, Array, LowerBound, Index, Index, halfway_between(LowerBound, Index));
               Other < Target -> chop1(Target, Array, Index, UpperBound, Index, halfway_between(Index, UpperBound))
             end
  end.

-ifdef(EUNIT).
chop1_test() -> assert(fun chop1/2).
-endif.

% Apply the supplied function to the test data and assert
% the correct result.
assert(F) ->
  [
    ?assert(-1 == F(3, array:from_list([]))),
    ?assert(-1 == F(3, array:from_list([1]))),
    ?assert(0 == F(1, array:from_list([1]))),
    ?assert(0 == F(1, array:from_list([1, 3, 5]))),
    ?assert(1 == F(3, array:from_list([1, 3, 5]))),
    ?assert(2 == F(5, array:from_list([1, 3, 5]))),
    ?assert(-1 == F(0, array:from_list([1, 3, 5]))),
    ?assert(-1 == F(4, array:from_list([1, 3, 5]))),
    ?assert(-1 == F(4, array:from_list([1, 3, 5]))),
    ?assert(-1 == F(6, array:from_list([1, 3, 5]))),
    ?assert(0 == F(1, array:from_list([1, 3, 5, 7]))),
    ?assert(1 == F(3, array:from_list([1, 3, 5, 7]))),
    ?assert(2 == F(5, array:from_list([1, 3, 5, 7]))),
    ?assert(3 == F(7, array:from_list([1, 3, 5, 7]))),
    ?assert(-1 == F(0, array:from_list([1, 3, 5, 7]))),
    ?assert(-1 == F(2, array:from_list([1, 3, 5, 7]))),
    ?assert(-1 == F(4, array:from_list([1, 3, 5, 7]))),
    ?assert(-1 == F(6, array:from_list([1, 3, 5, 7]))),
    ?assert(-1 == F(8, array:from_list([1, 3, 5, 7])))
  ].