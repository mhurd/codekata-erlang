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

% simple iterative solution before moving onto binary chop (add performance metrics?).
iterate(Target, Array) -> iterate(Target, Array, 0).
iterate(Target, [Target|_], Index) -> Index;
iterate(Target, [_|T], Index) -> iterate(Target, T, Index + 1);
iterate(_Target, [], _) -> -1.

iterate_test() -> assert(fun(T, A) -> iterate(T, A) end).

% Apply the supplied function to the test data and assert
% the correct result.
assert(F) ->
  [?assert(-1 == F(3, [])),
    ?assert(-1 == F(3, [1])),
    ?assert(0 == F(1, [1])),
    ?assert(0 == F(1, [1, 3, 5])),
    ?assert(2 == F(3, [1, 3, 5])),
    ?assert(2 == F(5, [1, 3, 5])),
    ?assert(-1 == F(0, [1, 3, 5])),
    ?assert(-1 == F(2, [1, 3, 5])),
    ?assert(-1 == F(4, [1, 3, 5])),
    ?assert(-1 == F(6, [1, 3, 5])),

    ?assert(0 == F(1, [1, 3, 5, 7])),
    ?assert(1 == F(3, [1, 3, 5, 7])),
    ?assert(2 == F(5, [1, 3, 5, 7])),
    ?assert(3 == F(7, [1, 3, 5, 7])),
    ?assert(-1 == F(0, [1, 3, 5, 7])),
    ?assert(-1 == F(2, [1, 3, 5, 7])),
    ?assert(-1 == F(4, [1, 3, 5, 7])),
    ?assert(-1 == F(6, [1, 3, 5, 7])),
    ?assert(-1 == F(8, [1, 3, 5, 7]))].