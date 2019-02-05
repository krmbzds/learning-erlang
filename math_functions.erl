-module (math_functions).
-export ([odd/1, even/1, filter/2, naive_split/1, split/1, tests/0,
          test_filter_even/0, test_filter_odd/0, test_naive_split/0,
          test_split/0]).

even(X) -> (X rem 2) =:= 0.

odd(X)  -> (X rem 2) =:= 1.

filter(F, L) -> [X || X <- L, F(X)].

naive_split(L) ->
  Even = filter(fun even/1, L),
  Odd = filter(fun odd/1, L),
  {Even, Odd}.

split(L) ->
  acc_split(lists:reverse(L), [], []).

acc_split([], Even, Odd) ->
  {Even, Odd};
acc_split([H|T], Even, Odd) ->
  case (H rem 2) of
    0 -> acc_split(T, [H|Even], Odd);
    1 -> acc_split(T, Even, [H|Odd])
  end.

% --------------------- TESTS ---------------------

tests() ->
  test_filter_even(),
  test_filter_odd(),
  test_naive_split(),
  test_split(),
  all_tests_ok.

test_filter_even() ->
  [2, 4, 6] = filter(fun even/1, [1, 2, 3, 4, 5, 6]),
  test_filter_even_ok.

test_filter_odd() ->
  [1, 3, 5] = filter(fun odd/1, [1, 2, 3, 4, 5, 6]),
  test_filter_odd_ok.

test_naive_split() ->
  {[2, 4, 6], [1, 3, 5]} = naive_split([1, 2, 3, 4, 5, 6]),
  test_naive_split_ok.

test_split() ->
  {[2, 4, 6], [1, 3, 5]} = split([1, 2, 3, 4, 5, 6]),
  test_split_ok.
