-module(triplets).
-export([compare/2, compare/3, test_compare/0]).

compare(A, B) ->
  compare_triplets(A, B, 0, 0).

compare_triplets([], [], ACC1, ACC2) ->
  [ACC1, ACC2];
compare_triplets([A|T1], [B|T2], ACC1, ACC2) ->
  if
    A > B -> compare_triplets(T1, T2, ACC1+1, ACC2);
    A < B -> compare_triplets(T1, T2, ACC1, ACC2+1);
    true  -> compare_triplets(T1, T2, ACC1, ACC2)
  end.

compare(A, B, C) ->
  compare_triplets(A, B, C, 0, 0, 0).

compare_triplets([], [], [], ACC1, ACC2, ACC3) ->
  [ACC1, ACC2, ACC3];
compare_triplets([A|T1], [B|T2], [C|T3], ACC1, ACC2, ACC3) ->
  if
    ((A > B) and (A > C)) -> compare_triplets(T1, T2, T3, ACC1+1, ACC2, ACC3);
    ((B > A) and (B > C)) -> compare_triplets(T1, T2, T3, ACC1, ACC2+1, ACC3);
    ((C > A) and (C > B)) -> compare_triplets(T1, T2, T3, ACC1, ACC2, ACC3+1);
    true                  -> compare_triplets(T1, T2, T3, ACC1, ACC2, ACC3)
  end.

test_compare() ->
  [1, 1] = compare([1, 1, 0], [0, 1, 1]),
  [0, 3] = compare([0, 0, 0], [1, 1, 1]),
  [3, 0] = compare([1, 1, 1], [0, 0, 0]),
  [2, 1] = compare([1, 1, 0], [0, 0, 1]),
  [3, 0, 0] = compare([1, 1, 1], [0, 0, 0], [0, 0, 0]),
  [0, 3, 0] = compare([0, 0, 0], [1, 1, 1], [0, 0, 0]),
  [0, 0, 3] = compare([0, 0, 0], [0, 0, 0], [1, 1, 1]),
  [0, 1, 2] = compare([0, 0, 0], [1, 0, 0], [0, 1, 1]),
  [1, 1, 1] = compare([1, 0, 0], [0, 1, 0], [0, 0, 1]),
  test_compare_ok.
