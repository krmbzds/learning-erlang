-module(lib_misc).
-export([for/3, qsort/1, pythag/1, perms/1, odds_and_evens1/1,
         odds_and_evens2/1, my_tuple_to_list/1]).

% Creating our own control flows

for(Max, Max, F) -> [F(Max)];
for(Min, Max, F) -> [F(Min)|for(Min+1, Max, F)].

% Double = fun(X) -> X * 2 end.
% lib_misc:for(1, 10, Double).
% [2,4,6,8,10,12,14,16,18,20]

% Square = fun(X) -> X * X end.
% lib_misc:for(1, 10, Square).
% [1,4,9,16,25,36,49,64,81,100]

qsort([]) -> [];
qsort([Pivot|T]) ->
  qsort([X || X <- T, X < Pivot])
  ++ [Pivot] ++
  qsort([X || X <- T, X >= Pivot]).

% L=[23,6,2,9,27,400,78,45,61,82,14].
% lib_misc:qsort(L).

pythag(N) ->
  [ {A, B, C} ||
    A <- lists:seq(1, N),
    B <- lists:seq(1, N),
    C <- lists:seq(1, N),
    A + B + C =< N,
    A*A + B*B =:= C*C
  ].

% lib_misc:pythag(16).
% [{3,4,5},{4,3,5}]
% lib_misc:pythag(30).
% [{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{12,5,13}]

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].

% This implementation goes over the list twice
odds_and_evens1(L) ->
  Odds  = [X || X <- L, (X rem 2) =:= 1],
  Evens = [X || X <- L, (X rem 2) =:= 0],
  {Odds, Evens}.

% This implementation goes over the list once
odds_and_evens2(L) ->
  odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
  case (H rem 2) of
    1 -> odds_and_evens_acc(T, [H|Odds], Evens);
    0 -> odds_and_evens_acc(T, Odds, [H|Evens])
  end;
odds_and_evens_acc([], Odds, Evens) ->
  {lists:reverse(Odds), lists:reverse(Evens)}.

% tuple_to_list implementation w/o built-in functions
my_tuple_to_list(Tuple) ->
  [element(I, Tuple) || I <- lists:seq(1, tuple_size(Tuple))].
