-module(lib_misc).
-export([for/3, qsort/1, pythag/1, perms/1]).

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
