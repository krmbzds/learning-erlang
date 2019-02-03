-module(lib_misc).
-export([for/3]).

% Creating our own control flows

for(Max, Max, F) -> [F(Max)];
for(Min, Max, F) -> [F(Min)|for(Min+1, Max, F)].

% Double = fun(X) -> X * 2 end.
% lib_misc:for(1, 10, Double).
% [2,4,6,8,10,12,14,16,18,20]

% Square = fun(X) -> X * X end.
% lib_misc:for(1, 10, Square).
% [1,4,9,16,25,36,49,64,81,100]
