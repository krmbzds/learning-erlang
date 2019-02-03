-module(mylists).
-export([sum/1, map/2]).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

% List comprehension version
% map(F, L) -> [F(X) || X <- L].
