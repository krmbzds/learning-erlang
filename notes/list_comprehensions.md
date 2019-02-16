# List Comprehensions

The notation

```erl
% The list of F(X) where X is taken from the list L
[F(X) || X <- L]
```

There is a simpler way to do

```erl
L = [1, 2, 3, 4, 5]

lists:map(fun(X) -> 2*X end, L).

% [2,4,6,8,10]
```

Using list comprehensions

```erl
[2*X || X <- L ].

% [2,4,6,8,10]
```

More examples

```erl
BuyList = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}].

[{Item, 2 * Quantity} || {Item, Quantity} <- BuyList].
% [{oranges,8},{newspaper,2},{apples,20},{pears,12},{milk,6}]

[{shop:cost(A), B} || {A, B} <- BuyList].
% [{5,4},{8,1},{2,10},{9,6},{7,3}]

[shop:cost(A) * B || {A, B} <- BuyList].
% [20,8,20,54,21]

lists:sum([shop:cost(A) * B || {A, B} <- BuyList]).
% 123

% If we make it into a function
total(L) ->
    lists:sum([shop:cost(A) *  B || {A, B} <- L]).
```

List comprehension version of map function

```erl
map(F, L) -> [F(X) || X <- L].
```

The generator part of a list comprehension works like a filter

```erl
[ X || {a, X} <- [{a,1},{b,2},{c,3},{a,4},hello,"wow"]].
% [1,4]
```
