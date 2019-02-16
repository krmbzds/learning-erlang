# Higher-Order Functions

## lists:map

```erl
Double = fun(X) -> X * 2 end.

lists:map(Double, [1, 2, 3, 4, 5]).

% [2,4,6,8,10]
```

## lists:filter

```erl
Even = fun(X) -> X rem 2 =:= 0 end.

lists:filter(Even, [1, 2, 3, 4, 5]).

% [2, 4]
```
