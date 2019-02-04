# Guard Clauses

```erl
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.
```

Guard sequences

```erl
f(X,Y) when is_integer(X), X > Y, Y < 6 ->
  % Do something...
```
