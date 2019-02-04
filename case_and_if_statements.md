# Case and If Expressions

## Case

The syntax

```erl
case Expression of
  Pattern1 [when Guard1] -> Expr_seq1;
  Pattern2 [when Guard2] -> Expr_seq2;
  ...
end
```

filter(P, L); it returns a list of all those elements X in L for which P(X) is true. Using case we can define filter as follows:

```erl
filter(P, [H|T]) ->
  case P(H) of
    true  -> [H|filter(P, T)];
    false -> filter(P, T)
  end;
filter(P, []) ->
  [].
```

Strictly speaking, case is unnecessary. This is how filter would have been defined using pure pattern matching

```erl
filter(P, [H|T]) ->  filter1(P(H), H, P, T);
filter(P, [])    ->  [].

filter1(true, H, P, T)  -> [H|filter(P, T)];
filter1(false, H, P, T) -> filter(P, T).
```

## If

The syntax

```erl
if
  Guard1 ->
    Expr_seq1;
  Guard2 ->
    Expr_seq2;
  ...
end
```

Example

```erl
if
  A > 0 ->
    do_this()
end
```
