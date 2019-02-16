# Error Handling

Example

```erl
sqrt(X) when X < 0 ->
  error({squareRootNegativeArgument, X});
sqrt(X) ->
  math:sqrt(X).
```

```erl
lib_misc:sqrt(-1).
```

```
** exception error: {squareRootNegativeArgument,-1}
   in function  lib_misc:sqrt/1
```


```erl
catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X  -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.
```

```erl
demo1() ->
  [catcher(I) || I <- [1, 2, 3, 4, 5]].
```

```erl
try_test:demo1().
[{1,normal,a},
 {2,caught,thrown,a},
 {3,caught,exited,a},
 {4,normal,{'EXIT',a}},
 {5,caught,error,a}]
```


Either write code to hand two specific cases

```erl
case f(X) of
    {ok, Val} ->
        do_some_thing_with(Val);
    {error, Why} ->
        %% ... do something with the error ...
end,
```

Or match expected value and move on

```
{ok, Val} = f(X),
do_some_thing_with(Val);
```
