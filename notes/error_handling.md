# Error Handling

### Trapping an Exception with try...catch

```erl
try FuncOrExpressionSeq of
  Pattern1 [when Guard1] -> Expressions1;
  Pattern2 [when Guard2] -> Expressions2;
  ...
catch
  ExceptionType1: ExPattern1 [when ExGuard1] -> ExExpressions1;
  ExceptionType2: ExPattern2 [when ExGuard2] -> ExExpressions2;
  ...
after
    AfterExpressions
end
```

### Shortcuts

We can omit several of the parts of a try...catch expression.

```erl
try F
catch
  ...
end
```

Above code is equivalent of the code below

```erl
try F of
  Val -> Val
catch
  ...
end
```

### Programming Idioms with try...catch

This function generates three different types of an exception and has two
ordinary return values

```erl
% try_test.erl

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).
```

Now we’ll write a wrapper function to call generate_exception in a try...catch
expression

```erl
% try_test.erl

demo1() ->
  [catcher(I) || I <- [1, 2, 3, 4, 5]]

catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X  -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.
```

Running this we obtain the following

```erl
try_test:demo1().
[{1,normal,a},
 {2,caught,thrown,a},
 {3,caught,exited,a},
 {4,normal,{'EXIT',a}},
 {5,caught,error,a}]
```

This shows that we can trap and distinguish all the forms of exception that
a function can raise.

### Improving Error Messages

```erl
math:sqrt(-1).
% ** exception error: bad argument in an arithmetic expression in function math:sqrt/1
%          called as math:sqrt(-1)
```

We can write a wrapper for this, which improves the error message.

```erl
sqrt(X) when X < 0 ->
  error({squareRootNegativeArgument, X});
sqrt(X) ->
  math:sqrt(X).
```

```erl
lib_misc:sqrt(-1).
% ** exception error: {squareRootNegativeArgument,-1}
%    in function  lib_misc:sqrt/1
```

### Notes

- ExceptionType is an atom, it is either `throw`, `exit`, or `error`.
- If ExceptionType is omitted, then the value defaults to `throw`.
- Internal errors that are detected by the Erlang runtime system always have the tag `error`.
- Individual tuples in the stack trace are of the form `{Mod,Func,Arity,Info}`.
    + `Mod`, `Func`, and `Arity` denote a function
    + Info contains the filename and line number of the item in the stack trace.

### Interesting Quote

Error messages are gold dust for programmers. They should never scroll up the
screen to vanish forever. They should go to a permanent log file that can be
read later.
