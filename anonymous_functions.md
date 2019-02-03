# Anonymous Functions

```erl
Hypot = fun(X, Y) -> math:sqrt(X*X + Y+Y) end.

% Hypot(3, 4)
% 5.0
```

```erl
TempConvert = fun({c, C}) -> {f, 32 + C*9/5};
                 ({f, F}) -> {c, (F-32)*5/9}
              end.

% TempConvert({c, 100}).
% {f,212.0}

% TempConvert({f, 32}).
% {c,0.0}
```

```erl
Even = fun(X) -> X rem 2 =:= 0 end.

% Even(2).
% true
% Even(3).
% false
```
