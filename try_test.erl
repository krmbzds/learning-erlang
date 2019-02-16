-module(try_test).
-export([demo1/0]).

catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X  -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.

demo1() ->
  [catcher(I) || I <- [1, 2, 3, 4, 5]].

% try_test:demo1().
% [{1,normal,a},
%  {2,caught,thrown,a},
%  {3,caught,exited,a},
%  {4,normal,{'EXIT',a}},
%  {5,caught,error,a}]

demo2() ->
  [{I, (catch generate_exception(I))} || I <- [1,2,3,4,5]].

% try_test:demo2(). [{1,a},
%        {2,a},
%        {3,{'EXIT',a}},
%        {4,{'EXIT',a}},
%        {5,{'EXIT',
%               {a,[{try_test,generate_exception,1,
%                         [{file,"try_test.erl"},{line,9}]},
%               {try_test,'-demo2/0-lc$^0/1-0-',1,
%                         [{file,"try_test.erl"},{line,28}]},
%               {try_test,'-demo2/0-lc$^0/1-0-',1,
%                         [{file,"try_test.erl"},{line,28}]},
%               {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
%               {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
%               {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
%               {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]}}}]

sqrt(X) when X < 0 ->
  error({squareRootNegativeArgument, X});
sqrt(X) ->
  math:sqrt(X).

% lib_misc:sqrt(-1).
% ** exception error: {squareRootNegativeArgument,-1}
%    in function  lib_misc:sqrt/1


% Stack traces
demo3() ->
  try generate_exception(5)
    catch
      error:X ->
        {X, erlang:get_stacktrace()}
    end.

% try_test:demo3(). {a,[{try_test,generate_exception,1,[{file,"try_test.erl"},{line,9}]},
%           {try_test,demo3,0,[{file,"try_test.erl"},{line,33}]},
%           {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
%           {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
%           {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
%           {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]}

