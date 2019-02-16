-module(my_time).
-import (my_time, [timestamp/0]).
-export([print_diff_now/2, my_time_func/1, my_date_string/0]).

print_diff_now(T1, T2) ->
  {Mega1, Sec1, Micro1} = T1,
  {Mega2, Sec2, Micro2} = T2,
  MegaDiff = integer_to_list(Mega2 - Mega1),
  SecDiff = integer_to_list(Sec2 - Sec1),
  MicroDiff = integer_to_list(Micro2 - Micro1),
  string:join([MegaDiff, SecDiff, MicroDiff], ":").

my_time_func(F)->
  T1 = timestamp(),
  F(),
  T2 = timestamp(),
  print_diff_now(T1, T2).

my_date_string()->
  {Year, Month, Day} = date(),
  {Hour, Minute, Second} = time(),
  % DD/MM/YYYY
  Date = string:join([integer_to_list(Day), integer_to_list(Month), integer_to_list(Year)], "/" ),
  Time = string:join([integer_to_list(Hour), integer_to_list(Minute), integer_to_list(Second)], ":"),
  string:join([Date, Time], " ").
