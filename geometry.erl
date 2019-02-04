-module(geometry).
-export([test/0, area/1]).

test() ->
  12 = area({rectangle, 3, 4}),
  144 = area({square, 12}),
  tests_worked.

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side;
area({circle, Radius}) -> math:pi() * Radius * Radius;
area({right_angled_triangle, A, B}) -> A * B / 2;
area({sphere, Radius}) -> 4 * math:pi() * Radius * Radius.
