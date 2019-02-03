-module(shop2).
-export([total/1]).
-import(mylists, [map/2, sum/1]).

total(List) ->
  sum(map(fun({Item, Quantity}) -> shop:cost(Item) * Quantity end, List)).

