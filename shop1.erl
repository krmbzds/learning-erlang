-module(shop1).
-export([total/1]).

total([{Item, Quantity}|T]) -> shop:cost(Item) * Quantity + total(T);
total([]) -> 0.
