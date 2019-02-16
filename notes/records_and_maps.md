# Records and Maps

## Records

Records are like tuples in disguise.
Using records it is possible to associate a name with each element in a tuple.

### Naming Tuple Items with Records

```erl
-record(Name, {
  %% the next two keys have default values
  key1 = Default1,
  key2 = Default2,
  %% The next line is equivalent to
  %% key3 = undefined
  key3,
  key4 = Default4
}).
```

### Storing Records

Record definitions can be included in Erlang source code files or put in files
with the extension `.hrl`, which are then included by Erlang source code files.

```erl
-record(todo, {status=reminder,who=joe,text}).
```

To read `records.hrl` from the shell, we use

```erl
rr("records.hrl").
% [todo]
```

### Creating and Updating Records

```erl
#todo{}.
% #todo{status = reminder,who = joe,text = undefined}

X1 = #todo{status=urgent, text="Fix errata in book"}.
% #todo{status = urgent,who = joe,text = "Fix errata in book"}

X2 = X1#todo{status=done}.
% #todo{status = done,who = joe,text = "Fix errata in book"}
```

X1 and X2 are new records. The syntax `#todo{key1=Val1, ..., keyN=ValN}` is
used to create a new record of type todo. The keys are all atoms and must be
the same as those used in the record definition. If a key is omitted, then a
default value is assumed for the value that comes from the value in the record
definition.

With X2 we copied an existing record. The syntax `X1#todo{status=done}` says to
create a copy of X1 (which must be of type todo), changing the field value
status to done. Remember, this makes a copy of the original record; the
original record is not changed.

### Extracting the Fields of a Record

```erl
#todo{who=W, text=Txt} = X2.
% #todo{status = done,who = joe,text = "Fix errata in book"}

W.
% joe

Txt.
% "Fix errata in book"
```

On the left side of the match operator (=), we write a record pattern with the
unbound variables `W` and `Txt`. If the match succeeds, these variables get
bound to the appropriate fields in the record. If we want just one field of a
record, we can use the “dot syntax” to extract the field.

```erl
X2#todo.text.
% "Fix errata in book"
```

### Pattern Matching Records in Functions

We can write functions that pattern match on the fields of a record and that
create new records.

```erl
clear_status(#todo{status=S, who=W} = R) ->
    %% Inside this function S and W are bound to the field
    %% values in the record
    %%
    %% R is the *entire* record
    R#todo{status=finished}
```

To match a record of a particular type, we might write the function definition.

```erl
do_something(X) when is_record(X, todo) ->
  %% ...
```

This clause matches when X is a record of type todo.

### Records Are Tuples in Disguise

Records are just tuples.

```erl
X2.
% #todo{status = done,who = joe,text = "Fix errata in book"}
```

Now let's tell the shell to forget the definition of todo.

```erl
rf(todo).
% ok

X2.
% {todo,done,joe,"Fix errata in book"}
```

## Maps

Maps are associative collections of key-value pairs.
The key can be any Erlang term. In Perl and Ruby they are called hashes; in C++
and Java they are called maps, and in Python they are called dictionaries.

### Syntax

```
#{ Key1 Op Val1, Key2 Op Val2, ..., KeyN Op ValN }
```

Where `Op` is either `=>` or `:=`.

```erl
F1 = #{ a => 1, b => 2 }.
% #{ a => 1, b => 2 }.
```

The expression `K => V` is used for two purposes, either to update the value of
an existing key K with a new value V or to add a completely new K-V pair to the
map. This operation always succeeds.

The expression `K := V` is used to update the value of an existing key K with a
new value V. This operation fails if the map being updated does not contain the
key K.

```erl
F3 = F1#{ c => xx }.
% #{ a => xx, b => 2 , c => xx}

F4 = F1#{ c := 3}
% ** exception error: bad argument key c does not exist in old map
```

### Pattern Matching the Fields of a Map

The value can contain variables that become bound if pattern matching succeeds

```erl
Henry8 = #{ class => king, born => 1491, died => 1547 }.
% #{ born => 1491, class=> king, died => 1547 }.

#{ born => B } = Henry8.
% #{ born => 1491, class=> king, died => 1547 }.

B.
% 1491

#{ D => 1547 }.
% * 4: variable 'D' unbound
```

A function that returns a map of the number of times a particular character
occurs in a string

```erl
count_characters(Str) ->
  count_characters(Str, #{}).
count_characters([H|T], #{ H => N }=X) ->
  count_characters(T, X#{ H := N+1 });
count_characters([H|T], X) ->
  count_characters(T, X#{ H => 1 });
count_characters([], X) ->
  X.
```

Usage example

```erl
count_characters("hello").
% #{101=>1,104=>1,108=>2,111=>1}
```

### Built-in Functions that Operate on Maps

```
maps:new() -> #{}
erlang:is_map(M) -> bool()
maps:to_list(M) -> [{K1,V1},..., {Kn,Vn}]
maps:from_list([{K1,V1},...,{Kn,Vn}]) -> M
maps:map_size(Map) -> NumberOfEntries
maps:is_key(Key, Map) -> bool()
maps:get(Key, Map) -> Val
maps:find(Key, Map) -> {ok, Value} | error
maps:keys(Map) -> [Key1,..KeyN]
maps:remove(Key, M) -> M1
maps:without([Key1, ..., KeyN], M) -> M1
maps:difference(M1, M2) -> M3
```
