-module(list).
-export([filter/2, reverse/1, concat/1, flatten/1]).

%% Filter list
filter(List, Pred) ->
    [Elem || Elem <- List, Elem =< Pred].

%% Reverse list
reverse(List) ->
    reverse(List, []).

reverse([], NewList) ->
    NewList;
reverse([Head | Tail], NewList) ->
    reverse(Tail, [Head | NewList]).	

%% Concatenate all lists from List to one
concat(List) ->
    concat(List, []).

concat([], NewList) ->
    NewList;
concat([Head | Tail], NewList) ->
    concat(Tail, concat(Head, NewList));
concat(Val, NewList) ->
    NewList ++ [Val].

%% Flat all elements in nested lists to one
flatten(List) ->
    concat(List).
