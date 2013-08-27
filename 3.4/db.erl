-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).


%% Create new database
new() ->
	[].

%% Destroy database
destroy(Db) when is_list(Db) ->
	ok.

%% Write new element to database
write(Key, Element, Db) when is_list(Db) ->
	[{Key, Element} | Db].

%% Delete existing element from database
delete(Key, Db) when is_list(Db) ->
	delete(Key, Db, []).

delete(_, [], NewList) ->
	NewList;
delete(Key, [{Key, _} | Tail], NewList) ->
	delete(Key, Tail, NewList);
delete(Key, [H | Tail], NewList) ->
	delete(Key, Tail, [H | NewList]).


%% Read values by key from database
read(Key, Db) when is_list(Db) -> 
	read(Key, Db, []).

read(_, [], Values) ->
	Values;	
read(Key, [{Key, Value} | Tail], Values) ->
	read(Key, Tail, [Value | Values]);
read(Key, [_ | Tail], Values) ->
	read(Key, Tail, Values).


%% Find elements in database
match(Element, Db) ->	
	match(Element, Db, []).
match(_, [], Keys) ->
	Keys;	
match(Element, [{Key, Element} | Tail], Keys) ->
	match(Element, Tail, [Key | Keys]);
match(Element, [_ | Tail], Keys) ->
	match(Element, Tail, Keys).
