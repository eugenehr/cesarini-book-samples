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
	lists:keystore(Key, 1, Db, {Key, Element}).

%% Delete existing element from database
delete(Key, Db) when is_list(Db) ->
	lists:keydelete(Key, 1, Db).

%% Read values by key from database
read(Key, Db) when is_list(Db)->
	read(Key, Db, []).

read(_, [], Res) ->
	Res;
read(Key, Db, Res) ->
	case lists:keytake(Key, 1, Db) of
		{value, {Key, Value}, Db1} -> read(Key, Db1, [Value | Res]);
		false -> Res
	end.

%% Find elements in database
match(Element, Db) when is_list(Db) ->
	match(Element, Db, []).

match(_, [], Res) ->
	Res;
match(Element, Db, Res) ->
	case lists:keytake(Element, 2, Db) of
		{value, {Key, _}, Db1} -> match(Element, Db1, [Key | Res]);
		false -> Res
	end.
