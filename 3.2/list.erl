-module(list).
-export([create/1, reverse_create/1]).

%% Create a new list with integers in range [1..N]
create(N) ->
	create(N, []).

create(N, List) when N > 0 ->
	create(N-1, [N | List]);
create(_, List) ->
	List.

%% Create a new list with integers in range [N..1]
reverse_create(N) ->
	reverse_create(1, N, []).

reverse_create(N, M, List) when N =< M ->			
	reverse_create(N+1, M, [N | List]);
reverse_create(_, _, List) ->
	List.	