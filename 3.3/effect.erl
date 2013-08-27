-module(effect).
-export([print/1, print_odd/1]).

%% Print all integers in range [1..N]
print(N) ->
	print(1, N).

print(N, M) when N =< M ->
	io:format("Number: ~p~n", [N]),
	print(N+1, M);
print(_, _) -> 
	ok.	

%% Print all odd integers in range [1..N]
print_odd(N) -> 
	print_odd(1, N).

print_odd(N, M) when N rem 2 == 1 ->
	io:format("Number: ~p~n", [N]),
	print_odd(N+1, M);
print_odd(N, M) when N =< M ->
	print_odd(N+1, M);
print_odd(_, _) ->
	ok.
