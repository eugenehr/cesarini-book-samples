-module(expr).
-export([sum/1, sum/2]).


%% Sum all integers in range [1..N]
sum(N) ->
	sum(1, N).

%% Sum all integer in range [N..M]
sum(N, M) when N > M ->
	throw({error, invalid_argument});
sum(N, M) ->
	sum(N, M, 0).
	
%% Internal tail-recursive function to sum all integers in range [N..M]	
sum(N, M, Acc) when N > M ->
	Acc;
sum(N, M, Acc) when N == M ->
	Acc + N;	
sum(N, M, Acc) ->
	sum(N+1, M-1, N + M + Acc).	
