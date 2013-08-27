-module(sort).
-export([qsort/1, msort/1, split/1]).

%% Quick sort
qsort([]) ->
    [];
qsort([Pivot | Rest]) ->
    qsort([X || X <- Rest, X < Pivot])
    ++ [Pivot] ++
    qsort([Y || Y <- Rest, Y >= Pivot]).


%% Merge sort
msort([]) -> 
    [];
msort([H]) ->
    [H];
msort([H1, H2 | []]) ->
    if 
        H1 < H2 -> [H1, H2];
        true -> [H2, H1]
	end; 		
msort(List) -> 
    {Ls1, Ls2} = split(List),
    merge(msort(Ls1), msort(Ls2)).


%% Reverse list
reverse(List) ->
    reverse(List, []).

reverse([], Ls2) ->
    Ls2;
reverse([H | Ls1], Ls2) ->
    reverse(Ls1, [H | Ls2]).	


%% Split list in half
split(List) ->
    split(List, List, []).

split([], Ls1, Ls2) ->
    {reverse(Ls2), Ls1};
split([_], Ls1, Ls2) ->
    {reverse(Ls2), Ls1};	
split([_,_ | Ls], [H1 | Ls1], Ls2) ->
    split(Ls, Ls1, [H1 | Ls2]).

%% Merge two lists
merge([], Ls2) ->
    Ls2;
merge(Ls1, []) ->
    Ls1;
merge([H1 | T1], [H2 | T2]) ->
    if
        H1 < H2 -> [H1 | merge(T1, [H2 | T2])];
        true -> [H2 | merge([H1 | T1], T2)]
    end.	