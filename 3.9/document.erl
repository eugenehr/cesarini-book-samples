-module(document).
-export([read_raw/1, read/1, index/1, format_indexes/1]).

%% Read raw document from file
read_raw(FileName) ->
    {ok, IoDevice} = file:open(FileName, [read]),
    try read_raw(IoDevice, []) of
        RawDoc -> 
            file:close(IoDevice),
            lists:reverse(RawDoc)
    catch
        Exception -> 
            file:close(IoDevice),
            io:format("Could not read raw document from file. ~p~n", [Exception])
    end.

%% Read raw document from IO device
read_raw(IoDevice, RawDoc) ->
    case file:read_line(IoDevice) of
        {ok, Line} -> read_raw(IoDevice, [Line | RawDoc]);
        eof        -> RawDoc
    end.

%% Read document from raw document
read(RawDoc) ->
    read(RawDoc, 1, []).
%% Tail-recursion optimized reading
read([], _, Doc) ->
    Doc;
read([Line | RawDoc], LineNum, Doc) ->
    read(RawDoc, LineNum + 1, Doc ++ [{LineNum, string:tokens(Line, " [](){}<>,./\\+-=%|-+:\t\n\r")}]).
    
%% Index words from all lines in document
index(Doc) ->
    index(Doc, []).
%% Tail-recursion optimized indexing
index([], Words) ->
    Words;
index([{LineNum, LineWords} | Doc], Words) ->
    index(Doc, index_words(LineNum, LineWords, Words)).

%% Index all words in line
index_words(_, [], Words) ->
    Words;
index_words(LineNum, [Word | LineWords], Words) ->
    NewWords = case lists:keysearch(Word, 1, Words) of
        {value, {Word, Numbers}} -> 
            lists:keyreplace(Word, 1, Words, {Word, Numbers ++ [LineNum]});
        false ->
            [{Word, [LineNum]} | Words]
    end,
    index_words(LineNum, LineWords, NewWords).

%% Format words indexes - exclude duplications and convert to ranges
format_indexes(Words) ->
    format_indexes(Words, []).

format_indexes([], Words) ->
    lists:sort(fun({Word1, _}, {Word2, _}) -> Word1 < Word2 end, Words);
format_indexes([{Word, Numbers} | Tail], Words) ->
    format_indexes(Tail, [{Word, list_to_ranges(Numbers)} | Words]).

%% Convert list of numbers to list of ranges and return it as a string
list_to_ranges([]) ->
    "";
list_to_ranges([Num | Numbers]) ->
    list_to_ranges(Numbers, Num, false, integer_to_list(Num)).

% End of list
list_to_ranges([], _, false, Res) ->
    Res;
list_to_ranges([], LastNum, true, Res) ->
    string:concat(Res, integer_to_list(LastNum));
% Next number equals to previous. Skip it    
list_to_ranges([Num | Numbers], Num, InRange, Res) ->
    list_to_ranges(Numbers, Num, InRange, Res);

list_to_ranges([Num | Numbers], LastNum, InRange, Res) ->
    {NewRes, NewInRange} = 
    if 
        % If next number is greater then last number for 1 then set flag InRange and continue
        LastNum + 1 == Num ->
            {case InRange of
                true -> Res;
                false -> string:concat(Res, "-")
            end,
            true};
        % else clear flag InRange, close last range (if exists) and append number to result after comma    
        true ->
            {string:concat(string:concat(case InRange of
                true -> string:concat(Res, integer_to_list(LastNum));
                false -> Res
            end, ","), integer_to_list(Num)),
            false}
    end,
    list_to_ranges(Numbers, Num, NewInRange, NewRes).
    

