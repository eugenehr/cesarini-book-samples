-module(parser).
-export([parse/1]).

%% Parse a string expression and return instruction-set for VM
parse(Expr) ->
    parse(Expr, {num, 0}, op_none).

parse([], _, _) ->
    {};
parse(Expr, LastOp, LastOpcode) ->
    case next_token(Expr) of
        {op_neg, TailExpr} ->
            {op_neg, parse(TailExpr, LastOp, LastOpcode)};
        {Opcode, TailExpr} when LastOpcode == op_none ->
            parse(TailExpr, LastOp, Opcode);
        {num, Number, TailExpr} ->
            case LastOpcode of
                op_none when LastOp == 0 -> parse(TailExpr, {num, Number}, LastOpcode);
                Opcode -> {Opcode, LastOp, {num, Number}}
            end
    end.


%% Extract next token from string
next_token([]) ->
    {};
next_token([C | Expr]) ->
    % io:format("C=~p, Expr=~p~n", [C, Expr]),
    if
        C == $( -> {op_beg, Expr};
        C == $) -> {op_end, Expr};
        C == $+ -> {op_add, Expr};
        C == $- -> {op_sub, Expr};
        C == $/ -> {op_div, Expr};
        C == $* -> {op_mul, Expr};
        C == $~ -> {op_neg, Expr};
        (C >= $0) and (C =< $9) -> take_number(Expr, list_to_integer([C]) );
        (C == $\r) or (C == $\n) or (C == $\t) or (C == $ ) -> next_token(Expr);
        true -> throw({invalid_character, C})
    end.

%% Extract number from string
take_number([H | Expr], N) when (H >= $0) and (H =< $9) ->
    take_number(Expr, N*10 + list_to_integer([H]) );
take_number(Expr, N) -> 
    {num, N, Expr}.
