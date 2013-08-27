-module(parser).
-export([parse/1]).

%% Parse a string expression and return instruction-set for VM
parse([]) ->
    {};
parse(Expr) ->
    case next_token(Expr) of
        {Opcode, TailExpr} when is_atom(Opcode) ->
            io:format("opcode=~p, tail=~p~n", [Opcode, TailExpr]),
            parse(TailExpr);
        {Number, TailExpr} ->
            io:format("number=~p, tail=~p~n", [Number, TailExpr]),
            parse(TailExpr)
    end.


%% Extract next token from string
next_token([]) ->
    {};
next_token(List) ->
    next_token(List, []).

next_token([C | Expr], Acc) ->
    io:format("C=~p, Expr=~p, Acc=~p~n", [C, Expr, Acc]),
    if
        C == $( -> {expr_start, Expr};
        C == $) -> {expr_end, Expr};
        C == $+ -> {op_plus, Expr};
        C == $- -> {op_minus, Expr};
        C == $/ -> {op_divide, Expr};
        C == $* -> {op_multiply, Expr};
        C == $~ -> {unary_minus, Expr};
        (C >= $0) and (C =< $9) -> next_token(Expr, [C | Acc]);
        (C >= $\r) or (C =< $\n) or (C =< $\t) or (C =< 32) -> next_token(Expr, []);
        true -> {Acc, Expr}
    end.
