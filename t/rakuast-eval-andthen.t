use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# `andthen` / `orelse` / `notandthen` lowering (ADR-0011, the write direction).
#
# raku models these as *list* infixes, so they render as
# `RakuAST::ApplyListInfix` with an operand list — the same node a comma list
# uses. Only the `,` infix lowered, so the whole family read fine and then
# failed with "EVAL does not yet support lowering RakuAST::ApplyListInfix".
#
# mutsu's internal AST keeps them as ordinary left-nested `Expr::Binary` nodes,
# so the lowerer folds the operand list back into that shape. The operator-name
# table also had no rows for them (they fell to its `Ident` catch-all, which is
# a different operator to the compiler).
#
# Passes under BOTH mutsu and raku.

plan 10;

# --- andthen ----------------------------------------------------------------
is EVAL(Q{1 andthen 2}.AST), 2, '`andthen` yields its right operand when the left is defined';
is EVAL(Q{1 andthen 2 andthen 3}.AST), 3, '`andthen` chains left-associatively';
is EVAL(Q{1 andthen $_ + 1}.AST), 2, '`andthen` topicalizes its left operand';

# --- orelse -----------------------------------------------------------------
is EVAL(Q{1 orelse 2}.AST), 1, '`orelse` yields its left operand when it is defined';
is EVAL(Q{my $x; $x orelse 5}.AST), 5, '`orelse` falls through an undefined left operand';

# --- notandthen -------------------------------------------------------------
is EVAL(Q{my $x; $x notandthen 5}.AST), 5,
    '`notandthen` yields its right operand when the left is undefined';
is EVAL(Q{1 orelse 2 orelse 3}.AST), 1, '`orelse` chains left-associatively';

# --- the result type is the operand's, not a Callable -----------------------
is EVAL(Q{(1 andthen 2).WHAT.gist}.AST), '(Int)',
    '`andthen` yields its operand, not a Callable';

# Not pinned here: `(* < 3 andthen 1)` through EVAL. `andthen` is a thunk
# barrier, so evaluating the source gives `(Int)` in BOTH implementations — but
# rakudo's own `EVAL(Q{...}.AST)` gives `(WhateverCode)`, i.e. rakudo does not
# round-trip that shape through its own RakuAST. mutsu's EVAL agrees with its
# own direct evaluation. An assertion either way would diverge, so this file
# stays silent about it rather than pinning one implementation's answer.

# --- the comma list is unchanged --------------------------------------------
is EVAL(Q{(1, 2, 3).elems}.AST), 3, 'a comma list still lowers';
is EVAL(Q{(1, 2, 3).join(",")}.AST), '1,2,3', 'a comma list keeps its order';
