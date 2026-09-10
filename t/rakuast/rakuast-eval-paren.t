use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 21 (ADR-0011): parenthesised expressions `(EXPR)`, both
# directions. A standalone `(EXPR)` is a `Circumfix::Parentheses` wrapping a
# single-statement `SemiList`. The read side (`.AST`) emits that node and the
# write side (`EVAL`) unwraps it to the inner expression. Verified against
# Rakudo; passes under BOTH mutsu and raku.

plan 4;

# --- read side: the parens render as Circumfix::Parentheses -----------------
ok Q/my $x; ($x = 5)/.AST.gist.contains('RakuAST::Circumfix::Parentheses.new('),
    'a parenthesised expression renders as Circumfix::Parentheses';

# --- a grouped assignment now round-trips (the slice-19 boundary) -----------
is EVAL(Q/my $x = 0; my $y = ($x = 5); $x + $y/.AST), 10,
    'a parenthesised assignment expression evaluates and returns its value';

# --- a grouped arithmetic subexpression -------------------------------------
is EVAL(Q/my $x = 3; my $y = ($x + 1); $y/.AST), 4,
    'a parenthesised arithmetic expression';

# --- a lone parenthesised variable ------------------------------------------
is EVAL(Q/my $x = 7; ($x)/.AST), 7,
    'a lone parenthesised variable is its value';
