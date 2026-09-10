use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 19 (ADR-0011): assignment expressions `$x = EXPR` in value
# position, both directions. mutsu's `AssignExpr` (e.g. the inner `$b = 5` of a
# chained assignment `$a = $b = 5`) converts to the same `ApplyInfix(Assignment)`
# as a statement assignment, and the write side lowers such an `ApplyInfix` in
# expression position back to an `AssignExpr`. Verified against Rakudo; passes
# under BOTH mutsu and raku.

plan 4;

# --- read side: a chained assignment round-trips through the gist ------------
my $g = Q[my $a; my $b; $a = $b = 5].AST.gist;
ok $g.contains('RakuAST::Assignment.new(:item)')
    && ($g.indices('RakuAST::Assignment.new(:item)').elems == 2),
    'a chained assignment renders two Assignment infixes';

# --- a chained assignment sets both variables -------------------------------
is EVAL(Q[my $a; my $b; $a = $b = 5; $a + $b].AST), 10,
    'chained assignment sets both variables';

# --- a longer chain ---------------------------------------------------------
is EVAL(Q[my $a; my $b; my $c; $a = $b = $c = 7; $a + $b + $c].AST), 21,
    'a three-deep assignment chain';

# --- the assigned expression is evaluated once ------------------------------
is EVAL(Q[my $a; my $b; $a = $b = 3 + 4; $a * $b].AST), 49,
    'the chained value is a full expression';
