use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 9 (ADR-0011): EVAL of the bare `for @x { … }` form,
# whose block has no explicit parameter and sees the loop value as `$_`.
# `Statement::For` with a plain `Block` body (not a `PointyBlock`) lowers to a
# `Stmt::For` with no named parameter. Verified against Rakudo; passes under BOTH
# mutsu and raku.

plan 4;

# --- bare for over a range, using $_ ----------------------------------------
is EVAL(Q[my $t = 0; for 1..4 { $t = $t + $_ }; $t].AST), 10,
    'bare for: sum 1..4 via $_ == 10';

# --- bare for over a list ---------------------------------------------------
is EVAL(Q[my $p = 1; for (2, 3, 4) { $p = $p * $_ }; $p].AST), 24,
    'bare for: product of a list via $_ == 24';

# --- $_ drives a method call ------------------------------------------------
is EVAL(Q[my $t = 0; for (-1, -2, -3) { $t = $t + $_.abs }; $t].AST), 6,
    'bare for: $_.abs accumulates to 6';

# --- the pointy form still works alongside ----------------------------------
is EVAL(Q[my $s = 0; for 1..4 -> $x { $s = $s + $x }; $s].AST), 10,
    'pointy for still lowers correctly';
