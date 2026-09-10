use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 20 (ADR-0011): EVAL of the C-style `loop (…; …; …) { … }`.
# `Statement::Loop` (with optional `setup`/`condition`/`increment`) lowers to a
# `Stmt::Loop`. The read side already works via the slice-19 assignment
# expression (the increment `$i = $i + 1`). Verified against Rakudo; passes under
# BOTH mutsu and raku.

plan 4;

# --- a three-part C-style loop accumulates ----------------------------------
is EVAL(Q[my $s = 0; loop (my $i = 0; $i < 5; $i = $i + 1) { $s = $s + $i }; $s].AST), 10,
    'C-style loop: sum 0..4 == 10';

# --- another with a product -------------------------------------------------
is EVAL(Q[my $p = 1; loop (my $i = 1; $i <= 4; $i = $i + 1) { $p = $p * $i }; $p].AST), 24,
    'C-style loop: 4! == 24';

# --- `next` skips an iteration ----------------------------------------------
is EVAL(Q[my $s = 0; loop (my $i = 0; $i < 6; $i = $i + 1) { next if $i %% 2; $s = $s + $i }; $s].AST), 9,
    'C-style loop with next: sum of odds < 6 == 9';

# --- a bare `loop { … }` broken by `last` -----------------------------------
is EVAL(Q[my $i = 0; loop { $i = $i + 1; last if $i >= 4 }; $i].AST), 4,
    'bare loop broken by last';
