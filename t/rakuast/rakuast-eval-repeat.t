use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 13 (ADR-0011): EVAL of `repeat { … } while/until C`.
# `Statement::Loop::RepeatWhile` lowers to a `Stmt::Loop` with `repeat => True`,
# which runs the body once before testing the condition. `repeat … until C`
# desugars to `repeat … while !C` (the prefix `!` from slice 11). Verified
# against Rakudo; passes under BOTH mutsu and raku.

plan 4;

# --- repeat/while loops while the condition holds ---------------------------
is EVAL(Q[my $i = 0; my $n = 0; repeat { $i = $i + 1; $n = $n + 1 } while $i < 3; $n].AST), 3,
    'repeat/while runs the body 3 times';

# --- the body runs at least once even when the condition starts false -------
is EVAL(Q[my $n = 0; repeat { $n = $n + 1 } while $n > 100; $n].AST), 1,
    'repeat/while runs the body once before the (already-false) test';

# --- repeat/until loops until the condition becomes true --------------------
is EVAL(Q[my $i = 0; repeat { $i = $i + 1 } until $i >= 5; $i].AST), 5,
    'repeat/until loops until the condition is true';

# --- repeat/until also runs at least once -----------------------------------
is EVAL(Q[my $n = 0; repeat { $n = $n + 1 } until $n < 100; $n].AST), 1,
    'repeat/until runs the body once before the (already-true) test';
