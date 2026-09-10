use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 11 (ADR-0011): EVAL of `unless`/`until` and the prefix
# `!`/`?` operators. mutsu desugars `unless X` to `if !X` and `until X` to
# `while !X`, so lowering these only needed the prefix `!` (and `?`) operator in
# the reverse operator map. Verified against Rakudo; passes under BOTH mutsu and
# raku.

plan 6;

# --- unless (desugars to `if !cond`) ----------------------------------------
is EVAL(Q[my $x = 0; my $r = 1; unless $x { $r = 5 }; $r].AST), 5,
    'unless: body runs when the condition is false';
is EVAL(Q[my $x = 7; my $r = 1; unless $x { $r = 5 }; $r].AST), 1,
    'unless: body is skipped when the condition is true';

# --- until (desugars to `while !cond`) --------------------------------------
is EVAL(Q[my $i = 0; until $i >= 3 { $i = $i + 1 }; $i].AST), 3,
    'until: loops until the condition becomes true';

# --- prefix ! ---------------------------------------------------------------
is EVAL(Q[my $x = 0; !$x].AST), True, 'prefix ! negates a falsy value to True';
is EVAL(Q[my $x = 5; !$x].AST), False, 'prefix ! negates a truthy value to False';

# --- prefix ? ---------------------------------------------------------------
is EVAL(Q[my $x = 5; ?$x].AST), True, 'prefix ? booleanizes a truthy value';
