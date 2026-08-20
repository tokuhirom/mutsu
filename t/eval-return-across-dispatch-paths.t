use Test;

plan 6;

# ADR-0037 Slice 1: `push_routine_with_location` (which maintains
# `routine_stack`, consulted by `enclosing_routine_exists()` and every other
# `routine_stack` consumer) used to be called only from the 0-arg fast path
# (`call_compiled_function_fast`) and the full named path
# (`call_compiled_function_named_inner`). The two "light" sub-call paths --
# `call_compiled_function_positional_light` (mandatory positional params) and
# `call_compiled_function_light[_spec]` (named params) -- pushed no routine
# frame at all, so `enclosing_routine_exists()` wrongly answered `false`
# inside a sub taking either path, even though a routine plainly enclosed it.
#
# Concretely: `EVAL 'return 1'` inside such a sub's body incorrectly escaped
# as an uncaught `X::ControlFlow::Return` instead of being treated as a real
# non-local `return` -- the same behavior a 0-arg sub already got right. This
# file pins one sub shape per dispatch path, matching the measured matrix in
# docs/adr/0037-eval-context-frame-owns-the-return-target.md section 1.3;
# raku answers `1` for every row (verified against `raku` on this machine).

sub zero() { EVAL 'return 1'; return 2 }
is zero(), 1, 'EVAL return in a 0-arg sub (fast path) returns from the sub';

sub pos1($x) { EVAL 'return 1'; return 2 }
is pos1(9), 1,
    'EVAL return in a mandatory-positional sub (positional-light path) returns from the sub';

sub named1(:$x) { EVAL 'return 1'; return 2 }
is named1(:x(9)), 1,
    'EVAL return in a named-param sub (light path) returns from the sub';

sub arr(@x) { EVAL 'return 1'; return 2 }
is arr([1, 2]), 1,
    'EVAL return in an array-param sub (full named path) returns from the sub';

sub opt($x?) { EVAL 'return 1'; return 2 }
is opt(), 1,
    'EVAL return in an optional-param sub (full named path) returns from the sub';

sub slurp(*@x) { EVAL 'return 1'; return 2 }
is slurp(1, 2), 1,
    'EVAL return in a slurpy-param sub (full named path) returns from the sub';
