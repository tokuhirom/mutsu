use Test;

# ADR-0033 Phase 4: Whatever-priming does not run through a *thunky* operator.
#
# `&&`, `||`, `//`, `and`, `or`, `andthen`, `orelse`, `notandthen` and the
# ternary's three parts compile their operands as thunks, and priming happens
# per thunk. So `* > 3 && * < 8` is TWO independent arity-1 WhateverCodes; the
# `&&` runs at its own evaluation time, sees a truthy Code object on the left,
# and yields the right-hand WhateverCode.
#
# Every expectation below was measured against rakudo before being written
# here. mutsu used to prime straight through these operators (producing one
# arity-2 closure) and to prime nothing at all inside a ternary.

plan 34;

# --- the headline correctness bug -------------------------------------------

is (* > 3 && * < 8).arity, 1, '&& does not merge two curries into arity 2';
is (* > 3 && * < 8)(5), True, 'the yielded right-hand curry takes one argument';
is (* > 3 && * < 8)(9), False, '... and evaluates only that thunk';
is (1..10).grep(* > 3 && * < 8).join(' '), '1 2 3 4 5 6 7',
        'grep(* > 3 && * < 8) greps on the right-hand thunk alone';
is (* > 3 && * < 8).WHAT.^name, 'WhateverCode', '&& of two curries is still a WhateverCode';

# Parenthesised operands are already materialised scopes; do not double-wrap.
is ((* > 3) && (* < 8)).arity, 1, 'explicitly parenthesised operands behave the same';

# Left-nested chain of barriers.
is (* > 3 && * < 8 && * != 5)(5), False, 'a chain of && yields the last thunk (false case)';
is (* > 3 && * < 8 && * != 5)(6), True, 'a chain of && yields the last thunk (true case)';

is (*.defined && *.Str)(7), '7', 'method-call curries either side of && are independent';

# --- the barrier is opaque to the enclosing scope ---------------------------

# `&&` contributes nothing to any scope above it, so the `+` below sees exactly
# one placeholder and curries to arity 1 (rakudo: `(WhateverCode)`).
is ((* > 3 && * < 8) + *).WHAT.^name, 'WhateverCode',
        'a barrier contributes no placeholder to the enclosing curry';
# (No `.arity` probe here: `.arity` is itself *inside* the priming scope, so
# `((...) + *).arity` is a larger WhateverCode in both implementations rather
# than a number -- see ADR-0033's Risks section on `.arity` composition. The
# barrier ends the scope in `(* > 3 && * < 8).arity` above, which is why that
# one does read back as 1.)

# --- the other short-circuit operators --------------------------------------

is (* + 1 && 5).WHAT.^name, 'Int', '&& with a plain right operand yields that operand';
is (* + 1 and * + 2).arity, 1, '`and` is a barrier too';
is (* + 1 orelse * + 2).arity, 1, '`orelse` is a barrier';
is (* > 3 orelse * < 8).arity, 1, '`orelse` yields the defined left curry';
is (* > 3 // * < 8).arity, 1, '`//` is a barrier';
is (1 < 2 && * > 3).arity, 1, 'a non-Whatever left operand still yields the right curry';
is (1..10).grep(* %% 2 || * > 7).join(' '), '2 4 6 8 10',
        '|| yields its truthy left curry';
is (1..10).grep(* > 3 || * > 8).join(' '), '4 5 6 7 8 9 10',
        '... measured on a second || shape';
is (* eq "a" or * eq "b")("b"), False, '`or` yields its truthy LEFT operand';

# A *bare* `*` is a Whatever value, not a curry, so a barrier just returns it.
is (* || 5).WHAT.^name, 'Whatever', 'a bare * operand of || stays a Whatever value';
is (* andthen 5).WHAT.^name, 'Int', 'a bare * is defined, so `andthen` yields the right side';
dies-ok { (* // 5)(Nil) }, 'invoking the Whatever that `//` yielded dies (no CALL-ME)';

# --- the ternary: mutsu used to prime nothing at all here -------------------

is (* + 1 ?? * + 2 !! * + 3).WHAT.^name, 'WhateverCode',
        'each ternary part is its own priming scope';
is (* + 1 ?? * + 2 !! * + 3)(10), 12, '... and the chosen branch is an arity-1 curry';
is (* + 1 ?? 2 !! 3).WHAT.^name, 'Int', 'a curried condition is truthy, yielding the then-branch';
is (1 ?? * + 2 !! * + 3).WHAT.^name, 'WhateverCode', 'a plain condition selects a curried branch';
is (1 ?? * + 2 !! * + 3)(5), 7, '... which is callable';
is (0 ?? * + 2 !! * + 3)(5), 8, '... and the else-branch likewise';
is (* ?? 1 !! 2).WHAT.^name, 'Int', 'a bare * condition is a truthy Whatever value';

# --- chained comparison must NOT be split (the Phase-4 prerequisite) --------
#
# `a < m < b` is expanded by the parser into `(a < m) && (m < b)` with the
# middle duplicated. That synthesized conjunction is a distinct operator
# (`TokenKind::ChainAnd`), NOT a thunk barrier: rakudo keeps the whole chain as
# a single priming scope. Contrast the user-written `&&` immediately below --
# the two must not collapse into each other.

is (0 <= * <= 5)(3), True, 'a chained comparison is ONE arity-1 curry (inside)';
is (0 <= * <= 5)(9), False, '... and both ends are checked (outside)';
is (1 < * < 10)(0), False, 'a chained comparison checks its left end too';
is (1 < * && * < 10)(0), True,
        'a user-written && with the same operands yields only its right thunk';

# Placeholder parameters must keep working through the same expansion.
is { $^a < $^b < $^c }(1, 2, 3), True, 'a chained comparison over placeholders still works';
