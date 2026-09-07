# A non-final `enum` statement no longer wins over the block's value

```raku
say EVAL(q|enum E <A B>; my $r = 41 + 1; $r|);
# raku:  42
# mutsu: Map.new((A => 0, B => 1))
```

Any block whose value was read while an `enum` declaration ran earlier in it
answered the enum's `Map`. It was not limited to the first statement, as the
ticket's title suggested — `EVAL 'my $x = 1; enum G <H I>; 7'` answered the
`Map` too — and it made `EVAL` unusable as an isolation boundary for a test
that has to declare an enum
(`t/user-infix-op-candidate-ranking.t` worked around it by hoisting the enum
to file scope).

## Root cause

`RegisterEnum` pushes the declaration's `Map`, and it should: that IS the value
of `enum` in expression position (`my $e = enum Foo <a b c>`, `do enum <a b>`).
Nothing popped it in *statement* position, so it parked at the frame's stack
base — where the unit's result is read from — and beat the block's real tail
value.

`class` and `sub` declarations do not have the problem because their statement
compilation pushes nothing at all; the expression wrapper in `expr_block.rs`
appends a separate op (`GetBareWord`, `PushLastRegisteredRole`) to produce the
value.

## Fix

`enum` is now declared in `Compiler::stmt_nets_a_stack_value`, the ADR-0052
mechanism that already pops a non-final `given`/`when`/`default`. Every block
compiler consults it, so a non-final `enum` is popped and a **final** one is
still the block's value — which is what rakudo does: `EVAL 'enum E <A B>'`
answers the `Map`, and a trailing `;` does not change that (the enum is still
the last statement).

## Scope

Pinned by `t/enum-decl-statement-value.t` (16 assertions measured against
rakudo 2026.07): the repro, a falsy tail value, a mid-block enum, a final enum
in a unit / `do`-block / sub body, the expression forms (`my $e = enum ...`,
`do enum`, the installed variants), `my enum`, an enum followed by a `class`
declaration, and a bare block ending in an enum. `roast/S12-enums/*.t`
(179 subtests) stay green.
