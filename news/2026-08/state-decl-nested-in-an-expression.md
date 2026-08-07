# A `state` declaration buried in an expression is still a declaration

Raku clones a block every time its enclosing block runs, so a `state` inside an
`if` branch or a bare block restarts on every execution of the construct that
contains it. mutsu emits an `OpCode::ResetStateLocals` at the head of such an
inline block, but only when the block's statements declare `state` — and the
test for that, `expr_has_state_decl`, recognized a declaration only when it
*was* the whole expression.

It usually is not. `++state $n` parses as a prefix operator wrapped around the
declaration, so:

```raku
sub f() { my @r; if 1 { @r.push(++state $n) }; @r.join(',') }
say (f(), f(), f()).join('|');   # was 1|2|3 — raku says 1|1|1
```

The walk now descends through operator, call, subscript and interpolation
shapes, and stops at anything that introduces a block of its own (`Block`,
`Lambda`, `AnonSub`, ...) — a `state` in there belongs to *that* clone and is
reset at its own entry, so descending would only emit a redundant reset.

The same predicate decides whether a `state` initializer's evaluation can be
skipped once the variable is initialized, so widening it also fixes a nested
declaration in an initializer (`state $a = (state $b = 0) + 1`) that has to run
its own `StateVarInit` on every call.

Pinned by `t/state-decl-nested-in-expression.t`, which passes under `raku` too.
