# A `:=` bind's compile-time flag no longer leaks into a nested declaration

While root-causing `Crypt::RC4`'s test-suite failure (`todo/tickets/dist-test-suite-failures-batch.md`),
found and fixed a general compiler bug: `my @x := do { ...; my uint8 @y =
0..N; @y[$i] = v; @y };` — a `:=` bind whose RHS is a `do {}` block that
itself declares and mutates a typed native array — died with "Cannot modify
an immutable Range", even though the identical code with plain `=` (or at
top level with no bind at all) worked correctly.

Root cause: the compiler's `bind_vardecl` field is a one-shot flag, set on
`self` right before a `:=` bind target's own store is compiled, meant to be
consumed by that declaration's `SetLocalDecl`. But it was set *before* the
RHS expression was compiled, and nothing cleared it while that RHS
compilation recursed — so a `my`-declared variable found anywhere inside
the RHS (here, `@y` inside the `do {}` block) wrongly inherited the outer
bind's context too, skipping the Range-to-array materialization a typed
native array declaration needs.

Fixed by snapshotting `bind_vardecl` immediately on entering
`Stmt::VarDecl`'s compilation and clearing the field before any RHS
compilation happens, threading the snapshot through the handful of sites
that previously read the (now correctly scoped) field directly. Pinned by
`t/bind-do-block-nested-vardecl-leak.t`.

A related but architecturally distinct issue — the same *runtime* flag
family (`self.bind_context` et al.) leaking across a live *function call*
boundary rather than a same-compile-unit nested block — remains open; see
`todo/deep/mark-context-flags-leak-across-live-call-boundary.md`.
