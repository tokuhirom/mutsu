# `snitch` — the 6.e debugging probe — is implemented, method and subroutine forms

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc`
(lines 1549, 1559).

`snitch` logs its invocant / argument and hands it straight back, so a debugging
probe can be spliced into the middle of a chain without changing the result. It
logs with `note` (to `$*ERR`) unless a `Callable` replaces the logger.

## Membership check

Core, per the two-part test in `CLAUDE.md`: (1) `raku -e 'use v6.e.PREVIEW;
(1..5).snitch'` resolves with no `use` of any module — the version pragma still
counts as core; and (2) it is documented under `raku-doc/doc/Type/Any.rakudoc`
with the signatures

```
multi  snitch(\snitchee)
multi  snitch(&snitcher, \snitchee)
method snitch(\snitchee: &snitcher = &note)
```

## Implementation

`Interpreter::dispatch_snitch` sits beside `dispatch_note` in
`runtime/methods_io_dispatch.rs` — it needs `&mut self` for the I/O, the same
reason `say`/`note`/`put` live there. It is gated on
`current_language_version().starts_with("6.e")` (the gate `builtin_rotor`
already established); below 6.e it returns `None` and falls through to ordinary
dispatch, which reports `No such method 'snitch'` exactly as rakudo does.

The subroutine takes the snitchee **last** so the feed operator reads naturally
(`(1..3).Seq ==> snitch() ==> map(*+2)`), with an optional leading `Callable`;
`builtin_snitch` splits the arguments that way and delegates to the method, so
there is one implementation of the semantics and one 6.e gate.

All six documented examples now produce byte-identical stdout *and* stderr to
real `raku`, with one exception noted below.

## Not covered: the writable form

```raku
(my $a = 42).snitch = 666;   # raku: prints 42, then $a is 666
```

still fails in mutsu with `X::Assignment::RO: cannot assign through .snitch on
non-instance`. This is **not** specific to `snitch`: `$a.VAR = 5` fails
identically, because mutsu has no mechanism for a *native* method to return its
invocant's container. mutsu's lvalue-method machinery is attribute-backed (a
user `method p() is rw { return-rw $!v }` works fine), and making a native
method return an lvalue needs `ContainerRef` to survive method dispatch —
ADR-0013 / Track B territory. Filed separately as
`todo/tickets/native-method-cannot-return-an-lvalue-container.md`.

## Pin

`t/snitch.t` — 13 assertions covering the pass-through, the custom-logger form,
both subroutine arities and the 6.e gate. Passes under real `raku` as well as
mutsu.

## Files

- `src/runtime/methods_io_dispatch.rs` — `dispatch_snitch`
- `src/runtime/methods_dispatch_match.rs` — the method arm, beside `note`
- `src/runtime/builtins_collection_deepmap.rs` — `builtin_snitch`
- `src/runtime/builtins.rs`, `src/runtime/system_eval_names.rs`,
  `src/parser/primary/ident/predicates.rs` — sub registration
