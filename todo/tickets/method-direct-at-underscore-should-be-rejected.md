# A method still silently accepts a direct (non-`do{}`-nested) bare `@_`

Found and partially fixed while triaging `t/placeholder-named-in-method-do.t`
(`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`).

Real `raku` only auto-adds `*%_` to a signature-less method, never `*@_`:

```
$ raku -e 'class A { method m { @_.raku.say } }; A.new.m(1,2)'
===SORRY!=== Placeholder variables (eg. @_) cannot be used in a method.
Please specify an explicit signature, like method m (*@_) { ... }
```

This session fixed the **nested-`do{}`** shape (`method m { do { @_ } }`,
`X::Placeholder::Block`) in `src/compiler/helpers_do_expr.rs`'s
`compile_do_block_expr` — it used to exempt both `%_` and `@_` from the
"unattached placeholder in a signature-less block" check when
`lexically_in_method`; now only `%_` is exempted.

**Still open:** a bare `@_` referenced *directly* in a method body (no `do{}`
in between) is still silently accepted and auto-slurped, e.g.
`class B { method m { @_.elems } }; say B.m(1, 2, 3);` prints `3` under mutsu
but is a `===SORRY!===` compile error under `raku`. This goes through a
completely different mechanism: `auto_signature_uses()` /
`apply_auto_positional_slurpy()` / `apply_auto_positional_slurpy_from_flag()`
in `src/method_signature_shared.rs`, called from three method-specific sites
(`src/compiler/helpers_method_body.rs::compile_method_body`,
`src/runtime/registration_class_body_method.rs` ×2, one for the effective
param defs and one for the byte-parity key snapshot, and
`src/runtime/registration_class_augment.rs`) plus the `d3_8a_byte_parity_tests`
module that pins the compiler/registration parity (ADR-0019 D3-8a/D3-9) —
all four/five call sites currently just INSERT the implicit `*@_` param when
the body scan finds a bare `@_`, silently legitimizing it, instead of
rejecting.

## Why this is left open

Unlike the `do{}` case (a local, single-function fix reusing an existing
"die with a pre-built error" pattern), fixing the direct case means changing
what `apply_auto_positional_slurpy`/`_from_flag` DO on a positional hit — from
"insert a param" to "the method declaration is invalid" — and threading that
decision back out through every one of the 3 call sites (compiler main-pass,
two runtime registration-time walkers, in lockstep per the D3-8a/D3-9 byte-
parity guarantee) so each can raise the right error at the right point (a
method compiled inline in the main pass vs. a runtime "throwaway compile").
None of that is deep/architectural, but it is more than a single-function
patch, and no test currently exercises the direct-usage shape (the fixed
`t/placeholder-named-in-method-do.t` only covers the `do{}`-nested one) — so
it is being tracked here rather than folded into that session's change.

## Repro

```raku
class B { method m { @_.elems } }
B.new.m(1, 2, 3);   # raku: ===SORRY!=== compile error. mutsu: silently 3.
```
