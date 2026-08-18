# A method reading bare `@_` directly is now rejected, matching Raku

Real `raku` only auto-adds `*%_` to a signature-less method, never `*@_`:

```
$ raku -e 'class A { method m { @_.raku.say } }'
===SORRY!=== Placeholder variables (eg. @_) cannot be used in a method.
Please specify an explicit signature, like method m (*@_) { ... }
```

A prior session fixed the `do{}`-nested shape (`method m { do { @_ } }`).
Still open until now: a bare `@_` referenced *directly* in a method body (no
`do{}` in between) was silently auto-slurped —
`class B { method m { @_.elems } }; B.new.m(1, 2, 3)` returned `3` under
mutsu instead of erroring.

## Fix

`method_signature_shared::needs_direct_positional_placeholder_die`/
`_from_flag` (formerly `apply_auto_positional_slurpy`/`_from_flag`, which
used to *insert* an implicit `*@_` — simply wrong per Raku semantics) now
detects the same shape but, instead of inserting the slurpy silently,
swaps the method's compiled/registered body for a synthetic
`X::Placeholder::Block` die (`direct_positional_placeholder_die_body`,
reusing the `placeholder_scope_error` builder the `do{}`-nested and class/
role-body-level sibling checks already share — moved into
`method_signature_shared.rs` so both the compiler and the runtime
registration walkers can call it). The implicit `*@_` is still inserted
into the param defs so the method accepts any call arity — the die is what
the caller observes regardless of how many arguments they passed, instead
of a less informative arity-mismatch error.

Wired into all four call sites: `Compiler::compile_method_body` (main-pass),
`class_body_method_decl` (×2 — the effective param defs and the
byte-parity key snapshot), and the class-augment registration walker —
matching the ADR-0019 D3-8a/D3-9 main-pass/registration-time parity
guarantee the existing `d3_8a_byte_parity_tests` suite pins.

Role methods are unaffected (`role_body_method_decl` never opted into this
auto-detection to begin with, pinned by the existing
`role_method_auto_positional_slurpy_not_applied` test) — a role method's
direct `@_` usage already surfaced as an arity mismatch, unchanged.

## Side finding

Writing the regression test surfaced a separate, pre-existing gap:
`EVAL`'s undeclared-variable static pre-check
(`check_eval_undeclared_vars`) doesn't know methods get an implicit
`*%_`/`*@_` at all, so `EVAL(q[class D { method m { %_.elems } }; D.new.m(a=>1)])`
raises `X::Undeclared` even though the same code runs fine outside `EVAL`.
Filed separately:
`todo/tickets/eval-undeclared-check-blind-to-implicit-method-slurpy.md`.

## Tests

`t/method-direct-underscore-rejected.t` (new) — direct `@_` in a class-body
method (called with 0 args and with args), a submethod, `%_` alone still
working, an explicit signature opting out, the `do{}`-nested sibling shape,
a plain sub still auto-getting `@_`, and a role method's (unaffected)
behavior. Uses `throws-like`'s block form to avoid the `EVAL`
pre-check gap above.

A byte-parity test fragility also surfaced and was fixed alongside this:
`d3_8a_byte_parity_tests::auto_positional_slurpy_method_byte_parity`
compares `Debug`-formatted `CompiledCode` between two independent compiles
of the same source; embedding an instance value (this fix's die body) in
that comparison exposed that its attribute `HashMap`'s iteration order
depends on each compile's own `Symbol`-interning history, not just its
content. Fixed by extending the test's existing ID-normalization helpers
with one that sorts `AttrMap({...})` entries before comparing.

PR [#6606](https://github.com/tokuhirom/mutsu/pull/6606).
