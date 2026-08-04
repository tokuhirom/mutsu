# A routine declared in one body does not redeclare a sibling body's

```raku
sub run(&body) { body() }
run { sub f($x) { ... }; f(1) }
run { multi f($x) { ... }; f(2) }   # Redeclaration of routine 'f'
```

Two blocks, two scopes, two unrelated routines — but mutsu's routine registry is
keyed by *package* alone, so the second declaration saw the first still
registered and raised `X::Redeclaration`. The exemption for this
(`allow_lexical_shadow`) existed already, gated on `block_scope_depth > 0`,
which an inlined bare block sets and a body executed as a routine or closure
never does. The same failure therefore hit two sibling `sub` bodies as well.

The compiler already knows the answer statically: it compiles a routine or
closure body with its own `Compiler`, and its hoist pass marks the copies it
registers early as `__lexical_hoist`. What was missing is that the *in-sequence*
registration of the very same declaration carried no such mark. It does now —
`Compiler::mark_lexical_body` sets the flag when a body compiler starts, and the
`SubDecl` arm marks the declaration it registers.

Two things had to be kept true while widening it:

- **A conflict inside one body is still `X::Redeclaration`.** `mark_lexical_body`
  records which names that body declares in a conflicting way (not every
  declaration `multi`) and refuses to mark those. Deciding this from the body's
  statements rather than from compile order matters: each declaration is
  compiled twice (hoist pass, then in sequence), so an "is this the first one I
  have seen?" test marks the hoist copy and leaves the real one bare.
- **A `multi` that shadows a single takes the name over.** Registration already
  cleared a shadowed same-named entry for a plain `sub`; the `multi` case did
  not, so the sibling scope's `sub f` stayed registered and went on answering
  `f(2)` from inside the block whose own `multi f` had just been declared.

Found under the real `Test` module: `roast/S12-subset/subtypes.t` builds each
`group-of` as a subtest block, and two of them declare `test-pos` — one `sub`,
one `multi`. The file aborted at the second, losing 60 of its 92 assertions. It
passes under `MUTSU_REAL_TEST=1` now, and still passes under the native
provider.

Pin: `t/sibling-scope-routine-shadow.t` (all eight assertions verified against
`raku`).

This does not make routine declarations lexically scoped — a routine declared in
a body is still visible after that body returns, which `raku` rejects at compile
time. That is the larger change; this one fixes the redeclaration rule alone.
