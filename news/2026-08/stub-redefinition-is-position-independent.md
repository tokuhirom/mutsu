# Redefining a yada stub no longer depends on file position

Raku allows redefining a stub routine without `supersede`:

```raku
sub lightning {...}
sub lightning {42}
say lightning();   # raku: 42 — mutsu died with X::Redeclaration
```

mutsu died on this at the top level, in a bare block, and with statements
between the pair — yet the identical shape passed inside
`t/stub-and-supersede.t`, because a *later* block's `use MONKEY-TYPING`
changed the registration behavior of the earlier, unrelated block
(hoist/pragma state applied file-globally).

Root cause: `RegisterSub` executes both hoisted at block top and in place.
The hoist pass registers the stub, then the real definition replaces it
(allowed — the existing def is a stub). When the stub's *in-place* copy then
re-executes, the registry holds the non-stub definition, so the guard raised
`X::Redeclaration` — the same declaration site erroring on its own
re-arrival.

Fix: registration remembers each yada-stub declaration site as
`(fully-qualified name, compile-time site fingerprint)`
(`registered_stub_decl_sites`). The redeclaration guard treats a stub whose
own site is already recorded as an idempotent no-op, while a textually NEW
stub after a definition — a different site, hence a different fingerprint —
still raises `X::Redeclaration` exactly like raku. Structurally identical
stubs at different lines are recognized as `same` by the line-insensitive
`registration_identity` early-return, so that path records the arriving
site's fingerprint too (the double-stub-then-define shape).

Pinned by `t/stub-redefine-position.t` (verified against raku).
