# A `&`-sigil named parameter binds

`sub f(:&cb)` bound nothing. The parameter was always undefined, whatever the
caller passed:

```raku
sub f(:&cb) { say &cb.defined }
f(:cb({ 1 }));          # raku: True        mutsu: False
```

`:$cb` worked, `:@cb` and `:%cb` worked; only `&` was broken, and it was broken
in four independent places at once, which is why it survived so long:

1. **The parser dropped the sigil.** Every other sigil is kept on a parameter's
   recorded name (`@l`, `%h`), positionally *and* when named — but a named `&`
   param was special-cased in three copies of `param_inner.rs` to store a bare
   `cb`. The body reads `&cb`, so the name it was stored under could never
   match.
2. **Every named-key derivation stripped only `@`/`%`.** The caller's Pair key
   is the bare name, so `&cb` has to strip down to `cb` in the compiled call
   plan (`opcode.rs`) and in the interpreted binder
   (`types/binding_signature.rs`).
3. **The candidate checks did not recognize it either**, so a `multi` with a
   `:&cb` rejected the call outright with `Unexpected named argument 'cb'
   passed` (`types/args_matching.rs`, `types/binding_helpers.rs`, and the
   surplus-named check in `types/binding_signature.rs`).
4. **The read went through the env, which a slot-only bind never reaches.**
   `&cb` compiles to `GetCodeVar("cb")`, and `resolve_code_var` looks the name
   up in the env; the light call path binds a parameter into its local slot and
   mirrors it into the env only when the compile-time `needs_env_sync` scan says
   a by-name reader exists — a scan that does not know `GetCodeVar` reads the
   `&`-prefixed local. `exec_get_code_var_op` now checks the executing frame's
   own `code.locals` for a `&`-sigil lexical first, and falls back to the
   by-name routine resolution when there is no such local (so `&some-routine`
   is unaffected).

Two neighbouring approaches were tried and rejected, both because they widened
what a frame publishes by name and broke *positional* `&` parameters: teaching
`compute_needs_env_sync` about `GetCodeVar`, and resolving the slot at compile
time in `Expr::CodeVar` (a nested block shares the parent compiler's
`local_map`, so the slot index belongs to the wrong frame). Reading the slot at
runtime against the executing frame touches neither.

While fixing (4), `attr_twigil_base` turned out to strip `@`/`%` but not `&`,
so an attributive callable parameter (`submethod BUILD(:&!cb)`) stopped
reaching its attribute once the sigil was preserved. `&!x` / `&.x` are
attribute forms like any other and are recognized as such now.

This is what roast's `Test::Tap` is built on — `tap-ok` takes `:&emit`,
`:&done` and `:&after-tap` and calls each as `after-tap() if &after-tap` — so
every one of those guards was silently skipping. Pinned by
`t/named-callable-param.t`.
