# `trait_mod:<does>` is now a callable sub, with writeback to the caller's variable

`trait_mod:<does>` — Raku's callable form of the `does` mixin operator, a
real always-present CORE.setting sub — did not exist as a callable-by-name
sub in mutsu at all. Calling it directly (`trait_mod:<does>($x, SomeRole)`,
the idiom `Hash::Restricted` and `Injector` use inside a custom
`trait_mod:<is>` handler to mix a role into a *declared variable's* value at
`is`-trait time, e.g. `my %h is restricted = ...`) always failed with
`Unknown function: trait_mod:<does>`, blocking any dist using this pattern.

## What changed

Three real CORE.setting overloads (verified against actual `raku`, including
its own multi-dispatch ambiguity behavior) are now registered as a genuine,
real-Raku-source prelude — `TRAIT_MOD_DOES_PRELUDE` in
`src/runtime/run.rs`, injected via
`src/runtime/run_prelude.rs::inject_trait_mod_does_prelude` (gated on the
literal name appearing in source, the same pattern the NativeCall/Rational
preludes already use) — rather than as hand-rolled Rust signature-matching
logic:

```raku
multi sub trait_mod:<does>(Variable:D \v, Mu:U $role) is export { ... }
multi sub trait_mod:<does>(Attribute:D $a, Mu:U $role) is export { ... }
multi sub trait_mod:<does>(Mu $doee, Mu $role) is export { ... }
```

Registering these as real, parsed `multi sub` `FunctionDef`s (not a native
Rust fallback tier) is what makes a user's own colliding `multi sub
trait_mod:<does>` candidate participate in the SAME
`choose_best_matching_candidate` narrowness/ambiguity engine ordinary
multi-dispatch uses — a native-only fallback is tried only when no compiled
candidate matches at all, so it could never collide. Verified against the
ticket's own minimal repro: declaring a second, colliding
`multi sub trait_mod:<does>(Mu \v, Mu \r)` now raises
`Ambiguous call to 'trait_mod:<does>(...)'`, matching real raku's own
behavior for the same repro (which fails the same way against its builtin).
The class-level overload's constraint is written `Mu $doee` rather than
Rakudo's `Mu:U $doee` — mutsu's narrowness ranking treats any smiley'd
constraint as "meaningfully typed" even over the universal `Mu`, which would
make this candidate always beat an untyped user candidate instead of tying
with it (see the doc comment on `TRAIT_MOD_DOES_PRELUDE` for the full
tradeoff).

Each candidate delegates to a small native primitive,
`__mutsu_trait_mod_does_apply` (`src/vm/vm_trait_mod_does_ops.rs`, wired into
`vm_call_func_ops.rs`'s native-fallback chain the same way
`__mutsu_cglobal_fetch`/`nativecast` already are), which performs the mixin
via the same `vm_does_values` the `does` operator itself uses (now
`pub(super)` instead of private). For the `Variable:D` overload specifically,
it resolves the reflected variable's *live* value by name
(`Interpreter::var_target_from_meta_value`, widened to `pub(crate)`) and
writes the mixed result straight back into that `env` slot — which is what
makes an immediate same-handler re-read (`v.var` again) already see the
mixed value.

**Writeback to the ORIGINAL CALLER's variable** — several frames further up,
at the `my %h is restricted = ...` declaration site — reuses the existing
`trait_mod_writeback_key`/`trait_mod_writeback_value` relay
(`src/runtime/mod.rs`), previously armed only around a Routine's
`trait_mod:<is>` dispatch (`registration_sub.rs`, for `sub foo(...) is
SomeTrait { ... }`). `vm_var_trait_ops::exec_apply_var_trait_op` now also
arms it around the Variable-trait dispatch (keyed to the plain variable name)
and, after the call returns, drains it using its own `code`/slot context to
perform the real local-slot + env write — generalizing a Routine-only
mechanism to the Variable case, exactly as scoped.

## Two general bugs found and fixed along the way

Both were found while getting `Hash::Restricted`'s actual dist test suite
running, and both are genuine, general interpreter bugs (not
`trait_mod:<does>`-specific), verified against real `raku`:

1. **`nextsame`/`callsame` inside a role mixed into a builtin value
   (`%h does R`) silently returned `Nil` instead of falling through to the
   real native method** (`Hash::AT-KEY`, etc.). `dispatch_mixin_method_call`
   only builds a "next candidate in MRO" chain when the mixin's inner value
   is a user-declared `Instance` (it needs a registered class to look
   `MethodDef`s up in) — a native `Hash`/`Array`/`Str` inner has none, so
   `nextsame` fell through the whole dispatch chain to the generic
   "exhausted MRO" `Nil`. Fixed with a new fallback,
   `native_mixin_base_next_candidate` (`src/runtime/builtins_dispatch_next.rs`,
   mirroring the existing `native_array_storage_next_candidate` bridge for
   an `is Array` subclass), wired into `dispatch_next_candidate`'s fallback
   chain at both call sites.
2. **A genuine exception thrown by a mixed-in role's `AT-KEY` override was
   silently swallowed into `Nil` by the `%h<key>` subscript sugar**
   (`src/vm/vm_var_index_ops.rs`'s `(ValueView::Mixin(..), ValueView::Str(key))`
   arm used `.unwrap_or(Value::NIL)` on the `AT-KEY` method-call result,
   discarding `Err` the same as a legitimate `Ok(Nil)`). Changed to `?`, so
   a genuine die propagates while a legitimate `Nil` return still falls back
   to the container's typed default. This is what made `Hash::Restricted`'s
   whole restriction mechanism (`dies-ok { %h<c> }` for a disallowed key)
   possible to observe correctly at all.

## Remaining gaps (filed as new tickets, not attempted this session)

Getting `Hash::Restricted`'s actual 32-subtest suite
(`t/01-basic.rakutest`) to fully pass surfaced three more, genuinely
separate pre-existing bugs, each filed as its own ticket:

- `todo/tickets/custom-var-trait-applied-after-initializer.md` — the
  compiler always emits a `VarDecl`'s initializer `SetLocal` *before* its
  `ApplyVarTrait`, so a custom `is Trait = initializer` declaration's trait
  handler never gets a chance to intercept the initial `STORE` call (with
  `:INITIALIZE`) the way `Hash::Restricted`'s `restrict-current` role
  depends on to auto-populate its allowed-keys set. This is the SOLE
  remaining blocker for `Hash::Restricted`'s `restrict-current`
  (non-parametric, `%h1`) branch — a well-understood, moderate-sized fix
  (generalizing the existing is-ClassName-with-STORE special-case in
  `vm_var_trait_ops.rs` to the generic `trait_mod:<is>` path), deliberately
  not attempted this session given its own architectural footprint (touches
  declaration/initializer compile ordering shared by every `VarDecl`).
- `todo/tickets/is-trait-angle-bracket-arg-not-parsed.md` — `is
  TraitName<a b>` argument sugar (a bareword word-list immediately after a
  trait name) is not parsed at all; `<a b>` is misparsed as an unrelated,
  discarded statement. Blocks `Hash::Restricted`'s `restrict-given`
  (parametric-role, `%h2`) branch entirely.
- `todo/tickets/set-name-on-builtin-type-package-no-op.md` — `.^set_name` on
  a builtin type's `.WHAT` (e.g. `Hash`) is a no-op; blocks 2 cosmetic
  subtests (`%h.^name.ends-with('(restricted)')`) that don't affect the
  dist's core restriction behavior.

With the `trait_mod:<does>` fix plus the two general bug fixes above,
`Hash::Restricted`'s test suite goes from **0/32 (immediate crash)** to
running the full file and correctly, deterministically rejecting disallowed
keys once the declaration-order gap above is also closed — that gap is the
one remaining item standing between mutsu and `Hash::Restricted`'s full
32/32.

## Regression test

`t/trait-mod-does-callable.t` covers: the ambiguity-collision repro
(confirming the builtin exists and multi-resolves against a colliding user
candidate, matching real raku's own error for the same repro); a
non-colliding, more-specific user extension candidate coexisting with the
builtin (the `WWW::GCloud::API` pattern); the writeback-to-caller-variable
behavior with a `Variable:D` handler mirroring `Hash::Restricted`'s actual
shape; and the `nextsame`-through-a-native-mixin fix.
