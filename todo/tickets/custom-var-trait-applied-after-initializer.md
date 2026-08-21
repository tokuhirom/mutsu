# `my %h is CustomTrait = initializer` applies the trait AFTER the initializer, so the trait's mixed-in `STORE` never sees `:INITIALIZE`

## Symptom

For a declaration like `my %h1 is restricted = a => 42, b => 666` (from
`Hash::Restricted`), mutsu compiles/executes the initializer assignment
*before* the `is restricted` trait mixes the `restrict-current` role into
`%h1`. By the time the role's `STORE` override could run (with
`:initialize(:$INITIALIZE)`, which the role uses to auto-populate its
`%!allowed` keys from the initial data), `%h1` is already populated via a
raw, non-dispatched store — the role's `STORE` method never runs at all for
the declaration's own initializer.

Confirmed via a diagnostic instrumented copy of `Hash::Restricted`'s role
(`say` added to `STORE`'s first line): for
`my %h1 is restricted = a => 42, b => 666;`, **no `STORE` diagnostic prints
at all**, even though the real dist's role only auto-populates `%!allowed`
inside `STORE`'s `if $INITIALIZE { ... }` branch. Real raku does call
`STORE` at declaration time (verified: it must, since `Hash::Restricted`'s
design depends on it and the dist's own test suite passes 32/32 under real
raku).

## Root cause

`src/compiler/stmt.rs`'s `VarDecl` compilation (~line 1746 `SetLocal`, ~line
1889 `ApplyVarTrait`) always emits the initializer's `SetLocal` **before**
the trait's `OpCode::ApplyVarTrait`, for every ordinary declaration. This is
deliberate for several *other* traits (e.g. `is default(...)`, whose own
comment at compiler/stmt.rs ~line 1709-1719 explains it must run AFTER the
store so a runtime-`Nil` initializer value survives before the default
trait's own handling replaces it) — so this ordering is not simply a bug to
flip globally; several existing behaviors depend on it.

`src/vm/vm_var_trait_ops.rs`'s `exec_apply_var_trait_op` already has a
**working compensation for one specific case**: when the trait name IS
itself a registered class/role with a `STORE` method (the `is ClassName`
form, e.g. `my %h is SomeRoleWithStore = ...`), it explicitly (lines
~500-559) gathers whatever `SetLocal` already stored via
`read_local_slot_or_name`/`get_env_with_main_alias`, constructs the
class/role instance, binds it to the variable, then **re-feeds the gathered
initializer values through `STORE` with `:INITIALIZE`**. That mechanism does
NOT apply to `Hash::Restricted`'s case, because `"restricted"` is a custom
*trait name* dispatched through `trait_mod:<is>` (a user-declared multi sub),
not itself a registered class/role name — the `is-ClassName` special block's
own guard (`self.registry().classes.contains_key(&trait_name) ||
self.registry().roles.contains_key(&trait_name)`) never matches.

## What's needed

Generalize the SAME re-feed-through-STORE mechanism to the generic
`trait_mod:<is>`-dispatch path (the `else` branch below the is-ClassName
block, ~line 582 onward in `vm_var_trait_ops.rs`, which is where
`todo/deep/trait-mod-does-not-callable-sub.md`'s `trait_mod:<does>` writeback
now also lives — see `news/2026-08/trait-mod-does-callable-sub.md`): after
`trait_mod:<is>` returns and any writeback is applied, if the FINAL value
bound to the variable is a `Mixin` whose composed role(s) declare a `STORE`
method, gather the already-`SetLocal`-stored raw value (same
`read_local_slot_or_name`/env read the is-ClassName block already does) and,
if non-empty, call `.STORE(raw_value, :INITIALIZE)` on the mixed value,
writing the result back the same way (`write_local_slot_or_name` +
`set_env_with_main_alias`).

This is a genuinely new, moderate-sized addition (not a one-liner): it needs
to detect "does this Mixin's role declare STORE" (a role-method lookup, not
just a class-name check like the is-ClassName block does), and to be careful
not to regress the `is default(...)`/other trait timing subtleties already
documented in `compiler/stmt.rs`.

## Blast radius

Confirmed affecting `Hash::Restricted`'s entire `restrict-current`
(non-parametric) branch — without this fix, `%!allowed` never gets populated
from the initial keys, so EVERY subsequent key access is (correctly, per two
other fixes landed alongside this ticket — see
`news/2026-08/trait-mod-does-callable-sub.md`) rejected as "not an allowed
key", including the keys that SHOULD be allowed. This is likely a
general pattern for any dist using the `Variable:D \v` + custom `is` trait +
role-with-`STORE`-mixin idiom with a `my %h is Trait = ...` initializer —
`Injector` (see the `trait_mod:<does>` ticket's own corpus note) uses the
same `Variable:D` + `.var` idiom but the `does` *operator* form rather than
`trait_mod:<is>` combined with an initializer, so it may or may not hit this
specific gap; not verified either way.

## Discovered via

Investigating `todo/deep/trait-mod-does-not-callable-sub.md` (now resolved —
see `news/2026-08/trait-mod-does-callable-sub.md`), while getting
`Hash::Restricted`'s actual 32-subtest suite running as far as possible.

## Repro

```raku
my role R {
    method STORE(\to_store) {
        say "R.STORE called with {to_store.raku}";
        callsame;
    }
}
multi sub trait_mod:<is>(Variable:D \v, Bool:D :$restricted!) is export {
    trait_mod:<does>(v, R) if $restricted;
}
my %h is restricted = a => 42, b => 666;
```
mutsu: no "R.STORE called" line prints at all (STORE never runs for the
declaration's own initializer). raku: prints "R.STORE called with
(:a(42), :b(666))".
