# `trait_mod:<does>` is not a callable sub

## Symptom

`Hash::Restricted`'s test suite (`roast/`-style dist sweep, un-triaged
`test_die` row in
[todo/tickets/dist-test-suite-failures-batch.md](../tickets/dist-test-suite-failures-batch.md))
dies immediately on `use Hash::Restricted;`:

```
Unknown function: trait_mod:<does>
  in sub trait_mod:<is> at lib/Hash/Restricted.rakumod line 75
```

Raku: loads and all 32 subtests pass.

## What the dist needs

`lib/Hash/Restricted.rakumod` defines a custom `is restricted` trait that
dynamically mixes a role into the DECLARED VARIABLE's type (not an instance)
at `my %h is restricted = ...` declaration time:

```raku
multi sub trait_mod:<is>(Variable:D \v, Bool:D :$restricted!) is export {
    die "..." unless v.var.WHAT ~~ Map;
    my $name = v.var.^name;
    if $restricted {
        trait_mod:<does>(v, restrict-current);   # <-- calls trait_mod:<does> as a plain sub
        v.var.WHAT.^set_name("$name\(restricted)");
    }
}
```

## Investigation (2026-08-20): the gap is narrower than originally thought

The original write-up assumed mutsu had **no** `Variable` MOP at all. That
turned out to be wrong — mutsu already has substantial, working machinery
here:

- `Variable:D \v` as a capture-parameter type constraint already matches
  (`type_matching.rs`, `varref_from_value`), **and** the bound `v` is a real
  reflective object (produced by the existing `.VAR`/"var-reflect" machinery
  in `methods_mut_dispatch.rs`, driven from `exec_apply_var_trait_op` in
  `vm/vm_var_trait_ops.rs`) — not just a raw pass-through value.
- `v.var` (the accessor the real dist calls) already resolves to the
  underlying container correctly. Verified directly:

  ```raku
  multi sub trait_mod:<is>(Variable:D \v, Bool:D :$restricted!) {
      say "isa Map: {v.var.WHAT ~~ Map}";     # mutsu: True (matches raku)
      say "var.^name: {v.var.^name}";          # mutsu: Hash (matches raku)
  }
  my %h is restricted = a => 1;
  ```
  Both lines match raku's output exactly.
- Instance-level `does` mixin (the `$obj does Role` operator) already works
  and already dispatches subsequent method calls (e.g. `AT-KEY`) through the
  mixed-in role for a built-in `Hash` — see `vm/vm_mixin_does_ops.rs`
  (`exec_does_op`/`exec_does_var_op`/`vm_does_values`). Verified:
  `my %h = a=>1,b=>2; %h does SomeRoleWithAtKey; %h<a>` correctly calls the
  role's `AT-KEY` override.
  - Found a smaller, separate bug while testing this: `nextsame` inside a
    mixed-in role's `AT-KEY` does not fall through to the real `Hash.AT-KEY`
    (returns `Nil` instead of the stored value) — raku returns the value.
    Not filed as its own ticket yet since it wasn't the focus of this
    session; worth a follow-up.
- mutsu already has a `trait_mod_writeback_key`/`trait_mod_writeback_value`
  mechanism (`runtime/mod.rs`, wired in `registration_sub.rs` and consumed in
  `vm_mixin_does_ops.rs`) that propagates a `does` mixin performed *inside* a
  `trait_mod:<is>` handler back to the caller's variable — but it is
  currently wired **only** for the `&sub`/Routine case (`is` traits on
  subs), not the Variable/`my %h is restricted` case.

**The actual remaining gap is narrow and specific**: `trait_mod:<does>` does
not exist as a callable-by-name multi sub at all — calling it as a plain
function (`trait_mod:<does>(v, role)`, as `Hash::Restricted` and other dists
do) always raises "Unknown function", regardless of argument types.
Confirmed this is a real CORE.setting sub in actual Raku, not something the
dist itself provides — declaring a *second* candidate for it against real
raku produces a multi-dispatch ambiguity against an existing builtin:

```
$ raku -e '
multi sub trait_mod:<does>(Mu \v, Mu \r) is export { }
my $x; trait_mod:<does>($x, Int);
'
Ambiguous call to '"'"'trait_mod:<does>(Any, Int)'"'"'; these signatures all match:
  (Mu:U $doee, Mu:U $role) from SETTING::src/core.c/traits.rakumod line 346
  (Mu \v, Mu \r) from -e line 2
```

So the missing piece is specifically: **register `trait_mod:<does>` as a
callable builtin** that performs the same operation as the `does` operator
(reusing `vm_does_values`), **and** extend the writeback mechanism above (or
build an equivalent) so a mixin performed through the function-call form
propagates back to the caller's variable the same way the `does` *operator*
form already does — not a full "no Variable MOP, no does-as-sub at all"
build from scratch as originally scoped.

## Part A priority triage (2026-08-20)

Grepped a fresh ~400-dist random sample of the fez ecosystem corpus (see
the sibling P5tie ticket's "Corpus method" section for how it was fetched —
the original sweep's cache was gone) for `Variable:D` capture-parameter
usage and `trait_mod:<does>` usage.

**Result: not single-dist.** Besides `Hash::Restricted`, the `Injector`
dist independently uses the *exact same* pattern — `Variable:D $v` +
`.var` + mixing a role into a declared variable:

```raku
# Injector/lib/Injector.rakumod
multi trait_mod:<is>(Variable:D $v, Bool :$injected!) {
    trait_mod:<is>($v, :injected{})
}
multi trait_mod:<is>(
    Variable:D $v,
    :%injected! (...)
) {
    $v does Injector::Injected::Variable;          # `does` operator form, not a call
    create-bind $v, :type($v.var.WHAT), |%injected
}
```

(`Injector` uses the `does` *operator* form, `$v does Role`, rather than
calling `trait_mod:<does>(...)` directly — so `Injector` alone would not
need the callable-sub gap fixed, only `Hash::Restricted` calls it
explicitly. But it confirms the `Variable:D` + `.var` + variable-level
`does`-mixin pattern itself is a real, if uncommon, idiom beyond this one
dist — not a one-off.)

Separately, `WWW::GCloud::API` declares its own **additional** multi
candidate extending `trait_mod:<does>` (a different, class-level signature,
`(Mu:U \doee, WWW::GCloud::API:U \r)`) — evidence that real Raku code
expects `trait_mod:<does>` to be a genuinely extensible multi, not just a
single hardcoded builtin. A correct fix should let a user-declared
`multi sub trait_mod:<does>` candidate coexist with the builtin one (the way
`trait_mod:<is>` already works per `has_proto("trait_mod:<is>")` /
`has_multi_candidates("trait_mod:<is>")` checks in `vm_var_trait_ops.rs`),
though `Hash::Restricted` itself does not need that extensibility.

**Verdict: worth doing, but not a quick patch.** The gap is real (confirmed
against actual raku behavior) and has more than one real ecosystem user, but
a *correct* fix touches: (1) registering `trait_mod:<does>` as a callable
builtin multi candidate, (2) making it coexist with user-declared extending
candidates the way `trait_mod:<is>` already does, and (3) generalizing the
existing `trait_mod_writeback_key`/`value` mechanism (currently
Routine-only) to the Variable case so the mixin performed through the
function-call form actually reaches the caller's `%h`. That is a genuine,
scoped feature slice — bigger than a one-line fix, smaller than the
EXPORTHOW::DECLARE/`monitor` MOP campaign this ticket was originally
compared to (no new MOP subsystem is needed; the `Variable` reflection and
`does`-mixin machinery already exist and already work, per the investigation
above). **Recommended as the next slice for a session that can budget real
implementation + testing time** (this session budgeted the investigation
plus a separate, unrelated parse-bug fix in the sibling P5tie ticket, and
did not have room left to implement and thoroughly test this one against
`Hash::Restricted`'s actual 32-subtest suite). Deferred, not attempted this
session.

## Repro

```raku
class Foo { }
multi sub trait_mod:<does>(Mu \v, Mu \r) is export {
    say "would mix {r.^name} into {v.VAR.name}";
}
my $x;
trait_mod:<does>($x, Foo);
```

mutsu: `Unknown function: trait_mod:<does>`. Verified against raku
(2026-08-20): raku does NOT run this cleanly either, but for a *different*
reason — it reports `Ambiguous call to 'trait_mod:<does>(Any, Foo)'`
because `trait_mod:<does>(Mu:U $doee, Mu:U $role)` already exists as a
CORE.setting builtin and this repro's own candidate collides with it. That
ambiguity (not "Unknown function") is itself proof the builtin exists in
real Raku and must exist in mutsu too — see "Investigation" above for the
real dist's actual (non-colliding, `Variable:D`-typed) signature and the
narrower, verified gap.
