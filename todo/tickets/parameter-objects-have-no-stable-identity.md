# `Parameter` objects have no stable identity

`Signature.params` builds a fresh `Parameter` on every access, so a role mixed
into one dies with that materialization:

```raku
role Q { }
sub f(:$mp) { }
my $p = &f.signature.params[0];
$p does Q;
say $p ~~ Q;                       # True  (does mutates the object now)
say &f.signature.params[0] ~~ Q;   # raku: True   mutsu: False
```

`$sig.params[0] === $sig.params[0]` is False for the same reason.

The *trait* case this ticket was originally filed for is fixed — see
`news/2026-08/param-trait-mixin-persists.md`: `check_param_custom_traits` records
the type a custom trait reblesses its `Parameter` into, and every later
materialization of a parameter carrying that trait is born as that type. That
replays the trait's effect on the parameter and nothing else its body did, and it
is what Cro::HTTP::Router needs. What is left is the honest version below.

## Root cause

There is no stored `Parameter` object to mix into. `Signature.params` is built
by `make_params_value_with_owner` (`src/value/signature.rs`), which maps each
`SigParam` through `sig_param_to_parameter_instance` — a fresh `Instance` with
attributes derived from the `ParamDef`, constructed on every access. A mixin
applied to one of those instances now sticks to *that* instance, but the next
access materializes a new one. The declaration-time trait
dispatch added in `check_param_custom_traits`
(`src/vm/vm_register_sub_ops.rs`) has the same problem from the other side: it
builds a throwaway `Parameter` to hand to the candidate.

## Why it matters

Any code that mixes into, or otherwise mutates, a `Parameter` it read out of a
signature loses the change on the next read. The trait replay above covers the
common case, but it is keyed on the trait *name*, so a trait that composes
different roles depending on the parameter it is applied to gets the last one
recorded, and a trait body that does anything other than `does` is not replayed
at all.

## Sketch of a fix

Either (a) record the applied roles back onto the `SigParam` at declaration
time and re-mix them whenever a `Parameter` instance is materialized — cheap,
and correct for the `does`-only traits real modules write, but it re-runs
nothing else the trait body did; or (b) materialize each signature's `Parameter`
objects once, cache them alongside the registered `SigInfo`
(`register_sig_info` already keys on the Signature instance id), and hand out
the cached values so a mixin sticks. (b) is the honest version.

Route (a) is what shipped. Route (b) is still the one that makes
`$sig.params[0] === $sig.params[0]` hold, which it currently does not, and it is
what would remove the per-trait-name keying above.
