# Mixin role method-name collisions now resolve by application order, not alphabetically

```raku
role A { method m { "A" } }
role Z { method m { "Z" } }
my $x = (0 but A) but Z;
say $x.m;   # raku: Z (later-applied wins)   mutsu (before): A (alphabetically first)
```

Rakudo's documented mixin semantics: the most-recently-applied role wins a
method-name collision — a later `but`/`does` layers over an earlier one.
`Value::Mixin(inner, mixins)` stores composed roles as a plain
`HashMap<String, Value>` keyed `__mutsu_role__{Name}`, which carries no
record of *when* a role was applied relative to the others in the same
layer. `dispatch_mixin_method_call` resolved a collision by walking
`role_names.sort()` — alphabetical order — so the answer depended on role
name spelling rather than application order; ADR-0019 Phase E's `mixin_chain`
classifier mirrored the same (wrong) alphabetical order deliberately, so it
would not diverge from `dispatch_mixin_method_call`'s existing decision.

Fixed by stamping a monotonic application-order sequence number
(`__mutsu_role_seq__{Name}`, reusing the existing global instance-id counter)
at every site that composes a role onto a mixin value (`does`/`but`, role
puns, parameterized-role puns, `has $.x does Role` attribute traits, and the
routine-mixin-rebuild path, which preserves an already-stamped sequence
rather than re-stamping on every rebuild). `dispatch_mixin_method_call` and
`mixin_chain` now sort by that stamp (most-recently-applied first) instead of
by name.

The stamp is pure bookkeeping, not part of a value's structural identity, so
`eqv`/`is-deeply` now explicitly ignore `__mutsu_role_seq__` entries when
comparing two mixin maps — otherwise two independently-built values with an
identical composition (e.g. two `(1..5) but Meows` expressions) would compare
unequal purely because each got a different sequence number.

Pin: `t/mixin-role-application-order.t` (four two-role collision cases in
both name-orderings, plus an `eqv` regression case for the bookkeeping-key
exclusion).
