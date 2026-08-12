# callsame from an override of a built-in Mu-level method (gist/Str/raku/new) returns Nil/Any instead of reaching the native implementation

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06). The
`native_mu_base_next_candidate` synthesized fallback and the `Mu.new` fallback
(`src/runtime/builtins_dispatch_next.rs`) do not deliver the native method's value to
`callsame`.

## Divergence

```raku
class C { method gist() { "custom+" ~ callsame } }
say C.new;
# raku:  custom+C.new          (callsame reaches Mu.gist)
# mutsu: "Use of Nil in string context" warning, then "custom+"

class D { has $.x; method new(|c) { my $obj = callsame; $obj } }
say D.new(x => 5).x;
# raku:  5                     (callsame reaches Mu.new, returns the built instance, .^name is D)
# mutsu: callsame returns Any; then "No such method 'x' for invocant of type 'Any'"
```

Same shape for `method raku()` and `method Str()` overrides (`E-str[E<...>]` in raku vs
`E-str[]` + Nil warning in mutsu). All four probes: `tmp/e9pre/n.raku`, `n2.raku`,
`probe-n3.raku` during the campaign (gitignored; shapes inlined above).

## Where to look

The four synthesized native fallbacks are force-pushed with empty `remaining` when the user MRO
is exhausted (`builtins_dispatch_next.rs:181-310` per the E8-E11 survey), and
`dispatch_next_candidate`'s search ends in a `Mu.new` fallback arm (:358-903). Two separate
symptoms: (1) for gist/Str/raku the native Mu-level implementation is either not invoked or its
value is dropped on the way back to `callsame` (Nil); (2) for `new` the fallback runs but the
constructed object is lost (Any comes back — check whether construction happens against the
wrong invocant or the return value is swallowed by the frame plumbing).

Sibling ticket for the same fallback family:
`todo/tickets/native-array-push-defer-fallback-broken.md` (array storage). ADR-0019 E9's cursor
design makes all four fallbacks ordinary sequence tail entries — fix here or re-verify at that
boundary, whichever lands first.

The E9-pre pins for these land with the fix.
