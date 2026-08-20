# Pair argument named-ness is now a call-site property

The oldest open `todo/deep/` ticket — "a `Pair` held in a variable is passed
as a *named* argument" — is closed out. It described mutsu encoding argument
named-ness in the *value* (`ValueRepr::Pair` vs `ValueRepr::ValuePair`)
instead of deciding it from call-site syntax the way raku does, so any `Pair`
reaching a call through a variable, an array element, `.pairs`, or
`Pair.new` misbound as a named argument (`Cro::HTTP::Client`'s
`!set-headers` was the surfacing case).

This was already fully addressed by
[ADR-0021](../../docs/adr/0021-argument-namedness-is-a-call-site-property.md)
(P1-P4, merged 2026-08-08/09): P1 gave method calls the same
`ContainerizePair` normalization the function path already had, P2 fixed
slip/capture named-ness to follow the source container, P3 inverted the
minting default so `Pair.new`, a fat-arrow assigned to a variable, and
array-literal pairs all mint as positional, and P4 cleaned up stale
`ValuePair` doc comments and marked the ADR Accepted. The ADR's checklist was
left showing P4 unchecked; this entry also fixes that drift.

Verified against the ticket's own repro matrix on 2026-08-20 — every line now
prints `Pair`, matching raku:

```raku
class C {
    multi method m(Pair $p) { say "Pair" }
    multi method m(Str $s)  { say "Str"  }
}
my $c = C.new;

$c.m(Pair.new('a', 1));           # Pair
my $p = a => 1;      $c.m($p);    # Pair
my $q = :a(1);       $c.m($q);    # Pair
my @l = [a => 1];    $c.m(@l[0]); # Pair
my $r = (a => 1);    $c.m($r);    # Pair
```

Only ADR-0021 P5 (a measured perf follow-up — dropping the now-redundant
`ContainerizePair` on the function path where the compiler can prove no
`Pair` can result) remains open, gated on a bench-CI before/after number per
repo policy.
