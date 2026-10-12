# A bare-name multi call ranks every arity in one set of tiers

A bare-name multi call used to try its candidates in stages: the ones
registered under the call's exact arity first, then those with optional or
slurpy parameters, then the slurpy ones, then every arity. Each stage was
tiered on its own, so a narrower candidate with an optional parameter could
not compete with an exact-arity one that also bound the call:

```raku
multi a(Cool $x)     { 'exact-Cool' }
multi a(Int $x, $y?) { 'opt-Int' }
say a(1);   # rakudo: opt-Int   mutsu before: exact-Cool
```

The dispatch plan now gathers every candidate of the name once, deduplicated
by fingerprint, and sorts the whole set into rakudo's topological tiers
(#12795). The stage heuristics that only approximated the tiers are gone: the
four exact-arity fast-path guards and the rule that kept a sub-signature
capture out of the optional stage. Two rank fixes replace what they did:

- An optional positional that a zero-argument call does not reach no longer
  counts as a typed position, so `multi c()` beats `multi c(Int $a?)` for
  `c()` in either declaration order.
- A capture with a sub-signature (`multi d(|c($x))`) ranks as rakudo sees
  it, a slurpy capture with a bind check and no declared positional. So
  `multi d($n)` beats it and it beats `multi d(*@a)`.

The dispatch program for a plan leaves out the candidates that cannot take
the call's arity, so a family with other arities keeps its fast path.
