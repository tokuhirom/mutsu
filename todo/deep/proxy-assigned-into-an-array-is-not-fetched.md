# A `Proxy` assigned into an Array is stored, not FETCHed — and a same-named lexical flips the loop that compensates

## Deep triage — 2026-09-01

This is a store-semantics campaign owned by ADR-0040, not an ADR-0045 loop
slice. The reproduced mismatch covers real-Array construction, whole-array
assignment, `push`, and indexed assignment; the same invariant must also be
defined for the remaining mutator and nested-store paths. Establish one
VM-level real-Array store boundary that FETCHes a non-decontainerized `Proxy`
before itemization, and route every assignment path through it. Do not retain
or extend `for`-loop auto-FETCH compensation as the mechanism.

## Symptom

Raku FETCHes a `Proxy` when it is assigned into an `Array`, so the element that
lands is a plain value. mutsu stores the `Proxy` itself:

```
$ raku  -e 'my $n = 5; my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }); say @a.raku'
[5]
$ mutsu -e 'my $n = 5; my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }); say @a.raku'
[Proxy]
```

mutsu compensates for that *inside the `for` loop*: `exec_for_loop_body`
auto-FETCHes a `Proxy` item for a non-`is rw` loop, and ADR-0045 §5 Q6 leaves it
alone for an `is rw` one (`vm_for_loop_body.rs`, the `auto_fetch_proxy` arm).
The compensation is fragile — **an unrelated same-named lexical anywhere in the
compilation unit changes the answer**:

```raku
# fine on its own
{
    my $n = 5;
    my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    for @a -> $x is rw { $x = 42 }
    say "n=$n";                              # raku 5, mutsu 5
}
```

```raku
{ my $x = 1; }                               # <-- add ONLY this
{
    my $n = 5;
    my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    for @a -> $x is rw { $x = 42 }
    say "n=$n";                              # raku 5, mutsu 42  <-- STORE fired
}
```

Any same-named declaration does it — a plain `my $x`, or the `my $x := …` that a
chunked rw multi-parameter loop now emits. It reproduces on `main`; it is not
caused by ADR-0045's element-alias routing.

## Why it matters here

`t/for-loop-element-alias.t`'s two ADR-0045 §5 Q6 rows pass only because no
other `$x` exists in that file. The `.kv` slice (ADR-0045 row 16) had to name its
new multi-parameter test's parameters `$p`/`$q` rather than the natural `$x`/`$y`
for exactly this reason, with a comment pointing here. That is a landmine: the
next person to write `$x` in that file gets a failure that has nothing to do with
what they changed.

## Where to start

The honest fix is the **assignment**, not the loop: `my @a = Proxy.new(...)`
should FETCH, so the element is a plain value and there is nothing for the loop
to compensate for. `t/for-loop-element-alias.t`'s own Q6 comment already states
that as the model ("Assigning a Proxy into an Array FETCHes it, so the element is
a plain value and the loop must write the ARRAY, never the Proxy's STORE
target") — mutsu just never implemented it and papered over it downstream.

Once the store FETCHes, check whether ADR-0045 §5 Q6's `auto_fetch_proxy` carve-
out in `vm_for_loop_body.rs` is still needed at all, or only for a `Proxy` that
reaches a loop by some other route (`for $proxy-list.list`, which
`t/proxy-list-transparency.t` pins). Note that `Proxy` in a *scalar* (`my $p :=
Proxy.new(...)`) must keep its container semantics — this is about what an
`Array` element assignment does, not about `:=`.

Then delete the `$p`/`$q` workaround comment in `t/for-loop-element-alias.t` and
rename those parameters back to `$x`/`$y`, which is what makes the row read
naturally.

## Reproduce

The two snippets above, no fixtures.
