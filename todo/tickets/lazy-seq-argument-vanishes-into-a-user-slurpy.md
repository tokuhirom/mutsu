# A lazy Seq passed to a user `*@a` slurpy arrives empty or as one element

Passing a lazy `Seq` — from `.map`, `.grep`, the sequence operator, or `gather`
— to a **user-defined** slurpy parameter silently loses the elements. No error,
no warning: the routine simply sees an empty or one-element list.

**`.grep` is a REGRESSION that landed on 2026-09-07 and is on `main`.**

## Repro

```
sub f(*@a) { say @a.raku };  f((1..3).map(*+0));            # mutsu []   raku [1, 2, 3]
sub f(*@a) { say @a.elems }; f((1..3).grep(*>0));           # mutsu 0    raku 3
sub f(*@a) { say @a.elems }; f((1, *+1 ... 4));             # mutsu 1    raku 4
sub f(*@a) { say @a.elems }; f(gather { take $_ for 1..4 });# mutsu 1    raku 4
```

## Controls that are CORRECT — the discriminator is laziness, not slurpiness

```
sub f(*@a) { say @a.elems }; f((1,2,3));        # 3 both
sub f(*@a) { say @a.elems }; f((1,2,3).Seq);    # 3 both  <- an EAGER Seq is fine
sub f(*@a) { say @a.elems }; f((1,2,3).List);   # 3 both
sub f(*@a) { say @a.elems }; f(1..3);           # 3 both
sub f(@a)  { say @a.elems }; f((1..3).map(*+0));# non-slurpy receives the Seq intact
```

The Seq itself reifies correctly everywhere else — `.elems`, `.join`, `~` and
stringification on the same value all work. Builtin listops also flatten it
correctly (`join(",", (1..3).map(*+0), 9)`). Only the **user**-slurpy binding
path drops it.

## The `.grep` row is a fresh regression — bisected

Built and measured three commits with the identical program
`sub f(*@a) { say @a.elems }; f((1..3).grep(*>0))`:

| commit | PR | result |
|---|---|---|
| `f62654e3d` | before the ADR-0058 step-3/4 work | **3** ✅ |
| `2c7fc1dde` | #7481 `feat/adr0058-step3b-grep` | **3** ✅ |
| `200923834` | #7496 `feat/real-array-map-defers` | **3** ✅ |
| **`182178e6b`** | **#7501 `feat/adr0058-step3b-grep-defer`** | **0** ❌ |
| `dccfd1737` | current `main` | **0** ❌ |

So **PR #7501 introduced it**, and CI was green. The `.map`, `gather` and
sequence-operator rows are *not* regressions — they answer `[]`/`1` at
`f62654e3d` too, i.e. they predate the ADR-0058 campaign.

## Severity

Tier S by the TRIAGE definition: a routine's arguments silently disappear, with
nothing detecting it. `*@a` slurpies are ubiquitous, and a caller passing a
`.grep` result is an ordinary idiom, so this is not a corner.

## Where to look

The argument-flattening path for a **user** sub's slurpy, as opposed to the
builtin listop path which flattens the same value correctly. PR #7501 made
`.grep` produce a deferred `Seq`; the slurpy binder evidently reifies a
deferred source differently from an eager one — note the eager `.Seq` control
passes. Start by diffing what `#7481` (correct) and `#7501` (broken) changed
about when a grep Seq is forced.

## Acceptance

- All four repro rows match raku.
- Every control above still passes.
- A `t/` pin covering `.map`, `.grep`, `gather` and the sequence operator into
  both a user `*@a` slurpy and a user `@a` positional, since the two paths
  currently disagree.
- Because `.map`/`gather` were already wrong before the campaign, fixing only
  the regression is not enough — the pin must cover all four producers.
