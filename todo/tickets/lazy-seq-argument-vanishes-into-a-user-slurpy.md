# A `gather`/sequence-operator Seq is bound as one element to a user `*@a` slurpy

> **Status 2026-09-08: the `.map` and `.grep` halves are FIXED** (the `*@a`
> binder now reifies a deferred map/grep Seq, the `*@` twin of the `+@`
> reification ADR-0058 step 3b added). Pin: `t/lazy-seq-into-user-slurpy.t`.
> What survives is the `gather` / sequence-operator half below, which has a
> **different mechanism** — the slurpy is not empty, it holds the Seq itself.
> The title and body below are kept for the history; read this box first.

## Original report — a lazy Seq passed to a user `*@a` slurpy arrives empty or as one element

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

## What is actually left (measured 2026-09-08, after the fix)

```
sub f(*@a) { say @a.elems }; f(gather { take $_ for 1..4 });   # mutsu 1   raku 4
sub f(*@a) { say @a.elems }; f((1, *+1 ... 4));                # mutsu 1   raku 4
```

**Different mechanism from the map/grep hole.** The slurpy is not empty — it
holds the Seq as a single element:

```
sub f(*@a) { say @a[0].^name; say @a[0].elems }; f(gather { take $_ for 1..4 });
# mutsu:  Seq / 4
```

So `flatten_into_slurpy` is *declining to flatten* a `gather`/sequence-operator
Seq, rather than reading an empty seed through pure code. Both report
`.is-lazy` `False` and `.^name` `Seq`, matching raku, so the value itself looks
right; only the flatten decision differs. Note a second, separable oddity found
while measuring it: `@a.raku` renders `[]` for that one-element array even
though `@a[0]` is a 4-element `Seq` — see
[code-object-renders-as-nothing-inside-a-list](code-object-renders-as-nothing-inside-a-list.md)
for a sibling rendering collapse.

Careful: a genuinely lazy source must NOT be reified here — `f(1..Inf)` binds
lazily through the `single_lazy_value` branch and would otherwise hang. That
branch is why the map/grep fix is safe, and any flatten change has to preserve
it (pinned as a control in `t/lazy-seq-into-user-slurpy.t`).

## Acceptance

- All four repro rows match raku (**two now do**).
- Every control above still passes.
- A `t/` pin covering `.map`, `.grep`, `gather` and the sequence operator into
  both a user `*@a` slurpy and a user `@a` positional, since the two paths
  currently disagree.
- Because `.map`/`gather` were already wrong before the campaign, fixing only
  the regression is not enough — the pin must cover all four producers.
