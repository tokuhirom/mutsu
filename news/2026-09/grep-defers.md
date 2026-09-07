# `grep` produces a deferred Seq too (ADR-0058 step 3b)

`.map` has answered a not-yet-run `Seq` since ADR-0058 steps 2/3a/3c. `.grep`
still ran its callback at the call, in all four spellings, so the two halves of
the same operation disagreed about when the callback runs.

```raku
my @a = 1,2,3;
my $s1 = @a.grep({ print "A"; $_ > 1 });        print "|";
my $s2 = @a.List.grep({ print "B"; $_ > 1 });   print "|";
my $s3 = (1,2,3).grep({ print "C"; $_ > 1 });   print "|";
my $s4 = grep({ print "D"; $_ > 1 }, @a);       print "|";
print "\n";
$s1.List; $s2.List; $s3.List; $s4.List;
```

| | at the calls | at the pulls |
|---|---|---|
| rakudo | `\|\|\|\|` | — |
| before | `AAA\|BBB\|CCC\|DDD\|` | — |
| after | `\|\|\|\|` | `AAABBBCCCDDD` |

All four of ADR-0058 §9.2's recorded rows now match rakudo, including the two
that were not about timing at all:

| §9.2 row | before | after = rakudo |
|---|---|---|
| `(1..3).grep` side-effect order | `RRRT` | `TRRR` |
| the listop spelling of the same | `RRRT` | `TRRR` |
| `grep({ $_ = 5 }, @a).eager` | `[1, 2, 3]` | `[5, 5, 5]` |
| `for @a.grep(…) { $_++ }` | `[1, 3, 4]` | unchanged |

## Two things the implementation turned up

**`builtin_grep` had no `source_var` branch at all.** The third row above was
broken *before* deferral and would have stayed broken after it: the listop grep
never wrote back through `$_`, because unlike `builtin_map` it never looked at
`pending_call_arg_sources`. A single concrete-array source now takes the same
promoting arm the method form does.

**`SeqSource::MapGrep`'s two `Option<Value>` fields were not mutually
exclusive.** `rw_source` (added by step 3c) and the `grep_source` this step
wanted describe four shapes that cannot co-occur — a body cannot be both an rw
map and a promoting grep — so they collapsed into one `MapGrepMode` enum
(`Map` / `MapRw(Value)` / `Grep` / `GrepArray(Value)`). The pull's dispatch is
now a total match rather than a tuple of `Option`s.

Deferring the promoting arm at all is only sound because the promotion is
published by mutating the source `ArrayData` **in place** rather than by
re-binding its name in the current frame — the pull happens wherever the Seq is
consumed, long after that frame is gone
(`news/2026-09/grep-promotion-is-published-in-place.md`).

## What the gates found: deferring the sibling is the detector

The diff is small; the gates turned up **six** defects, and **five were already
live on `main`** — holes in `.map`'s deferral that `grep` had been masking by
still being eager. Every one is the same shape: a consumer that reads a `Seq`'s
elements through pure code, which cannot pull, and so sees ADR-0034's empty
seed.

| consumer | symptom | broken for `.map` too? |
|---|---|---|
| `+@a` slurpy binder | `sub f(+@a) { @a }; f((1,2,3).map({$_}))` answered `()` | yes |
| boolean context | an EMPTY result read as TRUE, inverting every `!…grep(…)` | yes |
| `[$p?, *@r]` destructuring | a recursive quicksort unpacked the empty seed at every level | yes |
| `constant @x = …` | the constant was frozen as the empty seed | yes |
| slice index `@f[SEQ]` | the slice addressed no slots | yes |
| itemized array through the promoting arm | `grep({…}, $(1,2,3))` grepped its elements | this step's own |

The boolean one is worth naming twice. `Value::truthy`'s `is_map_grep_source`
arm carried a comment promising that *"the VM forces the body at every boolean
chokepoint it can reach with an `&mut Interpreter`"*. It did not — `eval_truthy`
had no such force. The comment documented an invariant nobody had implemented,
and it read as reassurance for two weeks.

**The transferable lesson**, recorded as ADR-0058 §9.5: a green gate on the step
that deferred is not evidence its consumers are covered. As long as a sibling
operation is still eager it keeps handing out reified Seqs, which hide exactly
the same holes. Step 4 should expect the same.

Two of the six surfaced **only** in the bundled-library battery gate
(`Text::CSV` 78_fragment / 90_csv) with `make test` and a full `make roast` both
green — the fourth time in one day that gate caught what the other two missed.

## Gates

`make test` PASS (3799 files / 40048 tests) · full local `make roast` PASS
(1436 files / 218962 tests) · `scripts/battery-testsuite.sh` **GATE PASSED**
(289/312). Pinned by `t/grep-defers.t` (11 rows, green under rakudo too).

Step 4 — retiring `create_lazy_map_list` and the `body_contains_return`
predicate — is what is left of ADR-0058.
