# Cloning a `Nil` stopped walking a 60-way jump table

`NanBox`'s `Clone` and `Drop` classified the word and, for anything that was not
an inline `Int` or `Num`, called `payload_op` -- the function that bumps or
releases the word's `Arc`/`Gc`/`Weak` payload. But six kinds carry their whole
value in the word itself (`Nil`, `Whatever`, `HyperWhatever`, `Bool`, `Package`,
`CompUnitDepSpec`); for those, `payload_op` walks a ~60-arm jump table to reach
an empty arm and returns.

That is not a rare case. `Nil` is the most-cloned value in the interpreter: it
is the fill of every locals frame, the `mem::replace` placeholder every argument
move leaves behind, and the `unwrap_or` of every stack pop. A `gdb` breakpoint
histogram on `bench-fib` put the split beyond argument: **216 of 220 sampled
`payload_op` calls were `Kind::Nil`**, and `perf` had the function at 3.9% of
the profile.

`kind_owns_payload(kind)` now gates both `Clone` and `Drop`. Its list of
payload-free kinds is a `macro_rules!`, expanded *both* as `payload_op`'s
do-nothing arm and as the predicate's negated test, so the two cannot disagree
about which kinds are payload-free -- there is only one list.

Two unit tests pin the classification from both sides: every kind the macro
lists really does survive a clone and a drop unchanged, and a refcounted kind
(`Str`) is never classified payload-free -- which would mean a clone that does
not bump and a drop that does not release. CI's Miri job runs for this diff
(`src/value/**` is in its trigger set), and its `gc::soundness_smoke` step drives
a real interpreter, so the changed clone/drop path is interpreted end to end.

Measured cross-build against `main`, release, pinned to one core (retired
instructions):

| benchmark | retired instructions |
| --- | ---: |
| `bench-tak` | **-4.75%** |
| `bench-fib` | **-3.59%** |
| `bench-mandelbrot` | -1.05% |
| `poly-call` | -0.46% |
| `bench-ctor` | -0.39% |
| `bench-class` | -0.38% |
| `bench-array` | -0.34% |
| `method-call` | -0.29% |
| `bench-hash` / `bench-grammar-parse` / `bench-yaml-parse` | ~0.00% |
| `bench-string` | +0.13% |

Unlike the rest of this series, this one helps *every* workload -- it is on the
path of every value copy in the interpreter.

## What did not work: `#[inline]` on `Clone`/`Drop`

The first version also marked both impls `#[inline]`, on the theory that the tag
test belongs at the call site. A same-binary A/B (which holds codegen fixed)
made that look free, but the cross-build comparison told the real story:
`NanBox::clone`/`drop` are called from thousands of sites, and inlining them
grew every one. `method-call` went from **-0.29% to +0.84%**, `bench-array` from
-0.34% to +1.12%, and every payload-heavy benchmark regressed the same way,
while `bench-fib`/`bench-tak` gained nothing measurable. Dropping the attributes
-- letting LLVM inline where it already wanted to -- is what turned this into a
change with no regressions at all.

The methodological point is worth keeping: a same-binary env switch isolates a
change's *logic* perfectly, but it cannot see a change's effect on **code size**,
because both arms share one compilation. When the change is an inlining hint,
the cross-build instruction count is the only measurement that answers the
question.
