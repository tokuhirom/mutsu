# The "slow-path multi-dispatch caller-param clobber" was really two s/// bugs

The 90_csv.t frontier bug recorded on 2026-08-12 as "a nested slow-path
multi-method call replaces method CSV's `$in` param with a stale value"
(todo/deep/slow-path-multi-dispatch-clobbers-caller-param.md) turned out to
have nothing to do with method dispatch or env writeback at all. Instrumenting
every whole-env restore and merge site showed `in=Sub` throughout iteration 2 —
the dispatch machinery was innocent. Bracketing the module line
`$fragment ~~ s:i{^ "row=" } = "" and self.rowrange ($fragment);` with probes
showed `$in` flipping across the *substitution*, not across `rowrange` (the
original forensics probed after the whole line, so the multi call took the
blame). Two independent, general `s///` bugs:

1. **`s///` as a smartmatch RHS mirrored its result into the enclosing
   `given`/`for` topic-source scalar.** `write_subst_topic_checked`
   (src/vm/vm_subst_exec.rs) unconditionally wrote the substitution result
   through `topic_source_var`. Inside `given $in { when Callable { $fragment
   ~~ s/.../...; } }` the topic is temporarily the smartmatch LHS
   (`$fragment`), so the mirror clobbered `$in` with the substituted string —
   later `$in()` died with `No such method 'CALL-ME' for invocant of type
   'Str'`. Fix: skip the mirror when `in_smartmatch_rhs` is set; the
   smartmatch handler owns every writeback in that position, including the
   `$_ ~~ s///` case via its own topic-source mirror. (Nested routine calls
   were never affected — `with_nested_run` takes `topic_source_var` — which is
   why the bug needed the substitution to sit directly in the RHS.)
   Pin: t/subst-smartmatch-topic-source.t.

2. **The `s{pat} = expr` assignment-replacement form parsed its RHS at full
   expression precedence,** so `s:i{^ "row="} = "" and self.rowrange(...)`
   swallowed `and self.rowrange(...)` into the replacement closure, where
   `"" and ...` short-circuited — the substitution looked right and
   `rowrange` silently never ran (rows/headers filtering dropped: 90_csv
   tests 474–484). Fix: parse the replacement with
   `expression_no_word_logical` (item-assignment precedence — `and`/`or`/
   `andthen`/... bind looser) at all four sites: the literal-replacement
   probe, the destructive and non-destructive (`S///`) expression fallbacks,
   and the compound-assign (`s{p} op= v`) form.
   Pin: t/subst-assign-replacement-precedence.t.

With both fixes (plus PR #6310's `:exists` fix), 90_csv.t went from aborting
at 36 subtests to 494/496, with only two remaining:

- **test 159 "Fragment, col"** — fails identically under rakudo 2026.06 with
  the vendored Slang::Tuxic 0.0.5 (verified locally): after `fragment
  "col=3"` returns single-column rows, the `out => Str` writer re-applies
  `@!crange` to the already-sliced rows and emits nothing. Not a mutsu bug;
  mutsu's output matches raku's exactly.
- **test 495 + end abort** — a real, separate mutsu bug: after a
  `csv (in => &provider, out => @prefill)` call (Callable in, Array:D out) in
  the 90_csv environment, a later `@in = gather while $in() -> $r { take $r
  }` runs its while loop (the provider gets called) but the takes are lost
  (`@in` reifies empty). Filed as
  todo/deep/gather-takes-lost-after-callable-in-array-out-csv.md.
