# Slow-path multi-method dispatch clobbers a caller frame's same-named param with a stale value

Found 2026-08-12 during the Text::CSV 90_csv.t sweep (after the five fixes in
PR #6307/#6310). This is the last root cause standing between 90_csv.t and the
whitelist: it explains failing tests 159 (`fragment col=`), 474/476
(`headers => @hdr, frag => "row=2-*"`), and the end-of-file abort
(`No such method 'CALL-ME' for invocant of type 'Str'`).

## Repro (minimal, needs the Text::CSV clone)

`tmp/csv-seq.raku` against `tmp/text-csv/lib`: iterate
`for ($fni, &provider) -> $in { Text::CSV.new.csv (in => $in, headers => @hdr, frag => "row=2-*") }`.
Iteration 1 (Str) passes. In iteration 2 (Sub), inside `method CSV`:

- at `given $in` the param is still the Sub (matches `when Callable`);
- the arm body first runs `self.rowrange ($fragment)` — **a `multi method`,
  so it dispatches through the slow path** (`methods_call_dispatch` →
  `methods_dispatch_new` → `methods_object_dispatch_new`);
- after that call returns, `$in` inside method CSV reads as **iteration 1's
  Str** (`$fni`), so `$in()` dies with `CALL-ME` on Str.

## Forensics so far

- Instrumented `Env::insert/insert_sym` on key `in`: during iteration 2 there
  is **no insert of the Str value at all** — every "in" insert carries the
  Sub. So the stale Str arrives via wholesale env-object reuse/restore
  (a saved/cloned env from iteration 1), not a keyed write.
- The caller loop's own `$in` (mainline) stays correct before/after the csv
  call; the clobber is confined to method CSV's frame after the nested
  slow-path call returns.
- Same bug family as the merge-writeback fixes in PR #6307: a callee env that
  contains a *copy* of some outer tier's (stale) binding gets merged back as
  if the callee had written it, because the caller has a same-named binding
  (`saved.contains_key` keep rule in `merge_method_env`, and the analogous
  slow-path whole-env writebacks, e.g. the "preserve side effects on
  variables that already existed" loop at
  `src/runtime/methods_object_dispatch_new.rs:~2005`).
- Suspected mechanism: the slow-path method env for `rowrange` is seeded from
  a persistent env snapshot that still holds `in => Str` from iteration 1
  (mainline env was legitimately `in => Str` then via the loop-param bind);
  the writeback then pushes that stale copy into method CSV's frame, which
  re-syncs its `$in` local slot from env (env_dirty).
- Next probe: instrument the slow-path env seeding + writeback for method
  `rowrange` (print the method env's `in` at entry and what the writeback
  merges), or gdb-break `methods_object_dispatch_new.rs:2008`-style restore
  loops with the repro.

## Why this is deep

It is the env_dirty dual-store / tree-walk-era whole-env writeback doing what
it was designed to do — the design conflates "callee overlay contains K" with
"callee wrote K". Point fixes (like #6307's `&`-alias and loop-param ones)
keep working, but each nested-call shape needs provenance the current merge
does not track. Related: memory `env-writeback-campaign-state-sync-bug`,
ADR-0018.

Debug artifacts: `tmp/csv-seq.raku` (repro), `tmp/csvdbg2/` (probed module
copy showing `$in` already Str at the Callable arm entry).
