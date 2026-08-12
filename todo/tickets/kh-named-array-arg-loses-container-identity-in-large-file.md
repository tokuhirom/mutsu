# A named `@`-array argument loses container identity (and its descriptor name) only in a large file (90_csv 507-508)

Found 2026-08-12 while closing out t/90_csv.t. After the closure-captured-name
leak fix let the file run to completion (524 tests), two newly-reached tests
fail: 507 ("AOH parse out with kh defaults to Hash") and 508 ("Headers kept").
rakudo passes both (verified locally with Slang::Tuxic 0.0.5 installed; its
only failure is 159, the known raku-parity row).

## Shape

```raku
{   is-deeply (csv (in => $fno, kh => my @kh, out => Hash), $aoh, "..."); # 505 ok
    is-deeply (@kh, ["1", "2", "3"], "Headers kept");                     # 506 ok
    }
{   is-deeply (csv (in => $fno, kh => my @kh), $aoh, "...");              # 507 FAILS
    is-deeply (@kh, ["1", "2", "3"], "Headers kept");                     # 508 FAILS: []
    }
```

Text::CSV's `method CSV` gates `out //= Hash` on `@kh.VAR.name ne "element"`.
For call 507 the gate reads "element", so the csv defaults to AoA and the
header writeback (`@kh = @h`) lands in a copy the caller never sees.

## What is known (probed on the fixed binary, 2026-08-12)

- The caller side is CORRECT: a post-call probe in the second block shows the
  caller's container is stamped `@kh` (the new declaration-time
  `descriptor_name` stamp works) — yet the callee (`method CSV`, three
  slurpy/forward hops down: `sub csv (*%args)` → `Text::CSV.csv (|%args)` →
  `$csv.CSV (..., |%args)`) receives a container whose descriptor is
  None/"element" and whose mutations do NOT reach the caller (508's `[]`).
  So the argument is being COPIED somewhere en route, for this call only.
- **Only reproduces in the large file.** `tmp/kh-trunc.t` (t/90_csv.t
  truncated at line 337, run with `mutsu -I lib` from the tmp/text-csv clone)
  reproduces deterministically; every hand-reduction so far — the same
  3-hop chain in isolation, the same two adjacent blocks, pre-warming the
  2-named-arg call shape with `csv (in => ..., meta => False)` × 3 — passes.
  Deleting EITHER large chunk of the preceding file (lines 109-215 or
  215-289) makes it pass, so the trigger is an interaction/volume effect,
  not a single poisoning statement. MUTSU_JIT=off does not change it.
- Call 505 (same shape + `out => Hash`) keeps identity through all hops
  (probes showed the name arriving intact), so the copy is specific to
  something about the second call's state, not the chain itself.

## Suspects for the next session

- A dispatch/binding cache keyed by callee+shape that, once some earlier
  call populated it, routes the second call through a binder that copies
  `@`-valued named args (`detach`/List reify) instead of aliasing.
- `sanitize_call_args_owned` / slurpy-hash rebuild paths that deep-copy
  values only when some latch (meta keys, shared-store activity, env size)
  has flipped earlier in the run.
- Instrument `Gc::ptr` identity of the array at each hop (sub csv / method
  csv / method CSV) in the failing file to find the hop where the pointer
  changes; then breakpoint that copy site.

## Repro assets

- `tmp/kh-trunc.t` (regenerate: first 337 lines of tmp/text-csv/t/90_csv.t).
- Oracle: `prove`-free — run and grep `not ok`; 507/508 fail, everything
  else (except 159) passes.
- Full suite context: `prove -e "target/debug/mutsu -I lib" t/90_csv.t` in
  `tmp/text-csv` → 524 tests, failures 159 + 507-508.
