# After any `start` block, a `for @m -> @row` loop rebinds the PREVIOUS iteration's container

## Symptom

Once a program has run a `start` block (anywhere, on any data), every later
`for @container -> @param { ... }` loop stops rebinding `@param` per iteration:
from the second iteration on it still holds the *first* iteration's element, so
in-place mutations pile up on one row and the rest of the source is never
touched.

```raku
my $p = start { 1 };
await $p;
my @m = [1, 2], [3, 4];
for @m -> @row { @row.push(9) }
say "A: ", @m;
```

- raku:  `A: [[1 2 9] [3 4 9]]`
- mutsu: `A: [[1 2 9] [1 2 9 9]]`

Drop the first two lines and mutsu agrees with raku, so the `start`/`await` is
the whole trigger. The `start` block does not have to touch `@m`, `@row`, or
anything else in the loop — a `start { 1 }` is enough.

## Status / provenance

**Pre-existing and unrelated to ADR-0045.** Confirmed on a release build of
`main` at `f678b032b` (before ADR-0045 slice 1) with the exact repro above. It
was found while writing `t/for-loop-element-alias.t`: the ADR's invariant rows
32 (`for @m -> @row { @row.push(9) }`) and 37 (the nested `<->` loop) both sit
after that file's row 27, which uses a `start` block, and were being sunk by
this leak rather than by anything the ADR touches. The workaround in that file
is to keep the `start`-block row last; the note there points here.

## Where to look

The trigger is thread-related, so the suspects are the cross-thread bare-name
lane and the env↔locals sync that runs once a thread has existed:

- `Interpreter::thread_redeclared_vars` and `sync_shared_vars_to_env` /
  `set_shared_var_sym` — `@row` is an `@`-sigil name, and
  `exec_for_loop_body`'s `masked_multi_params` mask is only installed for
  *multi*-param loops (`spec.multi_param_names`), never for the single named
  param this loop uses.
- `src/vm/vm_for_loop_body.rs`'s single-param bind: `saved_param` deliberately
  **skips `@`/`%`-sigil params** ("they bind a shared mutable container the body
  may legitimately reassign"), so an `@`-sigil loop param has no
  save/restore — and, more to the point, nothing that would keep a stale
  shared-lane copy from being re-injected over the per-iteration bind.

The failing value is the *previous* iteration's container, which is the shape a
stale `shared_vars` snapshot re-injected after the fresh bind would produce.

## Why this is a ticket and not a one-liner

The fix has to decide whether an `@`-sigil `for` parameter belongs on the
cross-thread bare-name lane at all. It is a fresh per-iteration binding, like
the multi-param names that `masked_multi_params` already keeps off the lane, so
the likely fix is to extend that mask to the single named parameter — but that
mask was written for multi-params on purpose and widening it touches every
`for ... -> $x` loop in the corpus. Measure before widening.

## Reproduce

`tmp/rwalias/start-leak3.p6` above, no fixtures. A slightly wider form (two
independent later loops, both corrupted) is in the same repro family:

```raku
{ my @p; for 1, 2 -> $v { @p.push(start { $v }) }; await @p; }
{ my @m  = [1, 2], [3, 4]; for @m  -> @row { @row.push(9) }; say "A: ", @m;  }
{ my @m2 = [1, 2], [3, 4]; for @m2 -> @r2  { @r2.push(9)  }; say "B: ", @m2; }
```

raku prints `[[1 2 9] [3 4 9]]` twice; mutsu prints `[[1 2 9] [1 2 9 9]]` twice.
