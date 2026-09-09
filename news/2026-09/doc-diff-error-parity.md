# The doc-diff harness now compares the half of the corpus it used to throw away

The doc-diff harness had one oracle rule since it was written: compare a block
only when reference `raku` **exits 0 and prints something**. Everything else was
counted as `no oracle` and dropped. On the 2026-09-09 morning sweep that was
**3916 of the corpus's 7768 blocks — half of it, never compared at all.**

The rule reads "raku did not print anything useful" as "this is not a runnable
example". Usually it meant the opposite: the block is an example that
**deliberately fails**, which is the entire point of the `Type/X*.rakudoc`
corpus, or one that succeeds silently. PLAN.md §6 has had "error / exception
parity" on its list the whole time; it turned out to be a property of the oracle
gate, not a separate harness.

## Three oracle modes

What raku itself does with a block now selects how the block is compared:

| raku does | mode | compared |
|---|---|---|
| exits 0, prints something | stdout parity | stdout (as before) |
| fails at **run** time | **error parity** | the failure: does mutsu fail, and with the same message |
| exits 0, prints nothing | **silent parity** | mutsu must also exit 0 quietly |
| fails at **compile** time (`===SORRY!===`), or times out | *no oracle* | nothing — a fragment, not an example |

The compile-error and timeout cases are what the gate was actually for, and they
still skip. Messages are compared without the backtrace beneath them — frame
text is implementation detail that would never match, and comparing it would
make every error block a finding — and without compile-time warnings, which raku
prints ahead of the exception and mutsu does not. `# ERROR`-marked blocks are no
longer dropped by the nondeterminism heuristic either: they are precisely the
blocks error parity exists to compare.

Findings are ranked by what the divergence *means*, because they are not equally
interesting. **`mutsu-accepts`** — raku refuses the program, mutsu runs it to a
clean exit — is a semantic divergence and the highest-signal thing the harness
can report. `error-mismatch` is usually about wording or the `X::` type.

## What it found

Compared blocks went from **2559 to 3971**, and the two new modes produced **294
findings no previous sweep could see**:

| bucket | compared | findings |
|---|---:|---:|
| stdout parity (pre-existing) | 2570 | 154 |
| error parity (new) | 339 | 241 |
| silent parity (new) | 1062 | 53 |

Seventy of those are `mutsu-accepts`. Eleven issues were filed from them, each
reduced to a minimal repro and re-run against `raku` v2026.07 first:

- **#7770** — a method call on an unhandled `Failure` returns a value instead of
  rethrowing, so `"nope".IO.open` followed by `.lines` answers `1` rather than
  failing. A silent wrong answer on the error path, inherited by every `Failure`
  producer.
- **#7771** — an exception thrown inside `start` is swallowed; the program exits
  0 as if nothing happened.
- **#7772** — every instance answers a phantom `.name` method no class declared.
- **#7773** — a known method name called on an undefined value returns `(Any)`
  instead of throwing (`$undefined.comb(/\w+/)`), so the bad value travels on
  into the data.
- **#7774** — a typed-array parameter (`Int @a`) accepts an untyped `Array`.
- **#7775** — `my @a is Foo` accepts a role that is not a container type, then
  ignores it (five doc rows, one root).
- **#7776** — `chdir` into a nonexistent directory succeeds when its parent
  exists.
- **#7777** — `.skip` silently ignores every argument after the first.
- **#7778** — native-type multi candidates are not distinguished: an `int`
  candidate takes an `Int`, and two native widths never look ambiguous.
- **#7779** — `.clone(:attr)` on a type object returns the type object.
- **#7780** — the roles structuring the built-in hierarchy are undeclared:
  `QuantHash`, `Stringy`, `Systemic`, `Baggy`, `PositionalBindFailover`,
  `Blob[T]`. Fifteen `Type/*.rakudoc` pages cannot compile their own synopsis
  line.

The remaining 171 `error-mismatch` findings are mostly wording, and largely one
root cause already filed as #7750 (runtime binding failures carrying the
compile-time "will never work with declared signature" wrapper). They are
recorded as a cluster rather than filed one by one — fix the wrapper and
re-sweep.

## Costs and comparability

The new modes run the oracle twice on blocks that used to cost a single run, so
a full sweep is roughly 2x slower (~2.5 h at `-j4` on a 4-core container).
`--/error-parity` restores the old behaviour and the old blind spot.

`mism` and `crash` keep their old meanings so their counts stay comparable with
earlier sweeps — 129 → 132 and 21 → 22, the small rise coming from the
`# ERROR` blocks the heuristic used to drop. The error/silent findings are a
separate `err` column in the sweep summary and the survey table, which now ranks
by `mism + crash + err`. A reader comparing this sweep to an older one needs to
know that `err` counts a population that was never compared before, not a
regression — the same care the retired `raku-drift-from-doc` bucket needs.
