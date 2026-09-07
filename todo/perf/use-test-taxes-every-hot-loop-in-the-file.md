# `use Test` taxes every hot loop in the file that loads it

Loading the vendored upstream `Test` module — which is what every `use Test`
does once PR #7523 lands (`news/2026-09/vendored-test-module-is-the-default-provider.md`)
— makes unrelated hot loops in the *same file* several times slower. The module
is not running; merely having loaded it is enough.

The extreme case in the `t/` suite, measured on a 4-core box, debug build:

| | wall |
| --- | --- |
| `t/rebound-return-hot-loop.t`, native `Test` provider | 4.84 s |
| same file, vendored `Test` | **13.18 s** |

That file has 7 assertions and a 20 000-iteration loop, so essentially none of
the 8.35 s is assertion cost. It is the single largest per-file regression in
the whole suite — 2% of the switch's total `t/` cost sits in this one file,
against a median per-file delta of 94 ms.

## Two linear costs, both already named in earlier work

Reduced to a file with no `use Test` at all (debug, median of three):

| variant | wall |
| --- | --- |
| the loop alone | 4.48 s |
| `+ 60 unused mainline `our` variables` | 7.26 s |
| `+ 60 unused mainline subs` | 6.59 s |
| `+ a bare `EVAL "1"`` | 5.10 s |
| `+ use Test` (vendored) | 12.77 s |

1. **Cost linear in mainline env size.** `use Test` imports ~60 names into the
   caller's env. Sixty *unused* `our` variables reproduce most of the effect on
   their own, so nothing about `Test` specifically is at fault — any module with
   a wide export list taxes every loop iteration in its importer. This is the
   same linear-in-env-size cost the vendoring campaign's fourth optimization
   pass measured at ~0.98 ns per env entry per assertion and filed as
   `todo/perf/method-dispatch-flattens-the-env-on-every-call.md`; that ticket
   framed it as a *method dispatch* cost, and this shows it is broader.
2. **The reflective latch.** Loading a module that `EVAL`s anywhere (upstream
   `Test`'s `throws-like` does) latches
   `opcode::reflective_name_access_possible()`, which is process-global and
   monotonic. That disables `SetLocal`'s `skip_env_write`, so every store writes
   the env mirror as well as its slot for the rest of the process. A bare
   `EVAL "1"` alone costs +14%.

The two do not explain the whole 2.85x on their own (60 pads plus a latch
predicts ~8.6 s against a measured 12.8 s), so a third term is unaccounted for.
Finding it is the first job here.

## Why this is worth fixing beyond `Test`

Neither cost is about the test suite. Both say: *importing a module makes the
importing scope's ordinary code slower, in proportion to how much it exports*.
That is a property any real Raku program hits — a program that `use`s a few
ecosystem modules pays it on every loop it runs — and it is invisible in a
benchmark that imports nothing.

The reflective latch is the worse of the two, because it is monotonic and
process-global: one `EVAL` anywhere, including inside a module the program never
calls into, permanently deoptimizes every lexical store in the process. A
per-frame or per-compunit determination would keep the fast path for the code
that provably cannot be reached reflectively.

## Reproducing

```sh
cargo build
# the outlier itself
time target/debug/mutsu t/rebound-return-hot-loop.t
MUTSU_REAL_TEST=0 time target/debug/mutsu t/rebound-return-hot-loop.t
```

The loop, isolated (write these to `tmp/`):

```raku
sub rebound($n) { my &return = sub ($v) { $v + 1000 }; return $n; }
sub plain($n) { return $n * 2; }
my $hot = 0; my $cold = 0;
for ^20000 { $hot += rebound(1); $cold += plain(1); }
say $hot, " ", $cold;
```

then prepend, in turn: nothing / sixty `our $padN = N;` lines / sixty
`sub padN() { N }` lines / `use MONKEY-SEE-NO-EVAL; my $z = EVAL "1";` /
`use Test;`.

Numbers here are local wall-clock, adequate for ranking the variants against
each other. Anything that ends up in a document must come from the bench CI
(`bench-history.tsv` on `bench-data`), per the measurement rule in CLAUDE.md.

## Related

- `todo/perf/method-dispatch-flattens-the-env-on-every-call.md` — the
  linear-in-env-size cost, framed narrowly as a dispatch problem.
- `todo/perf/interpreter-call-path-in-hot-loops.md` — the call path this loop
  spends its time in. Read its stale-diagnosis warnings before starting.
- `news/2026-09/vendored-test-module-is-the-default-provider.md` (PR #7523) —
  the measurement that surfaced this, including the per-file distribution
  showing every other file pays a flat ~94 ms and this one does not.
