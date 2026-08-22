# `race for` doesn't collect the loop's per-iteration results

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
717).

## Repro

```
my $r = race for ^10 -> $n { $n if $n %% 2 };
say $r.elems;
```

- raku: collects the truthy per-iteration values into a list (same count as plain `for`
  without `race`)
- mutsu: prints `Useless use of $number in sink context` and `$r.elems` is `1`

Confirmed that plain `for` (no `race`/`hyper` prefix) already collects results correctly in
mutsu — the bug is specific to the `race`/`hyper` statement-prefix combined with `for` used as
an expression.

## Root cause guess

The `race`/`hyper` statement-prefix wrapping a `for` loop is not wired into the same
list-collection / lazy-Seq path that a plain `for`-as-expression uses (see the "lazy-list
cluster" work in `docs/doc-diff-backlog.md`'s Deferred section, which made bare `for`/`while`/
`until` expressions lazy Seqs pulled on demand — `race for` looks like it bypasses that path
entirely and falls back to treating the loop as a sunk statement).

## Affected files (starting point)

- `src/vm/vm_control_ops.rs` — for-loop execution and the race/hyper statement-prefix wiring
- `src/compiler/stmt.rs` — wherever `race`/`hyper` prefixes are compiled onto a `for`

## Suggested next step

Compare `--dump-ast` for `my $r = for ^10 -> $n { $n if $n %% 2 };` (works) vs. the `race for`
variant (broken) to see whether the AST/compiled form differs in how the loop's produced value
is threaded to the assignment, then fix the `race`/`hyper` path to reuse the same collection
mechanism.
