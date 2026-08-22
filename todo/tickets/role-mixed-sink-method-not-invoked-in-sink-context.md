# A role-mixed `.sink` method is never invoked when the value is used in sink context

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Language/perl-func.rakudoc:2310`).

## Root cause

Raku calls a value's `.sink` method (if the value's type/role defines one) when the
value is the result of a statement whose value is discarded ("sink context" — e.g. a
bare expression-statement, or a sub call used as a full statement). mutsu never
invokes `.sink` at all in this situation — a role-mixed `.sink` method is simply never
called.

## Minimal repro

```raku
role R { method sink { say "sunk!" } };
(1) does R;
say "after";
```

- `raku`: prints `sunk!` then `after`.
- `mutsu` (`target/debug/mutsu`): prints only `after` — the mixed-in `.sink` method is
  never dispatched.

The doc's own (more elaborate) example relies on exactly this mechanism to implement
an "increment on sink, copy on assignment" idiom:

```raku
multi increment($b is rw) {
    ($b + 1) does role { method sink { $b++ } }
}
multi increment($b) { $b + 1 }
my $a = 1;
increment($a);
say $a;                 # raku: 2, mutsu: 1 (the mixed sink method never fires)
```

## Affected files (starting point)

- Wherever mutsu compiles/executes a statement in sink context (statement-expression
  evaluation that discards its value — grep for "sink context" / where the "Useless
  use of ..." warning is emitted, since that's the same context-detection machinery)
  — after evaluating a statement's expression in sink position, if the resulting
  value carries a role/class that defines a `sink` method, that method should be
  invoked (with no arguments) before the value is discarded.
- The mixin/role-dispatch machinery (`src/runtime/mixin.rs` or similar) for how a
  `does`-mixed method is looked up on a value, to reuse for the sink-method lookup.
