# The ecosystem parity ledger becomes a work queue

P5 of [ADR-0085](../../docs/adr/0085-ecosystem-testsuite-parity-measurement.md) —
the last phase of the zef-parity campaign ([#7785](https://github.com/tokuhirom/mutsu/issues/7785))
that still had nothing built. The corpus has been measured since 2026-09-11
(1625 distributions, 41.1% dist parity), which answered *what* fails. This turns
those 1625 records into an answer to *why*, and into tickets.

## The tool

`scripts/ecosystem-tickets.py` reads the ledger back, normalises every actionable
failure site — a `blocked_load` module error, and the `first_failure` of every
`regression`/`partial` test file — into a root-cause signature, and clusters
them. 985 clusters over the current corpus; fifteen of them affect ten or more
distributions.

Four decisions shape its output, and each of them was a way to get the ranking
wrong:

- **Impact is counted in distributions, never in failure sites.** `Invalid
  typename 'IndRef'` appears 100 times in the ledger and is one bug affecting six
  distributions; `Gnome::Gtk3` alone would otherwise top every list with its 55
  modules.
- **A cluster key keeps the payload that identifies the fix** (`nqp::<op>`, `No
  such method '<m>' on <type>`) **and drops what is volatile** (quoted strings,
  numbers, the temp path a parse failure mentions). Where members share one cause
  by construction — an unknown attribute trait, an unknown parent type — the
  cluster is the family and the payloads are listed inside it.
- **A message with no shared cause is keyed per distribution.** `not ok 7 -` is
  what the ledger records for an *unnamed* failing assertion, and it is the same
  string for two dozen unrelated wrong answers; merging them would have produced
  the single largest cluster in the report and no fix could have closed it.
- **The output is filed as issues, not committed as a queue file.** A generated
  queue over 1600 records would conflict on every sweep, and an issue number
  survives being closed where a path in a generated file does not. Each body
  carries `eco-cluster: <id>`, a digest of the signature, so the next sweep's
  "is this already filed?" is a tracker search rather than state in the repo.

## The lesson: a cluster is a hypothesis

Fifteen issues were filed, covering roughly 330 of the ~1200 non-green
distribution slots. What made them worth reading was not the clustering — it was
half an hour with the `raku` oracle *after* the clustering, which corrected four
of the five largest clusters and deleted one outright:

| the cluster said | what it was |
|---|---|
| `unknown trait 'is' -> 'json-skip-null'`, 61 dists | not the trait. mutsu handles user attribute traits across module boundaries, in `unit module` and braced-`module` form, with named arguments. `JSON::Class` **re-exports** the trait it imported (`OUR::{'&trait_mod:<is>'} := &trait_mod:<is>`), and that binding is invisible to importers — minimised to three files ([#7989](https://github.com/tokuhirom/mutsu/issues/7989)) |
| `No such method 'AST' for invocant of type 'Str'`, 13 dists | `'say 1'.AST` works today. The L10N tests call `.AST("AF")` — the localised-slang form, a 1-arity candidate that does not exist ([#8001](https://github.com/tokuhirom/mutsu/issues/8001)) |
| `Variable $.x used where no 'self' is available`, 35 dists | two unrelated gaps: NativeCall's `HAS` embedded-struct declarator ([#7991](https://github.com/tokuhirom/mutsu/issues/7991)), and `has Int ($.x, $.y)` — plain Raku, no NativeCall ([#7992](https://github.com/tokuhirom/mutsu/issues/7992)) |
| `has overflowed its stack`, 17 dists | not deep recursion. *Infinite* recursion: an imported `my proto sub f(|) is export {*}` does not shadow an enclosing `my sub f`, so `Net::netent`'s `my sub getnetbyname { use P5getnetbyname; getnetbyname(...) }` calls itself. Seven of lizmat's P5 wrappers are the same idiom ([#7994](https://github.com/tokuhirom/mutsu/issues/7994)) |
| `Use of Nil in string context`, 10 dists | **nothing at all.** Both interpreters print it as a warning and carry on. It was the harness recording a warning as a blocker |

Three of the fifteen are diagnostics tickets, and deliberately near the front:
17 distributions report only `An exception occurred while evaluating a CHECK`
([#8000](https://github.com/tokuhirom/mutsu/issues/8000)), so nothing can be said
about what they need until that message carries its inner exception. Fixing a
message splits a cluster — progress that moves no KPI.

The best impact-per-fix in the batch is
[#7999](https://github.com/tokuhirom/mutsu/issues/7999): one parse gap in
`Terminal::Widgets::Widget` blocks 16 distributions, 15 of them transitively.

## Two harness fixes that fell out

Both in `first_error_line()` (`scripts/ecosystem_common.py`), and both change what
*future* sweeps record:

- **A warning is no longer a cause.** `Use of Nil in string context`, `Use of
  uninitialized value`, `Potential difficulties` and friends are used only when
  the run produced nothing else. Ten distributions had a warning recorded as
  their blocker, hiding ten real causes.
- **A parse failure keeps its location.** mutsu prints `at <path>:<line>` plus a
  caret line; the extractor was dropping both while keeping the reason. The
  location is now appended as `... [at dist/lib/A.rakumod:50]`, and clustering
  strips it. The caret line stays dropped: it carries source text, which would
  fragment a cluster into one per offending line. 99 distributions are blocked by
  parse gaps whose records said only that the parser was unhappy somewhere
  ([#7988](https://github.com/tokuhirom/mutsu/issues/7988)).

`load_records()` moved from `ecosystem-sweep.py` into `ecosystem_common.py` so
the rollup that publishes the KPI and the tool that reads it cannot disagree
about what the corpus is.

## What is left

The tail: 970 clusters below ten distributions, which is a sampling job
(`--min-dists 1`) rather than a queue to drain. P4's local `make`-level wrapper
for a sweep is the only other piece of the campaign still missing.
