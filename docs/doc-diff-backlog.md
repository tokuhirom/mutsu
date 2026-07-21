# doc-diff backlog — raku-doc differential findings

Tracked ledger of every `raku-doc` example where **mutsu** diverges from reference
**raku**, produced by the doc-diff harness. This is the "ranked backlog of minimal
repros" that [PLAN.md](../PLAN.md) §8.1 calls for — the QA-campaign analogue of
[TODO_roast/BLOCKERS.md](../TODO_roast/BLOCKERS.md).

- Harness + method: [docs/qa-doc-diff-harness.md](qa-doc-diff-harness.md)
- Tools: `scripts/doc-diff-harness.raku` (one run), `scripts/doc-diff-sweep.sh` (whole corpus, parallel)

## How to refresh

```
cargo build
scripts/doc-diff-sweep.sh              # -j8 over Type/ + Language/, ~15 min
```

Outputs (all under `tmp/sweep/`, gitignored): `reports/<file>.txt` (per-file
minimal-repro reports), `progress.txt` (one stats line per file), `summary.txt`
(files ranked by `mismatch + crash`). Regenerate the survey table below from
`summary.txt`, and the counts drop as fixes land — that is the visible progress
signal.

**Always re-verify a finding directly before treating it as a real bug.** The
harness oracle-gates on raku, but doc examples drift and the harness can only compare
`# OUTPUT:`-style blocks. `raku-drift` findings (raku itself no longer matches the
doc) are version skew, not mutsu bugs — lowest priority.

## Corpus snapshot

- **Date:** 2026-07-21 (re-swept) · debug `mutsu` at main ≈ `1d4c7768` (adds #5057/#5060/#5063)
- **444 files scanned · 136 have signal**
- **match = 2026 · output-mismatch = 271 · mutsu-crash = 179 · raku-drift = 134**
- High-signal total (mismatch + crash) by corpus: **Type 223 · Language 227**
- Delta since the morning scan (`0292015a`): mismatch −64, crash −23, signal files −20
  (match +67). Landed: `.match` ordinal adverbs (#5057), non-Regex/Str matcher
  coercion (#5060), `Rat.Str` digit budget (#5063). `Type/Str.rakudoc` dropped
  from 9 mism/3 crash to 1/2; `regexes.rakudoc` from 20/9 to 9/8.

> Historical note: before the harness was made parallel-safe (#4982), sweeps run
> concurrently raced on a shared scratch file and manufactured phantom divergences
> (e.g. `syntax.rakudoc` reported 19 mismatches, only 3 real). Any pre-#4982 scan
> numbers — and memory/notes calling a file "block-misalignment garbage" — are
> unreliable; re-sweep instead.

## Triaged

### Resolved (will drop from the next sweep)
- `$.name()` self-accessor interpolation left `()` literal — **#4979**.
- Harness scratch-file race producing phantom findings — **#4982** (this is why the
  survey below supersedes every earlier scan).
- `regexes.rakudoc` [20] — positional captures (`$0`/`$1`/`$/[0]`) empty in `$/`
  after `s///` — **#4992**.
- `regexes.rakudoc` [13] — `<?@var>` / `<!@var>` array-variable lookahead assertions
  never matched — **#4994**.
- `regexes.rakudoc` [28] — `m:pos(N)` / `m:continue(N)` discarded the `(N)` argument
  and matched from the start — **#4996**.
- `typesystem.rakudoc` [1] — a quoted MOP pseudo-method call (`$obj."WHAT"()`) invoked
  the reflection macro instead of a user-defined `method WHAT`. `dispatch_method_by_name_1`
  intercepted `WHAT`/`HOW`/`WHO`/`WHY` before user-method resolution; now the quoted-call
  flag (`skip_pseudo_method_native`) makes those arms fall through to the user method.
- `typesystem.rakudoc` [10] — an anonymous enum value's `.^name` returned the internal
  marker `__ANON_ENUM__` instead of raku's empty string.
- `Str.rakudoc` [match] — `.match(/../, :1st/:2nd/:Nth)` ignored the ordinal adverb
  shortcuts (they parse as `st => 1`, `nd => 2`, `rd => 3`, `th => N`) and always
  returned the first match — **#5057**.
- `Str.rakudoc` [match] — `.match([1,2,3])` / `.match(123)` returned `Nil`; a defined
  non-Regex/non-Str matcher is now coerced to its string form and matched literally
  (`"1 2 3".match([1,2,3])` → `｢1 2 3｣`) — **#5060**.
- `Str.rakudoc` [parse-base] — `Rat.Str` printed the full exact terminating expansion
  (`'FF.DD'.parse-base(16)` → `255.86328125`); it now rounds to Rakudo's digit budget
  (`255.863281`) — **#5063**. Big Rats/FatRats are left on the old exact-expansion path
  pending a `BigFatRat` variant (see "FatRat-vs-Rat repr tag" under Deferred).
- `hashmap.rakudoc` [2] — the postcircumfix guillemet/double-angle subscript
  (`%h«oranges "$fruit"»`, `%h<<oranges "$fruit">>`) did not interpolate: it kept
  `"$fruit"` (quotes and all) as a literal key. The subscript path used a naive
  whitespace splitter (`angle_words_index_expr`, bare-`$name`-only) instead of the
  qqww word-splitter that a standalone `«...»` term uses; it now shares
  `split_quotish_words` via `angle_words_subscript_index_expr`, so quoted words and
  full sigil interpolation work and the single-word-scalar / multi-word-slice
  distinction is preserved. Pin: `t/angle-subscript-interpolation.t`.
- `perl-var.rakudoc` [2] (partial) — a CATCH that *handled* an exception (matching
  `when`/`default`, or `.resume`) wrongly left the handled exception in `$!` outside
  the `try`. Per Raku, `$!` is only updated when the exception propagates out
  unhandled; a handled `try` keeps `$!`'s pre-`try` value. Fixed in the try/catch VM
  op (restore the prior `$!` on the handled paths). Pin:
  `t/dollar-bang-handled-exception.t`. NB: the doc line still shows a residual
  `$!.^name` mismatch (`Any` vs `Nil`) because the *cleared* `$!` is `Value::NIL`,
  which reports `Any` — that is the deferred Nil-vs-Any identity knot below, not this
  fix.

### Deferred / deep (tracked elsewhere — do not re-open as a shallow slice)
These root causes account for a large share of the survey's `mism`/`crash` and are
intentionally deferred; see PLAN.md §8.5 and the ADRs:
- **Nil-vs-Any identity knot** — `Nil.rakudoc`, `Mu.rakudoc`, uninit-scalar `.raku`/gist. No clean safe subset (closed #4822 twice).
- **Lazy-list / container-repr (Track-B, fused with GC per ADR-0001)** — `List.rakudoc`, `Iterator.rakudoc`, `Iterable.rakudoc`, `Seq.rakudoc`, `list.rakudoc` (`loop`/`while`-as-lazy-list, flat-itemization depth).
- **`and`/`or`/`not` word-logical precedence** — `operators.rakudoc`, `control.rakudoc`, `traps.rakudoc` (looser than list-prefix; needs statement-level re-association).
- **FatRat-vs-Rat repr tag** — `Rat`/`FatRat`/`numerics` (`.^name` of a big FatRat is `Rat`).
- **`$/<key>` postcircumfix vs. lexical-name collision inside a block** — `regexes.rakudoc` [23]
  (`my regex line {...}; if "..." ~~ /<line> def/ { say $<line> }` → *No such method 'line' for Match*).
  When the hash-key of a `$/<key>` / `$<key>` access **names a lexical `my regex`/`token`/sub** and the
  access is **inside a block**, it mis-dispatches as a method call `$/.key`. Evidence it is a
  compile-context / runtime-scope bug, not a parse bug: `--dump-ast` is identical to the working
  top-level form (both `Index { index: Literal("key") }`); the same access works at top level, works for
  a builtin subrule key (`<alpha>`), and works for a `$<k>=(…)` named-capture key — only a
  block + lexical-regex-name-collision fails. Needs a focused look at how `Expr::Index` with a
  string-literal key resolves on `$/` when the key is also a lexical slot in a nested frame.
  (NB: `regexes.rakudoc` [3] `<same>` is a *separate* missing builtin subrule, not this root.)
- **WHICH-keyed QuantHash storage** — `QuantHash.rakudoc`, `Baggy`, `setbagmix` (Set/Bag key by stringification).
- **Custom `does Iterable`/`does Iterator` protocol** — `iterating.rakudoc`, `Iterator.rakudoc`.

### Untriaged
Everything in the survey below not listed above. The per-file minimal repros live in
`tmp/sweep/reports/<file>.txt` after a sweep — start from the highest-signal file and
re-verify each block against `raku` before writing a fix.

## Survey — files with divergences (high-signal first)

`mism` = output-mismatch · `crash` = mutsu exited non-zero where raku succeeded ·
`drift` = raku-drift-from-doc (version skew, low priority).

| file (under raku-doc/doc/) | mism | crash | drift |
|---|---:|---:|---:|
| Language/operators.rakudoc | 13 | 13 | 1 |
| Language/regexes.rakudoc | 9 | 8 | 3 |
| Language/traps.rakudoc | 15 | 2 | 4 |
| Type/Any.rakudoc | 6 | 10 | 6 |
| Type/List.rakudoc | 3 | 12 | 2 |
| Language/control.rakudoc | 6 | 7 | 1 |
| Language/variables.rakudoc | 7 | 3 | 5 |
| Type/IO/Path.rakudoc | 8 | 1 | 1 |
| Language/list.rakudoc | 6 | 3 | 3 |
| Type/Iterator.rakudoc | 4 | 5 | 2 |
| Language/objects.rakudoc | 5 | 3 | 3 |
| Language/perl-nutshell.rakudoc | 7 | 0 | 1 |
| Type/IO/Handle.rakudoc | 5 | 1 | 1 |
| Language/py-nutshell.rakudoc | 5 | 1 | 0 |
| Language/structures.rakudoc | 4 | 2 | 2 |
| Language/mop.rakudoc | 4 | 2 | 0 |
| Type/Match.rakudoc | 3 | 3 | 0 |
| Type/Range.rakudoc | 2 | 4 | 0 |
| Type/independent-routines.rakudoc | 1 | 5 | 2 |
| Type/Parameter.rakudoc | 1 | 5 | 1 |
| Language/numerics.rakudoc | 5 | 0 | 2 |
| Language/typesystem.rakudoc | 4 | 1 | 3 |
| Type/Mu.rakudoc | 4 | 1 | 2 |
| Language/concurrency.rakudoc | 4 | 1 | 1 |
| Language/grammars.rakudoc | 3 | 2 | 2 |
| Language/syntax.rakudoc | 3 | 2 | 1 |
| Type/Proc/Async.rakudoc | 3 | 2 | 0 |
| Language/functions.rakudoc | 3 | 2 | 0 |
| Language/nativecall.rakudoc | 1 | 4 | 0 |
| Language/experimental.rakudoc | 0 | 5 | 0 |
| Type/CallFrame.rakudoc | 4 | 0 | 1 |
| Language/unicode.rakudoc | 4 | 0 | 1 |
| Language/hashmap.rakudoc | 4 | 0 | 1 |
| Type/Routine.rakudoc | 3 | 1 | 1 |
| Type/Hash.rakudoc | 3 | 1 | 1 |
| Language/subscripts.rakudoc | 3 | 1 | 1 |
| Language/faq.rakudoc | 3 | 1 | 0 |
| Type/Backtrace.rakudoc | 1 | 3 | 0 |
| Language/containers.rakudoc | 3 | 0 | 4 |
| Type/IO/Spec/Win32.rakudoc | 3 | 0 | 2 |
| Type/IO/Spec/Unix.rakudoc | 3 | 0 | 2 |
| Type/Nil.rakudoc | 3 | 0 | 1 |
| Type/Array.rakudoc | 3 | 0 | 1 |
| Type/QuantHash.rakudoc | 3 | 0 | 0 |
| Language/perl-var.rakudoc | 3 | 0 | 0 |
| Type/Test.rakudoc | 2 | 1 | 0 |
| Type/Metamodel/DefiniteHOW.rakudoc | 2 | 1 | 0 |
| Type/Label.rakudoc | 2 | 1 | 0 |
| Language/js-nutshell.rakudoc | 2 | 1 | 0 |
| Language/signatures.rakudoc | 1 | 2 | 8 |
| Type/Cool.rakudoc | 1 | 2 | 5 |
| Type/Attribute.rakudoc | 1 | 2 | 1 |
| Type/Str.rakudoc | 1 | 2 | 0 |
| Type/Metamodel/EnumHOW.rakudoc | 1 | 2 | 0 |
| Type/Code.rakudoc | 0 | 3 | 3 |
| Type/Metamodel/Mixins.rakudoc | 0 | 3 | 0 |
| Language/nativetypes.rakudoc | 0 | 3 | 0 |
| Type/MixHash.rakudoc | 2 | 0 | 3 |
| Type/Junction.rakudoc | 2 | 0 | 3 |
| Type/Baggy.rakudoc | 2 | 0 | 3 |
| Type/Map.rakudoc | 2 | 0 | 2 |
| Type/Seq.rakudoc | 2 | 0 | 1 |
| Type/Scalar.rakudoc | 2 | 0 | 1 |
| Type/DateTime.rakudoc | 2 | 0 | 1 |
| Type/Iterable.rakudoc | 2 | 0 | 0 |
| Type/IO/Spec/Cygwin.rakudoc | 2 | 0 | 0 |
| Language/statement-prefixes.rakudoc | 2 | 0 | 0 |
| Language/phasers.rakudoc | 2 | 0 | 0 |
| Language/perl-func.rakudoc | 2 | 0 | 0 |
| Language/iterating.rakudoc | 2 | 0 | 0 |
| Language/glossary.rakudoc | 2 | 0 | 0 |
| Language/exceptions.rakudoc | 2 | 0 | 0 |
| Type/Pair.rakudoc | 1 | 1 | 2 |
| Type/Sub.rakudoc | 1 | 1 | 1 |
| Type/Metamodel/MethodContainer.rakudoc | 1 | 1 | 1 |
| Type/X/AdHoc.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleGroupHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/Documenting.rakudoc | 1 | 1 | 0 |
| Language/traits.rakudoc | 1 | 1 | 0 |
| Language/pod.rakudoc | 1 | 1 | 0 |
| Type/Unicode.rakudoc | 0 | 2 | 0 |
| Type/Metamodel/Trusting.rakudoc | 0 | 2 | 0 |
| Type/Lock/Async.rakudoc | 0 | 2 | 0 |
| Type/Formatter.rakudoc | 0 | 2 | 0 |
| Type/Mix.rakudoc | 1 | 0 | 2 |
| Type/Setty.rakudoc | 1 | 0 | 1 |
| Type/Set.rakudoc | 1 | 0 | 1 |
| Type/X/TypeCheck/Assignment.rakudoc | 1 | 0 | 0 |
| Type/X/Str/Match/x.rakudoc | 1 | 0 | 0 |
| Type/X/Proc/Async/MustBeStarted.rakudoc | 1 | 0 | 0 |
| Type/X/Proc/Async/CharsOrBytes.rakudoc | 1 | 0 | 0 |
| Type/X/Phaser/PrePost.rakudoc | 1 | 0 | 0 |
| Type/X/Method/InvalidQualifier.rakudoc | 1 | 0 | 0 |
| Type/X/Cannot/Empty.rakudoc | 1 | 0 | 0 |
| Type/Slip.rakudoc | 1 | 0 | 0 |
| Type/Sequence.rakudoc | 1 | 0 | 0 |
| Type/Real.rakudoc | 1 | 0 | 0 |
| Type/Proc.rakudoc | 1 | 0 | 0 |
| Type/Exception.rakudoc | 1 | 0 | 0 |
| Type/Buf.rakudoc | 1 | 0 | 0 |
| Type/Block.rakudoc | 1 | 0 | 0 |
| Type/Associative.rakudoc | 1 | 0 | 0 |
| Language/regexes-best-practices.rakudoc | 1 | 0 | 0 |
| Language/rb-nutshell.rakudoc | 1 | 0 | 0 |
| Language/quoting.rakudoc | 1 | 0 | 0 |
| Language/pragmas.rakudoc | 1 | 0 | 0 |
| Language/math.rakudoc | 1 | 0 | 0 |
| Language/ipc.rakudoc | 1 | 0 | 0 |
| Language/io-guide.rakudoc | 1 | 0 | 0 |
| Language/haskell-to-p6.rakudoc | 1 | 0 | 0 |
| Language/grammar_tutorial.rakudoc | 1 | 0 | 0 |
| Language/contexts.rakudoc | 1 | 0 | 0 |
| Language/classtut.rakudoc | 1 | 0 | 0 |
| Type/BagHash.rakudoc | 0 | 1 | 3 |
| Type/SetHash.rakudoc | 0 | 1 | 1 |
| Type/Metamodel/TypePretense.rakudoc | 0 | 1 | 1 |
| Type/Compiler.rakudoc | 0 | 1 | 1 |
| Type/X/TypeCheck/Splice.rakudoc | 0 | 1 | 0 |
| Type/X/ControlFlow.rakudoc | 0 | 1 | 0 |
| Type/Proxy.rakudoc | 0 | 1 | 0 |
| Type/PositionalBindFailover.rakudoc | 0 | 1 | 0 |
| Type/Method.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/Versioning.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/Stashing.rakudoc | 0 | 1 | 0 |
| Type/Lock/ConditionVariable.rakudoc | 0 | 1 | 0 |
| Type/IO/Special.rakudoc | 0 | 1 | 0 |
| Type/IO/Notification/Change.rakudoc | 0 | 1 | 0 |
| Type/IO/ArgFiles.rakudoc | 0 | 1 | 0 |
| Type/HyperWhatever.rakudoc | 0 | 1 | 0 |
| Type/CompUnit/Repository/FileSystem.rakudoc | 0 | 1 | 0 |
| Type/Callable.rakudoc | 0 | 1 | 0 |
| Language/using-modules/code.rakudoc | 0 | 1 | 0 |
| Language/unicode_entry.rakudoc | 0 | 1 | 0 |
| Language/optut.rakudoc | 0 | 1 | 0 |
| Language/newline.rakudoc | 0 | 1 | 0 |
