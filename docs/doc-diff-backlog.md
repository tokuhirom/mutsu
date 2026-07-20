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

- **Date:** 2026-07-21 · debug `mutsu` at main ≈ `0292015a` (includes #4963/#4967/#4979)
- **443 files scanned · 156 have signal**
- **match = 1959 · output-mismatch = 335 · mutsu-crash = 202 · raku-drift = 134**
- High-signal total (mismatch + crash) by corpus: **Type 287 · Language 250**

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

### Deferred / deep (tracked elsewhere — do not re-open as a shallow slice)
These root causes account for a large share of the survey's `mism`/`crash` and are
intentionally deferred; see PLAN.md §8.5 and the ADRs:
- **Nil-vs-Any identity knot** — `Nil.rakudoc`, `Mu.rakudoc`, uninit-scalar `.raku`/gist. No clean safe subset (closed #4822 twice).
- **Lazy-list / container-repr (Track-B, fused with GC per ADR-0001)** — `List.rakudoc`, `Iterator.rakudoc`, `Iterable.rakudoc`, `Seq.rakudoc`, `list.rakudoc` (`loop`/`while`-as-lazy-list, flat-itemization depth).
- **`and`/`or`/`not` word-logical precedence** — `operators.rakudoc`, `control.rakudoc`, `traps.rakudoc` (looser than list-prefix; needs statement-level re-association).
- **FatRat-vs-Rat repr tag** — `Rat`/`FatRat`/`numerics` (`.^name` of a big FatRat is `Rat`).
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
| Language/regexes.rakudoc | 20 | 9 | 3 |
| Language/operators.rakudoc | 13 | 13 | 1 |
| Type/Any.rakudoc | 8 | 10 | 4 |
| Language/traps.rakudoc | 15 | 2 | 4 |
| Type/List.rakudoc | 3 | 12 | 3 |
| Language/control.rakudoc | 6 | 7 | 1 |
| Type/Str.rakudoc | 9 | 3 | 0 |
| Language/variables.rakudoc | 7 | 4 | 5 |
| Type/IO/Path.rakudoc | 9 | 1 | 1 |
| Language/list.rakudoc | 6 | 3 | 3 |
| Language/structures.rakudoc | 6 | 3 | 2 |
| Type/Iterator.rakudoc | 4 | 5 | 2 |
| Language/objects.rakudoc | 5 | 3 | 3 |
| Language/perl-nutshell.rakudoc | 7 | 0 | 1 |
| Language/typesystem.rakudoc | 6 | 1 | 3 |
| Type/Test.rakudoc | 6 | 1 | 0 |
| Type/Capture.rakudoc | 6 | 1 | 0 |
| Type/IO/Handle.rakudoc | 5 | 1 | 2 |
| Language/subscripts.rakudoc | 5 | 1 | 0 |
| Language/py-nutshell.rakudoc | 5 | 1 | 0 |
| Language/mop.rakudoc | 4 | 2 | 0 |
| Language/functions.rakudoc | 4 | 2 | 0 |
| Type/Match.rakudoc | 3 | 3 | 0 |
| Type/Range.rakudoc | 2 | 4 | 0 |
| Type/independent-routines.rakudoc | 1 | 5 | 2 |
| Type/Parameter.rakudoc | 1 | 5 | 1 |
| Language/numerics.rakudoc | 5 | 0 | 2 |
| Type/Mu.rakudoc | 4 | 1 | 2 |
| Language/concurrency.rakudoc | 4 | 1 | 1 |
| Language/grammars.rakudoc | 3 | 2 | 2 |
| Type/Proc/Async.rakudoc | 3 | 2 | 0 |
| Language/syntax.rakudoc | 3 | 2 | 0 |
| Type/Cool.rakudoc | 1 | 4 | 5 |
| Language/nativecall.rakudoc | 1 | 4 | 0 |
| Type/Dateish.rakudoc | 0 | 5 | 0 |
| Language/experimental.rakudoc | 0 | 5 | 0 |
| Type/IO/CatHandle.rakudoc | 4 | 0 | 1 |
| Type/CallFrame.rakudoc | 4 | 0 | 1 |
| Language/unicode.rakudoc | 4 | 0 | 1 |
| Language/hashmap.rakudoc | 4 | 0 | 1 |
| Type/Routine.rakudoc | 3 | 1 | 1 |
| Type/Pair.rakudoc | 3 | 1 | 1 |
| Language/faq.rakudoc | 3 | 1 | 0 |
| Type/DateTime.rakudoc | 2 | 2 | 1 |
| Type/Backtrace.rakudoc | 1 | 3 | 0 |
| Type/Enumeration.rakudoc | 0 | 4 | 1 |
| Language/containers.rakudoc | 3 | 0 | 4 |
| Type/IO/Spec/Win32.rakudoc | 3 | 0 | 2 |
| Type/IO/Spec/Unix.rakudoc | 3 | 0 | 2 |
| Type/Nil.rakudoc | 3 | 0 | 1 |
| Type/Array.rakudoc | 3 | 0 | 1 |
| Type/Version.rakudoc | 3 | 0 | 0 |
| Type/QuantHash.rakudoc | 3 | 0 | 0 |
| Type/Iterable.rakudoc | 3 | 0 | 0 |
| Type/IO/Path/Parts.rakudoc | 3 | 0 | 0 |
| Language/perl-var.rakudoc | 3 | 0 | 0 |
| Type/Hash.rakudoc | 2 | 1 | 2 |
| Type/Metamodel/DefiniteHOW.rakudoc | 2 | 1 | 0 |
| Type/Label.rakudoc | 2 | 1 | 0 |
| Language/js-nutshell.rakudoc | 2 | 1 | 0 |
| Language/signatures.rakudoc | 1 | 2 | 7 |
| Type/Attribute.rakudoc | 1 | 2 | 1 |
| Type/Metamodel/EnumHOW.rakudoc | 1 | 2 | 0 |
| Type/Code.rakudoc | 0 | 3 | 3 |
| Type/Metamodel/Mixins.rakudoc | 0 | 3 | 0 |
| Language/nativetypes.rakudoc | 0 | 3 | 0 |
| Type/Junction.rakudoc | 2 | 0 | 3 |
| Type/Map.rakudoc | 2 | 0 | 2 |
| Type/Baggy.rakudoc | 2 | 0 | 2 |
| Type/Seq.rakudoc | 2 | 0 | 1 |
| Type/Scalar.rakudoc | 2 | 0 | 1 |
| Type/Real.rakudoc | 2 | 0 | 0 |
| Type/IO/Spec/Cygwin.rakudoc | 2 | 0 | 0 |
| Language/statement-prefixes.rakudoc | 2 | 0 | 0 |
| Language/phasers.rakudoc | 2 | 0 | 0 |
| Language/perl-func.rakudoc | 2 | 0 | 0 |
| Language/iterating.rakudoc | 2 | 0 | 0 |
| Language/glossary.rakudoc | 2 | 0 | 0 |
| Language/exceptions.rakudoc | 2 | 0 | 0 |
| Type/SetHash.rakudoc | 1 | 1 | 2 |
| Type/Sub.rakudoc | 1 | 1 | 1 |
| Type/Metamodel/MethodContainer.rakudoc | 1 | 1 | 1 |
| Type/X/AdHoc.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleGroupHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/Documenting.rakudoc | 1 | 1 | 0 |
| Language/traits.rakudoc | 1 | 1 | 0 |
| Language/pod.rakudoc | 1 | 1 | 0 |
| Language/classtut.rakudoc | 1 | 1 | 0 |
| Type/Unicode.rakudoc | 0 | 2 | 0 |
| Type/Method.rakudoc | 0 | 2 | 0 |
| Type/Metamodel/Trusting.rakudoc | 0 | 2 | 0 |
| Type/Lock/Async.rakudoc | 0 | 2 | 0 |
| Type/Formatter.rakudoc | 0 | 2 | 0 |
| Type/Date.rakudoc | 0 | 2 | 0 |
| Type/Mix.rakudoc | 1 | 0 | 2 |
| Type/Setty.rakudoc | 1 | 0 | 1 |
| Type/Set.rakudoc | 1 | 0 | 1 |
| Language/io-guide.rakudoc | 1 | 0 | 1 |
| Language/contexts.rakudoc | 1 | 0 | 1 |
| Type/X/TypeCheck/Binding.rakudoc | 1 | 0 | 0 |
| Type/X/TypeCheck/Assignment.rakudoc | 1 | 0 | 0 |
| Type/X/Str/Match/x.rakudoc | 1 | 0 | 0 |
| Type/X/Proc/Async/MustBeStarted.rakudoc | 1 | 0 | 0 |
| Type/X/Proc/Async/CharsOrBytes.rakudoc | 1 | 0 | 0 |
| Type/X/Phaser/PrePost.rakudoc | 1 | 0 | 0 |
| Type/X/Numeric/CannotConvert.rakudoc | 1 | 0 | 0 |
| Type/X/Mixin/NotComposable.rakudoc | 1 | 0 | 0 |
| Type/X/Method/InvalidQualifier.rakudoc | 1 | 0 | 0 |
| Type/X/Cannot/Empty.rakudoc | 1 | 0 | 0 |
| Type/Variable.rakudoc | 1 | 0 | 0 |
| Type/StrDistance.rakudoc | 1 | 0 | 0 |
| Type/Slip.rakudoc | 1 | 0 | 0 |
| Type/Signature.rakudoc | 1 | 0 | 0 |
| Type/Signal.rakudoc | 1 | 0 | 0 |
| Type/Sequence.rakudoc | 1 | 0 | 0 |
| Type/Proc.rakudoc | 1 | 0 | 0 |
| Type/Metamodel/PackageHOW.rakudoc | 1 | 0 | 0 |
| Type/Metamodel/MultipleInheritance.rakudoc | 1 | 0 | 0 |
| Type/Grammar.rakudoc | 1 | 0 | 0 |
| Type/Exception.rakudoc | 1 | 0 | 0 |
| Type/Complex.rakudoc | 1 | 0 | 0 |
| Type/Buf.rakudoc | 1 | 0 | 0 |
| Type/Block.rakudoc | 1 | 0 | 0 |
| Type/Associative.rakudoc | 1 | 0 | 0 |
| Type/Allomorph.rakudoc | 1 | 0 | 0 |
| Language/using-modules/introduction.rakudoc | 1 | 0 | 0 |
| Language/regexes-best-practices.rakudoc | 1 | 0 | 0 |
| Language/rb-nutshell.rakudoc | 1 | 0 | 0 |
| Language/quoting.rakudoc | 1 | 0 | 0 |
| Language/pragmas.rakudoc | 1 | 0 | 0 |
| Language/math.rakudoc | 1 | 0 | 0 |
| Language/ipc.rakudoc | 1 | 0 | 0 |
| Language/haskell-to-p6.rakudoc | 1 | 0 | 0 |
| Language/grammar_tutorial.rakudoc | 1 | 0 | 0 |
| Type/BagHash.rakudoc | 0 | 1 | 4 |
| Type/Metamodel/TypePretense.rakudoc | 0 | 1 | 1 |
| Type/Compiler.rakudoc | 0 | 1 | 1 |
| Type/X/TypeCheck/Splice.rakudoc | 0 | 1 | 0 |
| Type/X/ControlFlow.rakudoc | 0 | 1 | 0 |
| Type/Proxy.rakudoc | 0 | 1 | 0 |
| Type/PositionalBindFailover.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/Versioning.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/Stashing.rakudoc | 0 | 1 | 0 |
| Type/Lock/ConditionVariable.rakudoc | 0 | 1 | 0 |
| Type/IO/Special.rakudoc | 0 | 1 | 0 |
| Type/IO/Notification/Change.rakudoc | 0 | 1 | 0 |
| Type/IO/ArgFiles.rakudoc | 0 | 1 | 0 |
| Type/HyperWhatever.rakudoc | 0 | 1 | 0 |
| Type/CompUnit/Repository/FileSystem.rakudoc | 0 | 1 | 0 |
| Type/Callable.rakudoc | 0 | 1 | 0 |
| Type/Bool.rakudoc | 0 | 1 | 0 |
| Language/using-modules/code.rakudoc | 0 | 1 | 0 |
| Language/unicode_entry.rakudoc | 0 | 1 | 0 |
| Language/optut.rakudoc | 0 | 1 | 0 |
| Language/newline.rakudoc | 0 | 1 | 0 |
