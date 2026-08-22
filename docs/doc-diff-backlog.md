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

**When a finding is confirmed real (not raku-drift, not a harness false positive —
see "Known harness false positive" below), file it as a ticket immediately** under
`todo/tickets/<slug>.md` (or `todo/deep/` for high-blast-radius ones) per the root
`CLAUDE.md` conventions, and add a row to [Ticketed](#ticketed-open--linked-to-todo)
below linking the doc location to the ticket file. This is what keeps this backlog
and the `todo/` queue in sync — a finding sitting only in a sweep report or only in a
ticket file with no cross-link is easy to lose track of.

The **raw output of the latest committed sweep** is checked in under
[doc-diff-sweep/](doc-diff-sweep/) — read a per-file report there to get the minimal
repros without re-running the sweep. Re-copy it (see that dir's `README.md`) whenever
you refresh the survey.

**Always re-verify a finding directly before treating it as a real bug.** The
harness oracle-gates on raku, but doc examples drift and the harness can only compare
`# OUTPUT:`-style blocks. `raku-drift` findings (raku itself no longer matches the
doc) are version skew, not mutsu bugs — lowest priority.

## Corpus snapshot

- **Date:** 2026-08-22 (full re-sweep) · debug `mutsu` at main ≈ `7d45c5939`
- **444 files scanned · 124 have signal**
- **match = 2195 · output-mismatch = 184 · mutsu-crash = 112 · raku-drift = 120**
- High-signal total (mismatch + crash) = **296** (was 361 on 2026-07-22: mismatch
  −44, crash −21, signal files −6), reflecting the batch-1 (8 core `Type/` files)
  and batch-2 (`regexes`/`traps`/`variables`/`control`/`list`/`IO::Path`/`Any`/
  `objects`/`Iterator`/`typesystem`) ticketing rounds plus other fixes landed on
  `main` in between.
- **Raw per-file reports for this scan are NOT committed** (superseded the stale
  2026-07-22 [doc-diff-sweep/](doc-diff-sweep/) snapshot below) — regenerate with
  `scripts/doc-diff-sweep.sh` into `tmp/sweep/` (gitignored) to read the minimal
  repros; the committed `doc-diff-sweep/` dir still reflects the 2026-07-22 scan
  and is stale.

> Historical note: before the harness was made parallel-safe (#4982), sweeps run
> concurrently raced on a shared scratch file and manufactured phantom divergences
> (e.g. `syntax.rakudoc` reported 19 mismatches, only 3 real). Any pre-#4982 scan
> numbers — and memory/notes calling a file "block-misalignment garbage" — are
> unreliable; re-sweep instead.

> **2026-08-22 full re-sweep note:** this supersedes both the 2026-07-22 full-sweep
> snapshot and the 2026-08-22 partial re-checks of the 8 core `Type/` files and the
> batch-2 file set — the survey table below is now current for every row. The
> [Ticketed](#ticketed-open--linked-to-todo) section still lists those two rounds'
> findings (all still open); further rounds triaging the remaining untriaged files
> should append new dated subsections there rather than replacing them.

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
- `Type/QuantHash.rakudoc` [1]/[2]/[3] — `.Setty`/`.Baggy`/`.Mixy` on a
  `Set`/`Bag`/`Mix` (or `*Hash`) returned the bare mapped type object (`(Set)`,
  `(Bag)`, `(Mix)`) instead of coercing the receiver. `dispatch_setty_baggy_mixy`
  now delegates to the existing `.Set`/`.Bag`/`.Mix` (and `*Hash`) coercion
  helpers, preserving hashiness via the container's mutable flag. Also fixed
  `Mix.Set`/`Mix.Setty` dropping non-positive weights (`to_set` `Mix` arm kept
  every key) — **#5228**. Pin: `t/setty-baggy-mixy-coerce.t`.
- `hashmap.rakudoc` [1] — a Junction used as a hash-initializer key
  (`%( "a"|"b" => 1 )`) was stored under its stringification (`any(a, b)`) as a
  single literal key instead of threading. Per Rakudo a Junction key stores the
  value under each of its members (`%h<a> == %h<b> == 1`). Added `hash_pair_keys`
  (expands a Junction key to its members, else the key itself) and routed every
  hash-initializer `ValuePair` arm through it (`build_hash_from_items`,
  `coerce_to_hash`, `MakeHashFromPairs`), covering `%( )`, plain list assignment,
  and single-pair assignment. Pin: `t/hash-junction-key.t`.
- `SetHash.rakudoc` [1]/[2] — a QuantHash (SetHash/BagHash/MixHash) **slice**
  assignment (`$sh<a b> = False, True`) wrongly replaced the container with a
  fresh plain Hash of the raw rvalues, dropping every untouched member and the
  membership/count/weight semantics (mutsu gave `(apple kiwi)` for
  `<peach apple orange>.SetHash; $_<apple kiwi> = False, True` instead of
  `(kiwi orange peach)`). The named-slice-assign path only handled Array/Hash
  containers; added a mutable-Set/Bag/Mix arm that mirrors the single-key store
  (per-key membership/count/weight, Nil-pads a short rvalue rather than cycling,
  early-returns the per-key result list — Set → Bool, Bag → count, Mix → weight)
  and throws RO for an immutable Set/Bag/Mix. This also fixed the doc's
  `$fruits<apple banana kiwi>»++` hyper-increment over a SetHash slice. Pin:
  `t/quanthash-slice-assign.t`.
- `operators.rakudoc` [25]/[26] — the left-exclusive sequence operators
  (`^...` / `^...^`) failed to parse as an unparenthesized listop argument
  (`say 1 ^... 4`). `build_sequence_from_seeds` recognized `...`/`...^`/`…`/`…^`
  but not the `^`-prefixed forms; added them (strip the `^`, build the plain
  sequence, wrap in `.skip(1)`) — **#5116**. Pin: `t/seq-left-exclusive-listop.t`.
- `operators.rakudoc` [22]/[23] — a `Bool` was ordered by string, not numeric,
  value, so `0 cmp False`/`0 <=> False` were Less/More instead of Same and
  `min False, 0` dropped the first-on-tie rule. Normalize a Bool operand to its
  Int (False→0, True→1) in both `compare_values` and `spaceship_ordering`, fixing
  `cmp`/`<=>`/`before`/`after`/`min`/`max`/`sort` — **#5119**. Pin:
  `t/bool-numeric-compare.t`.
- `operators.rakudoc` [1] — `++$a.=abs` (`++($a.=abs)`) died with "prefix:<++>
  requires mutable arguments"; the prefix `++`/`--` compiler did not recognize an
  `AssignExpr` (the `.=` mutator shape) as an lvalue. Added an `AssignExpr` branch
  — **#5120**. Pin: `t/prefix-incr-dot-assign.t`.
- `operators.rakudoc` [17] — a qualified method call on a *type object*
  (`Foo.Bar::baz`) died X::Method::InvalidQualifier because `value_type_name`
  reports a type object's meta-type ("Package") and the non-instance path
  dispatched unqualified. Added a `Package` branch to
  `dispatch_qualified_non_instance_method` mirroring the instance path
  (`class_mro` + `resolve_method_with_owner`) — **#5124**. Pin:
  `t/qualified-parent-method-on-type-object.t`.
- `operators.rakudoc` [20]/[21] (partial) — `≅`/`=~=` short-circuited to True on
  any `a == b` (so `1 ≅ 1` stayed True at `$*TOLERANCE = 0`) and used `<=`. Now
  the short-circuit is infinities-only, the relative test is strict `<`, and pure
  reals skip the imaginary-part test. Also exempted built-in dynamics from
  X::Dynamic::Postdeclaration (`say $*OUT; { my $*OUT }`) via
  `is_builtin_dynamic_var` — **#5128**. Pin: `t/approx-equal-tolerance.t`.
  **Still open:** a bare `say $*TOLERANCE` reads undefined (not 1e-15); seeding it
  is blocked by the block-scope-dynamic desync below.
- `Mix.rakudoc` [1]/[2], `Baggy.rakudoc` [1] — Mix **construction** folded
  repeated-key weights with lossy f64 addition, so
  `(sugar => 0.1, sugar => 0.02).Mix<sugar>` was `0.12000000000000001` instead of
  `0.12` (and `Mix.new-from-pairs` the same). `MixData.weights` is still a
  `HashMap<String,f64>` store, but the two coercion ctors (`to_mix` in
  `quanthash_coerce.rs`, `dispatch_new_from_pairs`) now accumulate weights as
  exact `Value`s (`arith_add` keeps `Rat + Rat` exact via `mix_pair_weight_value`
  + `mix_accum`) and lower to the stored f64 only at the boundary, so the nearest
  double to `0.12` (which formats as `0.12`) is stored. Pin:
  `t/mix-weight-exact-accumulation.t`. **Still deferred:** Mix *arithmetic*
  operators (`$a (+) $b`) still add the already-f64 stored weights, so
  `(a=>0.1).Mix (+) (a=>0.02).Mix` remains `0.12000000000000001` — that needs the
  full exact-weight storage rework (the "FatRat-vs-Rat repr tag" class below), not
  a construction fix.

### Ticketed (open — linked to todo/)
Confirmed-real findings that have a filed ticket but are not yet fixed. Each row is the
doc location the harness flagged plus the ticket that tracks it; when the ticket is
resolved, move its content to `news/` (per `todo/README.md`) and delete the row here.

Found in the 2026-08-22 re-run of the 8 core `Type/` files (`Str`/`Array`/`List`/`Hash`/
`Num`/`Rat`/`Range`/`Map` — same set as the 2026-07-18 first run, which found 50
high-signal divergences on this set; this re-run found 12, most of the difference being
fixes that landed in between):

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Hash.rakudoc:65` | `WHAT {...}` (bare hash-literal block, no parens/var) misparses as two statements | [what-prefix-bare-hash-literal-block-arg.md](../todo/tickets/what-prefix-bare-hash-literal-block-arg.md) |
| `Type/Hash.rakudoc:336` | `my %h .= push(pair)` should leave `%h` empty, mutsu keeps the pair | [hash-dot-assign-push-result.md](../todo/tickets/hash-dot-assign-push-result.md) |
| `Type/List.rakudoc:219` | `(gather {...}).list.raku` keeps a spurious `.Seq` suffix when chained directly (no intermediate var) | [gather-chained-list-raku-seq-suffix.md](../todo/tickets/gather-chained-list-raku-seq-suffix.md) |
| `Type/List.rakudoc:1207` | global `rotor()` routine (`v6.e.PREVIEW`) not implemented; the `.rotor` method already works | [rotor-global-routine-missing.md](../todo/tickets/rotor-global-routine-missing.md) |
| `Type/Map.rakudoc:62` | `Map.new(a, 1, :b(2))` — bare colon-pair should bind as a named arg to `.new`, not a positional Pair | [map-new-bare-colonpair-named-arg.md](../todo/tickets/map-new-bare-colonpair-named-arg.md) |
| `Type/Range.rakudoc:80` | `@arr[$range-var]` doesn't flatten into `for` iteration (literal `@arr[0..2]` does) | [array-subscript-range-var-list-context-slip.md](../todo/tickets/array-subscript-range-var-list-context-slip.md) |
| `Type/Range.rakudoc:266` | `Range.int-bounds` method not implemented | [range-int-bounds-method-missing.md](../todo/tickets/range-int-bounds-method-missing.md) |
| `Type/Range.rakudoc:284` | `Range.minmax` on excluded-end Range should throw `X::AdHoc`, mutsu returns bounds silently | [range-minmax-excluded-ends-should-throw.md](../todo/tickets/range-minmax-excluded-ends-should-throw.md) |
| `Type/Str.rakudoc:647` | `.comb(:match)` (named-arg-only call) fails to dispatch at all; plain `.comb` and other arities work | [str-comb-named-arg-only-dispatch-missing.md](../todo/tickets/str-comb-named-arg-only-dispatch-missing.md) |

**Known harness false positive (not ticketed):** `Hash.rakudoc:21`, `Map.rakudoc:18`,
`Map.rakudoc:122` all flagged as `output-mismatch` on `.keys`/`.kv` iteration order.
Verified 2026-08-22 that raku's own hash/Map key order is randomized per-process (8
repeated single-line `raku -e` runs of the same `Map.rakudoc:122` example gave `(a b)`
6/8 times and `(b a)` 2/8) — the harness's single-run oracle comparison can't
distinguish this from a real ordering bug. The existing nondet heuristic (skips
`rand`/`.pick`/`.roll`/`now`/`Supply`) doesn't cover bare `.keys`/`.kv`/hash-iteration
output; improving that heuristic is a possible future harness enhancement, not tracked
as a ticket here.

Found in the 2026-08-22 batch-2 re-run of `regexes`/`traps`/`variables`/`control`/`list`/
`IO::Path`/`Any`/`objects`/`Iterator`/`typesystem`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Language/control.rakudoc:526,537` | `do when COND { BLOCK }` used as an expression crashes / gives the wrong value | [control-do-when-expression-value.md](../todo/tickets/control-do-when-expression-value.md) |
| `Language/control.rakudoc:717` | `race for` doesn't collect the loop's per-iteration results | [control-race-for-drops-results.md](../todo/tickets/control-race-for-drops-results.md) |
| `Language/control.rakudoc:388` | `if`/`unless`/`while` block body rejects a pointy-block parameter | [control-if-unless-pointy-block-param.md](../todo/tickets/control-if-unless-pointy-block-param.md) |
| `Language/control.rakudoc:854` | `default { }` cannot be used as a term nested inside an expression | [control-default-term-expression-position.md](../todo/tickets/control-default-term-expression-position.md) |
| `Language/control.rakudoc:973` | `proceed` inside a bare block with a trailing `when` modifier doesn't skip the enclosing `when` | [control-nested-when-proceed-skip.md](../todo/tickets/control-nested-when-proceed-skip.md) |
| `Language/control.rakudoc:1375` | `return-rw` doesn't return a mutable container | [control-return-rw-not-mutable.md](../todo/tickets/control-return-rw-not-mutable.md) |
| `Language/control.rakudoc:263` | statement-modifier `if` mid-comma-list inside parens is a hard parse failure | [control-if-modifier-mid-comma-list-parse-fail.md](../todo/tickets/control-if-modifier-mid-comma-list-parse-fail.md) |
| `Language/list.rakudoc:54` | a parenthesized `;`-separated statement list drops its trailing empty-statement element | [paren-statement-list-trailing-empty-element.md](../todo/tickets/paren-statement-list-trailing-empty-element.md) |
| `Type/IO/Path.rakudoc:128,290,423` | `IO::Path::Win32` inconsistently normalizes `/` vs `\` separators | [iopath-win32-separator-normalization.md](../todo/tickets/iopath-win32-separator-normalization.md) |
| `Type/IO/Path.rakudoc:561` | `~~ :w`/`:r`/`:x` smart-match ignores effective per-user permission (raw mode bits instead of `access()`) | [iopath-filetest-smartmatch-wrong-permission-check.md](../todo/tickets/iopath-filetest-smartmatch-wrong-permission-check.md) |

**Excluded from this batch-2 re-run (already deferred/resolved/drift/false-positive):**
- `Language/control.rakudoc` [1] (`{ block } or die;` not executing the block) — the
  already-**Deferred** "`and`/`or`/`not` word-logical precedence" cluster, explicitly named for
  `control.rakudoc`.
- `Language/control.rakudoc` [7] — hash-iteration-order `raku-drift`, both sides nondeterministic.
- `Language/control.rakudoc` [10] — `X::Multi::NoMatch` message text is shorter in mutsu but the
  exception type/meaning match; cosmetic, not ticketed.
- `Language/list.rakudoc` [1] tail (line 338, geometric sequence past i64 degrading to Float) —
  the named **Deferred** "big-Int→Float degradation in geometric sequence generation past i64"
  item.
- `Language/list.rakudoc` [2] (line 707, `@cards.shape` with enum-valued dimensions) — bucketed
  `raku-drift` (doc says "Deuce Deuce", raku prints "2 2"); mutsu's own `(*)` looks wrong too but
  wasn't ticketed per the skip-drift instruction — worth a second look later.
- `Type/IO/Path.rakudoc` [4] (line 509, `MAIN` sub recursively walking `.`) — mutsu times out;
  inherently dependent on this checkout's file count (walks `target/`/`vendor/`/`.git/`), not a
  clean minimal repro — flagged for awareness, not ticketed.
- `Type/IO/Path.rakudoc` [6] (line 609, `$*EXECUTABLE.IO.s`) — `raku-drift` and
  build-environment-dependent (binary size), not a bug.

Found in the same 2026-08-22 batch-2 re-run, `Type/Any.rakudoc` / `Language/objects.rakudoc` /
`Type/Iterator.rakudoc` / `Language/typesystem.rakudoc`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Any.rakudoc:961` | `.categorize`/`.classify` into an untyped Hash mis-renders a non-Str bucket key when the source array holds class instances | [categorize-into-hash-instance-bool-key-corruption.md](../todo/tickets/categorize-into-hash-instance-bool-key-corruption.md) |
| `Type/Any.rakudoc:1525` | `.snip` with multiple positional predicates silently drops all but the first | [snip-multiple-positional-matchers-dropped.md](../todo/tickets/snip-multiple-positional-matchers-dropped.md) |
| `Type/Any.rakudoc:1549,1559` | `.snitch` (v6.e.PREVIEW debugging method) is entirely unimplemented | [snitch-method-unimplemented.md](../todo/tickets/snitch-method-unimplemented.md) |
| `Type/Any.rakudoc:1420` | `$*COLLATION.set(...)` does not persist / is not honored by `coll` | [collation-set-not-persisted.md](../todo/tickets/collation-set-not-persisted.md) |
| `Language/objects.rakudoc:1067` | a user class `is Str` (or other native type) doesn't inherit native stringification | [str-subclass-loses-native-stringify.md](../todo/tickets/str-subclass-loses-native-stringify.md) |
| `Language/objects.rakudoc:1132` | a role's 0-arg `multi method` loses its trailing string literal, only when the composing class has its own extra attribute | [role-multi-method-trailing-literal-dropped.md](../todo/tickets/role-multi-method-trailing-literal-dropped.md) |
| `Language/objects.rakudoc:1185` | attribute order in a `.new`/gist under multiple inheritance is wrong (deterministic, not hash-order noise) | [multi-inheritance-attribute-gist-order.md](../todo/tickets/multi-inheritance-attribute-gist-order.md) |
| `Language/objects.rakudoc:1457` | `but`-mixing a role onto a `List` breaks its `Positional` binding | [list-but-role-loses-positional-binding.md](../todo/tickets/list-but-role-loses-positional-binding.md) |
| `Language/objects.rakudoc:1526` | `is default(0 but role :: {...})` on a typed Hash drops the role mixin on the default value | [hash-default-role-mixin-dropped.md](../todo/tickets/hash-default-role-mixin-dropped.md) |
| `Language/objects.rakudoc:65` | colon-call syntax with zero arguments (`.method:` immediately followed by `;`) fails to parse | [colon-call-empty-args-parse-error.md](../todo/tickets/colon-call-empty-args-parse-error.md) |
| `Language/objects.rakudoc:1397` | a parameterized role with a self-referential attribute type fails a spurious type-check, with a malformed error message | [parametric-role-self-referential-attribute-typecheck.md](../todo/tickets/parametric-role-self-referential-attribute-typecheck.md) |
| `Language/typesystem.rakudoc:657` | a custom `.gist` on a role-mixed native value is skipped when gisted inside an array/list | [role-mixed-value-gist-skipped-in-array.md](../todo/tickets/role-mixed-value-gist-skipped-in-array.md) |
| `Language/typesystem.rakudoc:846` | `enum ... does Role`'s overriding `ACCEPTS` is never dispatched by `~~`, plus a spurious `=>` warning | [enum-does-role-accepts-not-dispatched.md](../todo/tickets/enum-does-role-accepts-not-dispatched.md) |
| `Language/typesystem.rakudoc:594` | a role's `.new` instance reports the wrong `.HOW` metaclass | [role-instance-how-wrong-metaclass.md](../todo/tickets/role-instance-how-wrong-metaclass.md) |
| `Language/typesystem.rakudoc:611` | a forward-declared role stub used by another role is never upgraded to its real body | [forward-declared-role-stub-not-upgraded.md](../todo/tickets/forward-declared-role-stub-not-upgraded.md) |
| `Language/typesystem.rakudoc:644` | a role parameter's `fail(...)` default expression is never evaluated/enforced | [role-parameter-fail-default-not-enforced.md](../todo/tickets/role-parameter-fail-default-not-enforced.md) |

Three of these ([list-but-role-loses-positional-binding.md](../todo/tickets/list-but-role-loses-positional-binding.md),
[hash-default-role-mixin-dropped.md](../todo/tickets/hash-default-role-mixin-dropped.md),
[role-mixed-value-gist-skipped-in-array.md](../todo/tickets/role-mixed-value-gist-skipped-in-array.md))
share a hypothesis — a `but`/`does`-mixed value's role metadata not surviving a generic
storage/dispatch path — noted cross-links in each ticket; investigate together and merge into
one PR if a single fix site is found.

**Excluded from this sub-batch (already deferred/resolved/drift/false-positive):**
- `Type/Any.rakudoc` [2], [6]-[11] — hash/Set/Bag iteration-order/address `raku-drift`.
- `Language/objects.rakudoc` [1], [2] — `raku-drift` (stale doc error-message wording).
- `Type/Iterator.rakudoc` [1] — the **Deferred** "Custom `does Iterable`/`does Iterator`
  protocol" cluster, explicitly named for this file.
- `Type/Iterator.rakudoc` [2], [3], [5] — the **Deferred** lazy-list cluster's "`=:= IterationEnd`
  container identity" and "IterationEnd's repr (it is a Str internally...)" residues.
- `Type/Iterator.rakudoc` [4] — `raku-drift` (doc's stated OUTPUT no longer matches current raku
  for the custom-iterator example).
- `Language/typesystem.rakudoc` [3] — `raku-drift` (object hex-address text, inherently
  non-reproducible).
- `Language/typesystem.rakudoc` [6] — the exception *type* text is drift (`X::AdHoc` vs. real
  raku's `X::Role::Instantiation`), but mutsu not throwing at all is real — filed as
  [role-parameter-fail-default-not-enforced.md](../todo/tickets/role-parameter-fail-default-not-enforced.md)
  above.

Found in the same 2026-08-22 batch-2 re-run, `Language/regexes.rakudoc` /
`Language/traps.rakudoc` / `Language/variables.rakudoc`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Language/regexes.rakudoc:1331` | `<!:Script<Name>>` negated Unicode-property lookahead doesn't stop the preceding quantifier's backtrack | [regex-script-lookahead-negation-wrong.md](../todo/tickets/regex-script-lookahead-negation-wrong.md) |
| `Language/regexes.rakudoc:1349` | `<same>` builtin regex subrule is unimplemented | [regex-same-subrule-missing.md](../todo/tickets/regex-same-subrule-missing.md) |
| `Language/regexes.rakudoc:1595` | a regex-embedded `:my $c = $/;` (self-referencing the in-progress match) fails to parse | [regex-embedded-my-decl-self-referential-slash.md](../todo/tickets/regex-embedded-my-decl-self-referential-slash.md) |
| `Language/regexes.rakudoc:1602` | a regex-embedded `:my $c = ~$0;` captures an empty value instead of the current match text | [regex-embedded-my-decl-value-not-captured.md](../todo/tickets/regex-embedded-my-decl-value-not-captured.md) |
| `Language/regexes.rakudoc:1612` | a regex-embedded `:our $var = ...;` doesn't write back to the package variable | [regex-our-declarator-writeback-missing.md](../todo/tickets/regex-our-declarator-writeback-missing.md) |
| `Language/regexes.rakudoc:1966` | a subrule call with a block-literal argument (`<name: { ... }>`) fails entirely | [regex-subrule-block-argument-parse-fail.md](../todo/tickets/regex-subrule-block-argument-parse-fail.md) |
| `Language/regexes.rakudoc:1543` | capture-group numbering across an alternation branch is wrong | [regex-capture-numbering-across-alternation.md](../todo/tickets/regex-capture-numbering-across-alternation.md) |
| `Language/regexes.rakudoc:1587` | an embedded code block inside a quantified group doesn't persist its side effect on an outer `:my` variable | [regex-embedded-code-block-quantifier-scope.md](../todo/tickets/regex-embedded-code-block-quantifier-scope.md) |
| `Language/regexes.rakudoc:1816` | `s///` replacement string doesn't interpolate named captures (`$<name>`) | [subst-named-capture-interpolation-in-replacement.md](../todo/tickets/subst-named-capture-interpolation-in-replacement.md) |
| `Language/regexes.rakudoc:1823` | `s///` replacement over-escapes a literal `\:` | [subst-replacement-code-block-not-evaluated.md](../todo/tickets/subst-replacement-code-block-not-evaluated.md) |
| `Language/regexes.rakudoc:2290` | `S:g/@(EXPR)/.../ ` doesn't interpolate an array as a regex alternation | [subst-array-interpolation-as-alternation.md](../todo/tickets/subst-array-interpolation-as-alternation.md) |
| `Language/regexes.rakudoc:2623` | a custom grammar `ws` token override with `<!ww>` doesn't match per raku's boundary rules | [grammar-ws-boundary-and-vertical-whitespace.md](../todo/tickets/grammar-ws-boundary-and-vertical-whitespace.md) |
| `Language/regexes.rakudoc:2684` | `m:st(...)` regex adverb (starting positions) is unsupported | [regex-st-adverb-unsupported.md](../todo/tickets/regex-st-adverb-unsupported.md) |
| `Language/regexes.rakudoc:2892` | a grammar's `FAILGOAL` method isn't invoked when a goal-matching conjunction (`~`) fails | [grammar-failgoal-not-invoked.md](../todo/tickets/grammar-failgoal-not-invoked.md) |
| `Language/regexes.rakudoc:2935` | `<~~>` recursive self-match returns the wrong (inner, not outer) nesting level | [regex-recursive-self-match-wrong-nesting-level.md](../todo/tickets/regex-recursive-self-match-wrong-nesting-level.md) |
| `Language/traps.rakudoc:91` | `$++` inside a string-interpolated block doesn't reset per call the way raku's does | [dollar-plusplus-state-scope-in-interpolated-block.md](../todo/tickets/dollar-plusplus-state-scope-in-interpolated-block.md) |
| `Language/traps.rakudoc:212` | `$.attr *= 2` inside a method throws `X::Assignment::RO` where current raku silently no-ops | [dollar-dot-attr-compound-assign-spurious-ro-error.md](../todo/tickets/dollar-dot-attr-compound-assign-spurious-ro-error.md) |
| `Language/traps.rakudoc:1948` | `\|«@array` (flatten + hyper prefix combined) fails to parse | [flatten-hyper-prefix-parse-error.md](../todo/tickets/flatten-hyper-prefix-parse-error.md) |
| `Language/traps.rakudoc:1067` | `for EXPR ~~ /regex/ { BLOCK }` executes `BLOCK` where raku produces no output | [for-loop-over-smartmatch-result-executes-unexpectedly.md](../todo/tickets/for-loop-over-smartmatch-result-executes-unexpectedly.md) |
| `Language/traps.rakudoc:406`, `Language/variables.rakudoc:853` | a scalar's container isn't aliased when pushed into a collection without `.clone` — mutsu snapshots by value where raku aliases | [container-aliasing-not-preserved-into-collection.md](../todo/tickets/container-aliasing-not-preserved-into-collection.md) |
| `Language/variables.rakudoc:1551` | `».&?BLOCK` (hyper-call with a block self-reference) dispatches an empty method name | [hyper-call-block-self-reference-empty-method.md](../todo/tickets/hyper-call-block-self-reference-empty-method.md) |
| `Language/variables.rakudoc:134` | `my ($g) = LIST;` gives `$g.VAR.^name` of `Int` instead of `Scalar` (harness mis-bucketed as drift) | [paren-single-var-decl-var-scalar-name.md](../todo/tickets/paren-single-var-decl-var-scalar-name.md) |
| `Language/variables.rakudoc:768` | `anon class`/`anon sub` with a non-ASCII name fails to parse; `anon sub NAME` also gists without the `&` sigil | [anon-class-sub-non-ascii-name-and-sub-gist.md](../todo/tickets/anon-class-sub-non-ascii-name-and-sub-gist.md) |
| `Language/variables.rakudoc:1765` | `$*RAKU` reports the wrong metaclass name (`Perl`) and inconsistent stringification | [dollar-raku-wrong-metaclass-and-stringify.md](../todo/tickets/dollar-raku-wrong-metaclass-and-stringify.md) |
| `Language/variables.rakudoc:318` | `$?FILE` reports the relative invocation path instead of an absolute path | [dollar-question-file-relative-not-absolute.md](../todo/tickets/dollar-question-file-relative-not-absolute.md) |

**Excluded from this sub-batch (already deferred/resolved/drift/false-positive/environment):**
- `Language/regexes.rakudoc` [3], [8] — `raku-drift`.
- `Language/traps.rakudoc` [1], [2] — instances of the already-**Deferred** Nil-vs-Any identity
  knot (`%h is default(Nil)` and `:= Nil` both render `(Any)` instead of `Nil`).
- `Language/traps.rakudoc` [6] — Set iteration-order `raku-drift`.
- `Language/variables.rakudoc` [1] — mutsu errors partway through a recursive `.` directory walk
  (same shape as `Type/IO/Path.rakudoc` [4] from the earlier sub-batch); the real bug underneath
  it (`».&?BLOCK` dispatching an empty method name) was isolated to a small non-environment-
  dependent repro and filed as
  [hyper-call-block-self-reference-empty-method.md](../todo/tickets/hyper-call-block-self-reference-empty-method.md).
- `Language/variables.rakudoc` [3] — `$?FILE` reports mutsu's relative invocation path where raku
  resolves to an absolute path; confirmed real, low-priority/cosmetic, filed as
  [dollar-question-file-relative-not-absolute.md](../todo/tickets/dollar-question-file-relative-not-absolute.md).
- `Language/variables.rakudoc` [6], [7], [9], [10] — `raku-drift` (`$*DISTRO`, `$*VM.config`,
  `$*RAKU.compiler.version` are inherently environment/version-specific).
- `Language/variables.rakudoc` [8] — `$*VM.precomp-ext`/`.precomp-target` report `mutsu`/`mutsu`
  instead of raku's MoarVM-specific `moarvm`/`mbc`; this looks like an intentional identity
  difference (mutsu isn't MoarVM), not a bug — not ticketed.

Found in the 2026-08-22 batch-3 re-run of `IO::Handle`/`structures`/`mop`/`independent-routines`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/IO/Handle.rakudoc:385` | global `lines(IO_object)` sub form doesn't open/read the file, just wraps the path arg | [lines-sub-form-io-object-not-read.md](../todo/tickets/lines-sub-form-io-object-not-read.md) |
| `Type/IO/Handle.rakudoc:761` | `.open` stores the CWD-absolutized path instead of the path as given (`.Str`/`.path`) | [io-handle-open-stores-absolute-not-given-path.md](../todo/tickets/io-handle-open-stores-absolute-not-given-path.md) |
| `Type/IO/Handle.rakudoc:959,1013` | custom `IO::Handle` subclasses overriding `WRITE`/`READ`/`EOF` are never dispatched to by native print/say/read | [custom-io-handle-write-read-not-dispatched.md](../todo/deep/custom-io-handle-write-read-not-dispatched.md) |
| `Language/structures.rakudoc:233` | calling an undefined `Any`-typed value as a function throws instead of returning the args | [undefined-any-called-as-sub-throws.md](../todo/tickets/undefined-any-called-as-sub-throws.md) |
| `Language/structures.rakudoc:258` | `but Associative[Int, Int]` (built-in parametric role) fails as "non-composable" | [builtin-parametric-role-mixin-not-composable.md](../todo/tickets/builtin-parametric-role-mixin-not-composable.md) |
| `Language/structures.rakudoc:281` | `.sort` on a role-mixed (`but`) Hash returns the whole hash unsorted in a 1-element list | [role-mixed-hash-sort-method-dispatch-broken.md](../todo/tickets/role-mixed-hash-sort-method-dispatch-broken.md) |
| `Language/structures.rakudoc:26`, `Language/mop.rakudoc:120` | `$(LIST).VAR.^name` reports `List` instead of `Scalar` (item contextualizer doesn't itemize) | [item-contextualized-list-var-name-not-scalar.md](../todo/tickets/item-contextualized-list-var-name-not-scalar.md) |
| `Language/structures.rakudoc:458` | `$metadata.can($metadata, "uc")` returns `()` instead of `(uc uc)` when the invocant-arg is a `.HOW` | [metaclass-can-with-metaclass-as-invocant-arg-empty.md](../todo/tickets/metaclass-can-with-metaclass-as-invocant-arg-empty.md) |
| `Language/mop.rakudoc:329` | a grammar's `method ^parameterize` + parametric role application stack-overflows | [grammar-metaclass-parameterize-stack-overflow.md](../todo/deep/grammar-metaclass-parameterize-stack-overflow.md) |
| `Language/mop.rakudoc:34` | `constant NAME := Metamodel::ClassHOW.new_type(name => 'NAME')` immediately errors as "immutable" | [direct-metamodel-classhow-new-type-immutable-error.md](../todo/deep/direct-metamodel-classhow-new-type-immutable-error.md) |
| `Language/mop.rakudoc:93` | `.HOW.^name` on a hash literal is missing the `+{<anon>}` mixin suffix | [how-gist-missing-anon-mixin-suffix.md](../todo/tickets/how-gist-missing-anon-mixin-suffix.md) |
| `Type/independent-routines.rakudoc:110` | `EVAL` doesn't synthesize an `EVAL_N` filename for `$?FILE`, and ignores the `:filename` arg | [eval-dollar-question-file-not-synthesized.md](../todo/tickets/eval-dollar-question-file-not-synthesized.md) |
| `Type/independent-routines.rakudoc:148` | `repl()` global routine is unimplemented (mutsu already has REPL machinery to reuse) | [repl-routine-unimplemented.md](../todo/tickets/repl-routine-unimplemented.md) |
| `Type/independent-routines.rakudoc:473` | `open(:w, PATH)` — a named adverb before the positional path breaks argument parsing | [open-named-adverb-before-positional-path.md](../todo/tickets/open-named-adverb-before-positional-path.md) |
| `Type/independent-routines.rakudoc:687,692` | `.printf` method form is unimplemented on Str, and format directives don't autothread over a Junction | [printf-method-form-and-junction-autothread-missing.md](../todo/tickets/printf-method-form-and-junction-autothread-missing.md) |
| `Type/independent-routines.rakudoc:1497` | `done VALUE` inside a `supply {}` block drops the value instead of emitting it first | [supply-done-value-drops-emit.md](../todo/tickets/supply-done-value-drops-emit.md) |
| `Type/independent-routines.rakudoc:312` | hyper method call (`».method`) / `.map` on a `gather {...}` Seq returns empty instead of forcing it | [gather-hyper-method-call-empty-result.md](../todo/tickets/gather-hyper-method-call-empty-result.md) |

**Excluded from this batch-3 sub-run (already deferred/resolved/drift/false-positive):**
- `Type/IO/Handle.rakudoc` [385] itself was bucketed `raku-drift` by the harness (raku's own
  `/proc/$*PID/statm` numbers are PID-dependent and don't match the doc's stated numbers), but the
  *shape* of mutsu's output underneath that drift (wrapping the literal path instead of reading the
  file at all) is a real, separate bug — filed above as
  [lines-sub-form-io-object-not-read.md](../todo/tickets/lines-sub-form-io-object-not-read.md).
- `Language/structures.rakudoc` [36], [45] (`.WHICH` addresses) — `raku-drift`, pointer values are
  inherently non-reproducible.
- `Language/structures.rakudoc` [95], [108] (`%hash.list[0]`, `<a b c d>.Hash.kv` ordering) — the
  "Known harness false positive" hash/Set iteration-order nondeterminism documented above the
  Ticketed section; both sides are randomized per-process in real raku too.
- `Language/structures.rakudoc` [123] (`class SortedArray is Array { method iterator {...} }`) —
  matches the already-**Deferred** Lazy-list cluster's explicitly-named residue: "the custom `does
  Iterator` residue where an `is Array` subclass skips its user iterator (`__mutsu_array_storage`
  guard in `vm_for_loop_dispatch.rs`)".
- `Type/independent-routines.rakudoc` [1429] (`append %h, i => (1, 42)`) — bucketed `raku-drift`
  because the doc's stated error text doesn't match raku's actual error text; mutsu also throws (a
  different, generic "Unknown call: append" instead of a specific signature-mismatch error) — same
  "exception type/meaning matches, message text differs" shape already excluded elsewhere in this
  ledger (e.g. `Language/control.rakudoc` [10] above), not ticketed.

Found in the 2026-08-22 batch-3 re-run of `grammars`/`syntax`/`Proc::Async`/`nativecall`/
`experimental`/`unicode`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Language/grammars.rakudoc:112,132` | `[+]`/`[-]` reduce meta-op silently gives `0` for Match/user-Numeric objects (arith ops bypass the numeric-coercion bridge `.reduce()`/binary `+` use) | [reduce-metaop-numeric-coercion-bypassed.md](../todo/tickets/reduce-metaop-numeric-coercion-bypassed.md) |
| `Language/grammars.rakudoc:260` | `<sym>` named-capture inside a `token X:sym<...>` proto/multi-dispatch body isn't recorded on the Match | [grammar-sym-capture-not-in-match-hash.md](../todo/tickets/grammar-sym-capture-not-in-match-hash.md) |
| `Language/grammars.rakudoc:387` | a grammar-embedded custom assertion method (`<.method>`) sees `self` as an uninstantiated type object | [grammar-embedded-custom-assertion-method-self-type-object.md](../todo/tickets/grammar-embedded-custom-assertion-method-self-type-object.md) |
| `Language/grammars.rakudoc:289` | `.tail ~= ...` on a private class-attribute array silently no-ops instead of mutating the last element | [tail-lvalue-compound-assign-attribute-array-noop.md](../todo/tickets/tail-lvalue-compound-assign-attribute-array-noop.md) |
| `Language/grammars.rakudoc:509` | a grammar rule with dynamic-variable parameters (`rule TOP ($*word, $*extra)`) + `.parse(..., :args(...))` fails entirely (harness mis-bucketed as drift) | [grammar-dynamic-rule-parameters-args-fail.md](../todo/tickets/grammar-dynamic-rule-parameters-args-fail.md) |
| `Language/syntax.rakudoc:354,384` | adverbial-pair variable name syntax (`$var:adverb<value>`) is incomplete — quoted-identifier form is a hard parse error, interpolated adverb values don't resolve | [adverbial-pair-variable-name-syntax-incomplete.md](../todo/tickets/adverbial-pair-variable-name-syntax-incomplete.md) |
| `Language/syntax.rakudoc:429` | `OUR::` pseudo-package doesn't expose file-scope `our`-declared package symbols | [our-pseudopackage-missing-file-scope-symbols.md](../todo/tickets/our-pseudopackage-missing-file-scope-symbols.md) |
| `Language/syntax.rakudoc:1091` | a colon-call's trailing `.method` (`.substr: 0, 3  .uc`) binds to the wrong operand | [colon-call-trailing-dot-method-binds-wrong-operand.md](../todo/tickets/colon-call-trailing-dot-method-binds-wrong-operand.md) |
| `Type/Proc/Async.rakudoc:172,203` | `Proc::Async.new` shoves an unrecognized named arg (e.g. `:r`) into the spawned command's argv, corrupting it | [procasync-new-unknown-named-arg-leaks-into-argv.md](../todo/tickets/procasync-new-unknown-named-arg-leaks-into-argv.md) |
| `Type/Proc/Async.rakudoc:102,110,233` | `Proc::Async` output is silently dropped instead of passed through when nobody `.tap`s the stdout/stderr Supply | [procasync-untapped-stdout-not-passthrough.md](../todo/tickets/procasync-untapped-stdout-not-passthrough.md) |
| `Language/nativecall.rakudoc:1186` | `enum Name (a => 1; b => 2;)` — semicolon-separated variant list fails to parse as an enum at all (general parser bug, not NativeCall-specific) | [enum-semicolon-separated-variants-parse-fail.md](../todo/tickets/enum-semicolon-separated-variants-parse-fail.md) |
| `Language/nativecall.rakudoc:598` | `Pointer[T].deref` method is missing (the FFI call itself succeeds) | [nativecall-pointer-deref-method-missing.md](../todo/tickets/nativecall-pointer-deref-method-missing.md) |
| `Language/experimental.rakudoc:32` | `Buf`/`Blob.contents` method is missing | [buf-contents-method-missing.md](../todo/tickets/buf-contents-method-missing.md) |
| `Language/experimental.rakudoc:144` | a user-defined custom infix's RHS operand fails to parse when followed by `??...!!` | [custom-infix-rhs-operand-rejects-ternary.md](../todo/tickets/custom-infix-rhs-operand-rejects-ternary.md) |
| `Language/unicode.rakudoc:83,90` | `utf8-c8` decoding of an invalid byte uses a different synthetic private-use codepoint than raku | [utf8-c8-invalid-byte-codepoint-mismatch.md](../todo/tickets/utf8-c8-invalid-byte-codepoint-mismatch.md) |
| `Language/unicode.rakudoc:190,212,224` | `\c[NAME]` fails to resolve Unicode NameAlias corrections and multi-codepoint named sequences | [c-bracket-character-name-lookup-gaps.md](../todo/tickets/c-bracket-character-name-lookup-gaps.md) |

**Excluded from this batch-3 sub-run:**
- `Language/syntax.rakudoc` [5] (line 763, `%(...)`hash-constructor `.keys.join`) — hash
  iteration-order `raku-drift`, both sides nondeterministic (same known false positive as
  the survey table's `Hash.rakudoc`/`Map.rakudoc` note above).
- `Language/experimental.rakudoc` [2], [3], [4] (`use experimental :macros`, `macro`/`quasi`
  examples) — the already-tracked deep `macro`/`quasi`/unquote design work in
  `todo/deep/rakuast-remaining.md`'s "Macros" section; not re-ticketed here.
- `Language/nativecall.rakudoc` [2] (the `getaddrinfo`/DNS CStruct example) — beyond the
  parse-time `enum` bug ticketed above, this example also depends on a live DNS lookup
  (`google.com`) and full `CStruct`/`nativecast` FFI plumbing; not a clean minimal repro on
  its own and squarely within NativeCall's known, measured gaps
  (`todo/deep/nativecall-cannot-be-vendored.md`) — not ticketed further.

Found in the 2026-08-22 batch-3 re-run of `Parameter`/`Match`/`Mu`/`subscripts`/`concurrency`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Parameter.rakudoc:306` | a generic `::T` type parameter isn't resolved when reporting later type-check failures (shows literal `T`, and takes a completely different error-shape for a second case) | [generic-type-param-binding-error-message-unresolved.md](../todo/tickets/generic-type-param-binding-error-message-unresolved.md) |
| `Type/Parameter.rakudoc:347,395` | `Parameter.sub_signature`/`.modifier` reflection methods are unimplemented | [parameter-sub-signature-and-modifier-attrs-missing.md](../todo/tickets/parameter-sub-signature-and-modifier-attrs-missing.md) |
| `Type/Parameter.rakudoc:197` | `+@l` slurpy's "single argument rule" wrongly preserves element identity like `is raw` does | [plus-sigil-single-arg-rule-list-context-flattening.md](../todo/tickets/plus-sigil-single-arg-rule-list-context-flattening.md) |
| `Type/Match.rakudoc:20` | `$¢` (cursor-position variable) inside a regex-embedded code block is unimplemented | [dollar-cent-cursor-variable-in-embedded-regex-block-missing.md](../todo/tickets/dollar-cent-cursor-variable-in-embedded-regex-block-missing.md) |
| `Type/Match.rakudoc:40` | `$0` read inside a regex-embedded code block returns the raw string instead of a `Match` object | [dollar-numbered-capture-in-embedded-regex-block-returns-raw-value.md](../todo/tickets/dollar-numbered-capture-in-embedded-regex-block-returns-raw-value.md) |
| `Type/Mu.rakudoc:238` | default `.clone()` doesn't share Array/Hash-typed attribute containers with the original (found while investigating a raku-drift-bucketed example — the drift bucketing itself is correct, but a real bug was hiding in the same example) | [clone-array-hash-attribute-containers-not-shared.md](../todo/tickets/clone-array-hash-attribute-containers-not-shared.md) |
| `Type/Mu.rakudoc:267` | assigning a list through a `%`-sigil `rw` accessor doesn't coerce it to Hash pairs the way a direct `%var = list` does | [hash-attribute-rw-accessor-list-assignment-not-coerced-to-pairs.md](../todo/tickets/hash-attribute-rw-accessor-list-assignment-not-coerced-to-pairs.md) |
| `Type/Mu.rakudoc:435` | `.WHY`'s `Pod::Block::Declarator` doesn't stringify to the leading/trailing doc-comment text | [pod-why-declarator-object-not-stringified.md](../todo/tickets/pod-why-declarator-object-not-stringified.md) |
| `Type/Mu.rakudoc:119` | `.Capture` reads a raw stored attribute value instead of calling an overriding accessor method | [capture-coercion-uses-stored-attribute-not-accessor-method.md](../todo/tickets/capture-coercion-uses-stored-attribute-not-accessor-method.md) |
| `Type/Mu.rakudoc:515` | `next` thrown mid-evaluation of a `FIRST`-phaser comma expression corrupts later `take slip(...)` calls in the same `gather` | [next-inside-comma-expression-corrupts-following-take-slip.md](../todo/tickets/next-inside-comma-expression-corrupts-following-take-slip.md) |
| `Type/Mu.rakudoc:531` | `take-rw` doesn't preserve a mutable container alias through `gather` | [take-rw-loses-mutable-container-alias.md](../todo/tickets/take-rw-loses-mutable-container-alias.md) |
| `Language/subscripts.rakudoc:418` | a chained hash-then-array autovivification (`$h{"k"}[0] = v`) leaves the root variable showing `Any` on `.raku` | [nested-autovivification-then-raku-shows-any.md](../todo/tickets/nested-autovivification-then-raku-shows-any.md) |
| `Language/subscripts.rakudoc:462` | indexing a `:=`-bound infinite sequence through a hash slice (`%h{$key}[idx]`) divides by zero | [bound-infinite-sequence-hash-slice-divide-by-zero.md](../todo/tickets/bound-infinite-sequence-hash-slice-divide-by-zero.md) |
| `Language/subscripts.rakudoc:964` | `my @var is CustomClass = ...` never dispatches the class's `STORE`/overridden `Str` methods (custom Proxy-like container binding) | [is-typename-custom-container-store-protocol-unimplemented.md](../todo/deep/is-typename-custom-container-store-protocol-unimplemented.md) |
| `Language/concurrency.rakudoc:49,85` | a broken `Promise`'s exception isn't wrapped/mixed with `X::Promise::Broken` | [promise-broken-exception-not-wrapped-in-x-promise-broken.md](../todo/tickets/promise-broken-exception-not-wrapped-in-x-promise-broken.md) |
| `Language/concurrency.rakudoc:124` | `Promise.cause`'s backtrace duplicates the `in block <unit>` frame | [promise-cause-duplicate-in-block-backtrace-frame.md](../todo/tickets/promise-cause-duplicate-in-block-backtrace-frame.md) |
| `Language/concurrency.rakudoc:213` | `Promise.vow` doesn't protect against a second `.keep`/`.break` via the original Promise | [promise-vow-keep-protection-not-enforced.md](../todo/tickets/promise-vow-keep-protection-not-enforced.md) |
| `Language/concurrency.rakudoc:703` | `Thread.new(code => {...}).run` — `.run` method is unimplemented | [thread-new-run-method-missing.md](../todo/tickets/thread-new-run-method-missing.md) |
| `Language/concurrency.rakudoc:709` | `Thread.start({...})`'s block never runs — the main program exits before the spawned thread completes | [thread-start-block-not-awaited-before-process-exit.md](../todo/tickets/thread-start-block-not-awaited-before-process-exit.md) |

**Excluded from this batch-3 sub-run (already deferred/resolved/drift/false-positive):**
- `Type/Parameter.rakudoc` [3] (line 176) — `raku-drift` (the doc's stated `# OUTPUT` omits the
  `(42)` value that raku's actual current output includes).
- `Language/subscripts.rakudoc` [4] (line 51, a `.Mix` keyed by a `Date` object failing to match a
  distinct-but-equal-value `Date` lookup key) — the **Deferred** "WHICH-keyed QuantHash storage"
  cluster; confirmed the same root cause with a minimal repro (a plain `Hash` with the same Date
  keys looks up correctly, only `.Mix`/QuantHash storage fails), not re-ticketed.
- `Language/subscripts.rakudoc` [5], [6] (lines 225, 336) — hash/Bag iteration-order `raku-drift`,
  both sides nondeterministic (the known harness false positive for `.keys`/`{*}` iteration order).
- `Language/subscripts.rakudoc` [7] (line 348, `say @fib[]` on a `1,1, * + * … *` sequence fully
  reifying instead of showing `[...]`) — the **Deferred** lazy-list cluster's "closure_seq /
  scan_spec arrays stay force-capped on `@`-assign" residue; confirmed a bare `say @fib;` (no
  subscript at all) already shows the same full-reification bug for this exact sequence shape.
- `Language/concurrency.rakudoc` [2] (line 85) — bucketed `raku-drift` by the harness (correctly,
  since raku's current output no longer matches the doc's stated `# OUTPUT`), but re-verified
  directly: mutsu's own output diverges from raku's *actual* behavior for the same root cause as
  the `:49` finding above (missing `X::Promise::Broken` wrapping), so it is folded into
  [promise-broken-exception-not-wrapped-in-x-promise-broken.md](../todo/tickets/promise-broken-exception-not-wrapped-in-x-promise-broken.md)
  above rather than being skipped.

Found in the 2026-08-22 batch-4 re-run of `Backtrace`/`Scalar`/`perl-var`/
`Metamodel::ParametricRoleHOW`/`Lock::Async`/`X::Method::InvalidQualifier`/
`IO::Path::Parts`/`phasers`/`Metamodel::TypePretense`/`IO::Notification::Change`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Backtrace.rakudoc:15` | `$!.backtrace[N]` positional indexing always returns `Nil` | [backtrace-frame-indexing-returns-nil.md](../todo/tickets/backtrace-frame-indexing-returns-nil.md) |
| `Type/Backtrace.rakudoc:72,84,96` | `Backtrace` is missing `.next-interesting-index`, `.outer-caller-idx`, `.nice` | [backtrace-introspection-methods-missing.md](../todo/tickets/backtrace-introspection-methods-missing.md) |
| `Type/Scalar.rakudoc:43` | `my $b := 1; $b.VAR.^name` should be `Int` (bind creates no container), mutsu gives `Scalar` | [bind-scalar-literal-var-name-not-int.md](../todo/tickets/bind-scalar-literal-var-name-not-int.md) |
| `Language/perl-var.rakudoc:154` | `CompUnit::Repository::FileSystem`/`Installation` stringify as `TypeName.new` instead of `inst#<path>` | [compunit-repository-gist-missing-inst-prefix.md](../todo/tickets/compunit-repository-gist-missing-inst-prefix.md) |
| `Type/Metamodel/ParametricRoleHOW.rakudoc:29` | a parenthesized anonymous `role {...}` term (parameterized or not) is a hard parse error; `(class {...})` already works | [anon-role-expression-term-parse-fail.md](../todo/tickets/anon-role-expression-term-parse-fail.md) |
| `Type/Metamodel/ParametricRoleHOW.rakudoc:35` | `Metamodel::ParametricRoleHOW.new_type(...)` returns a type whose `.HOW` is `ClassHOW`, not `ParametricRoleHOW` | [metamodel-parametricrolehow-new-type-wrong-how.md](../todo/tickets/metamodel-parametricrolehow-new-type-wrong-how.md) |
| `Type/Lock/Async.rakudoc:162,202` | `Lock::Async` is missing `.protect-or-queue-on-recursion` and `.with-lock-hidden-from-recursion-check` | [lock-async-recursion-methods-missing.md](../todo/tickets/lock-async-recursion-methods-missing.md) |
| `Type/X/Method/InvalidQualifier.rakudoc:14` | `X::Method::InvalidQualifier` message says "a method" instead of naming the actual method | [invalid-qualifier-error-message-missing-method-name.md](../todo/tickets/invalid-qualifier-error-message-missing-method-name.md) |
| `Type/IO/Path/Parts.rakudoc:71` | `$parts[]` (empty postcircumfix index) on `IO::Path::Parts` shows the whole-object gist instead of iterating its 3 positional elements | [io-path-parts-empty-subscript-not-positional.md](../todo/tickets/io-path-parts-empty-subscript-not-positional.md) |
| `Language/phasers.rakudoc:401` | a `CONTROL { when CX::Take {...} }` handler is never invoked for `take` inside `gather` (unlike `CX::Warn`/`CX::Done`, which work) | [control-phaser-cx-take-not-intercepted.md](../todo/tickets/control-phaser-cx-take-not-intercepted.md) |
| `Type/Metamodel/TypePretense.rakudoc:15,47` | `Role ~~ Cool` is `False` (should be `True`, same as `Mu`/`Any`), and `.HOW.pretending_to_be` is unimplemented | [role-type-pretense-cool-incomplete.md](../todo/tickets/role-type-pretense-cool-incomplete.md) |

**Excluded from this batch-4 sub-run (already deferred/resolved/drift/false-positive/environment):**
- `Type/Scalar.rakudoc` [2] (line 53, `[1, 2, 3][0].VAR.^name` should be `Scalar`, mutsu gives
  `Int`) — the already-**Deferred**/deep "Array/Hash elements are stored bare — element reads lack
  itemization" cluster (`todo/deep/element-itemization-lost-in-scalar-binding.md`, ADR-0040); the
  `(1, 2, 3)[0]` (plain List, not Array) case in the same example already matches raku.
- `Language/perl-var.rakudoc` [1] (line 198, `$*KERNEL`/`$*DISTRO`/`$*VM` release/name/auth fields) —
  inherently environment/build-dependent (kernel version, OS distro, VM identity), same exclusion
  category as `Language/variables.rakudoc` [6]/[7]/[9]/[10]/[8] noted above; not a bug.
- `Type/IO/Notification/Change.rakudoc` — re-ran the harness; all 3 candidate blocks are bucketed
  "no oracle" (raku itself does not exit cleanly on any of them in this environment) rather than
  `mutsu-crash`, so there is no finding to compare against the survey table's stale `crash=1`. These
  examples use live filesystem-watching (`IO::Notification.watch-path`), which is inherently
  timing/environment-dependent and not a clean minimal repro; not ticketed.

Found in the 2026-08-22 batch-4 re-run of `Code`/`DateTime`/`perl-func`/
`Metamodel::ParametricRoleGroupHOW`/`Formatter`/`X::Cannot::Empty`/`Exception`/
`nativetypes`/`Compiler`/`IO::ArgFiles`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Code.rakudoc:166,175,195` | `.line`/`.file` reflection methods missing on `Sub`/`Method` | [code-line-file-reflection-missing-on-sub-method.md](../todo/tickets/code-line-file-reflection-missing-on-sub-method.md) |
| `Type/Code.rakudoc:81` | `$:name` named-placeholder variable fails to interpolate inside a double-quoted string (bare form already works) | [named-placeholder-var-interpolation-in-string-fails.md](../todo/tickets/named-placeholder-var-interpolation-in-string-fails.md) |
| `Type/DateTime.rakudoc:137` | a `DateTime` day-range-check throws the generic `X::OutOfRange` instead of the already-registered, more specific `X::Temporal::OutOfRange` | [datetime-day-out-of-range-uses-generic-outofrange.md](../todo/tickets/datetime-day-out-of-range-uses-generic-outofrange.md) |
| `Type/DateTime.rakudoc:281,302` | `.julian-date`/`.modified-julian-date` return `Num` (float noise) instead of an exact `Rat` | [datetime-julian-date-returns-num-not-rat.md](../todo/tickets/datetime-julian-date-returns-num-not-rat.md) |
| `Language/perl-func.rakudoc:2281` | `but`-mixing a role onto an `Array` silently drops the mixin entirely (`.^name` shows plain `Array`, no role methods attached) | [array-but-role-mixin-dropped-join-flatten.md](../todo/tickets/array-but-role-mixin-dropped-join-flatten.md) |
| `Language/perl-func.rakudoc:2310` | a role-mixed `.sink` method is never invoked when the value is used in sink context | [role-mixed-sink-method-not-invoked-in-sink-context.md](../todo/tickets/role-mixed-sink-method-not-invoked-in-sink-context.md) |
| `Type/Metamodel/ParametricRoleGroupHOW.rakudoc:21` | an anonymous *parametric* `role Name[...] {...}` literal is misparsed as an expression term — refines [anon-role-expression-term-parse-fail.md](../todo/tickets/anon-role-expression-term-parse-fail.md) above: re-verified that the parameterless `(role NAME {...})` form it also lists already parses fine on its own, it's specifically the `[...]` parametric signature that breaks | [parametric-role-literal-as-expression-term-misparsed.md](../todo/tickets/parametric-role-literal-as-expression-term-misparsed.md) |
| `Type/Metamodel/ParametricRoleGroupHOW.rakudoc:27` | `Metamodel::ParametricRoleHOW.new_type(...)` reports `.HOW` as `ClassHOW` instead of the specific metaclass it was called on — same finding as [metamodel-parametricrolehow-new-type-wrong-how.md](../todo/tickets/metamodel-parametricrolehow-new-type-wrong-how.md) above | (duplicate of the row above; cross-referenced from [direct-metamodel-classhow-new-type-immutable-error.md](../todo/deep/direct-metamodel-classhow-new-type-immutable-error.md), not re-filed) |
| `Type/Formatter.rakudoc:16,32` | `Formatter.new(FORMAT_STRING)` is unimplemented | [formatter-new-unimplemented.md](../todo/tickets/formatter-new-unimplemented.md) |
| `Type/X/Cannot/Empty.rakudoc:15` | `X::Cannot::Empty.new(:action, :what).message` returns an empty string instead of formatting "Cannot ACTION from an empty WHAT" | [x-cannot-empty-message-not-formatted.md](../todo/tickets/x-cannot-empty-message-not-formatted.md) |
| `Type/Exception.rakudoc:78` | `.backtrace.full` joins its frame lines without newlines (all frames run together on one line) | [backtrace-full-frames-not-newline-separated.md](../todo/tickets/backtrace-full-frames-not-newline-separated.md) |
| `Language/nativetypes.rakudoc:172` | `Pointer[T].raku` uses a bare type-parameter name and a named-arg constructor call instead of raku's fully-qualified positional form | [nativecall-pointer-raku-format-mismatch.md](../todo/tickets/nativecall-pointer-raku-format-mismatch.md) |
| `Type/Compiler.rakudoc:58` | `$*RAKU.compiler.verbose-config` is unimplemented (low priority — the real output is exhaustively MoarVM-build-specific) | [compiler-verbose-config-unimplemented.md](../todo/tickets/compiler-verbose-config-unimplemented.md) |
| `Type/IO/ArgFiles.rakudoc:34` | `$*ARGFILES.eof`/`.get` loops forever instead of terminating once stdin is exhausted (no file args given) | [argfiles-eof-infinite-loop-on-empty-stdin.md](../todo/tickets/argfiles-eof-infinite-loop-on-empty-stdin.md) |

**Excluded from this batch-4 sub-run:**
- `Type/Code.rakudoc` [3], [4] (lines 140, 153) — `raku-drift` (object hex-address
  text in `#`(Block|...)`/`#`(Sub|...)` gist output, inherently non-reproducible).
- `Type/Compiler.rakudoc` [1] (line 13, `$*RAKU.compiler`) — `raku-drift` and
  environment-dependent (compiler name/version string), same shape as the already-
  excluded `$*RAKU.compiler.version`/`$*VM.config` findings above; mutsu correctly
  reports its own identity (`mutsu (0.1.0)`) rather than impersonating rakudo, which
  is intentional, not a bug.

### Deferred / deep (tracked elsewhere — do not re-open as a shallow slice)
These root causes account for a large share of the survey's `mism`/`crash` and are
intentionally deferred; see PLAN.md §8.5 and the ADRs:
- **Nil-vs-Any identity knot** — `Nil.rakudoc`, `Mu.rakudoc`, uninit-scalar `.raku`/gist. No clean safe subset (closed #4822 twice).
- **Lazy-list cluster — MOSTLY RESOLVED 2026-07-23** (4 PRs; memory
  `lazy-list-cluster-progress`). What landed: Iterator `push-*`/`sink-all` on
  temporary receivers + count return values (#5292, shared
  `runtime/iterator_protocol.rs`); infinite `...` sequences survive `@`-array
  assignment as reify-on-demand lazy arrays = L2b step 6, plus the `lazy`-prefix
  sequence operand and the `gather do {…}; say` terminator misparse (#5294);
  `.flat` itemization depth — Array elements stay single/itemized (#5295);
  `loop`/`while`/`until` expressions are lazy Seqs pulled on demand (#5296,
  gather-lowered like `lazy for`). **Still deferred (the real container-repr
  core, fused with GC per ADR-0001):** closure_seq (`1, {rand} ... *`) /
  scan_spec arrays stay force-capped on `@`-assign because
  `S32-array/create.t` requires `.clone` to *share* the reifier — needs the
  element-cell store (TODO in `value_lazy.rs`); `=:= IterationEnd` container
  identity; IterationEnd's repr (it is a Str internally, so `.raku` quotes
  it); the custom `does Iterator` residue where an `is Array` subclass skips
  its user iterator (`__mutsu_array_storage` guard in
  `vm_for_loop_dispatch.rs`); big-Int→Float degradation in geometric
  sequence generation past i64 (`list.rakudoc` [1] tail).
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
- **CallFrame frame modeling** — `CallFrame.rakudoc`. Mostly landed: G4
  `.annotations→Map` (#5095), G2 the synthetic "setting" frame (`callframe(1)` at
  top level is now line 1 / code `Mu`), and G1/G3 the `for`-block frame (a `for`
  body is a distinct call frame, so the documented `calling-frame` walk reaches
  `(GLOBAL)`). The `for`-block level is a compile-time count (`callframe_block_depth`)
  passed as the hidden `__callframe_blocks` arg — zero runtime cost. **Remaining (1
  finding, deferred):** the statement-form `FIRST` phaser example (`$frame.code()`
  → `Code.new`) — Rakudo models a statement-form phaser as a `Code` frame and a
  block-form phaser as a `Block` frame, a distinction mutsu's AST does not preserve
  (both desugar to `Phaser { body }`), and there is no roast coverage. The
  remaining `.my<$the-answer>` example is raku-drift (`LoweredAwayLexical`). See
  [docs/callframe-introspection-plan.md](callframe-introspection-plan.md).

- **Sigilless-parameter scoping (`py-nutshell.rakudoc`)** — a sigilless binding
  shadowing the `i` term constant is fixed for `my \i` reads and single
  `-> \i { }` pointy params (#5113). **Mostly resolved** as of 2026-07-23; pin
  `t/sigilless-param-scoping.t`:
  - **Fixed** — `-> (\i, \j) { i + j }` (destructuring): `compile_closure_body`
    now allocates the `sub_signature` sigilless sub-params as sigilless locals
    and prepends a `MarkSigillessReadonly` prologue per sigilless sub-param, so a
    bare-word read resolves the binding, not the imaginary unit. Routine
    destructure (`sub f((\i,\j))`) is covered by the same
    `alloc_sub_signature_locals` sigilless registration.
  - **Fixed** — `for 1,2,3 -> \x { }; say x` (single for-param leak): this was
    NOT sigilless-specific — a sigiled `for ... -> $x` reusing an outer `my $x`
    leaked too. The single-param restore only touched env, not the compile-time
    local *slot* that the loop overwrote each iteration. The
    `for_param_restore_stack` entry now carries the colliding local slot
    (`spec.param_local`), and `RestoreForParam` writes the saved value back
    through it (both the array-source and int-range loop paths). LAST/post
    phasers still see the final value (restore stays deferred).
  - **Already worked** — `for ^5 -> \x { block-capturing x }` (nested-closure
    capture) and `py-nutshell` [5] `{ $_[0] + $_[1] }` over an `X`-crossed list
    topic both pass on current `main`; no change needed.
  - **Still deferred (1 niche case, compiler local-scope leak)** —
    `{ my \i = 5 }; say i` should revert `i` to the imaginary unit after the
    block, but a bare-block `my \i` leaks its `local_map`/`sigilless_locals`
    registration past the block, so the outer `say i` compiles to `GetLocal`
    (the now-Nil block slot) instead of `GetBareWord` (which would reach the
    imaginary-unit fallback). This is the general compiler bare-block
    local-scope leak — for a *sigiled* `{ my $x }; say $x` it surfaces only as a
    runtime "not declared" (vs raku's compile-time), and `i` is the sole name
    whose term fallback the leak observably suppresses. Fixing it means scoping
    the compiler's `local_map`/`sigilless_locals` per bare block (broad blast
    radius), so it is left for a dedicated pass.
- **List-infix (`Z`/`X`/meta/infix-func) comma precedence** — `operators.rakudoc`
  [24] (`say 100, 200 Z+ 42, 23` → raku `(142 223)`; `1, 2 Z 3, 4` → `((1 3) (2 4))`).
  `Z`/`X` are **looser than comma** in Raku, so the comma list on each side is the
  operand (`(100,200) Z+ (42,23)`). **Fixed for the statement/argument listop paths:**
  `say`/`print`/`put`/`note` and `is`/`ok`/`is-deeply` (#5268), and the no-paren
  **user-sub / imported-sub / hyphen-forward** call path (#5271) — each applies a
  per-argument `extend_listop_arg_list_infix` + whole-level
  `lift_list_infix_in_arg_list` (the paren form already lifted post-parse). **Still
  deferred (two sub-cases):**
  - **Builtin listop path** (`join`/`grep`/`map` in `identifier_call.rs` ~1490-1529):
    subtler raku semantics — `join "-", 1, 2 Z 3, 4` returns `""` in raku, not a
    clean `(1,2) Z (3,4)` cross — and a distinct code path, so it is NOT the same
    shallow lift.
  - **Comparison-operand precedence** (`1 == 1 Z 2 == 2` → raku `(True True)`, mutsu
    `False`): mutsu's list-infix operand is `range_expr`, tighter than comparison,
    inverting raku where the `Z` operand is the comparison level. A core
    precedence-layer redesign (do NOT bolt on) — see the pin memo.
- **Forward-declaration stub upgrade** — `operators.rakudoc` [6]
  (`sub a() { ... }; say a; sub a() { 42 }` → raku 42, mutsu X::Redeclaration). A
  `{ ... }` yada stub is a forward declaration a later real definition upgrades.
  Top-level is fixable (thread `existing_single_is_stub` through the
  `registration_sub.rs` guards + gate an inline stub-over-real no-op on
  `!__hoisted`), but it **regresses `stub-and-supersede.t`**: a *block-scoped*
  stub redefinition (`{ sub l {...}; throws-like 'l()', X::StubCode; sub l {42} }`)
  passes on `main` only because of `throws-like`'s EVAL context — the plain block
  form already errors on `main`. The real fix must design the hoist-pass +
  inline-pass double-registration together with block-shadow + EVAL; deferred.
- **Block-scope restore of a dynamic var with a pre-existing outer value** — a
  `{ my $*X = v; ... }` block does not fully restore `$*X` for `get_dynamic_var`
  when `$*X` had an outer (e.g. seeded) value: after the block, the plain read is
  restored but `get_dynamic_var("$*X")` still resolves the stale inner `v`. Surfaced
  by an attempt to seed `$*TOLERANCE` (reverted in #5128); it broke
  `S32-num/complex.t`'s `<=>`-with-negligible-imaginary subtest. This gap blocks a
  clean `$*TOLERANCE` default (operators.rakudoc [20] bare read). Needs the
  block-scope snapshot to cover the seeded/outer dynamic key.

### Untriaged
Everything in the survey below not listed above. The per-file minimal repros live in
`tmp/sweep/reports/<file>.txt` after a sweep — start from the highest-signal file and
re-verify each block against `raku` before writing a fix.

## Survey — files with divergences (high-signal first)

`mism` = output-mismatch · `crash` = mutsu exited non-zero where raku succeeded ·
`drift` = raku-drift-from-doc (version skew, low priority).

| file (under raku-doc/doc/) | mism | crash | drift |
|---|---:|---:|---:|
| Language/regexes.rakudoc | 12 | 3 | 3 |
| Language/operators.rakudoc | 5 | 6 | 1 |
| Language/control.rakudoc | 4 | 6 | 1 |
| Language/structures.rakudoc | 5 | 2 | 2 |
| Language/objects.rakudoc | 4 | 3 | 2 |
| Type/IO/Path.rakudoc | 5 | 1 | 1 |
| Language/variables.rakudoc | 4 | 2 | 5 |
| Type/Mu.rakudoc | 4 | 1 | 2 |
| Type/IO/Handle.rakudoc | 4 | 1 | 1 |
| Language/subscripts.rakudoc | 4 | 1 | 1 |
| Language/concurrency.rakudoc | 4 | 1 | 1 |
| Type/Any.rakudoc | 3 | 2 | 4 |
| Language/traps.rakudoc | 3 | 2 | 3 |
| Language/grammars.rakudoc | 3 | 2 | 1 |
| Type/Proc/Async.rakudoc | 3 | 2 | 0 |
| Type/independent-routines.rakudoc | 1 | 4 | 3 |
| Language/experimental.rakudoc | 0 | 5 | 0 |
| Type/Hash.rakudoc | 4 | 0 | 1 |
| Language/unicode.rakudoc | 4 | 0 | 1 |
| Language/typesystem.rakudoc | 3 | 1 | 2 |
| Type/Parameter.rakudoc | 2 | 2 | 1 |
| Language/syntax.rakudoc | 2 | 2 | 0 |
| Language/mop.rakudoc | 2 | 2 | 0 |
| Type/Backtrace.rakudoc | 1 | 3 | 0 |
| Type/Code.rakudoc | 0 | 4 | 2 |
| Type/Routine.rakudoc | 3 | 0 | 1 |
| Type/Map.rakudoc | 3 | 0 | 1 |
| Type/SetHash.rakudoc | 3 | 0 | 0 |
| Type/Iterator.rakudoc | 2 | 1 | 2 |
| Type/Test.rakudoc | 2 | 1 | 0 |
| Type/Range.rakudoc | 2 | 1 | 0 |
| Type/Metamodel/DefiniteHOW.rakudoc | 2 | 1 | 0 |
| Type/Label.rakudoc | 2 | 1 | 0 |
| Language/js-nutshell.rakudoc | 2 | 1 | 0 |
| Language/signatures.rakudoc | 1 | 2 | 7 |
| Type/Metamodel/Mixins.rakudoc | 1 | 2 | 0 |
| Type/Metamodel/EnumHOW.rakudoc | 1 | 2 | 0 |
| Type/Junction.rakudoc | 2 | 0 | 3 |
| Type/IO/Spec/Win32.rakudoc | 2 | 0 | 2 |
| Type/IO/Spec/Unix.rakudoc | 2 | 0 | 2 |
| Type/Scalar.rakudoc | 2 | 0 | 1 |
| Type/DateTime.rakudoc | 2 | 0 | 1 |
| Language/functions.rakudoc | 2 | 0 | 1 |
| Type/Pair.rakudoc | 2 | 0 | 0 |
| Type/Match.rakudoc | 2 | 0 | 0 |
| Language/py-nutshell.rakudoc | 2 | 0 | 0 |
| Language/perl-var.rakudoc | 2 | 0 | 0 |
| Language/perl-func.rakudoc | 2 | 0 | 0 |
| Type/Cool.rakudoc | 1 | 1 | 5 |
| Type/List.rakudoc | 1 | 1 | 2 |
| Type/BagHash.rakudoc | 1 | 1 | 2 |
| Type/Sub.rakudoc | 1 | 1 | 1 |
| Type/Metamodel/MethodContainer.rakudoc | 1 | 1 | 1 |
| Type/Attribute.rakudoc | 1 | 1 | 1 |
| Type/X/AdHoc.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleGroupHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/Documenting.rakudoc | 1 | 1 | 0 |
| Language/pod.rakudoc | 1 | 1 | 0 |
| Language/ipc.rakudoc | 1 | 1 | 0 |
| Language/faq.rakudoc | 1 | 1 | 0 |
| Type/Unicode.rakudoc | 0 | 2 | 0 |
| Type/Metamodel/Trusting.rakudoc | 0 | 2 | 0 |
| Type/Lock/Async.rakudoc | 0 | 2 | 0 |
| Type/Formatter.rakudoc | 0 | 2 | 0 |
| Language/nativecall.rakudoc | 0 | 2 | 0 |
| Type/Baggy.rakudoc | 1 | 0 | 4 |
| Language/containers.rakudoc | 1 | 0 | 4 |
| Language/numerics.rakudoc | 1 | 0 | 3 |
| Language/list.rakudoc | 1 | 0 | 3 |
| Type/MixHash.rakudoc | 1 | 0 | 2 |
| Type/CallFrame.rakudoc | 1 | 0 | 1 |
| Language/hashmap.rakudoc | 1 | 0 | 1 |
| Language/contexts.rakudoc | 1 | 0 | 1 |
| Type/X/Str/Match/x.rakudoc | 1 | 0 | 0 |
| Type/X/Proc/Async/MustBeStarted.rakudoc | 1 | 0 | 0 |
| Type/X/Proc/Async/CharsOrBytes.rakudoc | 1 | 0 | 0 |
| Type/X/Phaser/PrePost.rakudoc | 1 | 0 | 0 |
| Type/X/Method/InvalidQualifier.rakudoc | 1 | 0 | 0 |
| Type/X/Cannot/Empty.rakudoc | 1 | 0 | 0 |
| Type/Slip.rakudoc | 1 | 0 | 0 |
| Type/Setty.rakudoc | 1 | 0 | 0 |
| Type/Sequence.rakudoc | 1 | 0 | 0 |
| Type/Real.rakudoc | 1 | 0 | 0 |
| Type/Proc.rakudoc | 1 | 0 | 0 |
| Type/Iterable.rakudoc | 1 | 0 | 0 |
| Type/IO/Spec/Cygwin.rakudoc | 1 | 0 | 0 |
| Type/IO/Path/Parts.rakudoc | 1 | 0 | 0 |
| Type/Exception.rakudoc | 1 | 0 | 0 |
| Type/Buf.rakudoc | 1 | 0 | 0 |
| Type/Block.rakudoc | 1 | 0 | 0 |
| Type/Bag.rakudoc | 1 | 0 | 0 |
| Type/Associative.rakudoc | 1 | 0 | 0 |
| Language/traits.rakudoc | 1 | 0 | 0 |
| Language/statement-prefixes.rakudoc | 1 | 0 | 0 |
| Language/quoting.rakudoc | 1 | 0 | 0 |
| Language/phasers.rakudoc | 1 | 0 | 0 |
| Language/nativetypes.rakudoc | 1 | 0 | 0 |
| Language/math.rakudoc | 1 | 0 | 0 |
| Language/io-guide.rakudoc | 1 | 0 | 0 |
| Language/haskell-to-p6.rakudoc | 1 | 0 | 0 |
| Language/grammar_tutorial.rakudoc | 1 | 0 | 0 |
| Language/glossary.rakudoc | 1 | 0 | 0 |
| Language/exceptions.rakudoc | 1 | 0 | 0 |
| Language/classtut.rakudoc | 1 | 0 | 0 |
| Type/Metamodel/TypePretense.rakudoc | 0 | 1 | 1 |
| Type/Compiler.rakudoc | 0 | 1 | 1 |
| Type/X/TypeCheck/Splice.rakudoc | 0 | 1 | 0 |
| Type/Str.rakudoc | 0 | 1 | 0 |
| Type/Proxy.rakudoc | 0 | 1 | 0 |
| Type/PositionalBindFailover.rakudoc | 0 | 1 | 0 |
| Type/Method.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/Versioning.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/Stashing.rakudoc | 0 | 1 | 0 |
| Type/Lock/ConditionVariable.rakudoc | 0 | 1 | 0 |
| Type/IO/Notification/Change.rakudoc | 0 | 1 | 0 |
| Type/IO/ArgFiles.rakudoc | 0 | 1 | 0 |
| Type/HyperWhatever.rakudoc | 0 | 1 | 0 |
| Type/CompUnit/Repository/FileSystem.rakudoc | 0 | 1 | 0 |
| Language/using-modules/code.rakudoc | 0 | 1 | 0 |
| Language/unicode_entry.rakudoc | 0 | 1 | 0 |
| Language/regexes-best-practices.rakudoc | 0 | 1 | 0 |
| Language/optut.rakudoc | 0 | 1 | 0 |
| Language/newline.rakudoc | 0 | 1 | 0 |