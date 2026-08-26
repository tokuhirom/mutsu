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
- `Cool.rakudoc:1416` — `"foo".Rat` returned a silent `Rat` `0` instead of the lazy
  `X::Str::Numeric` `Failure` every sibling coercion already produced; the guard was
  simply missing from the `"Rat"` arm —
  [news](../news/2026-08/str-rat-coercion-should-fail.md).
- `X/Str/Match/x.rakudoc:15` — `.match(…, :x(BAD))` accepted a plain `Str` and then
  silently ignored the adverb; the accept-list is `Numeric`/`Range`/`*` (so `<2>` is
  in and `"2"` is out), `.match` returns the `Failure` while `.subst` throws, and the
  message always names `Str.match` —
  [news](../news/2026-08/str-match-x-adverb-type-not-validated.md).
- `independent-routines.rakudoc:687,692` — `.printf` had no method form, and the
  Junction handling was dispatch-shaped rather than directive-shaped: only `printf`
  has a `Junction:D` argument candidate, while the `Str(Cool) $format` parameter
  autothreads in both `printf` and `sprintf` —
  [news](../news/2026-08/printf-method-form-and-junction-autothread-missing.md).
- `Str.rakudoc:647` — `.comb(:match)` (named-arg-only) already dispatches: closed by
  the implicit-`*%_` retry, with the ticket's expected output corrected (raku returns
  plain `Str`s, not `Match`es, when no matcher is given) —
  [news](../news/2026-08/str-comb-named-arg-only-dispatch-missing.md).
- `objects.rakudoc:1067` — a `class Foo is Str {}` instance lost the parent's string
  payload; `Mu.new`'s `:value` named argument now lands in a reserved
  `__mutsu_str_value` attribute, the string twin of the existing
  `__mutsu_array_storage`/`__mutsu_int_value` payloads —
  [news](../news/2026-08/str-subclass-loses-native-stringify.md).
- `nativecall.rakudoc:598` — `Pointer[T].deref` was missing on a `--> Pointer[T]`
  native return (and SEGFAULTed for `Pointer[Str]` even where it existed);
  `.deref` is now `nativecast(.of, self)` as in Rakudo. See
  [news](../news/2026-08/nativecall-pointer-deref-method-missing.md).
- `nativetypes.rakudoc:172` — `Pointer[T].raku` rendered a bare type parameter and
  a named-arg constructor; `.raku`/`.gist`/`.^name` now all derive from one
  fully-qualified, parameterised name. See
  [news](../news/2026-08/nativecall-pointer-raku-format-mismatch.md).
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
| `Type/Hash.rakudoc:336` | `my %h .= push(pair)` should leave `%h` empty, mutsu keeps the pair | [hash-dot-assign-push-result.md](../todo/tickets/hash-dot-assign-push-result.md) |
| `Type/List.rakudoc:219` | `(gather {...}).list.raku` keeps a spurious `.Seq` suffix when chained directly (no intermediate var) | [gather-chained-list-raku-seq-suffix.md](../todo/tickets/gather-chained-list-raku-seq-suffix.md) |
| `Type/Map.rakudoc:62` | `Map.new(a, 1, :b(2))` — bare colon-pair should bind as a named arg to `.new`, not a positional Pair | [map-new-bare-colonpair-named-arg.md](../todo/tickets/map-new-bare-colonpair-named-arg.md) |
| `Type/Range.rakudoc:80` | `@arr[$range-var]` doesn't flatten into `for` iteration (literal `@arr[0..2]` does) | [array-subscript-range-var-list-context-slip.md](../todo/tickets/array-subscript-range-var-list-context-slip.md) |

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
| `Language/control.rakudoc:1375` | `return-rw` doesn't return a mutable container | [control-return-rw-not-mutable.md](../todo/tickets/control-return-rw-not-mutable.md) |
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
| `Type/Any.rakudoc:961` | ~~`.categorize`/`.classify` into an untyped Hash mis-renders a non-Str bucket key when the source array holds class instances~~ (the real bug was hash-gist rendering: the dispatching gist path ignored object-hash typed keys) | [resolved](../news/2026-08/categorize-into-hash-instance-bool-key-corruption.md) |
| `Type/Any.rakudoc:1525` | ~~`.snip` with multiple positional predicates silently drops all but the first~~ | [resolved](../news/2026-08/snip-multiple-positional-matchers-dropped.md) |
| `Type/Any.rakudoc:1549,1559` | `.snitch` (v6.e.PREVIEW debugging method) is entirely unimplemented | [snitch-method-unimplemented.md](../todo/tickets/snitch-method-unimplemented.md) |
| `Type/Any.rakudoc:1420` | `$*COLLATION.set(...)` does not persist / is not honored by `coll` | [collation-set-not-persisted.md](../todo/tickets/collation-set-not-persisted.md) |
| `Language/objects.rakudoc:1132` | ~~a role's 0-arg `multi method` loses its trailing string literal, only when the composing class has its own extra attribute~~ (nothing was truncated: the role's own method was outranked by the accessor of the role's own attribute) | [resolved](../news/2026-08/role-multi-method-trailing-literal-dropped.md) |
| `Language/objects.rakudoc:1526` | `is default(0 but role :: {...})` on a typed Hash drops the role mixin on the default value | [hash-default-role-mixin-dropped.md](../todo/tickets/hash-default-role-mixin-dropped.md) |
| `Language/objects.rakudoc:65` | colon-call syntax with zero arguments (`.method:` immediately followed by `;`) fails to parse | [colon-call-empty-args-parse-error.md](../todo/tickets/colon-call-empty-args-parse-error.md) |
| `Language/objects.rakudoc:1397` | ~~a parameterized role with a self-referential attribute type fails a spurious type-check, with a malformed error message~~ | [resolved](../news/2026-08/parametric-role-self-referential-attribute-typecheck.md) |
| `Language/typesystem.rakudoc:657` | a custom `.gist` on a role-mixed native value is skipped when gisted inside an array/list | [role-mixed-value-gist-skipped-in-array.md](../todo/tickets/role-mixed-value-gist-skipped-in-array.md) |
| `Language/typesystem.rakudoc:611` | a forward-declared role stub used by another role is never upgraded to its real body | [forward-declared-role-stub-not-upgraded.md](../todo/tickets/forward-declared-role-stub-not-upgraded.md) |
| `Language/typesystem.rakudoc:644` | ~~a role parameter's `fail(...)` default expression is never evaluated/enforced~~ (the `does` mixin path never evaluated role parameter defaults at all) | [resolved](../news/2026-08/role-parameter-fail-default-not-enforced.md) |

The "a `but`/`does`-mixed value's role metadata does not survive a generic storage/dispatch
path" family was investigated together (2026-08-26) and the hypothesis was CONFIRMED: the shared
mechanism is an interpreter site testing `ValueView::Instance` directly and therefore silently
downgrading a `ValueView::Mixin`. It was present, independently, in five string-coercion sites,
the `@`-sigil `:=` bind check, and the sink-context gate; `.sort` was the adjacent case of a
list-ish helper (`mixin_iteration_target`) that `.map`/`.grep` already used and `.sort` did not.
Written up in
[array-but-role-mixin-name-suffix-and-join-str.md](../news/2026-08/array-but-role-mixin-name-suffix-and-join-str.md);
[list-but-role-loses-positional-binding.md](../news/2026-08/list-but-role-loses-positional-binding.md)
and five siblings landed with it.

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
  raku's `X::Role::Instantiation`), but mutsu not throwing at all was real — fixed, see
  [role-parameter-fail-default-not-enforced.md](../news/2026-08/role-parameter-fail-default-not-enforced.md)
  above (mutsu now throws `X::Role::Instantiation` too).

Found in the same 2026-08-22 batch-2 re-run, `Language/regexes.rakudoc` /
`Language/traps.rakudoc` / `Language/variables.rakudoc`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Language/regexes.rakudoc:1331` | ~~`<!:Script<Name>>` negated Unicode-property lookahead doesn't stop the preceding quantifier's backtrack~~ (the real bug was every `<!:Prop>` at end of string) | [resolved](../news/2026-08/regex-script-lookahead-negation-wrong.md) |
| `Language/regexes.rakudoc:1349` | ~~`<same>` builtin regex subrule is unimplemented~~ | [resolved](../news/2026-08/regex-same-subrule-missing.md) |
| `Language/regexes.rakudoc:1595` | a regex-embedded `:my $c = $/;` (self-referencing the in-progress match) fails to parse | [regex-embedded-my-decl-self-referential-slash.md](../todo/tickets/regex-embedded-my-decl-self-referential-slash.md) |
| `Language/regexes.rakudoc:1602` | a regex-embedded `:my $c = ~$0;` captures an empty value instead of the current match text | [regex-embedded-my-decl-value-not-captured.md](../todo/tickets/regex-embedded-my-decl-value-not-captured.md) |
| `Language/regexes.rakudoc:1612` | a regex-embedded `:our $var = ...;` doesn't write back to the package variable | [regex-our-declarator-writeback-missing.md](../todo/tickets/regex-our-declarator-writeback-missing.md) |
| `Language/regexes.rakudoc:1966` | a subrule call with a block-literal argument (`<name: { ... }>`) fails entirely | [regex-subrule-block-argument-parse-fail.md](../todo/tickets/regex-subrule-block-argument-parse-fail.md) |
| `Language/regexes.rakudoc:1543` | capture-group numbering across an alternation branch is wrong | [regex-capture-numbering-across-alternation.md](../todo/tickets/regex-capture-numbering-across-alternation.md) |
| `Language/regexes.rakudoc:1587` | an embedded code block inside a quantified group doesn't persist its side effect on an outer `:my` variable | [regex-embedded-code-block-quantifier-scope.md](../todo/tickets/regex-embedded-code-block-quantifier-scope.md) |
| `Language/regexes.rakudoc:2684` | ~~`m:st(...)` regex adverb (starting positions) is unsupported~~ (`:st`/`:nd`/`:rd`/`:th` are aliases of `:nth`, not positional) | [resolved](../news/2026-08/regex-st-adverb-unsupported.md) |
| `Language/regexes.rakudoc:2935` | ~~`<~~>` recursive self-match returns the wrong (inner, not outer) nesting level~~ (it was unimplemented) | [resolved](../news/2026-08/regex-recursive-self-match-wrong-nesting-level.md) |
| `Language/traps.rakudoc:91` | ~~`$++` inside a string-interpolated block doesn't reset per call the way raku's does~~ (the interpolation block pushed no lexical scope, and no value-position block emitted a `state` reset) | [resolved](../news/2026-08/dollar-plusplus-state-scope-in-interpolated-block.md) |
| `Language/traps.rakudoc:212` | `$.attr *= 2` inside a method throws `X::Assignment::RO` where current raku silently no-ops | [dollar-dot-attr-compound-assign-spurious-ro-error.md](../todo/tickets/dollar-dot-attr-compound-assign-spurious-ro-error.md) |
| `Language/traps.rakudoc:1948` | `\|«@array` (flatten + hyper prefix combined) fails to parse | [flatten-hyper-prefix-parse-error.md](../todo/tickets/flatten-hyper-prefix-parse-error.md) |
| `Language/traps.rakudoc:1067` | `for EXPR ~~ /regex/ { BLOCK }` executes `BLOCK` where raku produces no output | [for-loop-over-smartmatch-result-executes-unexpectedly.md](../todo/tickets/for-loop-over-smartmatch-result-executes-unexpectedly.md) |
| `Language/traps.rakudoc:406`, `Language/variables.rakudoc:853` | ~~a scalar's container isn't aliased when pushed into a collection without `.clone` — mutsu snapshots by value where raku aliases~~ (two causes: `.item` compiled as an ordinary method call lost the container, and a positional `PositionalPair` argument had its pair capture suppressed as if it were a named argument) | [resolved](../news/2026-08/container-aliasing-not-preserved-into-collection.md) |
| `Language/variables.rakudoc:1551` | ~~`».&?BLOCK` (hyper-call with a block self-reference) dispatches an empty method name~~ | [resolved](../news/2026-08/hyper-call-block-self-reference-empty-method.md) |
| `Language/variables.rakudoc:134` | `my ($g) = LIST;` gives `$g.VAR.^name` of `Int` instead of `Scalar` (harness mis-bucketed as drift) | [paren-single-var-decl-var-scalar-name.md](../todo/tickets/paren-single-var-decl-var-scalar-name.md) |
| `Language/variables.rakudoc:768` | `anon class`/`anon sub` with a non-ASCII name fails to parse; `anon sub NAME` also gists without the `&` sigil | [anon-class-sub-non-ascii-name-and-sub-gist.md](../todo/tickets/anon-class-sub-non-ascii-name-and-sub-gist.md) |
| `Language/variables.rakudoc:1765` | ~~`$*RAKU` reports the wrong metaclass name (`Perl`) and inconsistent stringification~~ (`.put` also ignored a custom `.Str` -- a general bug) | [resolved](../news/2026-08/dollar-raku-wrong-metaclass-and-stringify.md) |
| `Language/variables.rakudoc:318` | ~~`$?FILE` reports the relative invocation path instead of an absolute path~~ | [resolved](../news/2026-08/dollar-question-file-relative-not-absolute.md) |

**Excluded from this sub-batch (already deferred/resolved/drift/false-positive/environment):**
- `Language/regexes.rakudoc` [3], [8] — `raku-drift`.
- `Language/traps.rakudoc` [1], [2] — instances of the already-**Deferred** Nil-vs-Any identity
  knot (`%h is default(Nil)` and `:= Nil` both render `(Any)` instead of `Nil`).
- `Language/traps.rakudoc` [6] — Set iteration-order `raku-drift`.
- `Language/variables.rakudoc` [1] — mutsu errors partway through a recursive `.` directory walk
  (same shape as `Type/IO/Path.rakudoc` [4] from the earlier sub-batch); the real bug underneath
  it (`».&?BLOCK` dispatching an empty method name) was isolated to a small non-environment-
  dependent repro and filed as
  [hyper-call-block-self-reference-empty-method.md](../news/2026-08/hyper-call-block-self-reference-empty-method.md).
- `Language/variables.rakudoc` [3] — `$?FILE` now resolves the main compilation unit to an absolute
  path, matching raku; pinned by
  [module-file-var-and-callframe.t](../t/module-file-var-and-callframe.t).
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
| `Language/structures.rakudoc:233` | ~~calling an undefined `Any`-typed value as a function throws instead of returning the args~~ | [resolved](../news/2026-08/undefined-any-called-as-sub-throws.md) |
| `Language/structures.rakudoc:26`, `Language/mop.rakudoc:120` | `$(LIST).VAR.^name` reports `List` instead of `Scalar` (item contextualizer doesn't itemize) | [item-contextualized-list-var-name-not-scalar.md](../todo/tickets/item-contextualized-list-var-name-not-scalar.md) |
| `Language/mop.rakudoc:329` | a grammar's `method ^parameterize` + parametric role application stack-overflows | [grammar-metaclass-parameterize-stack-overflow.md](../todo/deep/grammar-metaclass-parameterize-stack-overflow.md) |
| `Language/mop.rakudoc:34` | `constant NAME := Metamodel::ClassHOW.new_type(name => 'NAME')` immediately errors as "immutable" | [direct-metamodel-classhow-new-type-immutable-error.md](../todo/deep/direct-metamodel-classhow-new-type-immutable-error.md) |
| `Language/mop.rakudoc:93` | `.HOW.^name` on a hash literal is missing the `+{<anon>}` mixin suffix | [how-gist-missing-anon-mixin-suffix.md](../todo/tickets/how-gist-missing-anon-mixin-suffix.md) |
| `Type/independent-routines.rakudoc:110` | ~~`EVAL` doesn't synthesize an `EVAL_N` filename for `$?FILE`, and ignores the `:filename` arg~~ | [resolved](../news/2026-08/eval-dollar-question-file-not-synthesized.md) |
| `Type/independent-routines.rakudoc:148` | `repl()` global routine is unimplemented (mutsu already has REPL machinery to reuse) | [repl-routine-unimplemented.md](../todo/tickets/repl-routine-unimplemented.md) |
| `Type/independent-routines.rakudoc:473` | `open(:w, PATH)` — a named adverb before the positional path breaks argument parsing | [open-named-adverb-before-positional-path.md](../todo/tickets/open-named-adverb-before-positional-path.md) |
| `Type/independent-routines.rakudoc:312` | ~~hyper method call (`».method`) / `.map` on a `gather {...}` Seq returns empty instead of forcing it~~ (the real bug was `.is-lazy` being re-derived in three places; a plain `gather` is not lazy) | [resolved](../news/2026-08/gather-hyper-method-call-empty-result.md) |

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
| `Language/grammars.rakudoc:387` | ~~a grammar-embedded custom assertion method (`<.method>`) sees `self` as an uninstantiated type object~~ (the parse result's type is still `Match`, not the grammar -- [split out](../todo/tickets/grammar-parse-result-is-a-match-not-a-grammar-cursor.md)) | [resolved](../news/2026-08/grammar-embedded-custom-assertion-method-self-type-object.md) |
| `Language/grammars.rakudoc:289` | ~~`.tail ~= ...` on a private class-attribute array silently no-ops instead of mutating the last element~~ (the rw write-back rebuilt the array and rebound it by *name*, which only ever reached a plain lexical) | [resolved](../news/2026-08/tail-lvalue-compound-assign-attribute-array-noop.md) |
| `Language/syntax.rakudoc:384` | adverb values needing BEGIN-time evaluation don't resolve (`$a:foo«$c»` for a `constant $c`, `$foo:bar(1+1)`) — the key-less form (`:354`) is fixed, see [news](../news/2026-08/keyless-colon-pair-variable-names.md) | [begin-time-adverb-value-interpolation.md](../todo/deep/begin-time-adverb-value-interpolation.md) |
| `Language/syntax.rakudoc:429` | `OUR::` pseudo-package: sub-package members and `OUR::name` lookup fixed; `.keys` still carries builtin/dynamic-var noise | [our-pseudopackage-missing-file-scope-symbols.md](../todo/tickets/our-pseudopackage-missing-file-scope-symbols.md) |
| `Language/syntax.rakudoc:1091` | a colon-call's trailing `.method` (`.substr: 0, 3  .uc`) binds to the wrong operand | [colon-call-trailing-dot-method-binds-wrong-operand.md](../todo/tickets/colon-call-trailing-dot-method-binds-wrong-operand.md) |
| `Language/experimental.rakudoc:32` | ~~`Buf`/`Blob.contents` method is missing~~ | [resolved](../news/2026-08/buf-contents-method-missing.md) |
| `Language/experimental.rakudoc:144` | ~~a user-defined custom infix's RHS operand fails to parse when followed by `??...!!`~~ (a trait-less custom infix had list-infix, not additive, precedence) | [resolved](../news/2026-08/custom-infix-rhs-operand-rejects-ternary.md) |
| `Language/unicode.rakudoc:190,212,224` | ~~`\c[NAME]` fails to resolve Unicode NameAlias corrections and multi-codepoint named sequences~~ | [resolved](../news/2026-08/c-bracket-character-name-lookup-gaps.md) |

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
| `Type/Parameter.rakudoc:306` | ~~a generic `::T` type parameter isn't resolved when reporting later type-check failures (shows literal `T`, and takes a completely different error-shape for a second case)~~ | [resolved](../news/2026-08/generic-type-param-binding-error-message-unresolved.md) |
| `Type/Match.rakudoc:20` | ~~`$¢` (cursor-position variable) inside a regex-embedded code block is unimplemented~~ (it was implemented; `$$` before a `{` was misparsed as `${...}`) | [resolved](../news/2026-08/dollar-cent-cursor-variable-in-embedded-regex-block-missing.md) |
| `Type/Match.rakudoc:40` | ~~`$0` read inside a regex-embedded code block returns the raw string instead of a `Match` object~~ | [resolved](../news/2026-08/dollar-numbered-capture-in-embedded-regex-block-returns-raw-value.md) |
| `Type/Mu.rakudoc:238` | ~~default `.clone()` doesn't share Array/Hash-typed attribute containers with the original~~ (`.clone` already shared them; the *accessor* whole-container assignment replaced the container instead of assigning into it) | [resolved](../news/2026-08/clone-array-hash-attribute-containers-not-shared.md) |
| `Type/Mu.rakudoc:267` | assigning a list through a `%`-sigil `rw` accessor doesn't coerce it to Hash pairs the way a direct `%var = list` does | [hash-attribute-rw-accessor-list-assignment-not-coerced-to-pairs.md](../todo/tickets/hash-attribute-rw-accessor-list-assignment-not-coerced-to-pairs.md) |
| `Type/Mu.rakudoc:119` | ~~`.Capture` reads a raw stored attribute value instead of calling an overriding accessor method~~ (it also leaked private `$!` attributes) | [resolved](../news/2026-08/capture-coercion-uses-stored-attribute-not-accessor-method.md) |
| `Type/Mu.rakudoc:515` | ~~`next` thrown mid-evaluation of a `FIRST`-phaser comma expression corrupts later `take slip(...)` calls in the same `gather`~~ (the real bug was the FIRST "already ran" flag being cleared *after* the body, so FIRST re-fired every iteration) | [resolved](../news/2026-08/next-inside-comma-expression-corrupts-following-take-slip.md) |
| `Type/Mu.rakudoc:531` | `take-rw` doesn't preserve a mutable container alias through `gather` (the `take-rw @a[0]` subscript form is fixed; `take-rw $_` / `take-rw $x` need ADR-0045's element-container bind) | [take-rw-loses-mutable-container-alias.md](../todo/tickets/take-rw-loses-mutable-container-alias.md) |
| `Language/subscripts.rakudoc:418` | a chained hash-then-array autovivification (`$h{"k"}[0] = v`) leaves the root variable showing `Any` on `.raku` | [nested-autovivification-then-raku-shows-any.md](../todo/tickets/nested-autovivification-then-raku-shows-any.md) |
| `Language/subscripts.rakudoc:964` | `my @var is CustomClass = ...` never dispatches the class's `STORE`/overridden `Str` methods (custom Proxy-like container binding) | [is-typename-custom-container-store-protocol-unimplemented.md](../todo/deep/is-typename-custom-container-store-protocol-unimplemented.md) |

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

Found in the 2026-08-22 batch-4 re-run of `Backtrace`/`Scalar`/`perl-var`/
`Metamodel::ParametricRoleHOW`/`Lock::Async`/`X::Method::InvalidQualifier`/
`IO::Path::Parts`/`phasers`/`Metamodel::TypePretense`/`IO::Notification::Change`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Backtrace.rakudoc:15` | `$!.backtrace[N]` positional indexing always returns `Nil` | [backtrace-frame-indexing-returns-nil.md](../todo/tickets/backtrace-frame-indexing-returns-nil.md) |
| `Language/perl-var.rakudoc:154` | ~~`CompUnit::Repository::FileSystem`/`Installation` stringify as `TypeName.new` instead of `inst#<path>`~~ | [resolved](../news/2026-08/compunit-repository-gist-missing-inst-prefix.md) |
| `Type/X/Method/InvalidQualifier.rakudoc:14` | `X::Method::InvalidQualifier` message says "a method" instead of naming the actual method | [invalid-qualifier-error-message-missing-method-name.md](../todo/tickets/invalid-qualifier-error-message-missing-method-name.md) |
| `Type/IO/Path/Parts.rakudoc:71` | `$parts[]` (empty postcircumfix index) on `IO::Path::Parts` shows the whole-object gist instead of iterating its 3 positional elements | [io-path-parts-empty-subscript-not-positional.md](../todo/tickets/io-path-parts-empty-subscript-not-positional.md) |
| `Type/Metamodel/TypePretense.rakudoc:15,47` | ~~`Role ~~ Cool` is `False` (should be `True`, same as `Mu`/`Any`), and `.HOW.pretending_to_be` is unimplemented~~ | [resolved](../news/2026-08/role-type-pretense-cool-incomplete.md) |

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
| `Type/Code.rakudoc:81` | `$:name` named-placeholder variable fails to interpolate inside a double-quoted string (bare form already works) | [named-placeholder-var-interpolation-in-string-fails.md](../todo/tickets/named-placeholder-var-interpolation-in-string-fails.md) |
| `Type/DateTime.rakudoc:137` | ~~a `DateTime` day-range-check throws the generic `X::OutOfRange` instead of the already-registered, more specific `X::Temporal::OutOfRange`~~ | [resolved](../news/2026-08/datetime-day-out-of-range-uses-generic-outofrange.md) |
| `Type/DateTime.rakudoc:281,302` | `.julian-date`/`.modified-julian-date` return `Num` (float noise) instead of an exact `Rat` | [datetime-julian-date-returns-num-not-rat.md](../todo/tickets/datetime-julian-date-returns-num-not-rat.md) |
| `Type/Formatter.rakudoc:16,32` | `Formatter.new(FORMAT_STRING)` is unimplemented | [formatter-new-unimplemented.md](../todo/tickets/formatter-new-unimplemented.md) |
| `Type/X/Cannot/Empty.rakudoc:15` | ~~`X::Cannot::Empty.new(:action, :what).message` returns an empty string instead of formatting "Cannot ACTION from an empty WHAT"~~ | [resolved](../news/2026-08/x-cannot-empty-message-not-formatted.md) |
| `Type/Compiler.rakudoc:58` | ~~`$*RAKU.compiler.verbose-config` is unimplemented~~ (mutsu reports its own truthful config; the key set is build-specific, and the doc's own repro dies in rakudo too) | [resolved](../news/2026-08/compiler-verbose-config-unimplemented.md) |
| `Type/IO/ArgFiles.rakudoc:34` | `$*ARGFILES.eof`/`.get` loops forever instead of terminating once stdin is exhausted (no file args given) | [argfiles-eof-infinite-loop-on-empty-stdin.md](../todo/tickets/argfiles-eof-infinite-loop-on-empty-stdin.md) |

**Excluded from this batch-4 sub-run:**
- `Type/Code.rakudoc` [3], [4] (lines 140, 153) — `raku-drift` (object hex-address
  text in `#`(Block|...)`/`#`(Sub|...)` gist output, inherently non-reproducible).
- `Type/Compiler.rakudoc` [1] (line 13, `$*RAKU.compiler`) — `raku-drift` and
  environment-dependent (compiler name/version string), same shape as the already-
  excluded `$*RAKU.compiler.version`/`$*VM.config` findings above; mutsu correctly
  reports its own identity (`mutsu (0.1.0)`) rather than impersonating rakudo, which
  is intentional, not a bug.

Found in the 2026-08-22 batch-4 re-run of `operators`/`MixHash`/`Phaser::PrePost`/
`IO::Spec::Cygwin`/`quoting`/`classtut`/`Lock::ConditionVariable`/`newline`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Language/operators.rakudoc:3177` | `infix:<Z>(...)` function-call form dies at runtime ("Two terms in a row"), other `infix:<...>` ops work | [infix-z-function-call-form-parse-fails.md](../todo/tickets/infix-z-function-call-form-parse-fails.md) |
| `Language/operators.rakudoc:482` | `«=»` hyper-assignment to a nested-tuple destructuring target silently no-ops | [Fixed](../news/2026-08/hyper-assign-nested-destructuring-target-not-applied.md) |
| `Language/operators.rakudoc:567` | a hyper-operator wrapping another hyper-operator (`»>>+<<»`) fails to parse | [nested-hyper-operator-parse-fail.md](../todo/tickets/nested-hyper-operator-parse-fail.md) |
| `Language/operators.rakudoc:707` | `.raku` on a nested array-literal element drops the `$`-itemization prefix | [array-literal-nested-element-itemization-lost-in-raku.md](../todo/tickets/array-literal-nested-element-itemization-lost-in-raku.md) |
| `Language/operators.rakudoc:1675` | `[∘]` (empty-operand function-composition reduce) doesn't produce an identity `Callable` | [compose-reduce-empty-list-not-identity-callable.md](../todo/tickets/compose-reduce-empty-list-not-identity-callable.md) |
| `Type/X/Phaser/PrePost.rakudoc:15` | `X::Phaser::PrePost`'s message drops the failed `PRE`/`POST` condition's source text | [phaser-pre-post-message-drops-condition-source-text.md](../todo/tickets/phaser-pre-post-message-drops-condition-source-text.md) |
| `Type/IO/Spec/Cygwin.rakudoc:80` | `IO::Spec::Cygwin.is-absolute` doesn't recognize a Win32-style drive path (`C:\foo`) | [iospec-cygwin-is-absolute-missing-win32-drive-path.md](../todo/tickets/iospec-cygwin-is-absolute-missing-win32-drive-path.md) |
| `Language/quoting.rakudoc:368` | `< 42/10 >` (space-padded angle-quote word) doesn't produce the `RatStr` allomorph like the Complex case does | [angle-bracket-quoted-word-space-padded-loses-allomorph.md](../todo/tickets/angle-bracket-quoted-word-space-padded-loses-allomorph.md) |
| `Type/Lock/ConditionVariable.rakudoc:69` | `Lock`/`condition`/`Thread.start` signal-wait deadlocks when the lock/condition/counter locals are declared inside a loop body (works fine at top level / in a bare block) | [loop-scoped-lock-condition-thread-signal-hang.md](../todo/tickets/loop-scoped-lock-condition-thread-signal-hang.md) |
| `Language/newline.rakudoc:40` | `open()` doesn't recognize an `IO::Special` object (`<STDOUT>`/etc.) as a special-handle target, tries to open it as a literal path | [open-io-special-stdout-target-not-recognized.md](../todo/tickets/open-io-special-stdout-target-not-recognized.md) |
| `Type/MixHash.rakudoc:99` | `MixHash (^) MixHash` / `MixHash (+) MixHash` give garbage output (raw un-combined pairs) where the same ops on plain `Mix` work correctly | [mixhash-set-operators-give-wrong-uncoerced-output.md](../todo/tickets/mixhash-set-operators-give-wrong-uncoerced-output.md) |

**Excluded from this batch-4 sub-run (already deferred/resolved/drift/false-positive):**
- `Language/operators.rakudoc` [1] (line 1795, `<a b c> (+) (a => 2.5, b => 3.14).Mix`) — matches
  the already-documented "Still deferred: Mix *arithmetic* operators" residue under the
  `Mix.rakudoc`/`Baggy.rakudoc` entry in the Resolved section above (lossy f64 weight addition,
  `4.140000000000001` vs `4.14`).
- `Language/operators.rakudoc` [3]/[chained-Z parse crash] (line 3157, `say (1, 2 Z <a b c> Z <+
  ->).raku;`, and the simpler `say 1, 2 Z 3, 4 Z 5, 6;`) — a residue of the already-**Deferred**
  "List-infix (`Z`/`X`/meta/infix-func) comma precedence" cluster: the statement/argument listop
  fix (#5268/#5271) lifts a *single* `Z` occurrence in a comma-list argument, but a *second*,
  chained `Z` in the same argument still hard-fails to parse ("Two terms in a row") instead of
  just giving a wrong precedence result. Confirmed the fully-parenthesized form (`(1,2) Z (3,4) Z
  (5,6)`) parses and evaluates correctly, isolating the gap to the same listop-arg-list-lifting
  mechanism, not a new root cause.
- `Language/operators.rakudoc` [8] (line 602, `my @n = [\~] 1..*; say @n[^5];`) — matches the
  already-**Deferred** Lazy-list cluster's named "closure_seq / scan_spec arrays stay
  force-capped on `@`-assign" residue (confirmed: this exact `@`-assigned triangle-reduce over an
  infinite Range hangs).
- `Language/operators.rakudoc` [11] (line 2376, `Set(...) eqv Set(...)` with a custom `.WHICH`
  method) — matches the already-**Deferred** "WHICH-keyed QuantHash storage" cluster.
- `Language/operators.rakudoc` [12] (line 2507, bare `$*TOLERANCE` arithmetic) — matches the
  already-documented "Still open" residue of the `≅`/`=~=` fix (operators.rakudoc [20]/[21]
  above) and the "Block-scope restore of a dynamic var with a pre-existing outer value" deferred
  cluster.
- `Type/MixHash.rakudoc` [1], [3], [4] (lines 38/58/69, `.pairs`/`.keys.map(&WHAT)` element
  order) — hash/QuantHash iteration-order `raku-drift`/nondeterminism, the "Known harness false
  positive" documented above the Ticketed section; verified directly that repeated `raku` runs
  of the identical program (`MixHash.new: "a","a","b"=>0,"c"=>3.14; .keys.map(&WHAT)`) give
  different orders across runs (4 runs: `(Pair)(Str)(Pair)` ×3, `(Str)(Pair)(Pair)` ×1).
- `Language/classtut.rakudoc` [1] (line 805, `$o.^methods(:local)».name.join(', ')` on stub
  classes) — the real-`raku` output includes a synthesized `POPULATE` method that the doc's own
  illustrative `# OUTPUT` block (for a related, fuller example a few lines up) does not mention
  at all; this looks like an undocumented, Rakudo-build-specific internal method (confirmed: even
  a bare `class Foo {}; say Foo.^methods(:local)».name` shows `(POPULATE)` taking 2 positional
  args, i.e. internal plumbing, not user-visible language behavior) added to the current Rakudo
  build after this doc was written — treated as raku-implementation-drift, not a mutsu bug.

Found in the 2026-08-22 batch-5 re-run of `SetHash`/`Metamodel::Mixins`/`BagHash`/`pod`/
`containers`/`hashmap`/`Setty`/`Block`/`io-guide`/`Proxy`/`CompUnit::Repository::FileSystem`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/BagHash.rakudoc:112` | ~~`.add`/`.remove` methods are unimplemented on `BagHash` (subscript-based mutation already works)~~ FIXED | [news/2026-08/baghash-add-remove-methods.md](../news/2026-08/baghash-add-remove-methods.md) |
| `Type/Block.rakudoc:17` | `.signature` of a bare `{;}` block (implicit `$_` parameter) gists as the garbled `($$_?)` instead of `(;; $_? is raw = OUTER::<$_>)` | [implicit-topic-block-signature-gist-wrong.md](../todo/tickets/implicit-topic-block-signature-gist-wrong.md) |
| `Type/CompUnit/Repository/FileSystem.rakudoc:45` | ~~`.files(name, :ver)` introspection method is unimplemented~~ | [resolved](../news/2026-08/compunit-repository-filesystem-files-method-missing.md) |

**Excluded from this batch-5 sub-run (already deferred/resolved/drift/false-positive/duplicate):**
- `Type/SetHash.rakudoc` [1], [2] (lines 88, 99, `.keys`/`.values` order after `.new`/`.SetHash`
  coercion) — hash/SetHash iteration-order nondeterminism, the "Known harness false positive"
  documented above the Ticketed section.
- `Type/BagHash.rakudoc` [1], [2] (as bucketed: line 66 `output-mismatch`, lines 58/78/129
  `raku-drift`) — the same hash/BagHash iteration-order nondeterminism; verified directly that 3
  repeated `raku` runs of the identical `new-from-pairs` example gave `("b","c")` twice and
  `("c","b")` once.
- `Language/pod.rakudoc` [1] (line 170, `Magician.WHY`/`&duel.WHY.leading`/`.trailing`) —
  duplicated `pod-why-declarator-object-not-stringified` (same root cause: `Pod::Block::
  Declarator`'s `.gist` wasn't implemented, so both the bare-stringify and the
  `.leading`/`.trailing` accessor symptoms traced to the same gap). FIXED — see
  [news/2026-08/pod-why-declarator-object-not-stringified.md](../news/2026-08/pod-why-declarator-object-not-stringified.md);
  re-verified directly that this example's output now matches `raku`.
- `Language/containers.rakudoc` [1], [3], [4], [5] — `raku-drift` (exception-message wording
  drift, object-address/generated-variable-name text, and big-Int-seed-dependent `.raku` gist,
  all version/environment-specific).
- `Language/hashmap.rakudoc` [1] (line 444, `.kv` iteration order) — hash iteration-order
  nondeterminism, the same known false positive.
- `Type/Setty.rakudoc` [1] (line 112, `Set.new(1,2,3).keys`) — Set iteration-order
  nondeterminism, the same known false positive.
- `Language/io-guide.rakudoc` [1] (line 281, `temp $*OUT = open :w, $*SPEC.devnull;` not
  redirecting `say`) — re-verified directly: `open :w, PATH` (named adverb before the
  positional path) itself fails and returns a `Failure` in mutsu, which then gets assigned to
  `$*OUT`, so `say` silently keeps writing to the original stdout — this is a downstream
  consequence of the already-filed
  [open-named-adverb-before-positional-path.md](../todo/tickets/open-named-adverb-before-positional-path.md),
  not a separate bug; not re-filed.

Found in the 2026-08-22 batch-5 re-run of `Routine`/`signatures`/`Cool`/`Metamodel::Documenting`/
`Baggy`/`CallFrame`/`Slip`/`Buf`/`math`/`X::TypeCheck::Splice`/`HyperWhatever`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Routine.rakudoc:29` | `&?ROUTINE.^name` inside a `submethod` reports `Method` instead of `Submethod` | [routine-submethod-routine-name-wrong.md](../todo/tickets/routine-submethod-routine-name-wrong.md) |
| `Type/Routine.rakudoc:144` | `is cached` (`use experimental :cached`) doesn't memoize — every call re-executes the body | [is-cached-trait-not-caching.md](../todo/tickets/is-cached-trait-not-caching.md) |
| `Type/Routine.rakudoc:231` | `sub ... is rw` returning an array/hash element as the implicit last statement doesn't produce a mutable container back to the caller (broad blast radius — narrowed to a 3-line minimal repro) | [is-rw-sub-implicit-return-element-not-mutable.md](../todo/deep/is-rw-sub-implicit-return-element-not-mutable.md) |
| `Type/Cool.rakudoc:932` | `(0..0x1FFFF).sort(*.uniname.chars)` is ~18x slower than raku and times out under the harness's 10s budget (correct result, just too slow) | [uniname-sort-performance-gap.md](../todo/perf/uniname-sort-performance-gap.md) |
| `Type/Baggy.rakudoc:197` | ~~`classify-list` with an array mapper renders an out-of-range key as `Nil` instead of `(Any)` (plain `.classify` with a block mapper already gets this right)~~ | [resolved](../news/2026-08/classify-list-array-mapper-out-of-range-shows-nil.md) |
| `Type/Buf.rakudoc:84` | ~~`subbuf-rw($buf, from, len) = value` (bare function-call form) silently doesn't mutate; the method-call form (`$buf.subbuf-rw(from, len) = value`) already works~~ (the fix was mutating the buffer in place instead of rebuild-and-write-back) | [resolved](../news/2026-08/subbuf-rw-function-form-lvalue-not-mutating.md) |

**Excluded from this batch-5 sub-run:**
- `Type/Metamodel/Documenting.rakudoc` [1] (line 16, `#\|[...]`/`#=[...]` class-level declarator
  comments, `say Documented.WHY`) — duplicated `pod-why-declarator-object-not-stringified` (same
  root cause: `Pod::Block::Declarator`'s `.gist` wasn't implemented; that ticket's repro uses a
  `sub`, this one a `class`, but both hit the identical `Pod::Block::Declarator.new` gist
  fallback). FIXED — see
  [news/2026-08/pod-why-declarator-object-not-stringified.md](../news/2026-08/pod-why-declarator-object-not-stringified.md);
  re-verified directly that this example's output now matches `raku`.
- `Type/Baggy.rakudoc` [1], [4] (lines 43/355, `.grab`/`.hash` type-parameterization) —
  `raku-drift-from-doc`: raku's own current output no longer matches the doc's `# OUTPUT` text
  (floating precision / `Hash[UInt,Mu,Any]` vs the doc's stale `Hash[Any,Any]`), lower priority.
- `Type/Baggy.rakudoc` [3] (line 293, `bag <eggs spam spam spam>; .kv`) — known Bag/Baggy
  iteration-order false positive: verified non-determinism directly (3 fresh `raku` runs of the
  identical program in this session all returned `(eggs 1 spam 3)`, which itself already
  disagrees with the doc-diff harness's earlier-captured `(spam 3 eggs 1)` for the same command
  in the same session — confirming per-process hash-seed nondeterminism, not a mutsu bug).
- `Type/CallFrame.rakudoc` [1] (line 76, statement-form `FIRST $frame = callframe; ...; say
  $frame.code()`) — matches the file's own already-documented Deferred residue exactly (`Code.new`
  vs mutsu's `(Block)`; see the "CallFrame frame modeling" entry above).
- `Type/CallFrame.rakudoc` [2] (line 122, `$frame.my<$the-answer>`) — `raku-drift-from-doc`: raku's
  actual output is `(LoweredAwayLexical)`, not the doc's stale `42`.
- `Language/math.rakudoc` [1] (line 185, golden-ratio continued-fraction `1 + 1 / * ... *` indexed
  at `@phis[200]`) — narrowed the failure to `@phis[N]` returning `(Any)` for `N` somewhere in
  `30..35` (works through at least `N=30`, fails by `N=35`), which is exactly where the
  self-referential Rat/FatRat continued-fraction's numerator/denominator would first need
  bigint magnitude beyond `i64` — matches the already-**Deferred** Lazy-list cluster's "big-Int→
  Float degradation in geometric sequence generation past i64" residue.
- `Type/X/TypeCheck/Splice.rakudoc` [1] (line 30, `use experimental :macros; macro an-ast {
  quasi { 'yes AST' } }`) — matches the already-tracked deep `macro`/`quasi`/unquote design work
  in `todo/deep/rakuast-remaining.md`'s "Macros" section (same cluster already excluded for
  `Language/experimental.rakudoc` in the batch-3 sub-run above); not re-ticketed.

Found in the 2026-08-22 batch-5 re-run of `Test`/`Metamodel::EnumHOW`/`Sub`/`ipc`/`numerics`/
`contexts`/`Sequence`/`Bag`/`haskell-to-p6`/`PositionalBindFailover`/`using-modules::code`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Test.rakudoc:323` | ~~regex char class `<.-:letter-:digit>` (dot base + two chained `-` subtractions) matches everything instead of subtracting~~ | [resolved](../news/2026-08/charclass-dot-base-chained-subtraction-broken.md) |
| `Type/Test.rakudoc:400` | `.isa(Numeric)` (and other roles) wrongly returns `True` — `isa_check` conflates nominal class hierarchy with role composition | [isa-conflates-roles-with-nominal-supertypes.md](../todo/deep/isa-conflates-roles-with-nominal-supertypes.md) |
| `Type/Test.rakudoc:586` | ~~`throws-like`'s `message =>`/`gist =>` matchers are silently skipped when the thrown exception is `X::AdHoc` (e.g. from `fail`/`die` with a string)~~ | [resolved](../news/2026-08/throws-like-named-matchers-no-longer-silently-skipped.md) |
| `Type/Metamodel/EnumHOW.rakudoc:139,148,157` | `EnumHOW` is missing `.^enum_values`/`.^enum_from_value` (No such method) and `.^elems` (deterministic stack overflow) | [enumhow-missing-enum-values-elems-enum-from-value.md](../todo/tickets/enumhow-missing-enum-values-elems-enum-from-value.md) |
| `Type/Sub.rakudoc:19` | a user `sub Int(...)` wrongly shadows the built-in `Int(...)` type-coercion call syntax (only `&Int(...)` should reach it) | [user-sub-named-int-shadows-builtin-coercion-call.md](../todo/tickets/user-sub-named-int-shadows-builtin-coercion-call.md) |
| `Type/Sub.rakudoc:78` | ~~`is foo[1,2,3]` custom variable-trait bracket-argument sugar is misparsed as `is Set[Int]`-style type parameterization, folding the args into the (bogus) trait name~~ | [resolved](../news/2026-08/variable-trait-bracket-argument-misparsed-as-type-param.md) |
| `Language/ipc.rakudoc:14` | `run`/`shell` discard the child's stdout/stderr by default (`Stdio::null()`) instead of inheriting the parent's | [run-shell-discard-stdout-stderr-by-default.md](../todo/deep/run-shell-discard-stdout-stderr-by-default.md) |
| `Language/numerics.rakudoc:353` | calling `.^name` on a `MAIN`-bound `IntStr` argument corrupts a later, unrelated `IntStr.new(...).^name` (reports `Str`) | [main-allomorph-arg-name-corrupts-later-intstr-new.md](../todo/tickets/main-allomorph-arg-name-corrupts-later-intstr-new.md) |
| `Language/contexts.rakudoc:45` | a bare sub-CALL statement (`foo;`) returning a fresh custom-`.sink`-method instance never invokes `.sink` — the function-call-return residue recorded in [role-mixed-sink-method-not-invoked-in-sink-context.md](../news/2026-08/role-mixed-sink-method-not-invoked-in-sink-context.md) | (not re-filed — see Excluded below) |
| `Type/PositionalBindFailover.rakudoc:34` | ~~`does PositionalBindFailover` fails with `X::InvalidType` — the role is missing from the `BUILTIN_PARENT_TYPES` allow-list~~ (the shallow "recognize the type name" half only; the iterator-consultation behaviour stays with the Deferred custom-`Iterable`/`Iterator` cluster) | [resolved](../news/2026-08/positionalbindfailover-not-recognized-as-builtin-role.md) |
| `Language/using-modules/code.rakudoc:95` | a module's `EXPORT::DEFAULT` namespace isn't a real, symbolically-navigable package (`::("Test::EXPORT::DEFAULT::&ok")` fails) | [export-default-package-not-symbolically-navigable.md](../todo/deep/export-default-package-not-symbolically-navigable.md) |
| `Language/haskell-to-p6.rakudoc:263` | `.signature` on a `proto` sub reports a generic `($arg0)` placeholder instead of the declared signature | [proto-sub-signature-reports-generic-placeholder.md](../todo/tickets/proto-sub-signature-reports-generic-placeholder.md) |

**Excluded from this batch-5 sub-run:**
- `Language/ipc.rakudoc` [1] (`run 'git', 'status';`) — the `git status` text itself is
  environment-dependent (repo state), but the underlying reproducible defect (mutsu prints
  no output at all, regardless of repo state) is the real finding, ticketed above as
  `run-shell-discard-stdout-stderr-by-default.md`.
- `Language/numerics.rakudoc` [2], [3], [4] (lines 595, 609, 751) — `raku-drift-from-doc`
  (native-int wraparound, `uint8` array coercion, and atomic-increment counts in the doc's
  `# OUTPUT` no longer match current `raku`'s own output).
- `Language/contexts.rakudoc` [2] (line 130, `[~] [3, 5+6i, Set(<a b c>), ...]`) —
  `raku-drift-from-doc` (the `Set`'s `.keys` iteration order in the reduced string differs
  from the doc's `# OUTPUT`, and also varies run-to-run in real `raku` itself — the same
  Set/Bag/hash iteration-order nondeterminism documented as a known harness false positive
  above).
- `Language/contexts.rakudoc` [1] (line 45, `return [<a b c>] does role { method sink {...} }`)
  — see the ticket-column note above: this is the same `.sink`-in-sink-context gap already
  fixed for a mixin in `news/2026-08/role-mixed-sink-method-not-invoked-in-sink-context.md`;
  what remains here is that entry's documented "function-call return" residue (the `SinkPop` VM op's own code comment already
  says a normal sub's fresh-instance return is conservatively not auto-sunk, pending
  first-class container identity) — not re-filed as a separate ticket.
- `Type/Sequence.rakudoc` [1] (line 69, `$s.eager` twice on a `lazy 1..5` — should throw
  `X::Seq::Consumed` on the second call) — matches the already-**Deferred** Lazy-list
  cluster's container-repr/reification-consumption residue.
- `Type/Bag.rakudoc` [1] (line 53, `.keys.raku`/`.values.raku` element order) — Bag/hash
  iteration-order `raku-drift`/nondeterminism, the documented known harness false positive.

Found in the 2026-08-22 batch-6 re-run of `Label`/`IO::Spec::Win32`/`Pair`/`Attribute`/`Unicode`/
`X::Proc::Async::MustBeStarted`/`Proc`/`traits`/`glossary`/`Metamodel::Versioning`/
`regexes-best-practices`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Label.rakudoc:87,101,120` | ~~a hyphenated loop label (`MY-LABEL:`) is never recognized by `next`/`last`/`redo`, silently downgrading them to their unconditional/unlabeled form (and hanging forever for a mis-parsed `redo`)~~ (labels are ordinary identifiers; they are registered at their declaration site now) | [resolved](../news/2026-08/loop-label-name-rejects-hyphens.md) |
| `Type/Pair.rakudoc:61` | `.raku` on a `Hash` populated via slurpy `*%h` named-arg binding doesn't abbreviate `Bool::True`-valued pairs to `:key` the way real raku does (2026-08-26: re-measured — the signal is per-value Scalar-containerness of the hash's values, so this needs hash element containerization, not a `.raku` tweak) | [slurpy-hash-named-arg-raku-boolean-shorthand-missing.md](../todo/tickets/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) |
| `Type/Attribute.rakudoc:58` | `has @.attr is default(V) is rw` — assigning `Nil` resets the array to `[Any]` instead of `[V]` | [array-attribute-default-not-applied-on-nil-assign.md](../todo/tickets/array-attribute-default-not-applied-on-nil-assign.md) |
| `Language/traits.rakudoc:42` | a class-scoped `my $.counter` (dot-twigil'd `my` variable, not a `has` attribute) doesn't persist mutations across method calls | [class-scoped-my-dot-attribute-doesnt-persist.md](../todo/tickets/class-scoped-my-dot-attribute-doesnt-persist.md) |

**Excluded from this batch-6 sub-run (already deferred/resolved/drift/false-positive/duplicate):**
- `Type/IO/Spec/Win32.rakudoc` [1] (line 162, `.rel2abs`) and [4] (line 126, `.join`) —
  `raku-drift-from-doc` (both compare against the doc's stated Windows-style `# OUTPUT`, but the
  running environment's actual `$*CWD` is this Linux checkout's path).
- `Type/IO/Spec/Win32.rakudoc` [2] (line 190, `.split`) and [3] (line 251, `.splitpath`) — mutsu
  mis-assigns/mis-normalizes path components (e.g. `.split('/foo/')` gives `"\\"` as the directory
  component where raku gives `"/"`; `.splitpath('.')` swaps the dirname/filename slots) — same
  Win32-path-component-computation root cause as the already-open
  [iopath-win32-separator-normalization.md](../todo/tickets/iopath-win32-separator-normalization.md)
  (itself filed from this same `Type/IO/Spec/Win32.rakudoc` file per its own text), not re-filed as
  a separate ticket.
- `Type/Attribute.rakudoc` [2] (line 186, `is built(:bind)` + `my Foo:D $foo .= new: ...`) — the
  crash is the already-filed
  [lexical-typed-var-dot-equals-init-fails.md](../todo/tickets/lexical-typed-var-dot-equals-init-fails.md)
  (`my Type:D $var .= new;` fails to strip the `:D` smiley before building the `.=` call target);
  confirmed with the minimal two-line repro (`class Foo {}; my Foo:D $foo .= new;`) — not re-filed.
- `Type/Attribute.rakudoc` [3] (line 418, `.?DEPRECATED`) — `raku-drift-from-doc` (mutsu prints
  nothing for either `with` block; not investigated further since it's bucketed as drift by the
  harness).
- `Type/Proc.rakudoc:140` (`shell(...)` inside a `temp $*OUT` block) — narrowed to the simpler,
  already-filed
  [run-shell-discard-stdout-stderr-by-default.md](../todo/deep/run-shell-discard-stdout-stderr-by-default.md)
  (`shell()`'s spawned child process's stdout is lost even with **no** `$*OUT` redirection at all
  — confirmed with `shell("raku some-file-that-says-42.raku")` alone); not re-filed.
- `Language/regexes-best-practices.rakudoc:163` (`token ws { <!ww> \h* }` inside a grammar) — was
  the same finding as the then-open `grammar-ws-boundary-and-vertical-whitespace` ticket, whose
  repro crashed identically (`No such method 'ww' for invocant of type 'Match'`); both are now
  [resolved](../news/2026-08/grammar-ws-boundary-and-vertical-whitespace.md) — `<?ww>`/`<!ww>` are
  implemented as real zero-width assertions.

Found in the 2026-08-22 batch-6 re-run of `js-nutshell`/`IO::Spec::Unix`/`py-nutshell`/
`X::AdHoc`/`Metamodel::Trusting`/`X::Proc::Async::CharsOrBytes`/`Iterable`/
`statement-prefixes`/`exceptions`/`Metamodel::Stashing`/`optut`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Language/js-nutshell.rakudoc:384` | a user `multi prefix:<++>($a)` wins over the builtin `++` for `Int`/`Bool`/`Num` (the types raku's builtin has a *typed* candidate for); `is default` is not involved and the decision is made at parse time | [multi-is-default-loses-to-user-candidate-over-builtin.md](../todo/tickets/multi-is-default-loses-to-user-candidate-over-builtin.md) |
| `Language/js-nutshell.rakudoc:613` | `next`/`last LABEL` inside a labeled `repeat {} while` loop throws `X::ControlFlow` instead of being caught | [labeled-next-last-in-repeat-while-loop-throws.md](../todo/tickets/labeled-next-last-in-repeat-while-loop-throws.md) |
| `Type/IO/Spec/Unix.rakudoc:230,291` | `IO::Spec::Unix.split`/`.splitpath` mishandle all-slash, empty, and bare-`.` path inputs | [iospec-unix-split-splitpath-edge-cases-wrong.md](../todo/tickets/iospec-unix-split-splitpath-edge-cases-wrong.md) |
| `Language/py-nutshell.rakudoc:541` | `{ BLOCK } for LIST` (bare block as a `for` statement-modifier operand) parses as an uncalled closure term instead of being invoked per iteration with `$_` | [bare-block-for-statement-modifier-not-invoked-as-loop-body.md](../todo/tickets/bare-block-for-statement-modifier-not-invoked-as-loop-body.md) |
| `Type/X/AdHoc.rakudoc:56` | ~~`X::AdHoc.from-slurpy(...)` class method is entirely unimplemented~~ | [resolved](../news/2026-08/xadhoc-from-slurpy-method-missing.md) |
| `Language/statement-prefixes.rakudoc:32` | ~~`my @array = lazy { LIST-EXPR }` stores an unforceable `lazy(...)` placeholder element; `.eager` never forces/flattens it~~ (`lazy BLOCK` runs its block eagerly and marks the *result* lazy) | [resolved](../news/2026-08/lazy-block-prefix-array-assign-never-forces.md) |
| `Language/exceptions.rakudoc:428` | `.resume` on a `die`-based exception raised inside a nested sub does not resume execution at the `die`'s call site | [resume-does-not-return-to-die-call-site-in-nested-sub.md](../todo/deep/resume-does-not-return-to-die-call-site-in-nested-sub.md) |
| `Language/optut.rakudoc:12` | ~~a user-defined `circumfix:<...>` operator with a Unicode-letter delimiter (e.g. `α ... ω`) fails to parse at the call site~~ (the trigger was a lowercase closer, not Unicode: it was eaten as a speculative infix word) | [resolved](../news/2026-08/custom-circumfix-unicode-delimiter-call-parse-fail.md) |

**Related finding (not a new ticket):** `Type/Metamodel/Stashing.rakudoc:45` — a custom
metaclass composing `does Metamodel::Naming does Metamodel::Stashing` fails with
`X::InvalidType: Invalid typename 'Metamodel::Naming'` before its class body is even
considered. Added as a "Related finding" section to the existing
[direct-metamodel-classhow-new-type-immutable-error.md](../todo/deep/direct-metamodel-classhow-new-type-immutable-error.md)
deep ticket, since it's the same "script type creation directly through
`Metamodel::*`" territory as that ticket's own finding.

**Excluded from this batch-6 sub-run:**
- `Language/js-nutshell.rakudoc` [2] (line 562, `my Str %letters{Str}` iteration order)
  — the documented known harness false positive (hash/typed-hash iteration order is
  nondeterministic per-process in real raku too).
- `Type/IO/Spec/Unix.rakudoc` [1] (line 99, `$*SPEC.curupdir` block gist/pointer) and
  [2] (line 201, `$*CWD`-relative `.rel2abs` outputs) — both `raku-drift-from-doc`:
  the harness's checkout path (`$*CWD`) and the block's own gist address are
  inherently environment-dependent, not stable doc `# OUTPUT` values.
- `Language/py-nutshell.rakudoc` [1] (line 387, `%elem-for-symbol.kv` iteration order)
  — the documented known harness false positive (hash iteration order).
- `Type/Iterable.rakudoc` [1] (line 52, `(1..10).iterator.say`) — raku's internal
  `Iterator` implementation class names (e.g. `Rakudo::Iterator::IntRange`) are a
  Rakudo-internal implementation detail with no cross-implementation stability
  guarantee (mutsu has a single generic `Iterator` value type rather than a zoo of
  per-shape internal subclasses); treated the same as the documented hex-address
  exclusion — non-actionable, not ticketed.

Found in the 2026-08-22 batch-6 re-run of `Metamodel::DefiniteHOW`/`Junction`/`functions`/
`Metamodel::MethodContainer`/`faq`/`X::Str::Match::x`/`Real`/`Associative`/`grammar_tutorial`/
`Method`/`unicode_entry`:

| file:line | one-line summary | ticket |
|---|---|---|
| `Type/Metamodel/DefiniteHOW.rakudoc:18,27,80` | a bare `Type:D`/`Type:U` term loses its definiteness constraint entirely — `.^name`, `~~`, and `.^base_type` all treat it as the plain unconstrained type | [definiteness-constrained-type-object-identity-lost.md](../todo/deep/definiteness-constrained-type-object-identity-lost.md) |
| `Type/Junction.rakudoc:266` | ~~`Junction.new("one", 1..6)` doesn't flatten a `Range` values-argument into individual elements, so `.Bool` is wrong~~ (`\values` is `.list`-ified: every iterable flattens) | [resolved](../news/2026-08/junction-new-range-argument-not-flattened.md) |
| `Type/Junction.rakudoc:205` | a self-referential `$j = any (gather $j».take)...` combined with a value-producing `when` in a helper sub crashes with a stack overflow | [junction-self-referential-gather-stack-overflow.md](../todo/tickets/junction-self-referential-gather-stack-overflow.md) |
| `Type/Metamodel/MethodContainer.rakudoc:40` | `.^methods(:all)` ignores the `:all` adverb — returns own methods only, same as the plain call | [metaclass-methods-all-flag-ignored.md](../todo/tickets/metaclass-methods-all-flag-ignored.md) |
| `Language/faq.rakudoc:359` | ~~a custom `postcircumfix:<[...]>` operator's `+@slurpy` args are wrong when called via subscript syntax, though the identical logic works fine as a plain sub call~~ (three bugs: the built-in subscript won the longest-token race, the bracket was parsed below comma precedence, and `multi` dispatch ignored `+@`) | [resolved](../news/2026-08/custom-postcircumfix-slurpy-args-wrong-in-subscript-form.md) |
| `Language/faq.rakudoc:1108` | repeated big-Int addition (growing-magnitude Fibonacci-style loop) is ~14x slower than raku, timing out under the harness budget at 100k iterations | [bigint-repeated-addition-performance-gap.md](../todo/perf/bigint-repeated-addition-performance-gap.md) |
| `Type/Real.rakudoc:31` | adding two custom `Real`-subclass instances (via `.Bridge`) produces an exact `Rat` where raku produces an approximate `Num` | [real-subclass-generic-plus-produces-exact-rat-not-num.md](../todo/tickets/real-subclass-generic-plus-produces-exact-rat-not-num.md) |
| `Type/Associative.rakudoc:53` | `.of` on a class statically `does`-ing a parametric `Associative[Cool,DateTime]` reports `(Mu)` instead of the declared value type `(Cool)` | [associative-of-static-does-parametric-role-wrong-value-type.md](../todo/tickets/associative-of-static-does-parametric-role-wrong-value-type.md) |
| `Language/grammar_tutorial.rakudoc:679` | a grammar `rule` with multiple embedded code blocks and subrule calls executes them out of the declared left-to-right order (root-caused: a `make`-bearing block defers to the post-order reduce walk) | [grammar-action-ordering-vs-inline-code-blocks.md](../todo/deep/grammar-action-ordering-vs-inline-code-blocks.md) |
| `Language/unicode_entry.rakudoc:532` | ~~nested Unicode "curly" double quotes (`“...“...”...”`) fail to parse — the lexer doesn't track nesting depth for this quote pair~~ | [resolved](../news/2026-08/nested-unicode-curly-double-quotes-parse-fail.md) |

**Excluded from this batch-6 sub-run (already deferred/resolved/drift/false-positive/duplicate):**
- `Language/functions.rakudoc` [1] (line 1127, `&how-many.cando(...)` gist) — bucketed
  `raku-drift` overall (the doc's stated candidate-signature format is stale), but re-verified
  directly that mutsu's own gist of the returned `Sub`s is also wrong independent of that drift:
  `(how-many how-many)` instead of raku's `(&how-many &how-many)`. This is the same root cause
  (a `Sub`'s default stringify/gist drops the leading `&` sigil) already filed as
  [anon-class-sub-non-ascii-name-and-sub-gist.md](../todo/tickets/anon-class-sub-non-ascii-name-and-sub-gist.md)'s
  second repro; not re-filed.
- `Language/functions.rakudoc` [3] (line 720, `&infix:<XX>([1,(2,3)], [(4,5),6])`) — matches the
  already-**Deferred**/deep "Array/Hash elements are stored bare — element reads lack
  itemization" cluster (`todo/deep/element-itemization-lost-in-scalar-binding.md`, ADR-0040): the
  nested `List` literal `(2,3)` inside the outer `Array` literal `[1,(2,3)]` loses its
  itemization boundary, so the `X`-cross meta-op treats it as two separate elements instead of
  one itemized `List` element.
- `Type/Junction.rakudoc` [2], [3], [4] — `raku-drift-from-doc` (stale doc `# OUTPUT` text for
  the string-concatenation-junction example, the `:exists` junction-key example, and the
  `+any(...)` numeric-coercion-failure example's exception message format).
- `Type/Method.rakudoc` [1] — the named-invocant repro (`method ($invocant: $param) {...}`) and the
  type-only-invocant repro (`method (List:D:) {...}`) were one ticket, now FIXED; see
  [news/2026-08/method-literal-invocant-declaration-syntax-broken.md](../news/2026-08/method-literal-invocant-declaration-syntax-broken.md).

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
