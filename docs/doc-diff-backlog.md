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

The **raw output of the latest committed sweep** is checked in under
[doc-diff-sweep/](doc-diff-sweep/) — read a per-file report there to get the minimal
repros without re-running the sweep. Re-copy it (see that dir's `README.md`) whenever
you refresh the survey.

**Always re-verify a finding directly before treating it as a real bug.** The
harness oracle-gates on raku, but doc examples drift and the harness can only compare
`# OUTPUT:`-style blocks. `raku-drift` findings (raku itself no longer matches the
doc) are version skew, not mutsu bugs — lowest priority.

## Corpus snapshot

- **Date:** 2026-07-22 (re-swept) · debug `mutsu` at main ≈ `ecaee240` (adds #5238 big-FatRat split)
- **444 files scanned · 130 have signal**
- **match = 1951 · output-mismatch = 228 · mutsu-crash = 133 · raku-drift = 133**
- High-signal total (mismatch + crash) by corpus: **Type 183 · Language 178**
- Delta since the 2026-07-21 scan (`1d4c7768`): mismatch −43, crash −46, signal files −6
  (a wave of operator/regex/numeric slices landed in between — `operators.rakudoc`
  dropped off the top entirely, `regexes.rakudoc` from 9 mism/8 crash to 12/3, and
  the big-FatRat split #5238 cleared several `numerics.rakudoc` rows).
- **Raw per-file reports for this scan are committed under
  [doc-diff-sweep/](doc-diff-sweep/)** (`summary.txt`, `progress.txt`,
  `reports/<sanitized-path>.txt`) so a future session can read the minimal repros
  without re-running the ~15-minute sweep. Regenerate with `scripts/doc-diff-sweep.sh`
  and re-copy on the next refresh.

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
| Language/traps.rakudoc | 9 | 2 | 3 |
| Language/variables.rakudoc | 8 | 2 | 5 |
| Language/control.rakudoc | 4 | 6 | 1 |
| Language/list.rakudoc | 6 | 3 | 3 |
| Type/IO/Path.rakudoc | 7 | 1 | 1 |
| Type/Any.rakudoc | 4 | 3 | 5 |
| Language/objects.rakudoc | 4 | 3 | 3 |
| Type/Iterator.rakudoc | 2 | 5 | 2 |
| Language/typesystem.rakudoc | 5 | 1 | 2 |
| Type/IO/Handle.rakudoc | 5 | 1 | 1 |
| Language/structures.rakudoc | 4 | 2 | 3 |
| Language/mop.rakudoc | 4 | 2 | 0 |
| Type/independent-routines.rakudoc | 1 | 5 | 2 |
| Type/Parameter.rakudoc | 1 | 5 | 1 |
| Type/Match.rakudoc | 5 | 0 | 0 |
| Type/Mu.rakudoc | 4 | 1 | 2 |
| Language/subscripts.rakudoc | 4 | 1 | 1 |
| Language/concurrency.rakudoc | 4 | 1 | 1 |
| Language/grammars.rakudoc | 3 | 2 | 2 |
| Language/syntax.rakudoc | 3 | 2 | 1 |
| Type/Proc/Async.rakudoc | 3 | 2 | 0 |
| Language/nativecall.rakudoc | 1 | 4 | 0 |
| Language/experimental.rakudoc | 0 | 5 | 0 |
| Language/unicode.rakudoc | 4 | 0 | 1 |
| Language/functions.rakudoc | 3 | 1 | 1 |
| Language/py-nutshell.rakudoc | 3 | 1 | 0 |
| Language/faq.rakudoc | 3 | 1 | 0 |
| Type/Backtrace.rakudoc | 1 | 3 | 0 |
| Language/containers.rakudoc | 3 | 0 | 4 |
| Type/Hash.rakudoc | 3 | 0 | 3 |
| Type/Baggy.rakudoc | 3 | 0 | 3 |
| Type/IO/Spec/Win32.rakudoc | 3 | 0 | 2 |
| Type/IO/Spec/Unix.rakudoc | 3 | 0 | 2 |
| Type/Nil.rakudoc | 3 | 0 | 1 |
| Type/Map.rakudoc | 3 | 0 | 1 |
| Type/Array.rakudoc | 3 | 0 | 0 |
| Language/perl-var.rakudoc | 3 | 0 | 0 |
| Type/Routine.rakudoc | 2 | 1 | 1 |
| Type/Test.rakudoc | 2 | 1 | 0 |
| Type/Metamodel/DefiniteHOW.rakudoc | 2 | 1 | 0 |
| Type/Label.rakudoc | 2 | 1 | 0 |
| Language/js-nutshell.rakudoc | 2 | 1 | 0 |
| Language/signatures.rakudoc | 1 | 2 | 7 |
| Type/Cool.rakudoc | 1 | 2 | 5 |
| Type/Str.rakudoc | 1 | 2 | 0 |
| Type/Metamodel/EnumHOW.rakudoc | 1 | 2 | 0 |
| Type/Code.rakudoc | 0 | 3 | 3 |
| Type/List.rakudoc | 0 | 3 | 2 |
| Type/Metamodel/Mixins.rakudoc | 0 | 3 | 0 |
| Language/nativetypes.rakudoc | 0 | 3 | 0 |
| Type/Junction.rakudoc | 2 | 0 | 3 |
| Language/numerics.rakudoc | 2 | 0 | 2 |
| Type/Scalar.rakudoc | 2 | 0 | 1 |
| Type/DateTime.rakudoc | 2 | 0 | 1 |
| Language/hashmap.rakudoc | 2 | 0 | 1 |
| Type/Iterable.rakudoc | 2 | 0 | 0 |
| Type/IO/Spec/Cygwin.rakudoc | 2 | 0 | 0 |
| Language/statement-prefixes.rakudoc | 2 | 0 | 0 |
| Language/phasers.rakudoc | 2 | 0 | 0 |
| Language/perl-func.rakudoc | 2 | 0 | 0 |
| Language/iterating.rakudoc | 2 | 0 | 0 |
| Language/io-guide.rakudoc | 2 | 0 | 0 |
| Language/glossary.rakudoc | 2 | 0 | 0 |
| Language/exceptions.rakudoc | 2 | 0 | 0 |
| Language/contexts.rakudoc | 2 | 0 | 0 |
| Type/Sub.rakudoc | 1 | 1 | 1 |
| Type/SetHash.rakudoc | 1 | 1 | 1 |
| Type/Metamodel/MethodContainer.rakudoc | 1 | 1 | 1 |
| Type/Attribute.rakudoc | 1 | 1 | 1 |
| Type/X/AdHoc.rakudoc | 1 | 1 | 0 |
| Type/Range.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/ParametricRoleGroupHOW.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/Documenting.rakudoc | 1 | 1 | 0 |
| Language/traits.rakudoc | 1 | 1 | 0 |
| Language/pod.rakudoc | 1 | 1 | 0 |
| Language/classtut.rakudoc | 1 | 1 | 0 |
| Type/Unicode.rakudoc | 0 | 2 | 0 |
| Type/Metamodel/Trusting.rakudoc | 0 | 2 | 0 |
| Type/Lock/Async.rakudoc | 0 | 2 | 0 |
| Type/Formatter.rakudoc | 0 | 2 | 0 |
| Type/Mix.rakudoc | 1 | 0 | 3 |
| Type/Seq.rakudoc | 1 | 0 | 1 |
| Type/CallFrame.rakudoc | 1 | 0 | 1 |
| Type/Bag.rakudoc | 1 | 0 | 1 |
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
| Type/Pair.rakudoc | 1 | 0 | 0 |
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
| Language/haskell-to-p6.rakudoc | 1 | 0 | 0 |
| Language/grammar_tutorial.rakudoc | 1 | 0 | 0 |
| Type/BagHash.rakudoc | 0 | 1 | 3 |
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
| Language/newline.rakudoc | 0 | 1 | 0 |
