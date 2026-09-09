# doc-diff backlog — raku-doc differential findings

Tracked ledger of every `raku-doc` example where **mutsu** diverges from reference
**raku**, produced by the doc-diff harness. This is the "ranked backlog of minimal
repros" that [PLAN.md](../PLAN.md) §6 calls for — the QA-campaign analogue of
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

**When a finding is confirmed real (not a harness false positive — see "Known
harness false positive" below), file it as an issue immediately** on
`tokuhirom/mutsu`, labelled `todo:ticket` (or `todo:deep` for high-blast-radius
ones) per `docs/issue-workflow.md`, and add a row to
[Ticketed](#ticketed-open) below linking the doc location to that
issue. This is what keeps this backlog and the issue queue in sync — a finding
sitting only in a sweep report, or only in an issue with no cross-link, is easy to
lose track of.

The **raw output of the latest committed sweep** is checked in under
[doc-diff-sweep/](doc-diff-sweep/) — read a per-file report there to get the minimal
repros without re-running the sweep. Re-copy it (see that dir's `README.md`) whenever
you refresh the survey.

**Always re-verify a finding directly before treating it as a real bug.** The
harness oracle-gates on raku, but it can only compare what a doc block actually
prints.

**The `raku-drift` bucket no longer exists** (#7590). It was described here as
"version skew, not mutsu bugs — lowest priority", and that was measured wrong on
2026-09-07b: the bucket was only reachable *after* mutsu had already been found to
differ from raku, so it filed real divergences as non-bugs — 67 of its 114 blocks
were confirmed real mutsu bugs against 5 that the name actually described. Whether
raku still matches the doc's own `# OUTPUT:` is **provenance, not priority**, so it
now rides along as an annotation on an ordinary `output-mismatch` finding
("mutsu matches the doc's own `# OUTPUT:` here; raku does not").

**`nondet` in a summary is the noise floor, not a finding.** The harness runs the
oracle twice and drops any block whose *raku* output is not reproducible — unordered
container iteration (`Set`/`Bag`/`Mix`/`*Hash`/`Map`/`Hash.kv`/enum `.keys`), object
addresses and `WHICH` ids, thread ids. Those blocks used to be compared, diverge on
the unreproducible token alone, and land in the low-priority bucket on every run
forever; nine real mutsu bugs hid there, #7587 among them. A rising `nondet` count
means the corpus has more such examples, not that mutsu got worse.

## Corpus snapshot

- **Date:** 2026-09-09 (full re-sweep) · debug `mutsu` at `a44bd28` (main,
  through PR #7740) · `raku` v2026.07
- **443 files scanned · 80 have `mism`/`crash` signal**
- **match = 2409 · output-mismatch = 129 · mutsu-crash = 21 · oracle-nondet = 59**
- High-signal total (mismatch + crash) = **150**.

**The mismatch count is not comparable to the pre-#7590 sweeps, and the rise is
not a regression.** Until 2026-09-07b a third bucket, `raku-drift-from-doc`,
absorbed every divergence whose doc annotation raku itself no longer matched;
#7590 retired it because it was only ever reachable *after* mutsu already
differed from raku (67 of its 114 blocks were confirmed real mutsu bugs against
5 the name fit). Those findings now land in `output-mismatch` where they belong.
The honest comparison is against the *sum* of the old buckets:

| sweep | match | mismatch | crash | drift | nondet dropped |
|---|---:|---:|---:|---:|---:|
| 2026-09-06 | 2376 | 74 | 34 | 119 | — |
| 2026-09-07b | 2402 | 59 | 28 | 114 | — |
| **2026-09-09** | **2409** | **129** | **21** | *(retired)* | **59** |

So: `match` up again (2402 → 2409), crashes down (28 → 21), and the 59
`oracle-nondet` blocks are the noise the twice-run oracle gate now drops
instead of reporting — the noise floor, not findings. A rising `nondet` count
means the corpus has more unreproducible examples, not that mutsu got worse.

**Read a report, not this table, to pick work.** The survey at the bottom ranks
files by `mism + crash`; the per-file minimal repros are committed under
[doc-diff-sweep/reports/](doc-diff-sweep/reports/) (only signal files are kept,
captured output capped at 40 lines per section by the harness). Re-run the
sweep into `tmp/` when you need a truncated block in full.

**Always re-verify a finding directly before treating it as a real bug** — and
always re-sweep on current `main`, since a report goes stale as soon as a fix
lands.

## Triaged

### Resolved (will drop from the next sweep)
- `Language/operators.rakudoc:1795` — the baggy operators combined Mix weights as raw
  `f64`, so a `Rat` weight came back as `4.140000000000001`. Weights now combine under
  the numeric tower in one canonical place, which also fixed `(.)`, `Mix.total`, and the
  saturating weight renderer —
  [news](../news/2026-09/mix-weight-arithmetic-under-the-numeric-tower.md).
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
  **Over-claimed: the 2026-09-09 sweep still reports this line.** The fix landed
  on the `Mu.new` path only, and the doc's example reaches the payload through
  `self.bless(value => $str)` — `~S.new(value => "abc")` is `abc` in mutsu,
  `~S.bless(value => "abc")` is `S()`. Now filed as
  [#7759](https://github.com/tokuhirom/mutsu/issues/7759).
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

### Ticketed (open)

Confirmed-real findings with a filed issue, not yet fixed. When the issue is
resolved, write it up in `news/` (per `docs/issue-workflow.md`) and delete the
row here.

> The three rows filed from the 2026-09-07b sweep are **all fixed and closed**
> as of 2026-09-08: `Language/traps:858`'s lazy-Seq-into-a-slurpy half
> ([#7591](https://github.com/tokuhirom/mutsu/issues/7591), PR #7600),
> `Language/objects:1397`'s negative-range slice hang
> ([#7578](https://github.com/tokuhirom/mutsu/issues/7578), PR #7597), and
> `Type/Code:140`'s code-object-in-a-list rendering
> ([#7587](https://github.com/tokuhirom/mutsu/issues/7587), PR #7649). The
> harness ticket [#7590](https://github.com/tokuhirom/mutsu/issues/7590) is
> fixed too - this sweep is the first run with its output cap and oracle-nondet
> gate.

#### Filed from the 2026-09-09 sweep

Each was reduced and re-run against `raku` v2026.07 before filing.

| file:line | one-line summary | issue |
|---|---|---|
| `Language/math:185` | a FatRat addition **panics the process** (`attempt to add with overflow`, `add_sub.rs:168` narrows FatRat parts to `i64`) | [#7746](https://github.com/tokuhirom/mutsu/issues/7746) |
| `Language/structures:258` | a `but`-mixin on a Hash **silently loses the hash's contents** on the next store; the `%`-assign form corrupts the keys outright | [#7747](https://github.com/tokuhirom/mutsu/issues/7747) |
| `Type/Proxy:17` | `Proxy.new(FETCH/STORE)` bound to a name is not assignable — the documented type is unusable (`todo:deep`) | [#7748](https://github.com/tokuhirom/mutsu/issues/7748) |
| `Type/Pair:61` | adjacent colonpairs (`:a1:b2:c3`) stop parsing after the first one | [#7749](https://github.com/tokuhirom/mutsu/issues/7749) |
| `Language/signatures:35,262,274,329` | runtime binding failures carry the compile-time "will never work with declared signature" wrapper, and one throws the wrong `X::` type | [#7750](https://github.com/tokuhirom/mutsu/issues/7750) |
| `Type/PositionalBindFailover:34` | `does PositionalBindFailover` is ignored, so such an object cannot bind to `@a` | [#7751](https://github.com/tokuhirom/mutsu/issues/7751) |
| `Language/experimental:144` | `%%` / `%` by zero throw eagerly where raku returns a soft `Failure` (`div` already does the right thing) | [#7752](https://github.com/tokuhirom/mutsu/issues/7752) |
| `Language/traps:858` | a `...` sequence passed straight to a builtin listop collapses: `join` sees one element, `sum` sees none | [#7753](https://github.com/tokuhirom/mutsu/issues/7753) |
| `Language/numerics:595` | native integer increment does not wrap — `my int $x = 2**63-1; ++$x` promotes to a big `Int` | [#7754](https://github.com/tokuhirom/mutsu/issues/7754) |
| `Type/Junction:325`, `Type/List:351` | junctions do not flatten through infix `~`, and do not autothread out of a list into `join` | [#7755](https://github.com/tokuhirom/mutsu/issues/7755) |
| `Type/IO/CatHandle:162,595,688` | binary mode via `.encoding: Nil` ignored, `.words` merges across the handle boundary, empty-cat `.slurp` prints nothing instead of `Nil` | [#7756](https://github.com/tokuhirom/mutsu/issues/7756) |
| `Type/Metamodel/Mixins:18,63` | a parametric role with **named** parameters never matches (`role R[:$v]`) | [#7757](https://github.com/tokuhirom/mutsu/issues/7757) |
| `Type/Sub:78` | a sub-signature on a **named** parameter is ignored — every sub-parameter gets the whole array | [#7758](https://github.com/tokuhirom/mutsu/issues/7758) |
| `Language/objects:1067` | `.bless(value => …)` does not fill a `Str` subclass's payload (only `.new` does), so the instance stringifies as `S()` | [#7759](https://github.com/tokuhirom/mutsu/issues/7759) |

#### Triaged real, not yet filed (2026-09-09)

Re-run and confirmed during this sweep's triage but not filed one-by-one, so
the verification work is not lost. **File a ticket when you pick one up**, and
move its row above. The minimal repro for each is in the committed report under
[doc-diff-sweep/reports/](doc-diff-sweep/reports/).

| cluster | rows | shape |
|---|---|---|
| **Mixins lose the base value's identity** | `Language/objects:1457` (`(<a b> but R).^name` → `Array+{R}`, raku `List+{R}`), `Language/perl-func:2281`, `:2310` | the built-in payload is unreachable from the mixed/derived object |
| **Itemization depth** | `Type/Any:1307` (`.tree(1).flat.elems` → 2, raku 6), `Type/Any:311` (`».List.flat` keeps one level of nesting) | an extra level exactly one deep; `.tree` with no arg and `.tree(2)` are correct |
| **MOP metadata** | `Type/Code:195` (an auto-generated accessor `Method` has no `.line`), `Type/Code:166` (`&infix:<+>.file` → `Nil`), `Language/structures:458` (`ClassHOW.can("uc")` finds 1 candidate, raku 2), `Type/Metamodel/ConcreteRoleHOW:26` (`.^compose` missing), `Type/Metamodel/MethodContainer:15,40` | the metaobject exists but carries no source / inherited-method metadata |
| **Macros unimplemented** | `Language/experimental:78,93,104`, `Type/X/TypeCheck/Splice:30` | `use experimental :macros` — `quasi` does not parse at all |
| **Custom iterator protocol** | `Type/Iterator:277` (a class doing `Iterable`+`Iterator` binds as itself), `Type/Iterable:52`, `Type/Iterator:69` (`IterationEnd` in a list does not stop iteration), `:115` (`IterationEnd.raku` is `"IterationEnd"`), `:88` (`=:= IterationEnd` through a container) | see the deferred cluster below |
| **Grammar action side effects** | `Language/grammars:289` (`<.lit>` never fires the action method), `:387` (`"$a.[1]"` / `"$h.<k>"` emitted literally in interpolation) | the capturing `<lit>` form is correct |
| **Transcendental accuracy** | `Type/Cool:433` (`atanh(0.5)` → `...548`, raku `...549`), `:535` (`log10(1001)`), `:422` (`tanh(atanh(0.5))` → `0.49999999999999994`, raku `0.5000000000000001`) | last-ulp; mutsu evidently derives these rather than calling libm |
| **Standalone** | `Language/control:48` (a bare block before an infix must be a term — `{ ... } or die` dies), `Language/traps:63` (`%h is default(Nil)` stores `(Any)` for an explicit `Nil`), `Language/traps:1076` (`for "x" ~~ /(.)/ {...}` iterates nothing), `Type/List:417` (`(1..∞).List.gist` → `1..Inf`, raku `(...)`), `Type/Any:1549` (`.snitch(&dd)` → "Callable expected"), `Type/Format:58` (`Format.directives` missing), `Type/independent-routines:1429` (`append`/`push` with a Hash → "Unknown call"), `:473` (`.readchars` + `SeekFromCurrent` mixes bytes and characters), `:312` (`indir` + a lazy `gather` yields `()`), `Language/haskell-to-p6:475` (the reduction metaoperator over a user sub), `Language/py-nutshell:541`, `Language/perl-var:198` (`$*DISTRO` is a copy of `$*KERNEL`), `Language/subscripts:51` | |

**Environment noise, not findings.** `Language/variables:1719,1725,1737,1745,1756`
compare `$*DISTRO` / `$*VM` / `$*RAKU.compiler.version` against the reference
build, and `Type/independent-routines:148` needs an interactive REPL. They will
diverge on every run by construction. `Language/variables:868` is a
concurrency-interleaving example whose oracle happened to reproduce twice.

**Known harness false positive (not ticketed):** any block whose expected output
embeds an unordered-container iteration order, an object address, a `WHICH` id,
or a thread id. The twice-run oracle gate drops most of these (59 this sweep);
the survivors are ones raku happened to reproduce, e.g.
`Language/structures:108` (`<a b c d>.Hash.kv`) and `Type/List:695`
(`.Capture.keys`).

### Deferred / deep (tracked elsewhere — do not re-open as a shallow slice)
These root causes account for a large share of the survey's `mism`/`crash` and are
intentionally deferred; see PLAN.md §6 and the ADRs (the old §8.5 pointer was stale):
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
Everything in the survey below not listed above. The per-file minimal repros for the
2026-09-09 sweep are committed under [doc-diff-sweep/reports/](doc-diff-sweep/reports/)
(captured output truncated to 40 lines per section) — read those first; re-run
`scripts/doc-diff-sweep.sh` into `tmp/sweep/` only when you need a truncated block in
full or the tree has moved. Re-verify each block against `raku` before writing a fix.

## Survey — files with divergences (high-signal first)

`mism` = output-mismatch · `crash` = mutsu exited non-zero where raku succeeded ·
`nondet` = blocks dropped because the **oracle** disagreed with itself across two
runs (the noise floor, not findings). Regenerated from
[doc-diff-sweep/summary.txt](doc-diff-sweep/summary.txt) on every sweep.

| file (under raku-doc/doc/) | mism | crash | nondet |
|---|---:|---:|---:|
| Type/IO/CatHandle.rakudoc | 6 | 0 | 0 |
| Language/variables.rakudoc | 6 | 0 | 0 |
| Language/signatures.rakudoc | 5 | 0 | 1 |
| Language/objects.rakudoc | 5 | 0 | 0 |
| Type/Any.rakudoc | 4 | 1 | 4 |
| Type/independent-routines.rakudoc | 4 | 0 | 0 |
| Type/Junction.rakudoc | 4 | 0 | 0 |
| Type/Iterator.rakudoc | 4 | 0 | 0 |
| Type/IO/Spec/Win32.rakudoc | 4 | 0 | 0 |
| Type/Cool.rakudoc | 4 | 0 | 0 |
| Language/structures.rakudoc | 3 | 1 | 3 |
| Language/experimental.rakudoc | 0 | 4 | 0 |
| Language/traps.rakudoc | 3 | 0 | 1 |
| Type/List.rakudoc | 3 | 0 | 0 |
| Language/numerics.rakudoc | 3 | 0 | 0 |
| Language/list.rakudoc | 3 | 0 | 0 |
| Type/Code.rakudoc | 2 | 1 | 2 |
| Type/Map.rakudoc | 2 | 0 | 3 |
| Type/BagHash.rakudoc | 2 | 0 | 3 |
| Type/Hash.rakudoc | 2 | 0 | 2 |
| Language/typesystem.rakudoc | 2 | 0 | 2 |
| Type/Metamodel/MethodContainer.rakudoc | 2 | 0 | 1 |
| Type/Enumeration.rakudoc | 2 | 0 | 1 |
| Type/Backtrace.rakudoc | 2 | 0 | 1 |
| Language/subscripts.rakudoc | 2 | 0 | 1 |
| Type/Compiler.rakudoc | 2 | 0 | 0 |
| Type/CallFrame.rakudoc | 2 | 0 | 0 |
| Type/Attribute.rakudoc | 2 | 0 | 0 |
| Language/perl-var.rakudoc | 2 | 0 | 0 |
| Language/perl-func.rakudoc | 2 | 0 | 0 |
| Language/concurrency.rakudoc | 2 | 0 | 0 |
| Language/control.rakudoc | 1 | 1 | 1 |
| Language/grammars.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/Mixins.rakudoc | 0 | 2 | 0 |
| Type/Baggy.rakudoc | 1 | 0 | 2 |
| Type/IO/Spec/Unix.rakudoc | 1 | 0 | 1 |
| Type/IO/Handle.rakudoc | 1 | 0 | 1 |
| Type/Bag.rakudoc | 1 | 0 | 1 |
| Language/regexes.rakudoc | 1 | 0 | 1 |
| Language/py-nutshell.rakudoc | 1 | 0 | 1 |
| Language/contexts.rakudoc | 1 | 0 | 1 |
| Language/containers.rakudoc | 1 | 0 | 1 |
| Type/X/Str/Numeric.rakudoc | 1 | 0 | 0 |
| Type/X/Numeric/Real.rakudoc | 1 | 0 | 0 |
| Type/X/Numeric/DivideByZero.rakudoc | 1 | 0 | 0 |
| Type/X/Assignment/RO.rakudoc | 1 | 0 | 0 |
| Type/Whatever.rakudoc | 1 | 0 | 0 |
| Type/Thread.rakudoc | 1 | 0 | 0 |
| Type/Str.rakudoc | 1 | 0 | 0 |
| Type/Sequence.rakudoc | 1 | 0 | 0 |
| Type/Seq.rakudoc | 1 | 0 | 0 |
| Type/Routine.rakudoc | 1 | 0 | 0 |
| Type/Promise.rakudoc | 1 | 0 | 0 |
| Type/Positional.rakudoc | 1 | 0 | 0 |
| Type/Nil.rakudoc | 1 | 0 | 0 |
| Type/Metamodel/Primitives.rakudoc | 1 | 0 | 0 |
| Type/Lock/ConditionVariable.rakudoc | 1 | 0 | 0 |
| Type/Iterable.rakudoc | 1 | 0 | 0 |
| Type/Int.rakudoc | 1 | 0 | 0 |
| Type/IO/Path/Parts.rakudoc | 1 | 0 | 0 |
| Type/IO/Path.rakudoc | 1 | 0 | 0 |
| Type/ForeignCode.rakudoc | 1 | 0 | 0 |
| Type/Failure.rakudoc | 1 | 0 | 0 |
| Type/Exception.rakudoc | 1 | 0 | 0 |
| Type/CompUnit/Repository/Installation.rakudoc | 1 | 0 | 0 |
| Language/syntax.rakudoc | 1 | 0 | 0 |
| Language/perl-nutshell.rakudoc | 1 | 0 | 0 |
| Language/io.rakudoc | 1 | 0 | 0 |
| Language/functions.rakudoc | 1 | 0 | 0 |
| Language/classtut.rakudoc | 1 | 0 | 0 |
| Type/Sub.rakudoc | 0 | 1 | 1 |
| Type/Pair.rakudoc | 0 | 1 | 1 |
| Type/X/TypeCheck/Splice.rakudoc | 0 | 1 | 0 |
| Type/Proxy.rakudoc | 0 | 1 | 0 |
| Type/PositionalBindFailover.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/ConcreteRoleHOW.rakudoc | 0 | 1 | 0 |
| Type/Format.rakudoc | 0 | 1 | 0 |
| Language/optut.rakudoc | 0 | 1 | 0 |
| Language/math.rakudoc | 0 | 1 | 0 |
| Language/haskell-to-p6.rakudoc | 0 | 1 | 0 |
