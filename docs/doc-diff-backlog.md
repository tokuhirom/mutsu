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

**When a finding is confirmed real (not raku-drift, not a harness false positive —
see "Known harness false positive" below), file it as an issue immediately** on
`tokuhirom/mutsu`, labelled `todo:ticket` (or `todo:deep` for high-blast-radius
ones) per `docs/issue-workflow.md`, and add a row to
[Ticketed](#ticketed-open--linked-to-todo) below linking the doc location to that
issue. This is what keeps this backlog and the issue queue in sync — a finding
sitting only in a sweep report, or only in an issue with no cross-link, is easy to
lose track of.

The **raw output of the latest committed sweep** is checked in under
[doc-diff-sweep/](doc-diff-sweep/) — read a per-file report there to get the minimal
repros without re-running the sweep. Re-copy it (see that dir's `README.md`) whenever
you refresh the survey.

**Always re-verify a finding directly before treating it as a real bug.** The
harness oracle-gates on raku, but doc examples drift and the harness can only compare
`# OUTPUT:`-style blocks.

**Do NOT skip the `raku-drift` bucket.** It used to be described here as
"version skew, not mutsu bugs — lowest priority". That was measured wrong on
2026-09-07b: the bucket is only reachable *after* mutsu has already been found to
differ from raku, and 67 of its 114 blocks are confirmed real mutsu bugs against
5 that the name actually describes. See the ⚠ section under Corpus snapshot.

## Corpus snapshot

- **Date:** 2026-09-07b (full re-sweep) · debug `mutsu` at `dccfd1737` (main,
  through PR #7508) · `raku` v2026.07
- **444 files scanned · 60 have `mism`/`crash` signal** (plus 35 more that have
  only `drift` findings and therefore do **not** appear in the survey table —
  see the warning below)
- **match = 2402 · output-mismatch = 59 · mutsu-crash = 28 · raku-drift = 114**
- High-signal total (mismatch + crash) = **87**, down from **108** on 2026-09-06
  (mismatch 74 → 59, crash 34 → 28, signal files 66 → 60), **296** on
  2026-08-22 and **361** on 2026-07-22. `match` rose 2376 → 2402 over the last
  day: the blocks are not disappearing from the corpus, they are being answered
  correctly.

### ⚠ `raku-drift` is NOT a "not a mutsu bug" bucket — measured 2026-09-07b

**Read this before using the survey table to pick work.** The harness bucketing
(`scripts/doc-diff-harness.raku:72-93`) is:

```
if mutsu output == raku output   -> match
elsif mutsu exited non-zero      -> mutsu-error
else                             -> if raku output != the doc's `# OUTPUT:` -> raku-drift-from-doc
                                    else                                     -> output-mismatch
```

The `raku-drift` branch is **only reachable once mutsu already differs from
raku**. Every block in it is a mutsu-vs-oracle divergence. All 114 were
classified on 2026-09-07b (62 re-run directly):

| verdict | count | share |
|---|---|---|
| **REAL** — deterministic mutsu-vs-raku divergence | **67** | 59% |
| NONDET-only — the entire diff is a token no run reproduces | 33 | 29% |
| ENV-only | 9 | 8% |
| MUTSU-MATCHES-DOC — what the bucket name describes | 5 | 4% |

So the previous guidance here — "version skew, not mutsu bugs — lowest
priority" — was **deprioritizing 67 confirmed real divergences**, a larger pool
than the 87 the survey table ranks. The doc's `# OUTPUT:` annotation is a
*provenance* signal being used as a *priority* signal.

**Two structural consequences:**

1. **37% of the drift bucket (42 blocks) is pure harness noise**, present
   because the doc froze a token raku itself cannot reproduce twice: unordered
   iteration order (27 — `Set`/`Bag`/`Mix`/`*Hash`/`Map`/`Hash.kv`/enum
   `.keys`), object addresses and `WHICH` ids (13), a thread id, one racy
   example. Verified: five `raku -e 'say (bag <a b c>).kv.join(",")'` runs give
   five orders; a deterministic control is byte-identical 5/5. **Nine real
   mutsu bugs were hiding under that noise**, including
   [code-object-renders-as-nothing-inside-a-list](../todo/tickets/code-object-renders-as-nothing-inside-a-list.md)
   (`my &b = { $^a }; say (&b,)` → mutsu `()`, raku shows the block — a
   one-element list printing as empty).
2. **61 of the 114 drift findings, across 35 files, appear nowhere in this
   document**, because the survey table ranks by `mism + crash` and a
   drift-only file scores 0. Largest: `Language/signatures` (6),
   `Type/BagHash` (5), `Type/MixHash` (3), `Type/Enumeration` (3),
   `Language/numerics` (3), `Language/list` (3).

Both are tracked in
[doc-diff-harness-has-no-output-cap-or-nondeterminism-gate](../todo/tickets/doc-diff-harness-has-no-output-cap-or-nondeterminism-gate.md).
The robust fix for the noise is one line of policy — **run the oracle twice and
drop blocks whose raku output is not reproducible** — not a pattern list.

### ⚠ The harness does not cap captured output

A single doc example can produce a multi-megabyte report, and the refresh recipe
in [doc-diff-sweep/README.md](doc-diff-sweep/README.md) commits it verbatim.
On this sweep, `Type/IO/Path.rakudoc:509` (a `sub MAIN` that recursively
`.dir`-walks the working directory, so it enumerated `.git/` and `tmp/`)
produced a **131 492-line, 8.4 MB** report, and `Language/ipc.rakudoc:34`
captured a full git log at 1.6 MB. The committed copy under
[doc-diff-sweep/](doc-diff-sweep/) was truncated to 40 lines per captured
section (11 MB → 412 KB); a `... [N more lines truncated]` marker shows where.
**Re-run the sweep into `tmp/` to see any truncated block in full.**

### Previous snapshot (2026-09-06, for the delta above)

- 444 files scanned · 66 have signal · debug `mutsu` at `5885101fc` + PR #7380
- match = 2376 · output-mismatch = 74 · mutsu-crash = 34 · raku-drift = 119
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

Confirmed-real findings that have a filed issue but are not yet fixed. When the
issue is resolved, write it up in `news/` (per `docs/issue-workflow.md`) and
delete the row here.

> **The 2026-08-22 rounds' table was removed on 2026-09-07b: every one of its 25
> ticket links was dead** — all 25 tickets had been fixed and their files deleted
> (spot-checked against the deleting commits: `Fix named Pair handling in Hash
> push`, `make return-rw call results assignable`, `preserve Win32 path
> separators`, `preserve captures across regex alternation`, `implement repl
> routine`, …). The rows were left in place long after the work landed, which is
> exactly the drift this backlog is prone to. Read `git log -- docs/doc-diff-backlog.md`
> for the historical table.

#### Filed from the 2026-09-07b sweep

| file:line | one-line summary | ticket |
|---|---|---|
| `Language/traps.rakudoc:858` | a lazy Seq (`.map`/`.grep`/`...`/`gather`) passed to a user `*@a` slurpy arrives **empty**; the `.grep` face is a **regression bisected to PR #7501** | [lazy-seq-argument-vanishes-into-a-user-slurpy.md](../todo/tickets/lazy-seq-argument-vanishes-into-a-user-slurpy.md) |
| `Language/objects.rakudoc:1397` | `@a[0 .. $n]` **hangs forever** when `$n` holds a negative value (raku: `()`) | [array-slice-with-a-runtime-empty-reversed-range-hangs.md](../todo/tickets/array-slice-with-a-runtime-empty-reversed-range-hangs.md) |
| `Type/Code.rakudoc:140` | a `Block` inside a list renders as the empty string, so a one-element list prints as `()` | [code-object-renders-as-nothing-inside-a-list.md](../todo/tickets/code-object-renders-as-nothing-inside-a-list.md) |
| *(harness itself)* | no output cap, and `raku-drift` used as a priority bucket when it only ever contains mutsu-vs-oracle divergences | [doc-diff-harness-has-no-output-cap-or-nondeterminism-gate.md](../todo/tickets/doc-diff-harness-has-no-output-cap-or-nondeterminism-gate.md) |

#### Triaged real, not yet filed (2026-09-07b)

Every row below was **re-run against `raku` v2026.07 and reduced** during the
2026-09-07b triage; they are confirmed-real, not candidates. They are recorded
here rather than filed one-by-one so the sweep's verification work is not lost.
The minimal repro for each is in the committed report under
[doc-diff-sweep/reports/](doc-diff-sweep/reports/). **File a ticket when you pick
one up**, and move its row to the table above.

Grouped by the cluster the triage identified, since several share a mechanism:

| cluster | rows | shape |
|---|---|---|
| **Role mixins lose the base value's identity** | `Language/objects:1457` (`(<a b> but R).^name` → `Array+{R}`, raku `List+{R}`), `Language/perl-func:2281` (`join` over `@o but R` → `MIX`, raku `3>2>1`), `Language/perl-func:2310` (a mixin-supplied `sink` is never dispatched), `Language/objects:1067` (`class S is Str {}; ~S.bless(value=>"abc")` → `S()`, raku `abc`) | the built-in payload is unreachable from the mixed/derived object |
| **Role parameterization and `does` adverbs** | `Type/Metamodel/Mixins:18` (`role R[:$v]` binds the whole `Pair`), `:63` (`$c does R :value("hi")` → "Useless use … in sink context") | the argument passed at mixin time is not routed to the role's parameter/attribute |
| **Grammar non-capturing subrules** | `Language/grammars:289` (`<.lit>` never fires the action method, nor a grammar method of that name) | `<.name>` matches but its side effects are skipped; the capturing `<lit>` form is correct |
| **Itemization depth** | `Type/Any:311` (`my $x = [(4,5),6,7]; $x.List.raku` → `($(4, 5), 6, 7)`), `Type/Any:1307` (`.tree(1)` itemizes one level too deep) | an extra `$(...)` exactly one level down; `.tree` with no arg and `.tree(2)` are correct |
| **Soft-failure numerics** | `Language/experimental:144` (`my $f = 6 %% 0` throws; raku returns a `Failure`), and the `%` twin | worth sweeping the other divisors while in there |
| **IO handle plumbing** | `Type/IO/Handle:169` (bare `get` parses as the bareword string `"get"`), `:959` (`$PROCESS::OUT = open(...)` does not redirect; `$*OUT =:= $PROCESS::OUT` is `False`), `Type/independent-routines:473` (`.tell` after `readchars` on UTF-8 is 6 vs raku's 7) | |
| **`IO::Spec` / filetest tables** | `Type/IO/Spec/Win32:190` and `:251` (`split`/`splitpath` edge cases, pure string work), `Type/IO/Path:561` (`"/".IO ~~ :rw` → `True`; mutsu's own `.rw` says `False`) | small table-completion jobs |
| **Introspection / MOP surface** | `Type/Code:195` (an auto-generated accessor `Method` has no `.line`), `Language/structures:458` (`Metamodel::ClassHOW.^can("uc")` → 0), `Type/Code:166` (`&infix:<+>.file` → `Nil`) | the metaobject exists but carries no source/inherited-method metadata |
| **Standalone** | `Language/grammars:387` (`"$a.[1]"` / `"$h.<k>"` emitted literally in interpolation), `Language/control:48` (a bare block before an infix must be a **term**, not a call — `{ ... } or die` dies), `Type/Iterator:115` (`IterationEnd.^name` → `Str`), `:88` (`$c =:= C` → `True` through a scalar container), `Language/subscripts:51` (object-keyed `Mix`/`Set` lookup always misses — but see `which-keyed-quanthash-not-worth-campaign`), `Language/py-nutshell:541` (`-> (\i, \j)` binds `i` to the imaginary unit), `Language/perl-var:198` (`$*DISTRO` is a copy of `$*KERNEL`), `Language/structures:233` (`$Undeclared::thing` → `Nil`, raku `(Any)`), `Type/Any:1549` (`&dd.WHAT` → `Nil`), `Language/experimental:78/93/104` (macros unimplemented), `Language/traps:1076` (`for "x" ~~ /(.)/ {...}` iterates nothing) | |

Plus the **67 REAL findings inside the `raku-drift` bucket** described in the
Corpus snapshot above — notably native-`int` `++` not wrapping, `...` sequences
falling from `Int` to `Num` past 2⁶³, junctions not autothreading in list-element
or subscript position, `<( )>` capture markers ignored under `:g`/`comb`, and
binding failures reporting a compile-time "will never work with declared
signature" message where raku names the parameter and constraint.

**Known harness false positive (not ticketed):** any block whose expected output
embeds an unordered-container iteration order, an object address, a `WHICH` id,
or a thread id. See the Corpus snapshot — this is 37% of the drift bucket and the
fix is the oracle-twice gate, not a pattern list.
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
2026-09-07b sweep are committed under [doc-diff-sweep/reports/](doc-diff-sweep/reports/)
(captured output truncated to 40 lines per section) — read those first; re-run
`scripts/doc-diff-sweep.sh` into `tmp/sweep/` only when you need a truncated block in
full or the tree has moved. Re-verify each block against `raku` before writing a fix.

## Survey — files with divergences (high-signal first)

`mism` = output-mismatch · `crash` = mutsu exited non-zero where raku succeeded ·
`drift` = raku-drift-from-doc — **not low priority**: 59% of that bucket is
confirmed-real mutsu divergence (see the ⚠ section under Corpus snapshot).

**This table under-reports.** It ranks by `mism + crash`, so a file whose only
findings are `drift` scores 0 and does not appear at all — that is **61 findings
across 35 files** on this sweep, including `Language/signatures` (6) and
`Type/BagHash` (5). Read `doc-diff-sweep/summary.txt` and `progress.txt` for the
full picture until the harness ticket lands.
| file (under raku-doc/doc/) | mism | crash | drift |
|---|---:|---:|---:|
| Type/Any.rakudoc | 5 | 1 | 4 |
| Language/experimental.rakudoc | 0 | 4 | 0 |
| Language/objects.rakudoc | 2 | 1 | 2 |
| Type/IO/Path.rakudoc | 2 | 1 | 1 |
| Type/Iterator.rakudoc | 2 | 0 | 2 |
| Type/IO/Spec/Win32.rakudoc | 2 | 0 | 2 |
| Type/independent-routines.rakudoc | 2 | 0 | 2 |
| Language/traps.rakudoc | 2 | 0 | 2 |
| Type/IO/Handle.rakudoc | 2 | 0 | 1 |
| Language/subscripts.rakudoc | 2 | 0 | 1 |
| Type/Lock/Async.rakudoc | 2 | 0 | 0 |
| Language/py-nutshell.rakudoc | 2 | 0 | 0 |
| Language/perl-var.rakudoc | 2 | 0 | 0 |
| Language/perl-func.rakudoc | 2 | 0 | 0 |
| Type/Code.rakudoc | 1 | 1 | 3 |
| Language/structures.rakudoc | 1 | 1 | 3 |
| Language/control.rakudoc | 1 | 1 | 1 |
| Language/grammars.rakudoc | 1 | 1 | 0 |
| Type/Metamodel/Mixins.rakudoc | 0 | 2 | 0 |
| Language/variables.rakudoc | 1 | 0 | 4 |
| Type/Junction.rakudoc | 1 | 0 | 3 |
| Type/Metamodel/MethodContainer.rakudoc | 1 | 0 | 2 |
| Type/Map.rakudoc | 1 | 0 | 2 |
| Type/Backtrace.rakudoc | 1 | 0 | 2 |
| Language/typesystem.rakudoc | 1 | 0 | 2 |
| Type/Sub.rakudoc | 1 | 0 | 1 |
| Type/Hash.rakudoc | 1 | 0 | 1 |
| Type/CallFrame.rakudoc | 1 | 0 | 1 |
| Type/Baggy.rakudoc | 1 | 0 | 1 |
| Type/Attribute.rakudoc | 1 | 0 | 1 |
| Language/syntax.rakudoc | 1 | 0 | 1 |
| Language/functions.rakudoc | 1 | 0 | 1 |
| Language/contexts.rakudoc | 1 | 0 | 1 |
| Language/concurrency.rakudoc | 1 | 0 | 1 |
| Type/Sequence.rakudoc | 1 | 0 | 0 |
| Type/Lock/ConditionVariable.rakudoc | 1 | 0 | 0 |
| Type/Iterable.rakudoc | 1 | 0 | 0 |
| Type/IO/Path/Parts.rakudoc | 1 | 0 | 0 |
| Type/Failure.rakudoc | 1 | 0 | 0 |
| Type/Exception.rakudoc | 1 | 0 | 0 |
| Language/regexes.rakudoc | 1 | 0 | 0 |
| Language/nativetypes.rakudoc | 1 | 0 | 0 |
| Language/js-nutshell.rakudoc | 1 | 0 | 0 |
| Language/hashmap.rakudoc | 1 | 0 | 0 |
| Language/classtut.rakudoc | 1 | 0 | 0 |
| Type/Cool.rakudoc | 0 | 1 | 5 |
| Type/X/TypeCheck/Splice.rakudoc | 0 | 1 | 0 |
| Type/Proxy.rakudoc | 0 | 1 | 0 |
| Type/PositionalBindFailover.rakudoc | 0 | 1 | 0 |
| Type/Pair.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/Stashing.rakudoc | 0 | 1 | 0 |
| Type/Metamodel/ConcreteRoleHOW.rakudoc | 0 | 1 | 0 |
| Type/IO/Notification/Change.rakudoc | 0 | 1 | 0 |
| Type/Format.rakudoc | 0 | 1 | 0 |
| Language/optut.rakudoc | 0 | 1 | 0 |
| Language/nativecall.rakudoc | 0 | 1 | 0 |
| Language/math.rakudoc | 0 | 1 | 0 |
| Language/ipc.rakudoc | 0 | 1 | 0 |
| Language/haskell-to-p6.rakudoc | 0 | 1 | 0 |
| Language/faq.rakudoc | 0 | 1 | 0 |
