# Roast Blockers by Feature

Failing roast tests grouped by the missing feature that blocks them. Each entry
carries a **fix difficulty** estimate:

- **Easy** — localized, clear root cause, 1-2 failing subtests, no architectural blocker.
- **Medium** — a self-contained feature to implement; bounded scope.
- **Hard** — blocked on a deep architectural limitation (first-class container
  identity, real lazy infinite sequences, threading primitives, RakuAST,
  object-hash `%{Mu}` keys) or a large pile of disparate sub-features.

Last refreshed: **2026-06-15** — reorganized into a *pickup-order* taxonomy keyed
on **conflict with the main track** (see below), pruned completed entries
(`S17-supply/categorize.t` whitelisted #3071, plus the already-DONE
`misc2.t`/`write-int.t`/`io-path-cygwin.t`/`words.t`/`alias.t`), and consolidated
all "reference rakudo also fails / roast test bug" files into a single
**§F Unpassable** section so they stop polluting the work queue.

## Status — final stretch

Essentially every remaining file is gated on one of a small set of **Hard
architectural blockers**; there are no more cheap whitelist wins to cherry-pick.
The blockers, in rough order of how many files they gate:

- **First-class container identity** (scalar/element/attribute `Scalar`
  containers; `Value` carries a bare value with no per-slot container) — gates
  binding/`:=`-alias, `is rw`/take-rw, `.VAR`, typed-hash default survival,
  closure capture-by-container, object-hash `%{Mu}` keys, Arc-pointer typed-array
  flaky. This is PLAN.md's "🟣 第2優先" and the single largest lever.
- **Real lazy infinite sequences** — `@a[0..*]`, `X::Cannot::Lazy` on same-type
  lazy iterables. Closure-based infinite sequences (`1, 1, *+* ... *`) now
  generate on demand for scalar/`constant`/`.lazy` forms (`$s[100]`, `fib[100]`);
  the remaining gap is **lazy arrays** — `my @a = 1..*` still materializes to a
  finite prefix because Array mutation ops (clone/unshift/shift/:delete/elem-assign)
  have no reify-on-demand path yet.
- **Threading / concurrency primitives** — Semaphore, nonblocking await, lock
  contention, remaining Supply combinators (all of S17).
- **RakuAST** — `Formatter.AST`, anything needing a reflectable AST.
- **A long tail of distinct compile-time `X::` exception types** thrown at the
  exact right place (the misc.t-style campaign).

The highest-leverage work is the **architectural tracks in PLAN.md**
(Interpreter-execution-path removal + first-class containers). The per-file
analysis below is the *map*: when one of those blockers lands, it says which
files unblock.

## ▶ Pickup order — least main-track conflict first

The **main track** (PLAN.md) is **CP-1 env-loan / Interpreter-removal** plus the
**🟣 first-class-container** work (Track B). Both churn env ownership, dispatch,
and the `Value`/container representation. To run a roast session *in parallel*
without colliding, pick from the **top**:

1. **§A Isolated subsystems** (regex engine / unicode / IO / Buf / format / shaped
   arrays) — **zero** main-track conflict; live entirely in `runtime/regex*`,
   `runtime/io*`, `builtins/unicode.rs`, etc.
2. **§B Additive typed exceptions & module plumbing** — **low** conflict; new
   `X::` types + throw sites + parse-time checks, off the VM hot path.
3. **§C Self-contained semantic / builtin / parser-compiler fixes** — **low-to-medium**
   conflict; a few touch `vm_control_ops`/parser, so coordinate timing with CP-1.
4. **§D Blocked on the main track** — **do NOT pick**; these only move when the
   container-identity / lazy / dispatch tracks land.
5. **§E Threading / Concurrency** — Hard, separate axis (S17).
6. **§F Unpassable as written** — reference rakudo also fails or the roast test is
   buggy; do NOT target a whitelist.

---

## §A — Isolated subsystems (ZERO main-track conflict)

### Regex engine (`runtime/regex*.rs`)

- **S05-substitution/subst.t** — **Medium**. The remaining failures are
  regex-internal: `X::Syntax::Regex::NullRegex` (empty `s///`), `:samecase`/
  `:samemark` application in `.subst`, non-constant `:i`. Sequence these AFTER any
  open regex PR. (NOT whitelistable as a whole — rakudo itself fails tests 157,
  170.) See S05.md.
- **S05-match/capturing-contexts.t** — **Medium (multi-feature)**. 56/64. 8
  *disparate* failures: binding `$/` (`my $/ := 42`), index-stable positional slots
  so `(y)?`→Nil and `(z)*`→empty-list coexist, `%%` separator backtracking vs an
  outer anchor, capture markers `<(`/`)>`. No single-PR whitelist; each is a
  mini-feature.
- **S05-capture/array-alias.t** — **Hard**. 30/37 fail then aborts: named/sequential
  array captures (`@<foo>=...`) largely unimplemented.
- **S05-capture/hash.t** — **Hard**. 30/99 fail then aborts at line 134:
  package/hash captures (`%<foo>=...`) unimplemented.
- **S05-metasyntax/longest-alternative.t** — **Hard**. Timeout — LTM (longest-token
  matching) over many alternatives is not implemented efficiently.

### Unicode / Collation (`builtins/unicode.rs`)

- **S32-str/CollationTest_NON_IGNORABLE-3.t** — **Hard**, 1367/1369. 2 noncharacter
  cases (U+FFF0/U+FFFE, U+FFFF/U+1FFFE): ICU4X treats noncharacters as
  primary-ignorable; UCA-17/MoarVM assign implicit primary weights by codepoint.
  Needs sort-key-level weight injection (`icu_collator::write_sort_key_to`) or a
  partial UCA reimplementation. 2-test payoff.
  - *(DONE: S15-nfg/GraphemeBreakTest-3.t whitelisted #3294 — the 9 GB9c/GB11
    failures were the `unicode-segmentation` 1.12.0 GCB tables predating Unicode
    17.0; bumping the crate to 1.13.3 fixed all of them.)*

### IO (`runtime/io*`, IO builtins)

- **S32-io/io-handle.t** — **Hard**, 27/30. Remaining 3 (23 `.print-nl` reuse +
  `.nl-out=`, 29 `.WRITE`, 30 `.EOF/.WRITE`) all require a **user-subclassable
  IO::Handle with polymorphic READ/WRITE/EOF** so high-level read/write dispatch
  into user-overridden `method READ/WRITE/EOF`. Substantial feature.
- **S32-io/io-cathandle.t** — **Hard**. IO::CatHandle not implemented (note: rakudo
  itself fails test 31 "Cannot .elems a lazy list", so the file may be unpassable —
  verify before targeting a whitelist).
- **S32-io/io-path.t** — **Medium per-fix, low-ROI overall**. 6 independent
  failures. Test 31 `.gist` depends on a **Rakudo internal caching quirk** (Win32
  `.gist` renders the backslash form only *after* `.absolute` has been called) — do
  NOT replicate. The other 5 are doable but the file can't whitelist while 31 needs
  the quirk: `.parts` Win32 split (33), X::Assignment::RO on `.SPEC=`/`.CWD=` +
  `temp` (34/35), `.path` indir-independence (36), `.Numeric` Cool chain (37 — most
  tractable: IO::Path is Cool, numify via `.Str`).
- **S32-io/lock.t** — **Hard**. `.lock`/`.unlock` throw X::Method::NotFound; needs
  **fcntl POSIX record locks** (`F_SETLK`/`F_SETLKW`, `F_RDLCK`/`F_WRLCK`), NOT
  `flock(2)` (rakudo rejects an exclusive lock on a read-only fd via fcntl `EBADF`).
  Also cross-process blocking via subprocess `is_run`, IO::CatHandle, fd-reuse
  (`native-descriptor`). Timing-sensitive.

### Native shaped typed arrays

Non-shaped native typed arrays all pass and are whitelisted. Only the *shaped*
ones (`array[T].new(:shape(n))`) remain — **Hard**, needs real fixed-dimension
semantics (in-place `.map(* *= 2)`, `@a[*-1,*-2]` slices, `.raku` `:shape(...)`,
`@a = ()` resets to fixed-size defaults, `.grep`/`.values`/`.pairs` stale values).
~33/101 fail before abort.

- S09-typed-arrays/native-shape1-int.t / -num.t / -str.t

### Format / RakuAST

- **S32-str/format.t** — **Hard (blocked on RakuAST)**. 26/49 reachable pass; the
  Format class works but the file aborts at test 27 because `Formatter.AST` must
  return a `RakuAST::Node` and mutsu has no RakuAST. Local: t/format-class.t.

### Pod

- **S26-documentation/12-non-breaking-space.t** — **Hard (compile-time BEGIN
  hoisting)**. Subtest 2 plans `$nbchar-count + 1` but `@nbchars` is populated by a
  `BEGIN {}` at the *end* of the file; rakudo runs the later BEGIN at compile time
  (count 4), mutsu runs it at its textual position (count 0). Needs BEGIN-phaser
  hoisting.

---

## §B — Additive typed exceptions & module plumbing (LOW conflict)

New `X::` types + throw sites + parse-time checks; lives in exception/parser-error
code and module handlers, not the VM hot path. **Caveat:** most of these are NOT
whitelistable as whole files — their real value is the **reusable typed
exceptions** that carry across files.

- **S32-exceptions/misc.t** — **Hard**, 42/157. The big campaign (same style as the
  finished misc2.t): compile-time undeclared-symbol checking for non-EVAL programs,
  `X::NotParametric`, `X::Syntax::Extension::SpecialForm`, `X::Redeclaration` of
  subs/methods, `X::Bind`, sink-context "Useless use" warnings, ~30 one-off types.
  Highest-impact single file in this section. See S32.md.
- **S10-packages/basic.t** — **Medium part**, 18/59. Compile-time undeclared-symbol
  checking ("reference to class/role before definition dies") + X::Redeclaration of
  subs in a class. Overlaps misc.t.
- **S02-types/baghash.t** — **Medium**. 7 fail then aborts at 270/344
  (X::TypeCheck::Binding on BagHash iterator/coercion).
- **S02-types/mixhash.t** — **Medium**. 4 fail then aborts at 216/295
  (X::Str::Numeric on Mix coercion edge cases).

---

## §C — Self-contained semantic / builtin / parser-compiler fixes (LOW-MEDIUM conflict)

Touch specific builtins / method handlers / coercion / parser / `vm_control_ops`,
not env or dispatch core. A handful brush the VM control path, so coordinate
timing with CP-1 (they don't conflict in *content*, only in churning the same
files occasionally).

**Builtin / type / coercion (low conflict):**

- **S02-types/generics.t** — **Medium**. Nominalizable generic type.
- **S02-types/bag.t** — 254/255. Test 215 (BigInt bag weights) FIXED: `BagData.counts`
  is now `HashMap<String,BigInt>` (baghash.t whitelisted 344/344 by the same change).
  REMAINING test 252: Bag-union must preserve a `my class Foo is Bag` subclass type
  (Bag is a `Value`, not a subclassable Instance) — orthogonal, not BigInt-related.
- **S32-list/skip.t** — **Medium**. Plan mismatch (planned 55, ran 206 — loops more
  than planned; subtest counting/laziness off); 29 fail.
- **S03-operators/inplace.t** — **Medium**, 6/38 (from test 318 "constants"): `.=`
  in-place metaop on class instantiation / readonly constants.
- **S12-introspection/walk.t** — **Hard but self-contained MOP**. Needs `.WALK` +
  `WalkList` with all MRO orderings, submethod walking, lazy batch invocation,
  quiet-mode Failure capture.
- **S02-literals/allomorphic.t** — **Medium/Hard**, 1/119 (test 107). NOT ACCEPTS
  (that works) but same-named lexical-class redeclaration across two `gather`
  blocks: mutsu keys classes by name in one global map, so the second clobbers the
  first. Needs per-decl class identity (mirror `role_id`/`role_candidates`).

**Parser / compiler (medium — touches parser/compiler files):**

- **S04-statements/for.t** — **Medium**, 22/111. No exception on bad loop-var
  binding (`for 1,2 -> $a, $b, $c`) + topic-aliasing edge cases (VM control ops).
- **S06-signature/slurpy-params.t** — **Medium (multi-feature)**. Aborts at test 43:
  `+@`/`+foo` single-argument rule over ranges/lists + a Junction `*@a` slurpy that
  must not autothread (34/35).
- **S04-declarations/my-6e.t** — **Medium**. EVAL scope visibility (EVAL'd code must
  see the enclosing lexical scope).
- **Parser operators** — `ff`/`fff` flipflop, `==>`/`<==` feed precedence, hyper
  assignment, generalized negation meta (parser + compiler).
- **S03-operators/assign.t** — **Hard**, 35/193 then aborts: assignment used as a
  function (`&infix:<=>`), list-assignment in non-trivial contexts.
- **S03-metaops/hyper.t** — **Hard**. Timeout — `>>op<<` with assignment forms.
- **S32-array/adverbs.t** — **Hard**, 283/606. `X::Adverb` is implemented but the
  parser stops mid-file; needs adverb parsing on more subscript forms.

---

## §D — Blocked on the main track — do NOT pick until PLAN.md tracks land

Facets of first-class container identity, closure capture, the dual store, real
lazy infinite sequences, or object-hash `%{Mu}`. They only move when PLAN.md's
Interpreter-removal / first-class-container tracks advance.

**Container identity / closure capture** (the dominant cluster):

- **S02-types/capture.t** — 2/33 then aborts (planned 46). Tests 28-29: `$c[0]++` /
  `$c<a>++` on a `\($a)` Capture must write *through* to the original scalar
  container. Later abort needs X::Cannot::Lazy from Capture lazy ops.
- **S02-names-vars/variables-and-packages.t** — 16/39. Dominant: closure lexical
  capture by *container* (`{ my $x=100; $f={$x} }; my $x=999; $f()` wrongly reads
  999). Independent extras: `&OUR::grtz()` (32-34), X::Redeclaration::Outer (37-38),
  `$OUTER::_` topic (39), named sub closing over a later `my` (24-31).
- **S04-statements/gather.t** — 38/39; only failure is test 38 take-rw reference
  identity `@neighbors[1][1][0] =:= @spot[0][0]` (per-element Scalar containers).
- **S14-traits/attributes.t** — 4/8. Tests 5-8 need `$a.container.VAR does
  Role($arg)` (per-attribute container template). Local: t/attribute-trait-mod.t.
- **S12-methods/accessors.t** — 4/11. Contextualizing (rw) accessors return empty
  instead of the container value.
- **S03-binding/attributes.t** — 1/13 then aborts (test 13): `does`-mixing a role
  into a *bound* private scalar attribute's container.
- **S03-binding/nested.t** — **Medium**, 18/43. Binding to nested elements
  (`$a := @b[0]`) doesn't alias (off-by-one: expected 44, got 43).
- **S12-subset/subtypes.t** — 90/92 run, 13 fail. Dominant: closure writeback from a
  block/Whatever predicate stored in a `&`-var (write to captured `$wanted` lost on
  2nd call). Independent: `fail()` in a subset predicate (25), Junction-of-types in
  `where` (87), read-only topic (34), `where &var` on slurpy (85). 91-92 also fail
  in rakudo.
- **S04-blocks-and-statements/temp.t** — **Medium**, 30/37. `temp` restoration vs
  hash/array element containers.
- **S06-advanced/wrap.t** — **Hard**, 12/70 then aborts (planned 90): `.wrap`
  lexical visibility (wrapper closure can't see wrapped routine's lexicals).
- **S02-types/whatever.t** — **Hard**. 33 distinct WhateverCode features (dummy `*`
  assignment, `&infix:<+>(*,42)`, X+/Z+ metaop currying, `*++`, rw params, container
  preservation, compile-time WhateverCode, regex whatever curry) — no single root.
- **S02-types/pair.t** — **Hard**, 3/180. Tests 128/139 need `$pair.value` to alias
  the original variable's container; only test 171 (typed-assign throw) is
  independent (could move to §C if split out).
- **S32-hash/perl.t** — **Medium**, 12/55. Hash-in-Scalar vs deconted-Hash
  `.perl`/`.raku` differ (`(Str(Any),Mu)` typed-hash perlify + decont).
- **S32-hash/adverbs.t** — *big win landed, 823→1069/1128* (zen-slice adverbs,
  `X::Adverb`, `%h.name`/`@a.name`, Range-key slices). Remaining 59: typed-hash
  value-type default surviving variable rebinding (first-class container identity).
- **S32-array/splice.t** — **Hard (container identity, Phase 2)**, 136/381. Dominant:
  typed-array container identity through a multi-var `for` loop (`for @testing ->
  @a, $T`): mutsu compiles `@a` binding to assign-coerce (de-itemizes/reallocates,
  drops pointer-keyed type metadata); Raku binds `@a` as a true typed-element alias.
  *Partially addressed* #2898 (in-place mutators keep the typed container); the
  loop-alias step remains.
- **S02-names/is_default.t** — 141/146 (rakudo 2022.12 fails to compile it at line
  527, so unpassable anyway). Real failures: `is default(...)` on **hashes** rebound
  via a `for` signature — container-keyed default lost across copy/rebind.

**Object-hash `%{Mu}`** (non-Str-keyed hashes):

- **S32-list/classify.t** — 39/40 under FUDGE (tests 28-30 `(:@even,:@odd) := classify`
  are `#?rakudo skip`ped); only test 40 "classify works with Junctions" blocks the
  whitelist. **Subtle/implementation-dependent junction semantics** (verified
  2026-06-16): classify with a Junction-returning mapper stores the *junction itself*
  as the key (`$m.keys` = `any(True,False)` etc. — SAME in mutsu and rakudo). The gap
  is the *retrieve*: rakudo's `$m{ any(False,False) }` does NOT autothread — it returns
  the flat list `(cbd xyz)` by matching the stored junction key directly, whereas mutsu
  autothreads the subscript junction into per-eigenstate lookups → `any((Any),(Any))`.
  (Plain-hash junction subscripts DO autothread in both; classify result hashes are
  special.) Not a clean lever — defer until junction-key hash identity is modeled.
- **S09-hashes/objecthash.t** — **DONE**, whitelisted 62/62 (PR #3159, 2026-06-16).
- **S02-types/set.t** — last 2 failures ("coercion of object Hash to Set 1/2",
  `:{ }.Set`) need object-hash semantics. (Test 226 typed-hash bind type-check is
  already FIXED.)

**Real lazy infinite sequences:**

- **S03-operators/eqv.t** — 63/64. "Setty eqv Setty" FIXED (eqv now distinguishes
  Set/SetHash, Bag/BagHash, Mix/MixHash; set operators preserve first-operand
  mutability — PR set-op-mutability). Remaining: "Throws/lives in lazy cases" needs
  X::Cannot::Lazy for same-type lazy iterables (blocked: mutsu materializes `1…∞`,
  `.List`/`.Array` of an infinite Seq lose laziness — same root as lazy-arrays).
  (12/17/36/37 also fail in rakudo.)
- **S09-subscript/slice.t** — **Hard**, 9/39 then aborts at line 310: slices with
  infinite sequences (`@a[0..*]`).
- **S04-declarations/constant.t** — **Medium**. Lazy-seq `fib[100]` (46) now PASSES:
  closure-based infinite sequences (`0, 1, *+* ... *`) generate elements on demand
  via `LazyList.closure_seq` (re-invokes the generator over the growing history).
  Remaining blocker: `G::c` qualified name via constant-aliased enum (44) — a
  dispatch issue, NOT laziness. NOT whitelistable until that lands.

**Dispatch / MOP / EVAL-in-class:**

- **S06-advanced/lexical-subs.t** — **Medium**, 5/13 then aborts. X::Undeclared::Symbols
  for forward-referenced lexical `my sub`, plus `&foo` parameter precedence (**VM
  call cache** — `pos_light_call_cache`/`light_call_cache` are name-keyed and ignore
  a lexical `&name`) and inner-block lexical-sub leak. Partly §B, partly dispatch.
- **S12-methods/qualified.t** — **Hard (MOP)**, 6/7. The remaining subtest throws on
  parameterized roles (`$?ROLE`/`$?CLASS`, `R1[::T]` qualified dispatch, role-private
  classes).
- **S12-attributes/class.t** — **Hard**. X::Method::NotFound + EVAL inside a class
  body (depends on the EVAL-in-class blocker).

**Multi-dimensional slicing (subscript-as-lvalue):**

- **S32-array/multislice-6e.t** — **Medium**. Aborts at test 5 (planned 812):
  general `@a[0;0;0] = 999` lvalue path returns Nil / doesn't write back.
- **S32-hash/multislice-6e.t** — **Medium**. Same gap on hashes (test 5, planned 549).

---

## §E — Threading / Concurrency / Async — Hard, separate axis

S17 tests that still hang or fail need real threading primitives (Semaphore,
nonblocking await, lock contention) and the remaining Supply combinators. Basic
Promise/start/scheduler/channel work and are whitelisted; what's left is the
genuinely concurrent surface, hard to make deterministic.

**Timeout (hang):**
- S17-lowlevel/cas-int.t (partial then hangs)
- S17-lowlevel/semaphore.t (Semaphore not implemented)
- S17-promise/start.t (partial then hangs)
- S17-supply/syntax.t (partial then hangs — 2000-react stress; flaky tail, do NOT whitelist)

**Fail:**
- S17-lowlevel/lock.t (lock-contention)
- S17-procasync/encoding.t (Proc::Async encoding errors)
- S17-promise/then.t (dynamic variables not propagated to `.then` — Track-B blocked: lexical `%`/`@` thread-sharing)
- S17-scheduler/basic.t
- S17-supply/batch.t (known flaky — see CLAUDE.md)
- S17-supply/migrate.t, S17-supply/stable.t (abort on ThreadPoolScheduler)

**Related non-S17:**
- S07-hyperrace/basics.t (hyper/race parallelism)
- S32-io/socket-recv-vs-read.t (async socket ops)

(Note: `S17-promise/nonblocking-await.t` is now whitelisted #2991 — removed.)

---

## §F — Unpassable as written (reference rakudo also fails, or roast test bug)

Do NOT target a whitelist for these — only land *general* improvements they
motivate.

**Reference rakudo on this box fails to compile / dies:**
- S05-nonstrings/basic.t — `No such symbol 'Antelope'` + mainline has-method.
- S05-mass/rx.t — `::` backtracking-control NYI (dies line 38); mutsu also needs
  `<commit>` cut semantics.
- S05-metasyntax/angle-brackets.t — aborts at test 52 on `<$subrule>` compiling a
  `{...}`-code string (needs MONKEY-SEE-NO-EVAL); rakudo dies there too. 51/51
  runnable pass.
- S06-advanced/caller.t — `@_` in a method (rakudo: "Placeholder variables cannot
  be used in a method").
- S06-advanced/return_function.t — rakudo on this box **fails to compile** (line 19
  `($rv1,$rv2) := |(t2)`: "Cannot use bind operator with this left-hand side"), and
  even the test-1 expectation is stale: `my $x := |(f)` where `f` returns `:x<1>`
  binds a **Slip** `(x => 1)` (rakudo), not `1` as the test asserts; `g(|(f))` dies
  "Too few positionals". Roast-vs-rakudo version skew. (Verified 2026-06-16.)
- S06-advanced/return-prioritization.t — rakudo also dies at test 5 (LEAVE +
  return). 2/11 fail in mutsu.
- S06-operator-overloading/infix.t — "Code items cannot be rebound" (rebinding
  `&infix:<...>`).
- S02-names/pseudo-6d.t, pseudo-6e.t, S02-names-vars/names.t — `===SORRY===` in
  installed rakudo (roast-vs-rakudo version skew).
- S10-packages/require-and-use--dead-file.t — rakudo itself fails.
- S12-traits/basic.t, S12-traits/parameterized.t — removed `trait_auxiliary:<is>`.
- S12-class/open_closed.t — `use oo;` (module not in any repo).
- S12-attributes/trusts.t — `trusts B;` forward-references undeclared `B`.
- S12-meta/exporthow.t — rakudo dies at test 2 on EXPORTHOW SUPERSEDE. (mutsu has
  `X::EXPORTHOW::InvalidDirective`; not whitelistable.)

**Roast test bug (mutsu is self-consistent / more correct):**
- S32-str/sprintf.t — 166/174; the 8 failures (71-74, 101-104) have buggy expected
  values. See S32.md.

**Broad / no single root cause (re-measure before investing):**
- S02-types/range.t (56 failing) — broad range coercion/typecheck/lazy issues, well beyond a
  single fix. (S03-operators/range.t is now whitelisted: Range +/- Real #3209, reversed-range
  meta-ops `R..` + precedence worry, do-stmt-modifier `;` fix #3210.)

---

## Recently completed (since 2026-06-11; prune from this doc next refresh)

- **S17-supply/categorize.t** — whitelisted #3071 (Supply.categorize instance method
  + class-method guard + hash mapper `is default(...)`).
- Earlier DONE (already whitelisted): S32-exceptions/misc2.t, S03-buf/write-int.t,
  S32-io/io-path-cygwin.t, S16-io/words.t, S05-capture/alias.t, S29-os/system.t,
  S02-types/nil.t, S04-declarations/will.t, S06-signature/slurpy-blocks.t,
  S09-typed-arrays/arrays.t.
