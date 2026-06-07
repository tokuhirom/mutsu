# Roast Blockers by Feature

Summary: ~164 tests blocked across 19 features (fail, error, timeout).

Generated: 2026-05-30 (Unicode section updated 2026-06-05)

## Threading / Concurrency / Async (31 tests)

Most S17 tests timeout or fail due to incomplete threading primitives (Semaphore, Lock contention, nonblocking await) and Supply operators (batch, throttle, migrate, stable, zip-latest). Basic Promise/start and scheduler work partially but hang under stress.

**Timeout:**
- roast/S17-lowlevel/cas-int.t (partial pass then hangs)
- roast/S17-lowlevel/cas-loop.t (partial pass then hangs)
- roast/S17-lowlevel/semaphore.t (hangs immediately)
- roast/S17-lowlevel/thread.t (partial pass then hangs)
- roast/S17-procasync/stress.t (hangs under load)
- roast/S17-promise/nonblocking-await.t (hangs)
- roast/S17-promise/start.t (partial pass then hangs)
- roast/S17-scheduler/at.t (partial pass then hangs)
- roast/S17-scheduler/every.t (partial pass then hangs)
- roast/S17-scheduler/in.t (partial pass then hangs)
- roast/S17-supply/syntax.t (partial pass then hangs)
- roast/S17-supply/throttle.t (partial pass then hangs)

**Fail:**
- roast/S17-lowlevel/lock.t (hangs on lock contention tests)
- roast/S17-procasync/encoding.t (encoding error handling in Proc::Async)
- roast/S17-promise/then.t (dynamic variables not propagated to .then)
- roast/S17-scheduler/basic.t
- roast/S17-supply/basic.t (hangs)
- roast/S17-supply/batch.t (hangs)
- roast/S17-supply/categorize.t (class method guard missing)
- roast/S17-supply/migrate.t
- roast/S17-supply/stable.t
- roast/S17-supply/zip-latest.t

**Related non-S17:**
- roast/S07-hyperrace/basics.t (hangs - hyper/race parallelism)
- roast/S07-hyperrace/stress.t (hangs - hyper/race stress)
- roast/S29-context/sleep.t (sleep tests with scheduling, hangs)
- roast/S32-io/socket-recv-vs-read.t (async socket operations)

## throws-like / Exception Types (19 tests)

Many tests fail because mutsu doesn't throw the specific exception type the test expects. Recently implemented: X::Adverb (#2505), X::PseudoPackage::InDeclaration (#2507), X::Worry::Precedence::Range (#2502), X::IllegalDimensionInShape (#2503), X::TypeCheck::Binding::Parameter (#2477), X::Assignment::RO (#2477). PR #2532 adds proper `.message` methods for 17 exception types (X::Str::Numeric, X::Method::NotFound, X::Undeclared, X::Cannot::Lazy, X::ControlFlow::Return, X::OutOfRange, X::Immutable, X::Multi::NoMatch, X::Multi::Ambiguous, X::Redeclaration, X::StubCode, X::Bind, X::Match::Bool, X::Assignment::RO, X::NYI, X::Signature::Placeholder, X::IO::Closed). Remaining issues are throwing the right exception types in the right places.

- roast/S02-types/capture.t (X::Cannot::Lazy)
- roast/S02-types/baghash.t (X::TypeCheck::Binding)
- roast/S02-types/bag.t (X::TypeCheck::Binding)
- roast/S02-types/set.t (X::Assignment::RO)
- roast/S02-types/mixhash.t (X::Str::Numeric)
- roast/S02-types/range.t (still 56 failing — other issues beyond X::Worry)
- roast/S03-operators/range.t (still 18 failing — other issues beyond X::Worry)
- roast/S04-statements/for.t (no exception on bad params)
- roast/S04-statements/return.t (X::ControlFlow::Return — PR #2531 improves to 25/26)
- roast/S05-substitution/subst.t (missing Exception throws)
- roast/S06-advanced/lexical-subs.t (X::Undeclared::Symbols)
- roast/S09-typed-arrays/arrays.t (type constraint violations don't throw)
- roast/S12-attributes/class.t (X::Method::NotFound, EVAL-in-class)
- roast/S12-meta/exporthow.t (X::EXPORTHOW::InvalidDirective)
- roast/S32-array/adverbs.t (X::Adverb implemented, but 283/606 — parser stops mid-file)
- roast/S32-hash/adverbs.t (X::Adverb implemented, but still failing on some edge cases)
- roast/S32-exceptions/misc.t (broad: ~40 distinct compile-time error/exception features; 42/157 pass. X::Undeclared post/highexpect now done. Remaining: compile-time undeclared-symbol checking for non-EVAL programs, X::NotParametric, X::Syntax::Extension::SpecialForm, X::Redeclaration of subs/methods, X::Bind, sink-context "Useless use" warnings, and ~30 one-off exception types. See TODO_roast/S32.md for the full list.)
- roast/S32-exceptions/misc2.t (exception attribute matching)
- roast/S04-exceptions/exceptions-alternatives.t (3/3 failing)

## Native Typed Arrays (3 tests — shaped only)

Non-shaped native typed arrays (`int @a`, `num @a`, `str @a`) all pass now, along
with the general typed-array tests. Only the *shaped* native arrays remain.

**Passing now (whitelisted):**
- roast/S09-typed-arrays/native.t
- roast/S09-typed-arrays/native-int.t
- roast/S09-typed-arrays/native-num.t
- roast/S09-typed-arrays/native-str.t
- roast/S09-typed-arrays/arrays.t (84/84)
- roast/S02-types/signed-unsigned-native.t
- roast/S02-types/multi_dimensional_array.t
- roast/S09-multidim/XX-POS-on-dimensioned.t

**Still failing — shaped native arrays (`array[T].new(:shape(n))`):**
mutsu's shaped-array support is weak: `.map(* *= 2)` does not mutate in place,
`@a[*-1,*-2]` slices return 0, `.raku` omits `:shape(...)`, clearing a shaped
array (`@a = ()`) should reset to its fixed size of defaults rather than empty
it, and `.grep`/`.values`/`.pairs` see stale (pre-mutation) values. ~33 failing
of 101 run before the file aborts. These need real fixed-dimension array
semantics, not the per-fix work that fixed the non-shaped suites.
- roast/S09-typed-arrays/native-shape1-int.t
- roast/S09-typed-arrays/native-shape1-num.t
- roast/S09-typed-arrays/native-shape1-str.t

## Regex / Match Advanced Features (12 tests)

Advanced regex features: captures with aliases, reverse captures, % separator in .caps/.chunks, longest token matching (LTM), package variable captures, regex called as method from angle brackets, quantifier * with 0 matches.

- roast/S05-capture/alias.t (reverse capture, mixed captures)
- roast/S05-capture/array-alias.t (named/sequential array capture)
- roast/S05-capture/caps.t (% separator in .caps/.chunks)
- roast/S05-capture/hash.t (package hash captures)
- roast/S05-mass/recursive.t
- roast/S05-mass/rx.t (no backtrack into group)
- roast/S05-match/capturing-contexts.t (quantifier *, subrule capture)
- roast/S05-metasyntax/angle-brackets.t (calling regex as method)
- roast/S05-metasyntax/longest-alternative.t (timeout - LTM)
- roast/S05-nonstrings/basic.t
- roast/S05-substitution/subst.t (edge cases)
- roast/S03-buf/read-write-bits.t (timeout - Buf bit operations)

## Pseudo-packages / Symbol Lookup (8 tests)

Dynamic symbol lookup via pseudo-packages (MY::, OUR::, OUTER::, CALLER::, ::{}) doesn't work properly. `::{'$foo'}` syntax for indirect variable access is broken.

**Unpassable as written (reference rakudo on this box also fails to compile):**
- roast/S06-advanced/caller.t — `@_` in a method (and legacy `Control::Caller`);
  rakudo: "Placeholder variables (eg. @_) cannot be used in a method."
- roast/S02-names/pseudo-6d.t, pseudo-6e.t, S02-names-vars/names.t — all hit a
  `===SORRY===` compile error in the installed rakudo (e.g. names.t: "Variable
  '$' is not declared"). May be a roast-vs-rakudo version skew; cannot diff
  against the reference here.
- roast/S10-packages/require-and-use--dead-file.t — reference rakudo itself fails
  ("Cannot convert string to number ... 'RequireAndUse1.rakumod'").

**raku passes, mutsu fails (real work, but multi-feature — no single whitelist win):**
- roast/S02-names-vars/variables-and-packages.t (16/39 fail). The dominant root
  cause is **closure lexical capture**: a closure snapshots its captured scalar
  *by name/value*, not by container reference, so
  `{ my $x = 100; $f = { $x } }; my $x = 999; $f()` wrongly reads 999 (the later
  same-named outer `$x` hijacks the capture). mutsu *does* handle the two common
  shapes — shared in-scope mutation (`my $a=1; $f={$a}; $a=2; $f()` → 2) and
  independent factory counters — and it relies on caller-wins for them, so a
  naive merge-order flip regresses those. The principled fix needs first-class
  Scalar **container identity** (the same limitation as take-rw), which is a
  large multi-PR refactor: a prototype that promotes captured scalars to
  `ContainerRef` fixed tests 18-20 locally but broadly regressed value-type-
  sensitive sites in roast (typed vars / `let` / subset assignment / DESTROY /
  submethods / concurrency) because `ContainerRef` is not dereferenced at every
  consuming op. Closing that "deref everywhere" surface is the real prerequisite.
  Other failures here are independent features: `&OUR::grtz()` (OUR:: code
  lookup, 32-34), `X::Redeclaration::Outer` (37-38), `$OUTER::_` topic (39), and
  a named sub closing over a `my` declared/`BEGIN`-initialized later (24-31,
  needs the captured container hoisted to scope entry).
- roast/S10-packages/basic.t (18/59 fail) — mostly compile-time undeclared-symbol
  checking ("reference to class/role/module before definition dies") and
  `X::Redeclaration` of subs in a class; overlaps the Exception-Types blocker.

(roast/S10-packages/scope.t was stale here — it already passes and is whitelisted.)

## Traits / Metaprogramming (7 tests)

Trait system issues: `is` trait on variables, `will` trait, parameterized traits,
attribute traits, and the `trusts` mechanism for cross-class private attribute
access. routines.t now passes (#2492). User-defined `trait_mod:<is>` now
dispatches on `Attribute` objects (#2631).

**Unpassable as written (reference rakudo also fails to compile — do NOT attempt):**
- roast/S12-traits/basic.t — uses the removed `multi sub trait_auxiliary:<is>`
  category; rakudo: "Cannot add tokens of category 'trait_auxiliary'".
- roast/S12-traits/parameterized.t — same removed `trait_auxiliary:<is>`.
- roast/S12-class/open_closed.t — `use oo;` (module not in any repo).
- roast/S12-attributes/trusts.t — `trusts B;` forward-references `B` before its
  declaration; rakudo: "Type 'B' is not declared". (A pre-stubbed `B` would be
  needed; the file as written cannot compile.)

**Still failing (real mutsu work):**
- roast/S14-traits/attributes.t (4/8 now pass). User-defined `trait_mod:<is>`
  dispatched on `Attribute` objects works for named/positional-type traits
  (tests 1-4: `is noted`, the `$a.name` introspection). Tests 5-8 need
  `$a.container.VAR does Role($arg)` — mixing a parameterized role into a
  per-attribute *container template* so every instance's `$.attr.VAR` carries
  the role. Blocked on first-class per-attribute containers (mutsu stores
  attribute values as plain Values with no Scalar container), the same root
  limitation as take-rw container identity. Local test: t/attribute-trait-mod.t.
- roast/S04-declarations/will.t (17/19; tests 5/7/13 are `# TODO`). Two real
  failures: test 1 needs correct BEGIN-vs-CHECK phaser ordering interleaved with
  `will begin`/`will check` (CHECK fires LIFO after all BEGIN); test 17 needs
  `will leave` on a class-scoped `my` var to fire when the class body block is
  left. Blocked on phaser ordering/`will`-phaser-on-class-scoped-var, not traits.
- roast/S12-introspection/walk.t (passes in rakudo). Needs the `.WALK` method +
  `WalkList` type with all MRO orderings (:canonical, :super, :breadth,
  :descendant, :ascendant, :preorder, :omit, :include), submethod walking, lazy
  batch invocation, and quiet-mode Failure capture. Large self-contained feature.

## IO Advanced Features

Remaining work is mostly Win32/Cygwin path canonicalization (platform-specific
separator/volume/UNC semantics) plus a few niche edge cases. `pipe.t`, `spurt.t`,
`indir.t`, and `child-secure.t` now pass and are whitelisted.

**Resolved:**
- roast/S32-io/child-secure.t — `.child(:secure)` with X::IO::Resolve / X::IO::NotAChild (#2617, whitelisted)
- roast/S32-io/pipe.t, spurt.t, indir.t — already passing/whitelisted

**Still failing:**
- roast/S32-io/io-cathandle.t (IO::CatHandle not implemented — note: raku itself fails test 31 "Cannot .elems a lazy list", so the file may be unpassable as written)
- roast/S32-io/io-handle.t (nl-out=IO::Handle gist edge cases, internal chunking; subtest 2 nl-in fixed in #2618)
- roast/S32-io/io-path.t (39/43; subtests 16/26/27/28/29/30 fixed in #2616/#2617. Remaining: Win32 `.gist`/`.parts` canonicalization (31, 33), X::Assignment::RO on `.SPEC=`/`.CWD=` + `temp $*SPEC`/`temp $*CWD` (34, 35), `.path` indir-independence (36), `.Numeric` Cool chain (37). See TODO_roast/S32.md.)
- roast/S32-io/io-path-cygwin.t (IO::Path::Cygwin — UNC `//server/share`, drive letters `A:`, backslash separators in volume/dirname/basename/is-absolute; 10 failing)
- roast/S32-io/lock.t (file locking — `.lock`/`.unlock` throw X::Method::NotFound, not implemented)
- roast/S16-io/words.t (8/11; `$limit` fixed in #2616. Remaining: lazy close-on-exhaust for `words($fh, :close)` partial slices, and `IO::ArgFiles.new`)

## gather/take Laziness (2 tests)

Coroutine-based lazy gather/take implemented (#2511). range-iterator.t now passes (all 103 tests). Remaining issues: Seq laziness edge cases and take-rw container identity.

- roast/S04-statements/gather.t (38/39 pass — only failure is test 38 take-rw
  reference identity `@neighbors[1][1][0] =:= @spot[0][0]`. Blocked on
  first-class array-element containers: mutsu arrays store plain Values with no
  per-element Scalar container, so take-rw cannot preserve container identity
  through nested indexing. See TODO_roast/S04.md for details. Nested gathers,
  take inside m:g, and take on lists already pass.)
- roast/S32-list/seq.t (48/50 pass — .raku.EVAL roundtrip, methods on cached Seqs)

## Hyper/Meta Operators (4 tests)

Hyper operators (>>op<<) with assignment forms and hyper method dispatch on complex structures.

- roast/S03-metaops/hyper.t (timeout - >>op<< with assignment)
- roast/S03-metaops/infix.t (>>~=<< assignment forms)
- roast/S03-operators/inplace.t (.= on class instantiation)
- roast/S03-operators/assign.t (assignment as function, list assignment)

## EVAL Completeness (2 tests)

EVAL now passes all 30 tests in eval.t (#2513). Remaining issues are EVAL scope visibility and EVAL inside class bodies.

- roast/S04-declarations/my-6e.t (EVAL scope visibility)
- roast/S12-attributes/class.t (EVAL inside class body)

## Module/Package System (DONE — verified 2026-06-05)

All reachable tests in this group now pass and are whitelisted; the one
remaining is unpassable as written:

- roast/S11-modules/import-multi.t — **whitelisted, passes**
- roast/S11-modules/versioning.t — **whitelisted, passes** (`CORE-SETTING-REV`
  compile-time term + `BEGIN $*RAKU.version` folding to the compunit's language
  version + EVAL now sets parser lib paths so `use Foo; bar` resolves parenless
  exports)
- roast/S11-repository/cur-candidates.t — **whitelisted, passes**
- roast/S11-repository/cur-current-distribution.t — **whitelisted, passes**
- roast/S11-repository/curli-install.t — **whitelisted, passes (19/19)**.
  `use lib "inst#PATH"` installs a `CompUnit::Repository::Installation` as
  `$*REPO` (chained in front of the previous repo); the instance does
  `CompUnit::Repository::Installable`/`::Locally` via its MRO and answers
  `.id`/`.short-id`/`.loaded`. `.install` rejects an identical re-install
  (`already installed`); `.need` returns a real `CompUnit`
  (`.short-name`/`.version`/`.handle.globalish-package`) whose symbols stay
  hidden from `::('Foo')` until `GLOBALish.WHO.merge-symbols(...)` publishes them
  into GLOBAL. Side-fix: a single named for-loop param (`for ... -> $x`) no
  longer leaks across function calls that reuse the same variable name.
- roast/S19-command-line-options/01-dash-uppercase-i.t — **unpassable as written**:
  rakudo itself fails it (`@*INC` and `$*OS` no longer exist; "planned 8 tests,
  but ran 0").

## Multi Method / Subsignature Dispatch (2 tests remaining)

**Already passing/whitelisted (this section was stale — verified 2026-06-05):**
- roast/S06-multi/subsignature.t (whitelisted; remaining `not ok` are `# TODO`)
- roast/S12-methods/multi.t (whitelisted, passes)
- roast/S06-other/main.t (whitelisted, passes)

**Still failing:**
- roast/S12-methods/qualified.t (6/7; only "parameterizations and inheritance" —
  callsame in punned roles — fails)
- roast/S06-operator-overloading/infix.t — **unpassable as written**: rakudo
  itself fails to compile it ("Cannot bind to '&infix:<plus>' because Code items
  cannot be rebound" at line 40). mutsu runs further but the test relies on
  rebinding `&infix:<...>`, which is illegal in current Raku.

## WhateverCode / Currying Edge Cases (3 tests)

WhateverCode (*) in certain contexts: dummy assignment to *, &infix:<+>(*, 42) not making a closure, and multi-* expressions.

- roast/S02-types/whatever.t (33 distinct failing WhateverCode features: dummy
  `*` assignment, `&infix:<+>(*,42)` closure, X+/Z+ metaop currying with Whatever,
  `*++`, rw params, container preservation, compile-time WhateverCode, regex
  whatever curry — not a single root cause)
- roast/S03-operators/eqv.t — 62/64 (the 12/17/36/37 `# TODO huh?` failures also
  fail in rakudo). Set allomorph eqv is now fixed (`set(<42>) eqv set(42)` is
  False; see t/set-eqv-allomorph.t). Two subtests remain:
    - "Setty eqv Setty": needs `Set eqv SetHash` to be False (test 165). mutsu's
      `eqv` deliberately ignores Set/SetHash mutability because the set operators
      `(|)`/`(&)`/etc. don't reliably return a SetHash; comparing the flag would
      regress S03-operators/set_*.t. Blocked on set-operator mutability tracking.
    - "Throws/lives in lazy cases": `eqv` must throw X::Cannot::Lazy for two
      same-type lazy iterables. Blocked on real lazy infinite sequences — mutsu
      materializes `1…∞` into a finite (~33-element) list, so it has no lazy
      iterables to detect.
- roast/S02-types/generics.t (nominalizable generic)

## Sprintf / Format Edge Cases (2 tests)

The 6.d/S32-str/sprintf*.t suites all pass now. The non-6.d sprintf.t (zprintf) has roast test bugs. The `Format` class (6.e) is now implemented; format.t is blocked on RakuAST.

**Fail (roast test bugs — cannot pass as written):**
- roast/S32-str/sprintf.t (166/174 pass; the 8 failures, tests 71-74 and
  101-104, have buggy expected values — `%5.2g` cases expect a `g`/`G` exponent
  letter while the parallel `%20.2g` cases correctly expect `e`/`E`, and the
  `%020.2g` cases expect mantissa `34.1` where the un-padded twins expect the
  correct `3.14`. mutsu is self-consistent. See TODO_roast/S32.md.)

**Blocked on RakuAST:**
- roast/S32-str/format.t (26/49 reachable tests now pass; was 0). The Format
  class — `Format.new`, `~~ Format`, `.arity`/`.count`/`.Callable`, `CALL-ME`,
  stringification, and full `.fmt($format, $sep)` integration with arity-aware
  batching and `X::Str::Sprintf::Directives::Count` — is implemented, plus
  string interpolation of postcircumfix calls (`"$f()"`). The file aborts at
  test 27 because `Formatter.AST` must return a `RakuAST::Node` (lines 50-52)
  and mutsu has no RakuAST; a mid-file error aborts the remaining 23 `.fmt`
  tests. Local coverage: t/format-class.t (40 tests).

## Unicode / Collation (2 tests)

The `unicmp` infix operator is now implemented (ICU4X-backed default UCA, not
affected by `$*COLLATION`, unlike `coll`), and `use Test` is hoisted to BEGIN
time so `plan`/`ok` work before the textual `use Test;`. CollationTest 0/1/2 now
pass fully and are whitelisted. The GB18030/GB2312/Shift-JIS codecs were already
complete; they only needed to run from the roast spec root (their data files are
located relative to CWD) — `run-roast-test.sh` now runs them with CWD=roast.

**Resolved (whitelisted):**
- roast/S32-str/CollationTest_NON_IGNORABLE-0.t (2300/2300)
- roast/S32-str/CollationTest_NON_IGNORABLE-1.t (2301/2301)
- roast/S32-str/CollationTest_NON_IGNORABLE-2.t (2301/2301)
- roast/S32-str/gb18030-encode-decode.t (5/5)
- roast/S32-str/gb2312-encode-decode.t (5/5)
- roast/S32-str/shiftjis-encode-decode.t (9/9)

**Still failing:**
- roast/S32-str/CollationTest_NON_IGNORABLE-3.t (1367/1369 — 2 noncharacter
  cases fail: U+FFF0/U+FFFE and U+FFFF/U+1FFFE. ICU4X treats the noncharacters
  as primary-ignorable so the comparison falls through to the trailing
  codepoint, whereas UCA-17/MoarVM assign implicit primary weights by codepoint.
  Needs custom implicit-weight handling for reserved/noncharacters.)
- roast/S15-nfg/GraphemeBreakTest-3.t (157/166 — 9 failures are GB9c Indic
  conjunct break (Myanmar/Balinese/Khmer virama clusters) and GB11 emoji-ZWJ
  (`\x[2701,200D,2701]`). mutsu uses the `unicode-segmentation` crate, whose
  bundled Unicode version doesn't implement these UAX-29 rules for Unicode 17.0.
  Needs GB9c/GB11 post-processing or a crate upgrade.)

## Pod / Documentation (1 test remaining)

**Resolved (whitelisted):**
- roast/S26-documentation/07-tables.t — already passing/whitelisted (stale entry)
- roast/S26-documentation/08-formattingcodes.t — already passing/whitelisted (stale entry)
- roast/S26-documentation/block-trailing.t — **whitelisted, passes (50/50)**. A
  standalone trailing declarator comment (`#=` on its own line after the
  declaration) now carries the declaration's callable-type / proto / return-type
  metadata into `$=pod`, so `$=pod[i].WHEREFORE.^name` is the correct type
  (Method/Submethod/Routine/`Sub+{Callable[Str]}`) instead of a generic `Sub`.
  Fix in `src/runtime/io.rs` (the `#=`-on-own-line path threads
  `callable_type_override`/`is_proto`/`return_type` from `last_declarant`).
  Local test: t/declarator-trailing-wherefore.t.
- roast/S26-documentation/why-trailing.t — **whitelisted, passes (54/54)** (same fix).

**Still failing:**
- roast/S26-documentation/12-non-breaking-space.t — NOT a Pod issue. Subtest 2
  plans `$nbchar-count + 1` where `my $nbchar-count = @nbchars.elems` is read at
  the top of the file but `@nbchars` is populated by a `BEGIN {}` block at the
  *end* of the file. rakudo runs the textually-later `BEGIN` at compile time
  (before the top-level `my` initialization), so `$nbchar-count` is 4; mutsu runs
  `BEGIN` at its textual position, so it reads 0 and the subtest plan collapses to
  `1..1`. Blocked on **compile-time BEGIN-phaser execution / hoisting** ordering,
  not on Pod table rendering.

## Temporal / DateTime (DONE except an unpassable test)

**Resolved (whitelisted):**
- roast/S32-temporal/DateTime-Instant-Duration.t (68/68) — Duration now stores
  its seconds as a Rational (so `.tai` is a Rat / does Rational), `Duration % Real`
  returns a Duration computed with exact rational arithmetic (matching
  `Duration.new($seconds % $real)`), and `Duration.new` defaults to `0.0` (Rat).
  Also fixed a general `arith_mod` bug where `Rat % BigInt` (e.g. `(7/1) % (2**66)`)
  returned 0 — rational modulo now uses big-rational parts. (#TBD)

**Unpassable as written (cannot be fixed):**
- roast/S32-temporal/time.t — uses Perl 5 builtins `localtime`/`gmtime`/`times`
  which are NOT Raku core routines (rakudo itself fails to compile this file:
  "localtime used at lines ...; times ... Did you mean 'lines'?"), and the file
  contains two deliberate `flunk("FIXME Time::Local should by numifiable")` calls.
  It can never reach a passing state. Leave un-whitelisted.

## Subset Types / Where Clauses (2 tests)

Subset types with complex where clauses, and their interaction with type checking.

- roast/S12-subset/subtypes.t (no longer times out; 90/92 run, 13 failing as of
  the inline-`where`/`++` fixes below). Inline `where` constraints on `my`
  variables (`my $x where * > 0`, `my Int $n where { $_ %% 2 }`,
  `my $v where &predicate`) are now desugared into anonymous subsets so they are
  enforced on initialization AND on assignment, and `++`/`--` on a subset-typed
  scalar re-checks the constraint (`my Even $x = 2; $x++` throws
  X::TypeCheck::Assignment and preserves the value). Local test:
  t/where-constraint-var.t. **Remaining failures are NOT where-clause bugs:**
  the dominant one is closure writeback from a block/Whatever-code predicate
  stored in a `&`-variable — `my &pm = { $wanted = $^got; True }; 42 ~~ PS`
  updates `$wanted` the first time but a second invocation of the same captured
  block (e.g. via a later `my $x where &pm = 42`) re-runs the body yet its write
  to the captured `$wanted` is lost. This is the first-class container-identity /
  closure-capture limitation (lever C, in progress), not subset-specific. The
  same root cause blocks the signature `where &codevar` `neg arg`/`pos arg`
  subtests (the predicate's write to the outer `$wanted` does not propagate).
  Other independent failures: `fail()` inside a subset predicate (test 25),
  Junction-of-types in `where` (87), read-only enforcement of the `where` topic
  (34), and `where &var` enforcement on `|c`/slurpy params (85). Tests 91-92 also
  fail in reference rakudo here.
- roast/S02-names/is_default.t — **unpassable as written**: reference rakudo
  (2022.12) fails to compile it (`===SORRY===` at line 527, `has $.v is
  default(T)` inside a generic role). mutsu actually runs further (141/146) than
  the reference. Its remaining real failures are the `is default(...)` trait on
  **hashes** when the defaulted hash is stored in an array and re-bound via a
  `for` signature (`%a is raw`): the container-keyed default (keyed by the Arc
  pointer of the hash map) is lost across the copy/rebind, so `%a<o>` and
  `%a.VAR.default` return `(Any)` instead of the default. Blocked on first-class
  container identity (same root limitation), and unwhitelist­able regardless
  because of the line-527 generic-role compile failure.

## Binding / Container Semantics (4 tests)

Binding to attributes, nested binding, and container semantics (Scalar decontainerization, itemization).

- roast/S03-binding/attributes.t (binding private class attributes)
- roast/S03-binding/nested.t (binding structure elements)
- roast/S12-attributes/instance.t (ro array/hash accessors)
- roast/S12-methods/accessors.t (contextualizing accessors)

## Categorize / Classify with Complex Keys (1 test)

categorize.t now fully passes (28/28, whitelisted). minmax.t fully passes (whitelisted, #2510). classify.t nearly passes (38/40 — 2 edge cases remaining).

- roast/S32-list/classify.t (39/40 pass — only the "classify works with
  Junctions" subtest remains. NOTE (verified 2026-06-05): this is NOT a small
  fix. When the mapper returns a Junction, rakudo's `.classify` builds an
  **object hash** (`my Any %{Mu}`) keyed by the Junction's `.WHICH` identity
  (any(True,False) is one key, distinct from any(False,False)), and `$h{ any(...) }`
  retrieves by object-key identity WITHOUT autothreading the subscript. mutsu's
  `Value::Hash` is `HashMap<String, Value>` (string keys only) with no
  object-hash support, so this subtest is blocked on full object-hash
  (`%{Mu}` / non-Str keyed hash) semantics, not a classify-local tweak.)

## Miscellaneous (25 tests)

Various other issues with limited overlap. Recently resolved: temp.t (#2514), constant.t (#2512), array/perl.t (#2516), hash/perl.t (#2518 — partial), objecthash.t (#2517 — partial), splice.t (#2515 — partial), undef.t (90/91).

- roast/S02-lexical-conventions/unspace.t (45/110 - parser stops mid-file)
- roast/S02-literals/allomorphic.t (.ACCEPTS on allomorphs)
- roast/S02-magicals/sub.t (&?ROUTINE in regex/token/rule)
- roast/S02-types/nil.t (Nil in for loop, subset assignment)
- roast/S02-types/pair.t (Pair.value mutation, enum Pair)
- roast/S03-buf/write-int.t (error - Buf write-int callable)
- roast/S04-blocks-and-statements/temp.t (30/37 — improved via #2514, 7 remaining)
- roast/S04-declarations/constant.t (17/19 — improved via #2512, 2 remaining)
- roast/S06-advanced/return_function.t (return via named arg binding)
- roast/S06-advanced/return-prioritization.t (LEAVE overwriting return)
- roast/S06-advanced/wrap.t (wrap lexical visibility)
- roast/S06-signature/slurpy-params.t (slurpy + named interaction)
- roast/S06-signature/slurpy-blocks.t
- roast/S09-hashes/objecthash.t (28/62 — improved via #2517, typed hash edge cases remaining)
- roast/S09-subscript/slice.t (infinite sequence slices)
- roast/S14-roles/versioning.t (role version interaction)
- roast/S29-os/system.t (exit code -1 handling)
- roast/S32-array/splice.t (partial — improved via #2515, splice edge cases remaining)
- roast/S32-array/multislice-6e.t (multi-dim slicing)
- roast/S32-hash/multislice-6e.t (multi-dim hash slicing)
- roast/S32-hash/perl.t (43/55 — improved via #2518, remaining issues)
- roast/S32-io/spurt.t (file existence check)
- roast/S32-num/rat.t (Rational subclass, coercers)
- roast/S32-scalar/undef.t (90/91 — 1 remaining)
- roast/S32-str/val.t (timeout - val() parsing)
- roast/S32-list/skip.t (timeout)
- roast/S12-methods/defer-next.t (timeout)
- roast/S12-methods/private.t (timeout)
- roast/S04-exception-handlers/catch.t (timeout)
- roast/S06-signature/named-parameters.t (timeout - 104 tests)
- roast/S16-unfiled/getpeername.t (timeout)
- roast/S04-declarations/state.t (timeout)
- roast/S03-sequence/exhaustive.t (timeout)
