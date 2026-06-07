# Roast Blockers by Feature

Failing roast tests grouped by the missing feature that blocks them, ordered by
impact. Each entry carries a **fix difficulty** estimate:

- **Easy** — localized, clear root cause, 1-2 failing subtests, no architectural blocker.
- **Medium** — a self-contained feature to implement; bounded scope.
- **Hard** — blocked on a deep architectural limitation (first-class container
  identity, real lazy infinite sequences, threading primitives, RakuAST,
  object-hash `%{Mu}` keys) or a large pile of disparate sub-features.

Last refreshed: 2026-06-07 (stale/passing entries removed, counts re-measured,
difficulty ratings added).

## Threading / Concurrency / Async — **Hard**

S17 tests that still hang or fail need real threading primitives (Semaphore,
nonblocking await, lock contention) and the remaining Supply combinators.
Basic Promise/start/scheduler/channel work and are now whitelisted; what is left
is the genuinely concurrent surface, which is hard to make deterministic.

**Timeout (hang):**
- roast/S17-lowlevel/cas-int.t (partial pass then hangs)
- roast/S17-lowlevel/semaphore.t (hangs immediately — Semaphore not implemented)
- roast/S17-promise/nonblocking-await.t (hangs — await must yield the scheduler thread)
- roast/S17-promise/start.t (partial pass then hangs)
- roast/S17-supply/syntax.t (partial pass then hangs)

**Fail:**
- roast/S17-lowlevel/lock.t (hangs on lock-contention tests)
- roast/S17-procasync/encoding.t (encoding error handling in Proc::Async)
- roast/S17-promise/then.t (dynamic variables not propagated to `.then`)
- roast/S17-scheduler/basic.t
- roast/S17-supply/batch.t (known flaky — see CLAUDE.md; hangs intermittently)
- roast/S17-supply/categorize.t (class-method guard missing)
- roast/S17-supply/migrate.t
- roast/S17-supply/stable.t

**Related non-S17:**
- roast/S07-hyperrace/basics.t (hangs — hyper/race parallelism)
- roast/S32-io/socket-recv-vs-read.t (async socket operations)

## throws-like / Exception Types

Tests fail because mutsu doesn't throw the exact exception type the test expects,
or aborts mid-file at the first un-thrown error. `.message` methods for 17
exception types landed in #2532; what remains is *throwing the right type in the
right place*, plus compile-time undeclared-symbol checking.

- roast/S02-types/capture.t — **Easy**. 2/33 fail then aborts (planned 46). Needs
  X::Cannot::Lazy from `Capture` lazy ops; an un-thrown error aborts the rest.
- roast/S02-types/set.t — **Easy**. 1/246 fail (test 226): typechecking on a
  Hashified `Set` iterator (`my %s := Set(...)` must reject bad-typed assignment).
- roast/S02-types/bag.t — **Easy**. 2/255 fail: X::TypeCheck::Binding on Bag
  coercion.
- roast/S02-types/baghash.t — **Medium**. 7 fail then aborts at test 270/344
  (X::TypeCheck::Binding on BagHash iterator/coercion).
- roast/S02-types/mixhash.t — **Medium**. 4 fail then aborts at 216/295
  (X::Str::Numeric on Mix coercion edge cases).
- roast/S02-types/range.t — **Hard**. 56 failing — broad, well beyond the X::Worry
  fix; range coercion/typecheck/lazy issues, not a single root cause.
- roast/S03-operators/range.t — **Hard**. 18 failing — same broad range issues.
- roast/S04-statements/for.t — **Medium**. 22/111 fail: no exception thrown on bad
  loop-variable binding (`for 1,2 -> $a, $b, $c`), plus topic-aliasing edge cases.
- roast/S05-substitution/subst.t — **Medium**. 23/191 fail: missing Exception
  throws on illegal substitution forms + `.subst` adverb edge cases.
- roast/S06-advanced/lexical-subs.t — **Medium**. 5/13 fail then aborts at line 66:
  X::Undeclared::Symbols for forward-referenced lexical `my sub`.
- roast/S09-typed-arrays/arrays.t — **Medium**. Type-constraint violations on
  assignment to a typed array don't throw X::TypeCheck.
- roast/S12-attributes/class.t — **Hard**. X::Method::NotFound + EVAL-inside-class
  body (depends on the EVAL-in-class blocker below).
- roast/S12-meta/exporthow.t — **Medium**. X::EXPORTHOW::InvalidDirective +
  `EXPORTHOW` plumbing.
- roast/S32-array/adverbs.t — **Hard**. X::Adverb is implemented but the parser
  stops mid-file; 283/606 pass. Needs adverb parsing on more subscript forms.
- roast/S32-hash/adverbs.t — **Medium**. X::Adverb edge cases on hash subscripts.
- roast/S32-exceptions/misc.t — **Hard**. 42/157 pass. ~40 distinct compile-time
  error features: compile-time undeclared-symbol checking for non-EVAL programs,
  X::NotParametric, X::Syntax::Extension::SpecialForm, X::Redeclaration of
  subs/methods, X::Bind, sink-context "Useless use" warnings, ~30 one-off types.
  See TODO_roast/S32.md.
- roast/S32-exceptions/misc2.t — **Medium**. Exception attribute matching.

## Native Typed Arrays — shaped only — **Hard**

Non-shaped native typed arrays (`int @a`, etc.) and the general typed-array suites
all pass and are whitelisted. Only the *shaped* native arrays
(`array[T].new(:shape(n))`) remain, and they need real fixed-dimension semantics,
not per-fix patches: `.map(* *= 2)` must mutate in place, `@a[*-1,*-2]` slices
return 0, `.raku` omits `:shape(...)`, `@a = ()` must reset to fixed-size defaults
rather than empty, and `.grep`/`.values`/`.pairs` see stale pre-mutation values.
~33 of 101 fail before the file aborts.

- roast/S09-typed-arrays/native-shape1-int.t
- roast/S09-typed-arrays/native-shape1-num.t
- roast/S09-typed-arrays/native-shape1-str.t

## Regex / Match Advanced Features

- roast/S05-capture/caps.t — **Easy**. 1/43 fail (test 43): `.caps`/`.chunks` drops
  the zero-width `%`-separator captures, so the delimiter slots that should appear
  between matches are missing (`0 0 0` instead of `0 delim 0 delim 0 delim`).
- roast/S05-capture/alias.t — **Medium**. 14/32 fail: reverse capture (`$1` before
  `$0` textually) and mixed named/positional capture aliases yield empty strings.
- roast/S05-capture/array-alias.t — **Hard**. 30/37 fail then aborts: named/
  sequential array captures (`@<foo>=...`) are largely unimplemented.
- roast/S05-capture/hash.t — **Hard**. 30/99 fail then aborts at line 134: package/
  hash captures (`%<foo>=...`) unimplemented.
- roast/S05-mass/rx.t — **Medium**. Aborts at test 19/756 on a no-backtrack-into-
  group case that throws instead of failing the match; one un-thrown error halts
  the whole 756-test file, so the fix unblocks a large count.
- roast/S05-match/capturing-contexts.t — **Medium**. 8/64 fail: quantifier `*`
  with 0 matches and subrule capture in list context produce wrong capture shape.
- roast/S05-metasyntax/angle-brackets.t — **Medium**. Aborts at test 51/95: calling
  a regex as a method from `<&foo()>` / `<.foo>` angle-bracket assertions (NYI).
- roast/S05-metasyntax/longest-alternative.t — **Hard**. Timeout — LTM (longest
  token matching) over many alternatives is not implemented efficiently.
- roast/S05-nonstrings/basic.t — **Medium**. Aborts before test 1 (ran 0/5):
  matching a regex against a non-Str (matching coerces the subject) throws early.
- roast/S05-substitution/subst.t — see Exception Types above (same file).

## Pseudo-packages / Symbol Lookup — **Hard**

Dynamic symbol lookup via pseudo-packages (MY::, OUR::, OUTER::, CALLER::, `::{}`).
The dominant blocker is **first-class closure lexical capture** (see container
identity, below).

**Unpassable as written (reference rakudo on this box also fails to compile — do NOT attempt):**
- roast/S06-advanced/caller.t — `@_` in a method; rakudo: "Placeholder variables
  (eg. @_) cannot be used in a method."
- roast/S02-names/pseudo-6d.t, pseudo-6e.t, S02-names-vars/names.t — `===SORRY===`
  compile error in installed rakudo (roast-vs-rakudo version skew).
- roast/S10-packages/require-and-use--dead-file.t — reference rakudo itself fails.

**raku passes, mutsu fails (real work, but multi-feature — no single whitelist win):**
- roast/S02-names-vars/variables-and-packages.t (16/39 fail). Dominant root cause is
  **closure lexical capture by container**: a closure snapshots its captured scalar
  by name/value, not by container reference, so `{ my $x=100; $f={$x} }; my $x=999;
  $f()` wrongly reads 999. mutsu handles the two common shapes (shared in-scope
  mutation, independent factory counters) via caller-wins, so a naive merge-order
  flip regresses those. The principled fix needs first-class Scalar **container
  identity** (same limitation as take-rw) — a large multi-PR refactor. Other
  failures here are independent: `&OUR::grtz()` (32-34), X::Redeclaration::Outer
  (37-38), `$OUTER::_` topic (39), named sub closing over a later-declared `my`
  (24-31).
- roast/S10-packages/basic.t (18/59 fail) — **Medium** part: compile-time
  undeclared-symbol checking ("reference to class/role before definition dies") and
  X::Redeclaration of subs in a class; overlaps the Exception-Types blocker.

## Traits / Metaprogramming

**Unpassable as written (reference rakudo also fails to compile — do NOT attempt):**
- roast/S12-traits/basic.t, roast/S12-traits/parameterized.t — removed
  `trait_auxiliary:<is>` category.
- roast/S12-class/open_closed.t — `use oo;` (module not in any repo).
- roast/S12-attributes/trusts.t — `trusts B;` forward-references undeclared `B`.

**Still failing (real mutsu work):**
- roast/S14-traits/attributes.t — **Hard**. 4/8 pass. Tests 5-8 need
  `$a.container.VAR does Role($arg)` — mixing a parameterized role into a
  per-attribute *container template*. Blocked on first-class per-attribute
  containers (mutsu stores attribute values as plain Values, no Scalar container) —
  the same container-identity root limitation. Local test: t/attribute-trait-mod.t.
- roast/S04-declarations/will.t — **Medium**. 17/19 (tests 5/7/13 are `# TODO`). Two
  real failures: test 1 needs BEGIN-vs-CHECK phaser ordering interleaved with
  `will begin`/`will check` (CHECK fires LIFO after all BEGIN); test 17 needs
  `will leave` on a class-scoped `my` var to fire when the class body is left.
  Blocked on phaser ordering, not traits.
- roast/S12-introspection/walk.t — **Hard**. Needs the `.WALK` method + `WalkList`
  type with all MRO orderings (:canonical/:super/:breadth/:descendant/:ascendant/
  :preorder/:omit/:include), submethod walking, lazy batch invocation, quiet-mode
  Failure capture. Large self-contained feature.

## IO Advanced Features

Remaining work is mostly Win32/Cygwin path canonicalization plus a few niche edge
cases. `pipe.t`, `spurt.t`, `indir.t`, `child-secure.t` now pass.

- roast/S32-io/io-cathandle.t — **Hard**. IO::CatHandle not implemented (note: raku
  itself fails test 31 "Cannot .elems a lazy list", so the file may be unpassable
  as written).
- roast/S32-io/io-handle.t — **Medium**. nl-out / `IO::Handle` gist edge cases and
  internal chunking (subtest 2 nl-in fixed in #2618).
- roast/S32-io/io-path.t — **Medium**. 39/43. Remaining: Win32 `.gist`/`.parts`
  canonicalization (31, 33), X::Assignment::RO on `.SPEC=`/`.CWD=` + `temp $*SPEC`/
  `temp $*CWD` (34, 35), `.path` indir-independence (36), `.Numeric` Cool chain (37).
- roast/S32-io/io-path-cygwin.t — **Medium**. IO::Path::Cygwin: UNC `//server/share`,
  drive letters `A:`, backslash separators in volume/dirname/basename/is-absolute;
  10 failing.
- roast/S32-io/lock.t — **Medium**. `.lock`/`.unlock` throw X::Method::NotFound (file
  locking not implemented).
- roast/S16-io/words.t — **Medium**. 8/11. Remaining: lazy close-on-exhaust for
  `words($fh, :close)` partial slices, and `IO::ArgFiles.new`.

## gather/take Laziness — **Hard**

- roast/S04-statements/gather.t — 38/39 pass; only failure is test 38 take-rw
  reference identity `@neighbors[1][1][0] =:= @spot[0][0]`. Blocked on first-class
  array-element containers (arrays store plain Values, no per-element Scalar), so
  take-rw cannot preserve container identity through nested indexing.

## Hyper/Meta Operators

- roast/S03-metaops/hyper.t — **Hard**. Timeout — `>>op<<` with assignment forms.
- roast/S03-operators/inplace.t — **Medium**. 6/38 fail (from test 318 "constants"):
  `.=` in-place metaop on class instantiation / readonly constants.
- roast/S03-operators/assign.t — **Hard**. 35/193 fail then aborts: assignment used
  as a function (`&infix:<=>`), list-assignment in non-trivial contexts.

## EVAL Completeness — **Medium/Hard**

- roast/S04-declarations/my-6e.t — **Medium**. EVAL scope visibility (EVAL'd code
  must see the enclosing lexical scope).
- roast/S12-attributes/class.t — **Hard**. EVAL inside a class body (overlaps the
  Exception-Types entry above).

## Multi Method / Subsignature Dispatch

- roast/S12-methods/qualified.t — **Medium**. 6/7; only "parameterizations and
  inheritance" (callsame in punned roles) fails.
- roast/S06-operator-overloading/infix.t — **unpassable as written**: rakudo itself
  fails to compile it ("Code items cannot be rebound" — rebinding `&infix:<...>`).

## WhateverCode / Currying Edge Cases

- roast/S02-types/whatever.t — **Hard**. 33 distinct failing WhateverCode features:
  dummy `*` assignment, `&infix:<+>(*,42)` closure, X+/Z+ metaop currying with
  Whatever, `*++`, rw params, container preservation, compile-time WhateverCode,
  regex whatever curry — not a single root cause.
- roast/S03-operators/eqv.t — **Hard**. 62/64 (the 12/17/36/37 `# TODO huh?` also
  fail in rakudo). Two real failures: "Setty eqv Setty" needs `Set eqv SetHash`
  False (blocked on set-operator mutability tracking), and "Throws/lives in lazy
  cases" needs X::Cannot::Lazy for same-type lazy iterables (blocked on real lazy
  infinite sequences — mutsu materializes `1…∞` into a finite list).
- roast/S02-types/generics.t — **Medium**. Nominalizable generic type.

## Sprintf / Format

- roast/S32-str/sprintf.t — **unpassable** (roast test bugs). 166/174; the 8
  failures (71-74, 101-104) have buggy expected values; mutsu is self-consistent.
  See TODO_roast/S32.md.
- roast/S32-str/format.t — **Hard** (blocked on RakuAST). 26/49 reachable pass; the
  Format class is implemented but the file aborts at test 27 because
  `Formatter.AST` must return a `RakuAST::Node` and mutsu has no RakuAST. Local
  coverage: t/format-class.t.

## Unicode / Collation — **Hard**

- roast/S32-str/CollationTest_NON_IGNORABLE-3.t — 1367/1369. 2 noncharacter cases
  fail (U+FFF0/U+FFFE, U+FFFF/U+1FFFE): ICU4X treats noncharacters as
  primary-ignorable; UCA-17/MoarVM assign implicit primary weights by codepoint.
  Needs custom implicit-weight handling for reserved/noncharacters.
- roast/S15-nfg/GraphemeBreakTest-3.t — 157/166. 9 failures are GB9c Indic conjunct
  break (Myanmar/Balinese/Khmer virama) and GB11 emoji-ZWJ. The
  `unicode-segmentation` crate's bundled Unicode version doesn't implement these
  UAX-29 rules for Unicode 17.0. Needs GB9c/GB11 post-processing or a crate upgrade.

## Pod / Documentation — **Hard**

- roast/S26-documentation/12-non-breaking-space.t — NOT a Pod issue. Subtest 2
  plans `$nbchar-count + 1` where `$nbchar-count` is read at the top of the file but
  `@nbchars` is populated by a `BEGIN {}` at the *end* of the file. rakudo runs the
  textually-later BEGIN at compile time (so the count is 4); mutsu runs BEGIN at its
  textual position (count 0). Blocked on compile-time BEGIN-phaser hoisting.

## Subset Types / Where Clauses — **Hard**

- roast/S12-subset/subtypes.t — 90/92 run, 13 failing. Inline `where` on `my` vars
  and `++`/`--` re-checking are done (t/where-constraint-var.t). Remaining failures
  are NOT where-clause bugs: the dominant one is **closure writeback from a
  block/Whatever predicate stored in a `&`-variable** — the predicate's write to a
  captured outer `$wanted` is lost on the second invocation (first-class container
  identity / closure-capture, lever C). Also blocks the signature `where &codevar`
  subtests. Independent failures: `fail()` inside a subset predicate (25),
  Junction-of-types in `where` (87), read-only enforcement of the `where` topic (34),
  `where &var` on `|c`/slurpy params (85). Tests 91-92 also fail in reference rakudo.
- roast/S02-names/is_default.t — **unpassable as written**: reference rakudo
  (2022.12) fails to compile it (`===SORRY===` at line 527). mutsu runs further
  (141/146); the real remaining failures are `is default(...)` on **hashes** rebound
  via a `for` signature — container-keyed default lost across copy/rebind (container
  identity again).

## Binding / Container Semantics — **Hard**

All four are facets of mutsu storing attribute/element values as plain `Value`s with
no per-slot `Scalar` container (first-class container identity).

- roast/S03-binding/attributes.t — 1/13 fail then aborts (test 13): `does`-mixing a
  role into a *bound* private scalar attribute's container ("is the scalar attribute
  mixed in?") — needs the attribute to be a real container.
- roast/S03-binding/nested.t — **Medium**. 18/43 fail: binding to nested structure
  elements (`$a := @b[0]`) doesn't alias — writes through one don't reach the other
  (off-by-one symptom: expected 44, got 43).
- roast/S12-methods/accessors.t — 4/11 fail: contextualizing (rw) accessors return
  empty instead of the container value.

## Categorize / Classify with Complex Keys — **Hard**

- roast/S32-list/classify.t — 39/40. Only "classify works with Junctions" remains.
  When the mapper returns a Junction, rakudo builds an **object hash** (`my Any %{Mu}`)
  keyed by the Junction's `.WHICH`, and `$h{ any(...) }` retrieves by object-key
  identity WITHOUT autothreading the subscript. mutsu's `Value::Hash` is
  `HashMap<String, Value>` (Str keys only), so this is blocked on full object-hash
  (`%{Mu}` / non-Str-keyed hash) semantics, not a classify-local tweak.

## Multi-dimensional Slicing

- roast/S32-array/multislice-6e.t — **Medium**. Aborts at test 5 (planned 812).
  The fast-path single-element multidim assign works, but the general
  `@a[0;0;0] = 999` lvalue path returns Nil and doesn't write back — multidim
  subscript assignment isn't an lvalue except on the fast path.
- roast/S32-hash/multislice-6e.t — **Medium**. Aborts at test 5 (planned 549) — same
  multidim-subscript-as-lvalue gap on hashes.

## Miscellaneous

- roast/S32-array/splice.t — **Hard**. 245/381 fail (NOT "edge cases" — this is the
  dominant failure of the file). Root cause is **self-referential splice**: the test
  matrix does `@a.splice(..., @a)` / push-self forms, and mutsu's splice reads the
  replacement list lazily *while mutating the same array*, so the "remainder results"
  diverge. Needs splice to snapshot its replacement args before mutating.
- roast/S32-hash/perl.t — **Medium**. 12/55 fail: Hash-in-Scalar vs deconted-Hash
  `.perl`/`.raku` differ (`(Str(Any),Mu)` typed-hash perlification + decont rules).
- roast/S09-hashes/objecthash.t — **Hard**. 8/36 fail then aborts (planned 62) —
  typed/object-hash edge cases; overlaps the object-hash `%{Mu}` blocker.
- roast/S09-subscript/slice.t — **Hard**. 9/39 fail then aborts at line 310 — slices
  with infinite sequences (`@a[0..*]`); blocked on real lazy infinite sequences.
- roast/S02-literals/allomorphic.t — **Easy**. 1/119 fail: `.ACCEPTS` on an allomorph
  (e.g. `<42>` IntStr) doesn't smartmatch correctly.
- roast/S02-types/nil.t — **Medium**. 12/65 fail then aborts (planned 67): Nil in a
  `for` loop and Nil assignment to a subset-typed var.
- roast/S02-types/pair.t — **Medium**. 4/180 fail then aborts (planned 182): Pair
  `.value` mutation and enum-derived Pair behavior.
- roast/S03-buf/write-int.t — **Medium**. "Callable expected" runtime error — the
  `write-int*` family of Buf methods aren't wired as callables.
- roast/S04-blocks-and-statements/temp.t — **Medium**. 30/37 (7 remaining): `temp`
  restoration interacting with hash/array element containers (container identity).
- roast/S04-declarations/constant.t — **Medium**. 63/68 pass, 5 fail then aborts at
  line 331: `constant` with complex RHS (list/typed/sigilless) edge cases.
- roast/S06-advanced/return_function.t — **Medium**. Aborts at test 1 (planned 4):
  `return` via named-argument binding to a routine returns the wrong value.
- roast/S06-advanced/return-prioritization.t — **Medium**. 2/11 fail: a `LEAVE`
  phaser's value overwrites the routine's `return` value (got 2, expected 1).
- roast/S06-advanced/wrap.t — **Hard**. 12/70 fail then aborts (planned 90): `.wrap`
  lexical visibility — the wrapper closure can't see the wrapped routine's lexicals
  (closure capture / container identity).
- roast/S06-signature/slurpy-params.t — **Medium**. Aborts at test 29 (planned 86):
  slurpy-scalar (`*$x`) and slurpy+named interaction throw early.
- roast/S06-signature/slurpy-blocks.t — **Medium**. Aborts before test 1 (ran 0/6):
  slurpy params in block signatures throw at setup.
- roast/S32-list/skip.t — **Medium**. Plan mismatch: planned 55, ran 206 (the file
  loops more than planned, so subtest counting/laziness is off); 29 fail.
- roast/S29-os/system.t — **Medium**. Aborts at test 3 (planned 41): `shell`/`run`
  exit-code -1 (command-not-found) handling.
