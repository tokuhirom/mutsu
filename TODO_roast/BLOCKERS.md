# Roast Blockers by Feature

Summary: ~170 tests blocked across 19 features (fail, error, timeout).

Generated: 2026-05-30

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
- roast/S32-exceptions/misc.t (X::Undeclared)
- roast/S32-exceptions/misc2.t (exception attribute matching)
- roast/S04-exceptions/exceptions-alternatives.t (3/3 failing)

## Native Typed Arrays (11 tests)

Native typed arrays (int @a, num @a, str @a) don't properly report as Positional[int] etc., and shaped native arrays have various issues with :exists, type checking, and positional role compliance.

- roast/S09-typed-arrays/native-int.t
- roast/S09-typed-arrays/native-num.t
- roast/S09-typed-arrays/native-str.t
- roast/S09-typed-arrays/native.t
- roast/S09-typed-arrays/native-shape1-int.t
- roast/S09-typed-arrays/native-shape1-num.t
- roast/S09-typed-arrays/native-shape1-str.t
- roast/S09-typed-arrays/arrays.t (typed array constraint enforcement)
- roast/S02-types/signed-unsigned-native.t (timeout - native int types)
- roast/S02-types/multi_dimensional_array.t
- roast/S09-multidim/XX-POS-on-dimensioned.t

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

- roast/S02-names/pseudo-6d.t (OUR::.{} binding)
- roast/S02-names/pseudo-6e.t (root access to lexicals)
- roast/S02-names-vars/names.t (::{'$foo'} lookup)
- roast/S02-names-vars/variables-and-packages.t
- roast/S06-advanced/caller.t (caller.sub, caller.line)
- roast/S10-packages/basic.t (nested package autovivification)
- roast/S10-packages/scope.t (package scope visibility)
- roast/S10-packages/require-and-use--dead-file.t (%*INC tracking)

## Traits / Metaprogramming (7 tests)

Trait system issues: `is` trait on variables, `will` trait, parameterized traits, attribute traits, and the `trusts` mechanism for cross-class private attribute access. routines.t now passes (#2492).

- roast/S04-declarations/will.t
- roast/S12-traits/basic.t (is trait on variables)
- roast/S12-traits/parameterized.t
- roast/S14-traits/attributes.t (trait application to attributes)
- roast/S12-attributes/trusts.t (cross-class private access)
- roast/S12-class/open_closed.t (augment/open classes)
- roast/S12-introspection/walk.t (.walk with :canonical, :super, :breadth)

## IO Advanced Features (9 tests)

IO::CatHandle not implemented, IO::Path subclasses (::Unix, ::Cygwin) incomplete, file locking, pipe large blob handling, indir(), and child-secure path resolution.

- roast/S32-io/io-cathandle.t (IO::CatHandle not implemented)
- roast/S32-io/io-handle.t (input-line-separator, chunking)
- roast/S32-io/io-path.t (IO::Path::Unix)
- roast/S32-io/io-path-cygwin.t (IO::Path::Cygwin)
- roast/S32-io/child-secure.t (X::IO::Resolve)
- roast/S32-io/lock.t (file locking)
- roast/S32-io/pipe.t (large blob piping)
- roast/S32-io/spurt.t (file existence guard)
- roast/S32-io/indir.t (timeout - indir with path validation)
- roast/S16-io/words.t (handle close detection)

## gather/take Laziness (2 tests)

Coroutine-based lazy gather/take implemented (#2511). range-iterator.t now passes (all 103 tests). Remaining issues: nested gathers, take-rw, and Seq laziness edge cases.

- roast/S04-statements/gather.t (36/39 pass — nested gathers, take-rw, take inside m:g)
- roast/S32-list/seq.t (48/50 pass — .raku.EVAL roundtrip, methods on cached Seqs)

## Hyper/Meta Operators (5 tests)

Hyper operators (>>op<<) with assignment forms, reduce operator edge cases ([,], [min], [^^]), and hyper method dispatch on complex structures.

- roast/S03-metaops/hyper.t (timeout - >>op<< with assignment)
- roast/S03-metaops/infix.t (>>~=<< assignment forms)
- roast/S03-metaops/reduce.t ([=:=], [,], [min], [^^])
- roast/S03-operators/inplace.t (.= on class instantiation)
- roast/S03-operators/assign.t (assignment as function, list assignment)

## EVAL Completeness (2 tests)

EVAL now passes all 30 tests in eval.t (#2513). Remaining issues are EVAL scope visibility and EVAL inside class bodies.

- roast/S04-declarations/my-6e.t (EVAL scope visibility)
- roast/S12-attributes/class.t (EVAL inside class body)

## Module/Package System (6 tests)

Module versioning, import-multi semantics, CompUnit::Repository, and distribution metadata are incomplete.

- roast/S11-modules/import-multi.t (X::Redeclaration in imports)
- roast/S11-modules/versioning.t (core-revision)
- roast/S11-repository/cur-candidates.t (error - repository queries)
- roast/S11-repository/cur-current-distribution.t
- roast/S11-repository/curli-install.t (C::R::Installable role)
- roast/S19-command-line-options/01-dash-uppercase-i.t

## Multi Method / Subsignature Dispatch (5 tests)

Multi method dispatch on type signatures doesn't work properly. Subsignature matching in multi dispatch is incomplete.

- roast/S06-multi/subsignature.t
- roast/S12-methods/multi.t (multi method on Str/Numeric signatures)
- roast/S12-methods/qualified.t (callsame in punned roles)
- roast/S06-operator-overloading/infix.t
- roast/S06-other/main.t (MAIN with enum type constraints)

## WhateverCode / Currying Edge Cases (3 tests)

WhateverCode (*) in certain contexts: dummy assignment to *, &infix:<+>(*, 42) not making a closure, and multi-* expressions.

- roast/S02-types/whatever.t
- roast/S03-operators/eqv.t (eqv on references)
- roast/S02-types/generics.t (nominalizable generic)

## Sprintf / Format Edge Cases (2 tests)

The 6.d/S32-str/sprintf*.t suites all pass now. The non-6.d sprintf.t (zprintf) has roast test bugs.

**Fail (roast test bugs):**
- roast/S32-str/sprintf.t (166/174 pass; 8 failures from buggy test expectations)
- roast/S32-str/format.t

## Unicode / Collation (6 tests)

Unicode collation (CollationTest files fail because `plan` is called before `use Test`), NFG grapheme break detection, and character encoding (GB18030, GB2312, Shift-JIS).

- roast/S32-str/CollationTest_NON_IGNORABLE-0.t (error - plan before use Test)
- roast/S32-str/CollationTest_NON_IGNORABLE-1.t (error - same)
- roast/S32-str/CollationTest_NON_IGNORABLE-2.t (error - same)
- roast/S32-str/CollationTest_NON_IGNORABLE-3.t (error - same)
- roast/S15-nfg/GraphemeBreakTest-3.t (grapheme segmentation)
- roast/S32-str/gb18030-encode-decode.t
- roast/S32-str/gb2312-encode-decode.t
- roast/S32-str/shiftjis-encode-decode.t

## Pod / Documentation (4 tests)

Pod6 formatting codes (Pod::FormattingCode type), table rendering, and trailing declarator docs.

- roast/S26-documentation/07-tables.t
- roast/S26-documentation/08-formattingcodes.t (Pod::FormattingCode)
- roast/S26-documentation/12-non-breaking-space.t
- roast/S26-documentation/block-trailing.t
- roast/S26-documentation/why-trailing.t

## Temporal / DateTime (2 tests)

Duration arithmetic (Inf, NaN, modulo), and time numification.

- roast/S32-temporal/DateTime-Instant-Duration.t (Duration.new(Inf/NaN), modulo)
- roast/S32-temporal/time.t (Time::Local numification)

## Subset Types / Where Clauses (2 tests)

Subset types with complex where clauses, and their interaction with type checking, hang or fail.

- roast/S12-subset/subtypes.t (timeout)
- roast/S02-names/is_default.t

## Binding / Container Semantics (4 tests)

Binding to attributes, nested binding, and container semantics (Scalar decontainerization, itemization).

- roast/S03-binding/attributes.t (binding private class attributes)
- roast/S03-binding/nested.t (binding structure elements)
- roast/S12-attributes/instance.t (ro array/hash accessors)
- roast/S12-methods/accessors.t (contextualizing accessors)

## Categorize / Classify with Complex Keys (1 test)

categorize.t now fully passes (28/28, whitelisted). minmax.t fully passes (whitelisted, #2510). classify.t nearly passes (38/40 — 2 edge cases remaining).

- roast/S32-list/classify.t (38/40 pass — 2 edge cases remaining)

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
