# Roast Blockers by Feature

Summary: 188 tests blocked across 20 features (151 fail, 6 error, 31 timeout).

Generated: 2026-05-25

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

## throws-like / Exception Types (22 tests)

Many tests fail because mutsu doesn't throw the specific exception type the test expects (e.g., X::TypeCheck::Binding, X::Adverb, X::Syntax::Variable::Twigil, X::Worry::Precedence::Range, X::IllegalDimensionInShape, X::Cannot::Lazy, X::Assignment::RO, X::PseudoPackage::InDeclaration, X::EXPORTHOW::InvalidDirective, X::ControlFlow::Return, X::Comp::BeginTime). The code often silently succeeds or throws a generic error instead of the specific typed exception.

- roast/S02-types/array-shapes.t (X::IllegalDimensionInShape)
- roast/S02-types/capture.t (X::Cannot::Lazy)
- roast/S02-types/baghash.t (X::TypeCheck::Binding)
- roast/S02-types/bag.t (X::TypeCheck::Binding)
- roast/S02-types/set.t (X::Assignment::RO)
- roast/S02-types/mixhash.t (X::Str::Numeric)
- roast/S02-types/range.t (X::Worry::Precedence::Range)
- roast/S02-names-vars/variables-and-packages.t (X::PseudoPackage::InDeclaration)
- roast/S03-operators/range.t (X::Worry::Precedence::Range)
- roast/S04-statements/for.t (no exception on bad params)
- roast/S04-statements/return.t (X::ControlFlow::Return, X::Comp::BeginTime)
- roast/S05-substitution/subst.t (missing Exception throws)
- roast/S06-advanced/lexical-subs.t (X::Undeclared::Symbols)
- roast/S09-typed-arrays/arrays.t (type constraint violations don't throw)
- roast/S12-attributes/class.t (X::Method::NotFound, EVAL-in-class)
- roast/S12-meta/exporthow.t (X::EXPORTHOW::InvalidDirective)
- roast/S14-roles/mixin-6e.t (X::Syntax::Variable::Twigil)
- roast/S32-array/adverbs.t (X::Adverb)
- roast/S32-hash/adverbs.t (X::Adverb)
- roast/S32-exceptions/misc.t (X::Undeclared)
- roast/S32-exceptions/misc2.t (exception attribute matching)
- roast/S04-exceptions/exceptions-alternatives.t

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

## Traits / Metaprogramming (8 tests)

Trait system issues: `is` trait on variables, `will` trait, parameterized traits, attribute traits, routine traits interacting with .wrap, and the `trusts` mechanism for cross-class private attribute access.

- roast/S04-declarations/will.t
- roast/S12-traits/basic.t (is trait on variables)
- roast/S12-traits/parameterized.t
- roast/S14-traits/attributes.t (trait application to attributes)
- roast/S14-traits/routines.t (trait/wrap interaction)
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

## gather/take Laziness (3 tests)

gather/take doesn't properly implement laziness - gather blocks execute eagerly instead of on-demand. Nested gathers and take-rw also have issues.

- roast/S04-statements/gather.t (laziness, nested gathers, take-rw)
- roast/S07-iterators/range-iterator.t (timeout - lazy iterator)
- roast/S32-list/seq.t (planned 50, ran 22 - Seq laziness)

## Hyper/Meta Operators (5 tests)

Hyper operators (>>op<<) with assignment forms, reduce operator edge cases ([,], [min], [^^]), and hyper method dispatch on complex structures.

- roast/S03-metaops/hyper.t (timeout - >>op<< with assignment)
- roast/S03-metaops/infix.t (>>~=<< assignment forms)
- roast/S03-metaops/reduce.t ([=:=], [,], [min], [^^])
- roast/S03-operators/inplace.t (.= on class instantiation)
- roast/S03-operators/assign.t (assignment as function, list assignment)

## EVAL Completeness (3 tests)

EVAL works for basic cases but fails for advanced scenarios: EVAL with specific language, nested EVAL, some edge cases past test 23 of 30.

- roast/S29-context/eval.t (23/30 passing)
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

## Sprintf / Format Edge Cases (7 tests)

sprintf %g/%G formatting, and the large sprintf-b/d/e/f/x test suites timeout due to sheer volume (2000+ subtests each). The format.t file seems to pass mostly.

**Timeout:**
- roast/S32-str/sprintf-b.t (2282 tests, likely hangs mid-way)
- roast/S32-str/sprintf-d.t (4565 tests)
- roast/S32-str/sprintf-e.t (2282 tests)
- roast/S32-str/sprintf-f.t (2282 tests)
- roast/S32-str/sprintf-x.t (2282 tests)

**Fail:**
- roast/S32-str/sprintf.t (%g/%G formatting)
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

## Temporal / DateTime (3 tests)

Duration arithmetic (Inf, NaN, modulo), $*TZ dynamic variable not pre-set, and time numification.

- roast/S32-temporal/DateTime-Instant-Duration.t (Duration.new(Inf/NaN), modulo)
- roast/S32-temporal/local.t ($*TZ not defined by default)
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

## Categorize / Classify with Complex Keys (3 tests)

categorize/classify work for basic cases but fail when using multi-level keys or more complex categorization patterns.

- roast/S32-list/categorize.t (2/28 passing)
- roast/S32-list/classify.t (2/40 passing)
- roast/S32-list/minmax.t (:k, :v, :kv adverbs on min/max)

## Miscellaneous (15 tests)

Various other issues with limited overlap:

- roast/S02-lexical-conventions/unspace.t (45/110 - parser stops mid-file)
- roast/S02-literals/allomorphic.t (.ACCEPTS on allomorphs)
- roast/S02-magicals/sub.t (&?ROUTINE in regex/token/rule)
- roast/S02-types/nil.t (Nil in for loop, subset assignment)
- roast/S02-types/pair.t (Pair.value mutation, enum Pair)
- roast/S03-buf/write-int.t (error - Buf write-int callable)
- roast/S04-blocks-and-statements/temp.t (temp restore semantics)
- roast/S04-declarations/constant.t (constant reassignment errors)
- roast/S06-advanced/return_function.t (return via named arg binding)
- roast/S06-advanced/return-prioritization.t (LEAVE overwriting return)
- roast/S06-advanced/wrap.t (wrap lexical visibility)
- roast/S06-signature/slurpy-params.t (slurpy + named interaction)
- roast/S06-signature/slurpy-blocks.t
- roast/S09-hashes/objecthash.t (typed object hash semantics)
- roast/S09-subscript/slice.t (infinite sequence slices)
- roast/S14-roles/versioning.t (role version interaction)
- roast/S29-os/system.t (exit code -1 handling)
- roast/S32-array/perl.t (circular structure .raku)
- roast/S32-array/splice.t
- roast/S32-array/multislice-6e.t (multi-dim slicing)
- roast/S32-hash/multislice-6e.t (multi-dim hash slicing)
- roast/S32-hash/perl.t (Hash in Scalar perlification)
- roast/S32-io/spurt.t (file existence check)
- roast/S32-num/rat.t (Rational subclass, coercers)
- roast/S32-scalar/undef.t (undefine semantics)
- roast/S32-str/val.t (timeout - val() parsing)
- roast/S32-list/skip.t (timeout)
- roast/S12-methods/defer-next.t (timeout)
- roast/S12-methods/private.t (timeout)
- roast/S04-exception-handlers/catch.t (timeout)
- roast/S06-signature/named-parameters.t (timeout - 104 tests)
- roast/S16-unfiled/getpeername.t (timeout)
- roast/S04-declarations/state.t (timeout)
- roast/S03-sequence/exhaustive.t (timeout)
