# Roast Blockers by Feature

Failing roast tests grouped by the missing feature that blocks them, ordered by
impact. Each entry carries a **fix difficulty** estimate:

- **Easy** — localized, clear root cause, 1-2 failing subtests, no architectural blocker.
- **Medium** — a self-contained feature to implement; bounded scope.
- **Hard** — blocked on a deep architectural limitation (first-class container
  identity, real lazy infinite sequences, threading primitives, RakuAST,
  object-hash `%{Mu}` keys) or a large pile of disparate sub-features.

Last refreshed: 2026-06-11 (re-measured every listed file against the current
release build; removed entries that are now whitelisted — `S02-types/nil.t`,
`S04-declarations/will.t`, `S06-signature/slurpy-blocks.t`,
`S32-exceptions/misc2.t`, `S09-typed-arrays/arrays.t` — and folded the stale
"main engineer / sub-engineer" fleet framing into a plain difficulty/subsystem
taxonomy, since work is now single-threaded).

## Status (2026-06-11)

The project is in its **final stretch**: every remaining file below has been
re-measured, and essentially all of them are gated on one of a small set of
**Hard architectural blockers** — there are no more cheap whitelist wins to
cherry-pick. The blockers, in rough order of how many files they gate:

- **First-class container identity** (scalar/element/attribute `Scalar`
  containers; `Value` carries a bare value with no per-slot container) — gates
  binding/`:=`-alias, `is rw`/take-rw, `.VAR`, typed-hash default survival,
  closure capture-by-container, object-hash `%{Mu}` keys, Arc-pointer typed-array
  flaky. This is PLAN.md's "🟣 第2優先" and the single largest lever.
- **Real lazy infinite sequences** — `@a[0..*]`, `fib[100]`, `X::Cannot::Lazy`
  on same-type lazy iterables; mutsu materializes `1…∞` to a finite list.
- **Threading / concurrency primitives** — Semaphore, nonblocking await, lock
  contention, Supply combinators (all of S17).
- **RakuAST** — `Formatter.AST`, anything needing a reflectable AST.
- **A long tail of distinct compile-time `X::` exception types** thrown at the
  exact right place (misc.t / misc2.t-style campaigns).

So the highest-leverage work is no longer per-file roast picking but the
**architectural tracks in PLAN.md** (Interpreter-execution-path removal +
first-class containers). The per-file analysis below is kept as a map: when one
of those blockers lands, this doc says which files it unblocks.

## How to read the tiers

The tiers are a **difficulty / subsystem taxonomy**, not a work queue for a
fleet (the old "main/sub engineer, stay out of these files" framing is gone — we
work single-threaded now):

- **Tier 1** — isolated subsystems (regex engine, IO, unicode, Buf/OS builtins).
- **Tier 2** — additive `X::` exception types + module plumbing.
- **Tier 3** — self-contained semantic fixes in specific builtins / methods / coercion.
- **Tier 4** — touches VM control / return handling / parser / compiler.

"**Container identity / closure capture / lazy / dispatch**" items are collected
under "Do NOT pick — architectural blocker" — they only move when the PLAN.md
tracks advance.

### Tier 1 — fully isolated subsystems

Live in the regex engine (`runtime/regex*.rs`), IO (`runtime/io*`, IO builtins),
unicode (`builtins/unicode.rs`), and Buf/OS builtins.

**Already whitelisted (done):** S16-io/words.t, S05-capture/alias.t,
S32-io/io-path-cygwin.t, S29-os/system.t.

**Skip — not whitelistable (reference rakudo also fails, or environment quirk):**
do NOT spend time here, the file cannot reach a clean pass as written.
- S05-nonstrings/basic.t — rakudo fails to compile it (`No such symbol 'Antelope'`).
- S05-mass/rx.t — rakudo can't compile it either (`::` backtracking-control NYI,
  dies at line 38). mutsu also needs `<commit>` cut semantics; even fixed, upstream
  is unpassable.
- S05-metasyntax/angle-brackets.t — aborts at test 52 on `<$subrule>` compiling a
  `{...}`-code string as a regex; real rakudo dies there too (no MONKEY-SEE-NO-EVAL).
  Not reachable to `plan 95` without code-string subrules.

**Genuinely-achievable remainder** (self-contained features; concrete approach
given so the next worker can start cold):

1. **S03-buf/write-int.t** — *Hard (large but mechanical).* 2530 tests.
   **Reclassified — the blockers are Whatever-currying, not 128-bit.**
   128-bit `read-int128`/`write-int128`/`-uint128` and dynamic interpolated method
   names (`."read-int{8*$_}"(...)`) already work across all endiannesses
   (verified: `blob8.new(1..16).read-int128(0,BigEndian)` returns the right value).
   The test aborts in *setup* (before `plan`) on two Whatever-composition bugs:
   - `1 +< (*-1) - 1` throws "Callable expected" — a WhateverCode `(*-1)` does not
     curry through an enclosing `1 +< … - 1` into a new WhateverCode.
   - `(^*).roll` evaluates to the un-rolled Range `0..^N` instead of a
     WhateverCode `{ (^$_).roll }` — `.roll` (and method calls generally) on a
     `^*` Whatever-range is not deferred into the curried closure.
   Fix the Whatever-currying of chained infix ops and trailing method calls
   first (broad impact, well beyond this file); the Buf widths are already done.

**Defer — needs deep/crate-level work (file fully passes only after a big lift):**
- S32-io/io-path.t — 6 *independent* failures; one (test 31 `.gist`) depends on a
  **Rakudo internal caching quirk**: Win32 `.gist` renders the backslash form only
  *after* `.absolute` has been called on that object (the test calls `.absolute`
  then `.gist`). Do NOT replicate the mutate-on-`.absolute` behavior. The other 5
  (`.parts` Win32 split 33, X::Assignment::RO on `.SPEC=`/`.CWD=` + `temp` 34/35,
  `.path` indir-independence 36, `.Numeric` Cool chain 37) are each doable —
  `.Numeric` (37) is the most tractable (IO::Path is Cool → numify via `.Str`) —
  but the whole file can't whitelist while 31 needs the quirk, so it's low-ROI.
- S32-io/lock.t — needs **fcntl POSIX record locks** (`F_SETLK`/`F_SETLKW` with
  `F_RDLCK`/`F_WRLCK`), NOT `flock(2)`: rakudo rejects an exclusive lock on a
  read-only fd via fcntl `EBADF`, which `flock` would allow. Also tests cross-process
  blocking via subprocess `is_run`, `IO::CatHandle`, and fd-reuse (`native-descriptor`).
  Genuinely Hard; timing-sensitive.
- S05-match/capturing-contexts.t — 8 *disparate* failures: binding `$/`
  (`my $/ := 42`), index-stable positional slots so `(y)?`→Nil and `(z)*`→empty-list
  coexist, `%%` separator backtracking against an outer anchor, capture markers
  `<(`/`)>`. Each is its own mini-feature; no single-PR whitelist.
- S32-str/CollationTest_NON_IGNORABLE-3.t — 2 noncharacter cases. ICU4X treats
  noncharacters (U+FFFE/U+FFFF/…) as primary-ignorable; UCA-17 gives them implicit
  primary weights by codepoint. No substitute codepoint exists near U+FFF0
  (U+FFF9–FFFD are assigned), so the fix needs sort-key-level weight injection
  (`icu_collator` exposes `write_sort_key_to`) or a partial UCA reimplementation.
  Hard; 2-test payoff.
- S15-nfg/GraphemeBreakTest-3.t — GB9c (Indic conjunct) / GB11 (emoji-ZWJ) need a
  `unicode-segmentation` upgrade or UAX-29 post-processing. Hard.

### Tier 2 — additive exception types & module plumbing

Mostly **additive**: new `X::` types + throw sites + parse-time checks. Lives in
exception/parser-error code and module handlers, not the VM hot path.

**Reality check (verified):** these files are mostly **not whitelistable** as
whole files — reference rakudo itself fails some, and the rest are blocked on
deep features (container identity, regex internals). So Tier 2's real value is
the **reusable typed exceptions** each file motivates — those carry across files
(`X::Redeclaration` / `X::Undeclared::Symbols` are misc2.t prerequisites) — not
the per-file whitelist. The misc2.t campaign is **done and whitelisted**.

**Landed (typed exceptions + features; details in per-synopsis TODO files):**

- **S32-exceptions/misc2.t** — *DONE, whitelisted (265/265).* The full
  compile-time-exception campaign (`X::Parameter::WrongOrder`, `X::Obsolete`,
  `X::Syntax::Perl5Var`, `X::Placeholder::*`, `X::Redeclaration`,
  `X::Multi::NoMatch` → Failure, …) landed across ~35 PRs.
- **S04-declarations/constant.t** — `X::Redeclaration` on same-scope `constant`
  redeclaration; plus `constant := value` now raises (24/26 fixed, #2893). Still
  NOT whitelistable: lazy-seq `fib[100]` (46) + `G::c` qualified name via constant
  alias (44). 65/72. See S04.md.
- **S06-advanced/lexical-subs.t** — unknown `foo()` throws typed
  `X::Undeclared::Symbols`. Remaining: bareword-undeclared detection, `&foo`
  parameter precedence (VM call cache), inner-block lexical-sub leak. See S06.md.
- **S12-meta/exporthow.t** — `X::EXPORTHOW::InvalidDirective`. NOT whitelistable:
  rakudo itself dies at test 2 on EXPORTHOW SUPERSEDE. See S12.md.
- **S32-hash/adverbs.t** — *big win, 823→1069/1128.* Zen-slice adverbs, `X::Adverb`,
  `%h.name`/`@a.name`, Range-key slices. Remaining 59: typed-hash value-type
  default surviving rebinding (first-class container identity). See S32.md.
- **S05-substitution/subst.t** — `X::Assignment::RO` for `s///`/`tr///` on a
  matching string literal. NOT whitelistable: rakudo itself fails tests 157, 170.
  Remaining is regex-internal (`:mm`/samemark, `:samecase`,
  `X::Syntax::Regex::NullRegex`, non-constant `:i`). See S05.md.

**Next pickups (bounded, one small PR each):**

1. **S05-substitution/subst.t regex-internal pieces** — sequence AFTER any open
   regex PR: `X::Syntax::Regex::NullRegex` (empty `s///`), `:samecase`/`:samemark`
   application in `.subst`.
2. **S32-exceptions/misc.t** — the *other* compile-time-exception file (still
   42/157; ~40 distinct types). Same campaign style as misc2.t, now reusable.

**Cross-cutting blockers (architectural — see "Do NOT pick" below):**
- **First-class container identity** — adverbs.t typed-hash missing-key default
  surviving variable rebinding; needs value-carried type metadata.
- **VM call cache** — lexical `&foo`/`&infix` parameter precedence over a
  same-named package sub (`pos_light_call_cache`/`light_call_cache` are name-keyed
  and ignore a lexical `&name`).
- **`if { given { } }` value propagation** (pre-existing) — a trailing `given` in
  an `if`/`with`-value position returns Nil (e.g. `sub f { with $v { ... } }` when
  `$v` is a var). This blocks io-handle.t test 22's nested-`with`/`given`
  topic-source isolation; fix the value-propagation bug first, then re-isolate the
  topic source (the naive Given-wrap of the `with` body broadly regresses
  `with`-as-value — see PR #2696's reverted first attempt).

### Tier 3 — self-contained semantic fixes (off the VM core)

Touch specific builtins / method handlers / type coercion, not env or dispatch.
**Caveat (verified 2026-06-11):** most of these turned out to have a Hard blocker
hiding inside, so none is a clean single-PR whitelist right now:

- S32-array/splice.t — the basic self-referential splice now works; the 245
  failures are in the typed-array subtest harness (`$T.new`/`.WHAT` over typed
  arrays), not the snapshot. Hard.
- S32-hash/perl.t — needs Hash itemization on scalar assignment (`my $a = %h` →
  `${…}` perlify) = container semantics, not a local coercion tweak. 12 fail.
- S02-types/pair.t — 3 fail; 128/139 need `$pair.value` to alias the original
  variable's container (container identity). Test 171 (typed-assign throw) is the
  only non-container one.
- S12-methods/qualified.t — the one remaining failure is the parameterized-role
  subtest (`$?ROLE`/`$?CLASS`, `R1[::T]` qualified dispatch) = heavy MOP, not
  "callsame in punned roles" as previously labeled.
- S12-introspection/walk.t — `.WALK` + `WalkList` (large but self-contained MOP).
- S06-signature/slurpy-params.t — autothread + `+@` single-arg-rule + range
  handling (multi-feature; aborts at 43/86).

### Tier 4 — VM control / parser / compiler

These touch `vm/vm_control_ops.rs`, return handling, or the parser/compiler.

- S04-statements/for.t (loop-var binding — VM control ops)
- S06-advanced/return_function.t, S06-advanced/return-prioritization.t
  (return/LEAVE; note return-prioritization.t: reference rakudo also dies at
  test 5 on this box, so it is not whitelistable as written)
- Parser operators: `ff`/`fff` flipflop, `==>`/`<==` feed precedence, hyper
  assignment, generalized negation meta (parser + compiler)
- S32-array/multislice-6e.t, S32-hash/multislice-6e.t (subscript-lvalue path)

### Do NOT pick — blocked on an architectural blocker (PLAN.md tracks)

These are facets of first-class container identity, closure capture, the dual
store, real lazy infinite sequences, or threading shared-state. They only move
when PLAN.md's Interpreter-removal / first-class-container tracks advance.

- **Container identity / closure capture**: S02-types/capture.t,
  S02-names-vars/variables-and-packages.t, S04-statements/gather.t (take-rw),
  S14-traits/attributes.t, S12-methods/accessors.t, S03-binding/attributes.t,
  S03-binding/nested.t, S12-subset/subtypes.t, S02-names/is_default.t,
  S04-blocks-and-statements/temp.t, S06-advanced/wrap.t, S02-types/whatever.t,
  S32-hash/perl.t, S02-types/pair.t
- **Object-hash `%{Mu}`**: S32-list/classify.t, S09-hashes/objecthash.t
- **Lazy infinite sequences**: S03-operators/eqv.t, S09-subscript/slice.t,
  S32-list/skip.t
- **Typed-array type-metadata** (Arc-pointer keying — folded into container
  identity): S09-typed-arrays/native-shape1-*.t. (Note: S09-typed-arrays/arrays.t
  is now whitelisted.)
- **Threading / concurrency**: the entire S17 section below.

---

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

- roast/S02-types/capture.t — **Hard**. 2/33 fail then aborts (planned 46). The
  two failures (28-29) are NOT exception types: `$c[0]++` / `$c<a>++` on a
  Capture built from `\($a)` must write *through* to the original scalar
  container — first-class container identity (lever C). The later abort needs
  X::Cannot::Lazy from `Capture` lazy ops.
- roast/S02-types/set.t — **Hard** (remaining). Test 226 (typed-hash bind
  type-check) is FIXED: `my Int %h := <untyped hash>` now throws
  X::TypeCheck::Binding. The only remaining failures are the 2 not-reached tests
  "coercion of object Hash to Set 1/2" (`:{ }.Set`), which need full object-hash
  (`%{Mu}`) semantics — same blocker as classify/objecthash below.
- roast/S02-types/bag.t — **Hard**. 2/255 fail. NOT exception types: test 215
  needs Bag weights to hold values larger than i64 (`200000000000000000019`) —
  mutsu stores `BagData` counts as `i64`, so big weights collapse to 1; a faithful
  fix needs BigInt-capable bag weights across all set/bag/mix ops. Test 252 needs
  Bag-union to preserve a `my class Foo is Bag` subclass type (Bag is a `Value`,
  not a subclassable Instance).
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
- roast/S32-exceptions/misc2.t — **DONE, whitelisted (265/265).** The full
  compile-time-exception campaign landed (~35 PRs). Use misc.t (above) as the
  next file in the same style.

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

- roast/S05-capture/alias.t — **DONE** (whitelisted; tests 11-13 are `# TODO`).
  Numbered scalar capture aliases (`$N=<atom>`, reverse, arbitrary-start with
  auto-numbering), `:s` sigspace propagation into alternation branches, and
  preserving an aliased group's inner subrule capture as a nested subcap
  (`$<family>=(<ident>)` -> `$<family><ident>`).
- roast/S05-capture/array-alias.t — **Hard**. 30/37 fail then aborts: named/
  sequential array captures (`@<foo>=...`) are largely unimplemented.
- roast/S05-capture/hash.t — **Hard**. 30/99 fail then aborts at line 134: package/
  hash captures (`%<foo>=...`) unimplemented.
- roast/S05-mass/rx.t — **not whitelistable**. Reference rakudo can't compile this
  file (`::` backtracking-control NYI, dies at line 38). mutsu additionally aborts
  at test 20 on `<commit>` (cut semantics, NYI), and later subtests want
  compile-time regex exception types. Even with those, upstream is unpassable — do
  NOT target a whitelist; only land general backtracking-control improvements.
- roast/S05-match/capturing-contexts.t — **Medium (multi-feature)**. 56/64. 8
  *disparate* failures: binding `$/` (`my $/ := 42`), index-stable positional slots
  so `(y)?`→Nil and `(z)*`→empty-list coexist, `%%` separator backtracking vs an
  outer anchor, capture markers `<(`/`)>`. No single-PR whitelist.
- roast/S05-metasyntax/angle-brackets.t — **not whitelistable as written**. 51/51
  runnable subtests pass, but the run aborts at test 52: `<$subrule>` compiles a
  string containing a `{...}` code block as a regex, which needs MONKEY-SEE-NO-EVAL.
  Real rakudo dies at the same point (no MONKEY-SEE-NO-EVAL), so `plan 95` is
  unreachable. (Earlier "test 51 `<&foo()>`" was a `#?rakudo skip`, not the blocker.)
- roast/S05-metasyntax/longest-alternative.t — **Hard**. Timeout — LTM (longest
  token matching) over many alternatives is not implemented efficiently.
- roast/S05-nonstrings/basic.t — **unpassable as written**. Reference rakudo on
  this box fails to compile it: `No such symbol 'Antelope'` (`<.isa(::Antelope)>`)
  plus "Useless declaration of a has-scoped method in mainline" for the
  mainline `regex monster {...}` declarations. Do NOT attempt.
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
- roast/S32-io/io-handle.t — **Hard now** (27/30). Easy wins landed: `.say` of a
  collection now honors per-element custom `.gist` (incl. type objects), nested
  `with`/`given` no longer clobbers the outer topic source var, and strict ASCII
  decode throws. The remaining 3 subtests (23 `.print-nl` reuse + `.nl-out=`,
  29 `.WRITE`, 30 `.EOF/.WRITE`) all require a **user-subclassable IO::Handle with
  polymorphic READ/WRITE/EOF** so the high-level read/write methods dispatch into
  user-overridden `method READ/WRITE/EOF`. Substantial feature, not an edge case.
- roast/S32-io/io-path.t — **Medium per-fix, low-ROI overall**. 6 independent
  top-level failures. Test 31 `.gist` depends on a **Rakudo internal caching quirk**:
  Win32 `.gist` renders the backslash form only *after* `.absolute` has been called
  on that object (the test does `.absolute` then `like .gist, /$abs/`). Do NOT
  replicate mutate-on-`.absolute`. Others are each doable but the file can't
  whitelist while 31 needs the quirk: `.parts` Win32 split (33), X::Assignment::RO
  on `.SPEC=`/`.CWD=` + `temp $*SPEC`/`temp $*CWD` (34, 35), `.path`
  indir-independence (36), `.Numeric` Cool chain (37 — most tractable: IO::Path is
  Cool, numify via `.Str`).
- roast/S32-io/io-path-cygwin.t — **DONE** (whitelisted). IO::Path::Cygwin: UNC
  `//server/share`, drive letters `A:`, backslash separators in
  volume/dirname/basename/is-absolute, plus cygwin absolute/relative.
- roast/S32-io/lock.t — **Hard**. `.lock`/`.unlock` throw X::Method::NotFound.
  Needs **fcntl POSIX record locks** (`F_SETLK`/`F_SETLKW`, `F_RDLCK`/`F_WRLCK`),
  NOT `flock(2)` — rakudo rejects an exclusive lock on a read-only fd via fcntl
  `EBADF` (which `flock` would allow). Also tests cross-process blocking via
  subprocess `is_run`, `IO::CatHandle`, and fd-reuse (`native-descriptor`).
  Timing-sensitive.
- roast/S16-io/words.t — **DONE** (whitelisted). Lazy word iterator with
  close-on-exhaust + `IO::ArgFiles.new(@files)`.

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

- roast/S12-methods/qualified.t — **Hard (MOP)**. 6/7; the one remaining subtest
  ("parameterizations and inheritance") throws during setup on parameterized
  roles (`$?ROLE`/`$?CLASS`, `R1[::T]` qualified dispatch, role-private classes),
  not "callsame in punned roles" as previously labeled.
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
- roast/S02-literals/allomorphic.t — **Medium/Hard**. 1/119 fail (test 107
  `.ACCEPTS`). Root cause is NOT ACCEPTS (that works) but same-named lexical-class
  redeclaration: the test declares `my class IntFoo` in two separate `gather`
  blocks; mutsu keys classes by name in a single global `self.classes` map, so the
  second registration clobbers the first, and instances created from the first
  dispatch `.Numeric` to the wrong (second) class. A proper fix needs per-decl
  class identity (mirroring the existing `role_id`/`role_candidates` machinery for
  roles), touching class storage + dispatch + `.^name` display.
- roast/S02-types/pair.t — **Hard (container identity)**. 3/180 fail. Tests
  128/139 need `$pair.value` to alias the original variable's container; only
  test 171 (typed-assign throw) is independent. See "Do NOT pick" above.
- roast/S03-buf/write-int.t — **Hard (large but mechanical)**. 2530 tests. The
  128-bit read/write and dynamic interpolated method names already work; the file
  aborts in *setup* on two Whatever-currying bugs (see the Tier 1 entry for the
  exact reproductions). Fix Whatever-currying first.
- roast/S04-blocks-and-statements/temp.t — **Medium**. 30/37 (7 remaining): `temp`
  restoration interacting with hash/array element containers (container identity).
- roast/S04-declarations/constant.t — **Medium**. 65/72 (24/26 fixed in #2893).
  Remaining: lazy-seq `fib[100]` (46), `G::c` qualified name via constant alias
  (44), multi-candidate sharing (69+). NOT whitelistable until the lazy-seq lands.
- roast/S06-advanced/return_function.t — **Medium**. Aborts at test 1 (planned 4):
  `return` via named-argument binding to a routine returns the wrong value.
- roast/S06-advanced/return-prioritization.t — **not whitelistable as written**.
  Reference rakudo on this box also dies at test 5 (only 4 ok then a die in
  `LEAVE`-with-`return`). 2/11 fail in mutsu; do NOT target a whitelist.
- roast/S06-advanced/wrap.t — **Hard**. 12/70 fail then aborts (planned 90): `.wrap`
  lexical visibility — the wrapper closure can't see the wrapped routine's lexicals
  (closure capture / container identity).
- roast/S06-signature/slurpy-params.t — **Medium (multi-feature)**. Aborts at
  test 43 (planned 86): the `+@`/`+foo` single-argument rule over ranges/lists,
  plus a Junction `*@a` slurpy that must not autothread (34/35).
- roast/S32-list/skip.t — **Medium**. Plan mismatch: planned 55, ran 206 (the file
  loops more than planned, so subtest counting/laziness is off); 29 fail.
