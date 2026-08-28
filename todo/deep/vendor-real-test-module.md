# Vendor rakudo's real `Test` module — measured as reachable, 9 thin `nqp::` ops away

mutsu provides `Test` natively (`src/runtime/test_functions.rs`); `use Test` is
intercepted in `runtime_module.rs`. Of the native providers surveyed on
2026-08-01 (`docs/batteries/pod-to-text.md`), `Test` is the one that is actually
within reach — unlike `NativeCall`
(`todo/deep/nativecall-cannot-be-vendored.md`), which is not.

**This file grows by accretion and the sections below are in chronological
order, so most of them are historical. Read the LAST section first** — it
carries the current measurement and the current blocker list. As of 2026-08-20:
the module is vendored and driven by `MUTSU_REAL_TEST=1`, 76 of 1436 whitelisted
roast files and 18-20 `t/` files still regress under it.
`scripts/test-module-sweep.sh`'s pass predicate has now been fixed (it
classifies on exit status, not just a text grep) — see the last section.

## What was measured

Upstream `rakudo-2026.06/lib/Test.rakumod`: 953 lines, 90 `nqp::` references,
**11 distinct ops**. Copied to `Test2.rakumod` (only `unit module Test;` renamed,
to bypass mutsu's `use Test` interception) and run:

```
$ mutsu -I <dir> -e 'use Test2; say "loaded"'
loaded
```

**It parses and it loads.** That already puts it in a different category from
`NativeCall` and `experimental`, neither of which parses. Calling into it is
where it stops:

```
$ mutsu -I <dir> -e 'use Test2; plan 1; ok 1, "x"'
Unsupported nqp:: op: nqp::getstdout
  in sub _init_io at Test2.rakumod line 44
  in sub plan at Test2.rakumod line 100
```

Missing ops — **9 of 11**, all thin. Every one of them is used in exactly one or
two places, so the semantics `Test` actually depends on are narrow:

| op | `Test.rakumod` use site | what it needs |
| --- | --- | --- |
| `getstdout`, `getstderr`, `setbuffersizefh` | 44-45, `sub _init_io` — `nqp::setbuffersizefh(nqp::getstdout(), 0)` | the process's standard handles as VM handles, and unbuffering them (TAP ordering) |
| `can` | 743-744 — `try $obj.raku if nqp::can($obj, 'raku')` | does this object have this method |
| `eqaddr` | 160-161 — `nqp::eqaddr($expected.WHAT, Mu)` | object identity (used on type objects) |
| `join`, `split` | 461-462 and 772-773 — `nqp::join("\n$indents# ", nqp::split("\n", ...))` | string join/split over an nqp list, for `diag` indentation |
| `time`, `time_n` | 117-118 — `$time_before = nqp::time` | integer / float wall clock |

Present already: `istype`, `iseq_i`.

Implementation sites on the mutsu side: the `nqp::` arms live in
`src/runtime/builtins.rs` (`"nqp::atkey"`, `"nqp::sha1"`, `"nqp::decont"`, ...),
and the "unimplemented op must not alias a builtin" guard that produces the
error above is `src/runtime/builtins_operators_fallback.rs:967`. Note `join`,
`split`, `time` and `can` all collide with Raku builtins of the same short name
— that guard exists precisely so they fail loudly instead of silently answering
with Raku semantics, so implement them as real full-name arms, not by relaxing
the guard (see `news/2026-07/qualified-call-no-longer-aliases-a-builtin.md`).

None is in the hard tiers from
`news/2026-07/nqp-op-layer-measured-and-rejected.md` (no thunk-taking control
structures, no P6opaque representation ops). This is tier A plus a small amount
of handle plumbing.

## Why it is still not a casual change

Every `t/*.t` file and every roast file goes through `Test`. Swapping the
implementation swaps the foundation the entire suite stands on, so a subtle
difference in `is`/`is-deeply`/`todo`/`subtest` semantics shows up as thousands
of diffs at once. Sequence it deliberately:

1. ~~Implement the 9 ops, each with a `t/nqp-*.t` pin. They are independently
   useful — `getstdout`/`getstderr`/`eqaddr`/`can` show up in other real
   dists.~~ **DONE** — `src/runtime/nqp_ops_process.rs`, pinned by
   `t/nqp-process-ops.t`
   (`news/2026-08/nqp-process-ops-for-the-real-test-module.md`). One extra fix
   was needed: a no-paren zero-arg `nqp::` term was special-cased for
   `gethostname` alone, so bare `nqp::time` — written on ~40 lines of
   `Test.rakumod` — raised "Could not find symbol '&time' in 'nqp'". With the
   ops in, `plan`/`ok`/`is`/`isnt`/`is-deeply`/`subtest` from the unmodified
   upstream file all produce correct TAP.
2. Vendor `Test.rakumod` verbatim to `modules/Rakudo-Core/lib/Test.rakumod` but
   **do not** remove the interception yet; exercise it against a representative
   sample of `t/` and roast. **IN PROGRESS** — the file is now vendored (md5
   `f34dec45d52ad099c37f42fdbd93e277`, `news/2026-08/test-module-vendored-behind-a-switch.md`)
   and driven by **`MUTSU_REAL_TEST=1`**, which replaced the throwaway
   `unit module Test2;` rename; the sweep is `scripts/test-module-sweep.sh`.
   The exercise has been run both by hand (see "Where the alias stands" below)
   and as a bulk sweep over a 1-in-9 sample of `t/` (see "Bulk sweep" below).
   Seven general interpreter bugs found and fixed; **every assertion of the
   unmodified upstream module now runs**, `cmp-ok` included
   (`news/2026-08/caller-lexical-indirect-operator-lookup.md`). What is left
   before step 3 is the pass over the test files that lean on mutsu's lenient
   native `is` (`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`) and a
   *full* sweep (`tmp/sweep.sh 1`) rather than the 1-in-9 sample.
3. Only then flip `runtime_module.rs`, and expect the first full `make roast` to
   be the real review.
4. `Test::Util` (roast's helper, `roast/packages/Test-Helpers/`) is a separate
   thing and already loaded from source — check it still composes.

Do not start this in the same PR as anything else.

## Where the alias stands (2026-08-01)

Driving the unmodified upstream file under the `Test2` alias, **everything in
the happy path already works**: `plan`, `ok`, `nok`, `is`, `isnt`, `is-deeply`,
`like`, `unlike`, `isa-ok`, `does-ok`, `can-ok`, `dies-ok`, `lives-ok`,
`is-approx`, `eval-dies-ok`, `eval-lives-ok`, `throws-like`, `subtest`, `todo`,
`skip`, `pass`, `done-testing` — including nested subtests and the outer
counter surviving them, and a *failing* assertion now reports its location
exactly as raku does. Six general bugs were found and fixed getting there:

| what | fix |
| --- | --- |
| the `nqp::` ops it needs, and bare `nqp::time` in term position | `news/2026-08/nqp-process-ops-for-the-real-test-module.md` |
| mutsu's native Test provider overruling the module's own routines | `news/2026-08/imported-test-routines-beat-the-native-provider.md` |
| `proclaim !($got ~~ $rx), $desc` losing its argument list (forward-declared sub, prefix-`!` argument) | `news/2026-08/listop-argument-may-start-with-a-boolean-prefix.md` |
| `@vars.push: item [...]` dropping the array, so every `subtest` restored garbage | `news/2026-08/item-is-a-listop.md` |
| the module's `END` reading `$num_of_tests_run` at its registration-time value, so the plan check reported "You planned 9 tests, but ran 6" on a file that had emitted all nine `ok` lines | `news/2026-08/end-phaser-sees-live-lexicals.md` |
| `$?FILE` inside a module naming the script, and `callframe` reporting the same, so `proclaim`'s location walk ran off the end of the stack and every *failing* assertion died on `Any.file` | `news/2026-08/module-file-var-and-callframe.md` |
| `&CALLER::LEXICAL::("infix:<$op>")` splitting into two statements, so `cmp-ok` could not turn its string operator into a callable | `news/2026-08/caller-lexical-indirect-operator-lookup.md` |

## Bulk sweep (2026-08-01)

The by-hand exercise above finds what the happy path needs; a bulk sweep finds
what the *distribution* of real test files needs. Method: rewrite each `t/*.t`
file's `use Test;` to `use Test2;`, run it with `-I tmp/core`, and classify the
first failing line — over a 1-in-9 sample, 301 of 2704 files. Throwaway scripts:
`tmp/core/classify.sh` (alias run + signature) and `tmp/core/sweep.sh` (diffs the
aliased run against the same file's native-`Test` run, which is what you want
when a signature is ambiguous).

**198 / 301 files fully clean** at the start of the sweep, **255 / 301** after
the END-phaser fix. The residue lined up exactly with the two tickets open at
that point — 30 files on the `$?FILE`/`callframe` failure-report path and 7 on
`No such method 'file'` — plus ~8 single-file `not ok`s that are ordinary
pre-existing gaps rather than `Test` differences.

### Re-run after the `$?FILE`/`callframe` fix (2026-08-01)

Same 1-in-9 sample, with `news/2026-08/module-file-var-and-callframe.md` in.
Scripts this time: `tmp/sweep.sh` (runs each sampled file twice from the same
path — verbatim, and with `use Test;` rewritten to `use Test2;`) and
`tmp/sweep-analyze.sh`. Note the analysis deliberately does **not** compare the
two runs byte-for-byte: the real module is routinely *more* faithful than the
native provider (richer `throws-like` subtests, `'<code>' died` descriptions
instead of `code dies`), so the question is whether a file that passed still
passes.

| | files |
| --- | --- |
| pass under both | **285 / 301** |
| regress under the real module | 15 |
| fail under both (pre-existing) | 1 |

None of the 15 is an unfixed `Test` incompatibility:

- **6 files** assert against mutsu's *lenient* native `is` — `is Point.WHAT,
  '(Point)'` and `is $fh.lines, 'A B C'` fail under `raku` too. These are test
  files to correct, tracked in
  `todo/tickets/local-tests-rely-on-a-lenient-native-is.md` (~50 files suite-wide
  at this sample rate).
- **4 files** hit `callframe` reporting a frame's `.file` and `.line` from
  different sources (`# at t/foo.t line 666` for a 106-line file), all through
  `throws-like` — `todo/tickets/callframe-line-and-file-come-from-different-frames.md`.
  The rendered output is byte-identical to before the `$?FILE` fix, so this is
  the remaining half of that problem rather than a regression.
- The rest are ordinary single-assertion gaps unrelated to `Test`
  (`orelse` short-circuiting inside a listop argument, `nextsame`+`where`
  ordering, a `:v<>` version adverb import).

`cmp-ok`, the last blocked assertion, was fixed on 2026-08-01 —
`news/2026-08/caller-lexical-indirect-operator-lookup.md`. Its output is now
byte-identical to `raku`'s, failing assertions included.

### Full sweep, all 2717 files (2026-08-01)

The sample above is superseded. `tmp/sweep-full.sh` is the parallel (`xargs -P`)
variant of `tmp/sweep.sh` with no stride; the analysis is the same
`tmp/sweep-analyze.sh`. Two further scripts split the residue:
`tmp/sweep-classify.sh` reports each regressed file's **first** failing line (a
plain diff is misleading here — the real module emits *extra* `ok` lines, so the
first `>` line of a diff is usually noise), and `tmp/sweep-raku-check.sh` runs
each regressed file under `raku` to say whether the file itself is wrong.

| | before the typed-exception fix | after |
| --- | --- | --- |
| pass under both | 2617 | **2641 / 2717** |
| regress under the real module | 86 | **64** |
| passes only under the real module | 1 | 1 |
| fail under both (pre-existing) | 13 | 12 |

Splitting the 86 by `raku`'s own verdict: **29 files `raku` also fails** (the
test file is wrong — mostly the lenient-`is` shapes of
`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`, though a few are
files `raku` cannot even parse because they exercise mutsu-specific syntax, so
that bucket is not purely lenient-`is`), and **57 files `raku` passes** — real
mutsu gaps that only the strict module exposes.

Of those 57, **29 were one root cause**: a compile-time error arriving as
`X::AdHoc` because its class was spelled only inside the message text.
`news/2026-08/typed-exception-class-from-the-message-convention.md` fixed 20 of
them. The other 9 are errors whose message names no class at all (`Confused.
parse error at …` where raku raises `X::Syntax::Malformed` /
`X::Bind::Slice` / …) — worth its own pass.

### The exception-class residue is closed (2026-08-01)

That pass is done. The 9 split into three unrelated causes and all but one are
fixed:

| cause | files | fix |
| --- | --- | --- |
| `X::Bind::Slice` was never registered, so its own `.new` did not exist | 2 | `news/2026-08/bind-slice-is-a-real-exception-class.md` |
| a parse failure had no class at all — the parser's generic `Confused. parse error at …` | 5 | `news/2026-08/parse-failures-carry-a-syntax-exception-class.md` |
| a bare call statement did not sink its value, so a string-form `throws-like` never threw | 1 | `news/2026-08/statement-call-sinks-its-value.md` |

The one file left over (`t/block-lexical-scope.t`) is not an exception-classing
bug at all: it wants `X::Undeclared::Symbols ~~ X::Undeclared`, i.e. the
unregistered-hierarchy problem of
`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`.

### Re-measured after the parse-failure classes (2026-08-01)

| | before | after |
| --- | --- | --- |
| pass under both | 2644 | **2652 / 2722** |
| regress under the real module | 62 | **57** |
| passes only under the real module | 1 | 1 |
| fail under both (pre-existing) | 12 | 12 |

Splitting the 57 by `raku`'s own verdict: **27 files `raku` also fails** (test
files to correct — `todo/tickets/local-tests-rely-on-a-lenient-native-is.md`)
and **30 files `raku` passes** (real mutsu gaps). Two systemic causes stand out
in that 30 and are worth taking before the individual gaps:

- **A class declared inside an EVAL'd code string is named after the *module*
  that called EVAL.** `throws-like 'class Foo { ... }', X::Attribute::Undeclared`
  reports `.package-name` as `Test2::Foo`, and `composition-not-composable.t`
  gets `.target-name` `Test2::B`. `EVAL ..., context => CALLER::` has to compile
  in the caller's package, not the module's — the same `current_package` family
  as `news/2026-08/nested-type-short-name-owner-scope.md`.
- **Exception classes raku names that mutsu does not raise**:
  `X::Role::Initialization` (arrives as `X::Undeclared::Symbols`),
  `X::Syntax::Augment::Illegal` (arrives as `X::Augment::NoSuchType`), and
  `X::Phaser::PrePost` with an empty `.message`.

### Re-measured after the lenient-`is` pass (2026-08-01)

| | at the start of the day | now |
| --- | --- | --- |
| pass under both | 2617 | **2675 / 2725** |
| regress under the real module | 86 | **37** |
| passes only under the real module | 1 | 1 |
| fail under both (pre-existing) | 13 | 12 |

The 37 split **6 that `raku` also fails** and **31 real gaps**. The test-file
bucket is essentially exhausted: what remains in it
(`begin-phaser-begintime.t`, `method-private-errors.t`,
`listop-arg-loose-logical-precedence.t`, `placeholder-named-in-method-do.t`,
`use-version-short-adverb.t`, `vm-panic-boundary.t`) fails under `raku` for
reasons unrelated to `is`'s leniency — mutsu-specific syntax, a module `raku`
cannot find — and is listed for individual triage in
`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`.

### Six of those causes closed the same day

| cause | fix | files it freed under the alias |
| --- | --- | --- |
| a bare type name did not resolve through its enclosing packages in call position or `augment` | `news/2026-08/bare-type-name-under-a-package.md` | `role-initialization.t`, `augment-role-anon.t`, `augment-nosuchtype.t`, `eval-type-decl-and-phaser-message.t` |
| a `try` re-caught a `Failure` something had already handled | `news/2026-08/try-does-not-recatch-a-handled-failure.md` | — |
| `use fatal` leaked out of an `EVAL`, so one `throws-like 'use fatal; …'` poisoned the rest of the file | `news/2026-08/eval-does-not-leak-use-fatal.md` | `statement-call-sinks-its-value.t` |
| `=begin` at end of input was not a Pod directive | `news/2026-08/pod-begin-at-end-of-input.md` | `pod-begin-without-identifier.t` |
| a type declared in EVAL'd code counted as an undeclared routine; `X::Phaser::PrePost` had an empty `.message` | `news/2026-08/eval-type-decls-and-prepost-message.md` | `phaser-prepost.t` |
| 40 assertions in 19 files asserted against the lenient native `is` | `news/2026-08/test-files-asserted-against-a-lenient-is.md` | 19 files |

Open systemic causes, in the order worth taking them:

1. ~~`todo/deep/eval-context-argument-is-ignored.md`~~ **DONE** —
   `news/2026-08/eval-context-argument.md`. The `CALLER::` pseudo-stash now
   records the package of the frame it was taken from and `EVAL`'s `context`
   argument compiles the snippet there, so an EVAL'd snippet's own types are no
   longer named after the calling module. Freed `attribute-undeclared.t` and
   `composition-not-composable.t` under the alias; pin
   `t/eval-context-package.t`.
2. ~~`todo/tickets/user-trait-mod-multi-shadows-builtin-traits.md`~~ **DONE** —
   a user `trait_mod:<is>` that matches nothing keeps the builtin trait (#5689).
3. **`todo/tickets/use-fatal-leaks-out-of-a-sub-or-do-block.md`** — the same
   pragma still leaks out of a routine body and a `do {}` block; the `EVAL` half
   is done.
4. **`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`** —
   `X::Undeclared::Symbols ~~ X::Undeclared` and friends. Blocks
   `block-lexical-scope.t`.

Re-run `scripts/test-module-sweep.sh` to re-measure after each.

### Re-measured under the real switch, not the alias (2026-08-02)

With `news/2026-08/eval-context-argument.md` and the vendoring in
(`news/2026-08/test-module-vendored-behind-a-switch.md`):

| | at the start of 2026-08-01 | now |
| --- | --- | --- |
| pass under both | 2617 | **2693 / 2732** |
| regress under the real module | 86 | **26** |
| passes only under the real module | 1 | 1 |
| fail under both (pre-existing) | 13 | 12 |

The 26 split **6 that `raku` also fails** — `listop-arg-loose-logical-precedence.t`,
`use-version-short-adverb.t`, `begin-phaser-begintime.t`,
`method-private-errors.t`, `vm-panic-boundary.t`,
`placeholder-named-in-method-do.t`, all in
`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`'s individual-triage
list — and **20 real mutsu gaps**:

```
bigrat-sort-compare.t      block-lexical-scope.t     emit-done-controlflow.t
error-reporting-quality.t  group-of.t                io-cathandle-lazy.t
is-lazy-io-lines.t         leave-in-if-branch.t      multi-where-otf-dispatch.t
proxy-list-transparency.t  subscript-adverbs.t       throws-like-gather-sink.t
undeclared-when-type.t     whatever-code-fixes.t     handles-proto-dispatch-mut-invocant.t
gate-b-callee-name-collision-and-deref-capture.t     module-file-var-and-callframe.t
qualified-call-does-not-alias-builtin.t              test-assertion-line-number.t
throws-like-outer-var-writeback.t
```

Several of the last group are mutsu's *own* pins, written against the native
provider's wording or its `throws-like` shape; those are files to re-point at
the real module rather than interpreter gaps. Triage each before assuming a bug.

Exit status was measured for the first time and is already faithful — a failing
assertion exits 1, a short plan exits 255 — which is what `prove` reads, so
step 3 does not need work there.

### Triaged, 15 left (2026-08-02), then 13, then 12

Three of the 15 are fixed (rows struck through below) and two more are triaged
to their own deep tickets, leaving **12**. The lesson repeated twice in one day:
a row labelled "the pin asserts native-provider behaviour, re-point the test
file" was wrong *both* times — run the file under `raku` before believing such a
label.

`module-file-var-and-callframe.t` now passes, and so does `leave-in-if-branch.t`
— its `@events = ()` between assertions was silently dropped because the real
module's `is-deeply` is a *module routine*, making `is-deeply @events, [...]` a
statement call with a named argument, which severed the caller's container cell
(`news/2026-08/named-arg-statement-call-keeps-the-caller-cell.md`, pin
`t/named-arg-stmt-call-keeps-caller-cell.t`). So does
`throws-like-outer-var-writeback.t` — the real `throws-like` EVALs its string
argument, and an EVAL'd `my` clobbered a same-named caller lexical (it was NOT
a native-provider-shaped pin after all;
`news/2026-08/eval-my-stays-scoped-to-the-eval.md`, pin
`t/eval-my-shadows-caller-lexical.t`). Those two fixes also freed
`handles-proto-dispatch-mut-invocant.t` and `multi-where-otf-dispatch.t`
without further work. The other 15 all read
`native / real=FAIL / raku` — rakudo runs the *same module* over the same file
successfully, so the difference is genuinely in mutsu.

| group | files | note |
| --- | --- | --- |
| ~~exception-class hierarchy~~ | ~~`block-lexical-scope.t`, `gate-b-callee-name-collision-and-deref-capture.t`~~, `undeclared-when-type.t` | **not one cause, and not the hierarchy ticket.** The first two were mutsu raising the *wrong class*: an undeclared variable read and a call to a CORE term constant both answered `X::Undeclared::Symbols` where raku answers `X::Undeclared` (the two are unrelated — `X::Comp` is a role, not a superclass — so registering one under the other would have been wrong). Fixed: `news/2026-08/undeclared-variable-is-not-undeclared-symbols.md`. `undeclared-when-type.t` is separate: `when SomeUndeclaredType` is `X::Comp::Group` in raku (a *parse* failure, "needs parens to avoid gobbling block") and `X::Undeclared::Symbols` in mutsu |
| ~~the pin asserts *native-provider* behaviour~~ | ~~`qualified-call-does-not-alias-builtin.t`~~, ~~`test-assertion-line-number.t`~~ | **the first was mislabelled and the label's reasoning was wrong.** "Under the real module `Test::ok` legitimately exists" is not true: rakudo's `Test.rakumod` declares `multi sub ok(...) is export`, a *lexical*, so `raku -e 'use Test; Test::ok(1,"q")'` says `Could not find symbol '&ok' in 'Test'` exactly as the pin asserts. mutsu leaked every module's lexical `sub`/`multi sub` under its package name. Fixed: `news/2026-08/module-sub-is-not-a-package-symbol.md`. **Verify a "re-point the pin" label against `raku` before believing it**. `test-assertion-line-number.t` was mislabelled the same way: `raku` reports the *caller's* line for a `is test-assertion` helper and mutsu reports the helper's own, because `is test-assertion` is a parser flag in mutsu and the real module reads it back through the MOP (`$r.^mixin(role …)` + `nqp::can`). Not a pin to re-point either — `todo/deep/test-assertion-trait-is-not-introspectable.md` |
| deferred `Seq` reification destroys the value | `is-lazy-io-lines.t` | `todo/deep/deferred-seq-materialization-destroys-the-original.md` — the real `is` opens with `$got.defined`, which guts a lazy `.lines` |
| individual gaps | `bigrat-sort-compare.t` (`cmp-ok` calls `infix:«<»` as a *routine value*; FatRat vs Num answers differently there than the compiled operator), `proxy-list-transparency.t` (`is-deeply` does not FETCH `Proxy` list elements — reports `$(Proxy, Proxy)`), `emit-done-controlflow.t`, `error-reporting-quality.t`, `group-of.t` (`is-deeply` reports "planned 2 tests, but ran 0" inside a subtest), `io-cathandle-lazy.t` (no longer aborts — `.cache` on a lazy Seq answering `Seq` instead of `List` was fixed in `news/2026-08/lazy-seq-cache-list-name.md`; 2 of 9 subtests still fail because `IO::CatHandle.handles` is itself wrongly lazy/`Array`, `todo/tickets/cathandle-handles-wrongly-lazy-array.md`), `subscript-adverbs.t` (**not a `Test` difference and not even about the closure**: `(@a[0]:p).value = 'x'` builds a *snapshot* Pair, so the write has to find the array by scanning `self.env` — and the file's own later `{ my @a = … }` block flips the first block from `PushBlockFrame` to `BlockScope`, which puts `@a` in a local slot where the scan cannot see it. `todo/deep/subscript-p-pair-is-a-snapshot-not-a-container.md`), `throws-like-gather-sink.t` (+ part of `emit-done-controlflow.t`: `todo/deep/eval-context-frame-owns-the-return-target.md`), `whatever-code-fixes.t` | one at a time |

`tmp/recheck.sh <file>.t …` runs the named files native / real / raku and is the
per-file tool; `scripts/test-module-sweep.sh` is the full measurement. Note the
per-file harness must create a `tmp/` under its working directory — several of
these files spurt fixtures there, and without it they die before their first
assertion and read as a false pass.

## Blocker found while doing step 1: the native provider shadows an import — FIXED

**Resolved** (`news/2026-08/imported-test-routines-beat-the-native-provider.md`,
pin `t/test-fn-import-shadow.t`): an imported routine now beats the native
provider for the `Test` module's own export list, so step 2's temporary alias
runs its own routines. The `Test::Util` / `Test::Tap` half of the same rule is
deferred to `todo/tickets/retire-native-test-util-overrides.md`, which does not
block this ticket. Kept below because the symptom is so easy to misread.

Step 2's "exercise it under a temporary alias" did not work, and the
reason was a general bug rather than anything about `Test`. `exec_call`
(`src/runtime/calls.rs:301`) dispatches every name in
`is_test_function_name()` to `call_test_function` **before** user-routine
resolution and **without any gate at all** — not even the
`loaded_modules.contains("Test")` gate its sibling in
`builtins_operators_fallback.rs:230` applies. So a module that exports its own
`ok`/`is`/`plan`/... is silently overruled by mutsu's native TAP routines:

```
$ mutsu -I tmp/core -e 'use Test2; plan 3; ok 1, "first"; zlike("hi", /h/, "z"); ok 1, "third"'
1..3            # module's plan
ok 1 - first    # mutsu's NATIVE ok  <-- wrong routine
ok 1 - zlike    # module's zlike -> module's proclaim, its own counter at 1
ok 2 - third    # native ok again
```

The two implementations then keep separate counters, which looks exactly like a
stale module lexical and costs an hour to misdiagnose — the tell is that the
module's own `proclaim` is entered only once. `like`/`unlike` are not affected
only because their argument shapes make the native handler decline.

The fix was the rule from
`news/2026-07/qualified-call-no-longer-aliases-a-builtin.md`: decide on
*whether a declaration exists*, not on whether the name is a builtin. With
`use Test` intercepted natively there is no declaration to compete with, so the
guard cannot regress the ordinary path.

## Reproducing the measurement in one minute

```bash
R=<rakudo-2026.06 source tree>          # or re-download the release tarball
mkdir -p tmp/core
python3 - <<PY
s = open("$R/lib/Test.rakumod").read()
open('tmp/core/Test2.rakumod','w').write(s.replace('unit module Test;','unit module Test2;',1))
PY
printf 'use Test2;\nplan 1;\nok 1, "x";\n' > tmp/core/t.raku
timeout 30 target/debug/mutsu -I tmp/core tmp/core/t.raku
```

The rename is only to bypass mutsu's `use Test` interception in
`runtime_module.rs` — nothing else in the file is touched. Today this prints
`Unsupported nqp:: op: nqp::getstdout`, and each op implemented moves the error
further down the file. When it runs clean, step 2 above is done.

Beware when re-measuring which ops are missing: **the shell here is zsh**, which
does not word-split a plain `$var`, so `ops=$(...); for op in $ops` silently
iterates once. Use `... | while read op` or `$(...)` directly in the `for`.

## Step 3 is much further away than the `t/` sweep implies: roast, measured (2026-08-02)

Every measurement above is over `t/`. **roast had never been run under
`MUTSU_REAL_TEST=1`**, and roast is the larger consumer — 1435 whitelisted
files, all of them standing on `Test`. Measured on `fdc4ea69d` (release build,
`prove -j6`, the same per-file timeouts `make roast` uses):

```bash
MUTSU_REAL_TEST=1 MUTSU_BIN=target/release/mutsu \
  prove -j6 -e 'scripts/run-roast-test.sh' $(cat roast-whitelist.txt)
```

| | files |
| --- | --- |
| whitelisted (all pass under the native provider — `main` is protected) | 1435 |
| **regress under the real `Test`** | **343** |

So step 3 cannot be "flip `runtime_module.rs` and expect the first `make roast`
to be the review": that flip regresses 24% of the whitelist. The `t/` residue
(21 files, of which 5 `raku` also fails) is not a proxy for it.

### What the 343 are

Split by whether the file loads a roast helper module:

| | files |
| --- | --- |
| uses `Test::Util` or `Test::Tap` (269 whitelisted files do) | 159 |
| uses neither | 184 |

**The helper-module intercept is not the dominant cause.** Flipping
`user_test_decl_beats_native` from `is_test_module_export` to
`is_test_function_name` — the one-line retirement in
`todo/tickets/retire-native-test-util-overrides.md` — was measured on top of
`MUTSU_REAL_TEST=1`: **343 → 315** (32 files fixed, 4 newly broken). Worth
having, but it is a tenth of the problem, not the problem.

The 184 non-helper files are ordinary interpreter gaps that only the strict
module exposes, in the same distribution the `t/` sweep found: 30 abort
mid-file with a plan mismatch (`# You planned N tests, but ran M`, each from its
own cause — an unresolved symbol, `skip` rejecting a non-integer count, …), the
rest are single assertions.

### `Test::Tap` is a second native provider, and retiring it is its own slice

`use Test::Tap` is a native no-op (`runtime_module.rs`), exactly like `use Test`
— the real `roast/packages/Test-Helpers/lib/Test/Tap.rakumod` is never loaded,
and `tap-ok` is answered by `src/runtime/test_functions/tap_ok.rs`. That is why
44 whitelisted files (the `S17-supply` cluster) report `You planned N tests, but
ran 0` under the real `Test`: the native `tap-ok` and the module keep separate
counters. It is *not* the `user_test_decl_beats_native` guard — `tap-ok` is
already in `TEST_MODULE_EXPORTS`, so the guard is consulted and correctly
declines only because no declaration exists to find.

Deleting the `module == "Test::Tap"` arm makes all 44 load the real module, and
`elems.t` then passes under both providers. But **6 of the 44 regress under the
*native* provider** once the real `tap-ok` runs, because it asserts with the
real `is-deeply` where mutsu's native `tap-ok` was lenient:

| file | what the real `tap-ok` exposes |
| --- | --- |
| `S17-supply/rotor.t` | `Supply.rotor` emits `List`s where rakudo emits `Array`s — `[(1,2,3)]` vs `[[1,2,3]]` |
| `S17-supply/classify.t`, `categorize.t`, `interval.t`, `merge.t`, `reduce.t` | the tap callback's `@res.push($_)` collects **nothing** (`got: []`) when the emit runs on a timer/scheduler thread |

Both are real bugs (`raku` passes all six), so the retirement is a slice that
has to fix them first — see `todo/tickets/retire-native-test-tap.md`.

### The order this implies

1. Land the individual interpreter gaps the strict module exposes — the 184
   non-helper roast files and the `t/` residue are the same kind of work.
2. ~~`todo/tickets/retire-native-test-tap.md` (44 files, 6 real bugs behind
   it).~~ **DONE** — `news/2026-08/retired-native-test-tap.md`. All 44 pass with
   the real `Test::Tap` and the intercept is gone. Seven general fixes were
   needed, none of them the cause the ticket predicted: five live-`Supply`
   combinator gaps (`news/2026-08/live-supply-combinators.md`), the
   scheduler-driven interval (`news/2026-08/scheduler-driven-supply-interval.md`),
   and a `&`-sigil *named* parameter that never bound at all
   (`news/2026-08/named-callable-parameter-binds.md`) — which is what made
   `tap-ok`'s own `:&emit`/`:&done`/`:&after-tap` guards silently skip.
3. ~~`todo/tickets/retire-native-test-util-overrides.md` (worth 32 roast files on
   top of the real `Test`; already 227/228 on its own `t`-side measurement).~~
   **DONE** — `news/2026-08/retired-native-test-util-overrides.md`. All 228
   whitelisted `use Test::Util` files pass with the guard widened. The last
   blocker was `S03-operators/repeat.t` test 56, and it was not a `Test::Util`
   difference: a warning raised from a plain arithmetic opcode never reached the
   `CONTROL` handler `warns-like` installs, and a leaf closure's return path then
   discarded the handler's writes
   (`news/2026-08/warn-from-a-non-call-op-reaches-its-control-handler.md`).
4. Only then step 3.

Re-measure with the command at the top of this section, not with
`scripts/test-module-sweep.sh` alone.

### Re-measured after the `Test::Tap` retirement (2026-08-02)

| | before | after |
| --- | --- | --- |
| regress under the real `Test`  | 343 | **301 / 1435** |
| of those, using `Test::Tap` | 44 | **2** |
| of those, using any helper module | 159 | 117 |

The two `Test::Tap` files left are `S17-supply/first.t` and
`S17-supply/interval.t`, and neither is a `Test::Tap` problem: both report *all*
subtests passing and then exit 255 on a plan mismatch at END — the real `Test`
module's own plan check, i.e. ordinary step-1 residue.

So the remaining work is what step 1 always was: the ~184 non-helper files plus
the helper-module residue, one general interpreter gap at a time.

### Re-measured after the compunit-lexical fix (2026-08-03)

The largest single identified cause in that residue is closed for scalars:
`news/2026-08/module-file-scope-lexical-is-not-the-callers.md`. A `unit`
compunit's file-scope `my` scalars now live in their own store instead of sharing
an env key with the loading scope, so a test file's own `my $output = ''` no
longer *is* `Test.rakumod`'s `$output`.

| | before | after |
| --- | --- | --- |
| regress under the real `Test` | 301 | **255 / 1435** |

18 files fixed against this sweep's own baseline of 271 (the 301 above predates
the `Test::Util` retirement). Two files appear only in the "after" list —
`6.d/S32-str/sprintf-e.t` and `S04-exception-handlers/catch.t` — and both pass
when run alone; they are `-j6` load artifacts, not regressions.

`@`/`%` compunit lexicals were deliberately left sharing the caller's env
(`Test.rakumod`'s `@vars` is the one that matters here) because every mutating
method resolves its receiver by name out of `self.env` — see
`todo/tickets/module-file-scope-array-and-hash-still-share-the-caller.md`.

**No single dominant cause is left.** The 255 split 110 mid-file aborts
(`Failed: 0` + a plan mismatch) and 145 files losing individual assertions, and
grouping the aborts by their last non-TAP line gives a long tail: the largest
group is 2 files (`Unknown role: CN`; `Did you mean 'flat'?`), and 71 abort with
no diagnostic line at all. ~~So from here it is one gap at a time rather than
another leverage play.~~ (Wrong — see "Stop concluding one-at-a-time" below.)
Two starting points worth naming:

### Re-measured 2026-08-03, and a quarter of the residue is a *timeout* class

Two fixes landed against the 255
(`news/2026-08/strict-does-not-reject-a-declared-bind.md`,
`news/2026-08/typed-exceptions-carry-their-attributes.md`), and the sweep was
re-run on `b9affee86`: **248 / 1435**. But re-running each of those 248 **alone**
with a 120 s timeout shows **64 of them pass** — so the honest count is

| | files |
| --- | --- |
| genuine failures | **185** |
| pass alone, fail under `prove -j6` | 64 |

That second bucket is not flakiness in the usual sense and must not be
quarantined: it is the real module's per-assertion cost. **10 of the 64 exceed
the 30 s per-file budget even with the machine to themselves** (22–61 s:
`S04-declarations/state.t`, `S03-buf/read-write-bits.t`, and the six
`sprintf-*.t`), and the other 54 are the same effect under contention.

Two different causes hide in there, and they need different answers:

- `S32-str/sprintf-d.t` — 0.9 s native, **22.2 s** real, **19.1 s raku**. mutsu
  is as fast as rakudo here; the file is simply 4565 interpreted `Test`
  assertions. Nothing to fix in the interpreter — the roast runner needs a
  bigger budget for these files once the switch flips.
- `S04-declarations/state.t` — 3.7 s native, **61.8 s** real, **0.9 s raku**, a
  67× deficit. Its `lives-ok { … for ^2000000 { $ = foo } }` takes 2.9 s under
  the native provider and 40.3 s under the module's, for byte-identical user
  code. Isolated: a block invoked through an imported sub costs ~1.5× more per
  iteration, and a callee declared *inside* that block costs another 1.7–3.8×.
  Recorded in `todo/perf/interpreter-call-path-in-hot-loops.md`; that is the
  real blocker of the two.

So step 3 needs the call path as well as the 185 correctness gaps.

### Named starting points (from the 2026-08-03 abort classification)

- ~~`S02-names/strict.t` / `S02-lexical-conventions/comments.t`~~ **DONE** —
  `news/2026-08/strict-does-not-reject-a-declared-bind.md`.
- ~~six files aborting on a missing exception attribute~~ **DONE** —
  `news/2026-08/typed-exceptions-carry-their-attributes.md`. One of that family
  is left: `X::Syntax::Pod::BeginWithoutIdentifier` has no `.filename`
  (`S32-exceptions/misc2.t`) — that one is the `X::Comp` file/line metadata
  rather than a per-class attribute. The other, "`X::Match::Bool` has no
  `.instead`" (`S24-testing/fails-like.t`), **was not an attribute gap at all**:
  rakudo has no `.instead` there either, and the test matches on `.message`.
  mutsu asked for `.instead` because `throws-like`'s `*%matcher` had been
  overwritten by the nested `fails-like`'s
  (`news/2026-08/slurpy-parameter-does-not-leak-to-the-caller.md`). A missing-method
  error inside `Test.rakumod` is as likely to be the *wrong value* as a missing
  method — check what the module thinks it is holding first.
- ~~`skip() was passed a non-integer number of tests`~~ — found: the file is
  `roast/S32-list/skip.t`, and it is not a `skip`-shape problem. The file
  deliberately imports selectively (`BEGIN my (&plan, …) = do { use Test; … }`)
  so that the *core* `skip` routine stays visible, and mutsu leaks a `use` inside
  a block into the enclosing scope, so `Test`'s `skip` answered instead. That is
  the general bug to fix (`use` is lexically scoped in raku); it is not on the
  `skip` implementation at all. The `env`-side half of that leak
  (`todo/tickets/use-inside-a-block-leaks-to-the-enclosing-scope.md`) is now
  fixed, but the file still does not reach the `skip()` call: with the leak
  gone, `plan 55;` reaching for the popped `Test::plan` proto/multi through the
  captured `&plan` reference stack-overflows instead — a separate, pre-existing
  bug, `todo/tickets/routine-value-self-recursion-after-import-scope-pop.md`.

### Classifying the 145 assertion-losers, and the clusters it found (2026-08-03)

The 185 genuine failures split 40 mid-file aborts and 145 files that merely lose
assertions. Classifying the 145 by their *first* `not ok` (`tmp/classify-assert.sh`,
run under `xargs -P4`) turns them into something workable — and one cluster
dominates:

| first failing assertion | files |
| --- | --- |
| `right exception type (X::…)` | **17** |
| everything else | 128, a long tail of one-offs |

Splitting that 17 by the class asked for: `X::Syntax::CannotMeta` 6,
`X::Comp::Group` 5, `X::Syntax::Missing` 4, `X::UnitScope::Invalid` 2,
`X::Syntax::NonAssociative` 2, then singletons.

**`X::Syntax::CannotMeta` and friends were not missing at all** — the parser had
already diagnosed the construct precisely and named the class in the message, and
two layers of error flattening buried it
(`news/2026-08/parse-error-keeps-its-exception-class.md`). Do not read a
"right exception type" failure as "mutsu does not raise that class" before
checking whether the diagnosis is already in the message text.

Landed against this residue on 2026-08-03:

| fix | what it freed |
| --- | --- |
| `news/2026-08/slurpy-parameter-does-not-leak-to-the-caller.md` — a callee's `*%slurpy` overwrote the caller's same-named binding, so `throws-like`'s `%matcher` came back holding `fails-like`'s | `S24-testing/fails-like.t` |
| `news/2026-08/parse-error-keeps-its-exception-class.md` — a classified parse diagnosis survives the "Confused." wrapper | `S03-metaops/not.t`, `S03-metaops/zip.t`, `S03-operators/is-divisible-by.t` |
| `news/2026-08/pair-subsignature-dispatch.md` — dispatch could not match `Pair (:key(…), :value(…))`, so `Test::Util`'s `group-of` lost to the native provider | `S03-metaops/cross.t`, `S03-operators/arith.t` (with the above) |
| `news/2026-08/missing-block-is-a-syntax-missing.md` — a required-but-absent block is `X::Syntax::Missing` | `S04-statements/if.t`, `S02-names/identifier.t` |

Still open in the named clusters:

- **`X::Comp::Group` (5 files)** — mutsu never groups compile-time errors, so a
  `throws-like …, X::Comp::Group` has nothing to match. Needs the sorrows/panic
  collection rakudo's `X::Comp::Group` carries, not a message tweak.
- **`X::Syntax::Missing`, the remainder** — `S04-statements/terminator.t` moved on
  to wanting `X::Syntax::Malformed` for `my $x =`, and
  `S02-lexical-conventions/minimal-whitespace.t`'s `@arr [0]` fails before any
  block alternative is reached (rakudo's "Missing block" there comes from a
  different rule).
- **`X::ControlFlow` (3 files: `S04-statements/do.t`, `redo.t`,
  `S04-blocks-and-statements/pointy.t`)** — a `next`/`last`/`redo` with no
  enclosing loop is a control *signal* in mutsu, which `try`/`CATCH` deliberately
  passes through, so it is uncatchable. Filed as
  `todo/deep/loop-control-signal-is-not-catchable.md`: the only correct fix is a
  dynamic loop-handler depth, and the sweep has to be complete to be safe.

### (superseded) the 2026-08-03 pre-fix notes

- `S02-names/strict.t` and `S02-lexical-conventions/comments.t` abort with
  `X::Undeclared: Variable '$time_after' is not declared` raised *inside*
  `Test.rakumod`'s own `_diag`, reached through `throws-like` → `subtest`. Both
  files exercise `use strict`, and `throws-like` EVALs its argument, so the
  module's file-scope lexical is being read from a frame whose package context
  is the EVAL's rather than the module's. Pre-existing (it fails the same way
  before the compunit-lexical store), but it is the store's neighbourhood.
- `skip() was passed a non-integer number of tests` — the real module's `skip`
  is stricter than the native one about its argument shape.

### Re-measured 2026-08-03 (evening), and the residue has no dominant cause left

Sweep on `fd2e24b75` (`prove -j6`, release): **190 / 1435**. Re-run alone with a
180 s budget: **PASS 71 / FAIL 119**, so the honest count is **119**, down from
132 at midday. Three fixes landed against it that afternoon, and re-running the
same 119 alone with all three in gives **PASS 6 / FAIL 113**:

| fix | files it freed |
| --- | --- |
| `news/2026-08/three-parse-failures-keep-their-malformed-class.md` | `S04-statements/terminator.t`, `S02-literals/pairs.t` |
| `news/2026-08/end-phasers-run-in-install-order.md` | all four of `S04-phasers/{multiple,ascending-order,descending-order,interpolate}.t` |
| `news/2026-08/range-is-iterable-for-the-map-family.md` | — (a hand-probe finding, not in the residue) |

Both of the first two were *general* interpreter bugs that only the strict
module exposed, and neither was "mutsu cannot do this":

- The three `X::Syntax::Malformed` files had all been rejected already — each
  rejection was a *soft* parse error, so the alternative backtracked and the
  class was lost to the generic "Confused." **Do not read a "right exception
  type" failure as a missing class** (the third time this lesson has been
  recorded here).
- The four phaser files were an **END ordering** bug: a module's END ran before
  the script's, and under the real module the module's END *is* the plan check,
  so the file reported "You planned 2 tests, but ran 1" and then emitted the
  missing `ok`. **A plan mismatch is not always a lost assertion — check
  whether the count is right but the order wrong.**

Classifying the remaining 113 by first `not ok` (excluding `# TODO`-marked
lines, which are expected failures and must be filtered out — `tmp/classify-first-fail.sh`
does not) leaves **no cluster larger than 2**: 9 files abort with no diagnostic
line at all, 2 apiece for `The object is-a 'Nil'`, `code dies` and
`binding of not yet existing elements should autovivify (3)`, and a long tail of
one-offs. The `right exception type` family is down to singletons.

~~So from here it is genuinely one file at a time.~~ **This conclusion has now
been written three times (2026-08-03 midday, 2026-08-03 evening, and once
before) and has been wrong every time — see "Stop concluding one-at-a-time"
below.** The 9 diagnostic-free aborts are still the best-value place to start,
since an abort costs a whole file:
`integration/99problems-41-to-50.t`, `integration/advent2009-day20.t`,
`integration/advent2012-day14.t`, `integration/advent2013-day10.t`,
`S02-types/{instants-and-durations,capture}.t`,
`S04-exception-handlers/control.t`, `S04-statements/{given,repeat}.t`,
`S06-operator-overloading/sub.t`, `S06-other/main-semicolon.t`,
`S09-typed-arrays/native-num.t`, `S12-subset/subtypes.t`,
`S14-roles/{anonymous,parameterized-mixin}.t`,
`S24-testing/11-plan-skip-all-subtests.t`, `S28-named-variables/init-instant.t`,
`S29-context/die.t`, `S32-io/io-path.t`, `S32-num/{complex,real-bridge}.t`,
`S32-temporal/DateTime.t`.

`S24-testing/11-plan-skip-all-subtests.t` is closed
(`news/2026-08/whatever-code-value-keeps-a-hash-composer.md`): `{:err(/Sub/),
:status(*.so)}` composed a *Block*, because the brace disambiguator read the
`.so` of `*.so` as an implicit-topic call. `Test::Util`'s `is_run` then matched
its no-test-name candidate, which lost the description and answered through the
native provider's separate counter. **A "plan mismatch plus a blank
description" is worth checking against the argument's own type before looking at
`Test` at all.**

`S32-num/real-bridge.t` is closed
(`news/2026-08/builtin-role-composes-its-own-roles.md`): a `class Fixed2 does
Real` was not `Numeric`, because `role_parents` only records *user*
compositions and `Real does Numeric` is a built-in one. Third instance of the
same mechanism after `Instant`/`Duration` — **when a file emits every assertion
but numbers only some of them, look for a type relation that diverted one
`Test.rakumod` candidate group to the native provider.**

`S32-num/complex.t` is closed
(`news/2026-08/unicode-infix-alias-resolves-by-name.md`): six Unicode infix
aliases (`≅ ⩵ ⩶ ≠ ≤ ≥`) parsed inline but did not resolve when reached by
*name*, and `cmp-ok` reaches its string operator by name
(`&CALLER::LEXICAL::("infix:<$op>")`). **An operator that works inline is not
evidence that `&infix:<op>` works** — the parser and the by-name dispatch are
separate tables, and only the parser had the aliases.

`S12-subset/subtypes.t` is closed
(`news/2026-08/sibling-scope-routine-shadow.md`): a `sub f` in one subtest block
made a `multi f` in the *next* one an `X::Redeclaration`, because mutsu keys the
routine registry by package and the lexical-shadow exemption was gated on a
scope depth that only an *inlined* bare block sets. **A declaration compiled in
a routine/closure body is registered twice** (hoist pass, then in sequence), so
any "have I seen this name before?" test in the compiler counts the hoist copy
first.

`S09-typed-arrays/native-num.t` is closed
(`news/2026-08/native-array-slice-assign-reads-through-its-cell.md`): `nok @arr`
passes the array to `ok`'s `Mu $cond`, which boxes the caller's binding into a
`ContainerRef`, and the slice-assign path's raw `ValueView::Array` test then
stopped matching — so `@arr[^3] = …` stringified its Range into one key. **The
native provider never takes such an argument as a Raku value, so any test file
that hands a container to a `Test` routine is exercising the cell form for the
first time.**

`S06-operator-overloading/sub.t` is **closed — 29/29 under the real module**, and
it took exactly the three independent fixes its triage predicted. **A file that
fails at three unrelated layers is worth triaging in full before starting on
it**: fixing only the visible first failure would have bought nothing here.

| layer | fix |
| --- | --- |
| a `sub infix:["@"]` declared inside a `lives-ok { … }` was still in the registry when a later `EVAL` string was *parsed*, so that string's `@ 5 @` read as an infix with a missing operand (aborted the file after 24 of 29) | `news/2026-08/block-local-routine-scope.md` |
| the two assertions lost before that wanted `X::Syntax::Extension::TooComplex` / `X::Syntax::Extension::Category` where mutsu answered the generic "Missing block" | `news/2026-08/operator-extension-name-error-classes.md` |

The second fix also produced a measurement rule worth repeating: **a
`throws-like` with a type argument is not a pin.** The first draft of its pin
used `throws-like 'sub infix:[/./] …', X::Syntax::Extension::TooComplex` and
passed 12/12 *without* the fix, because mutsu's native `throws-like` does not
check that argument. Reading `.^name` off the caught exception instead, the same
pin fails 8/14 without the fix.

`roast/S12-methods/qualified.t` is worth a second look too: its Malformed
assertion passes now and the file moved on to `Cannot dispatch to method me on
Parent because it is not inherited or done by Bar` in its inheritance subtest.

### Working the diagnostic-free aborts (2026-08-04)

Two of the listed aborts were taken, and both were general interpreter bugs the
strict module merely exposed. Neither was in `Test.rakumod` and neither was
about assertions.

| file | cause | fix |
| --- | --- | --- |
| `S02-types/capture.t` (now 46/46) | the atomic shared-array store seeded itself from an **undereferenced `ContainerRef`**, so it started EMPTY and dropped `Test.rakumod`'s whole `@vars` subtest stack the first time the file spawned a thread | `news/2026-08/shared-array-mutation-through-a-container-cell.md` |
| `S04-statements/given.t` (still failing, one step further) | an EVAL'd `sub` collided with a routine only the **registry** knew about, so `produce-tester`'s per-subtest `sub test-given` raised "Redeclaration of routine" | `news/2026-08/eval-sub-shadows-a-registered-routine.md` |

Three things worth carrying forward:

- **`@`/`%` module lexicals are `ContainerRef` cells as seen from the module's
  own subs.** Any code that reads an env binding's `ValueView` and expects a
  bare `Array`/`Hash` is wrong there. The bug above matched neither arm and
  silently took the `_ => default()` branch.
- **A fix in this campaign routinely unmasks the next bug.** The `given.t` fix
  made `roast/S04-statements/return.t` test 15 fail — it had been passing
  because the EVAL died of the very redeclaration being removed, so the
  snippet's `return` was never exercised. Local full `make roast` caught it; the
  underlying lexotic-`return` bug is fixed in the same PR.
- **`!routine_stack.is_empty()` is not "is a routine live".** A bare block, a
  `for` body and an *anonymous* `sub` all push a `RoutineFrame` with
  `is_block: true`. `Interpreter::enclosing_routine_exists()` is the predicate
  that asks the real question — but it is only correct for an EVAL *unit*; the
  other users of that compile path recompile closure bodies where the live frame
  IS the routine.

### `integration/advent2012-day14.t` (2026-08-05): the trigger was a routine that never ran

Closed 6/6 —
`news/2026-08/sequence-closure-env-does-not-shadow-a-live-lexical.md`. The file
aborted with `X::Cannot::Empty` inside its own `is-prime-beta`, and the cause was
that `Test.rakumod` *contains* `&CALLER::LEXICAL::(…)` in `cmp-ok`. Compiling
that routine sets the process-global `REFLECTIVE_NAME_ACCESS_SEEN`, which makes
`capture_closure_env` snapshot the **whole env by value** for every closure in
the program; a sequence generator merges its capture over the live env, so the
self-referential `my @primes = …, -> $p { … } … *` kept seeing the hoisted empty
array on every deferred pull.

`integration/99problems-41-to-50.t` was taken next and is **blocked, not
fixable here**: its grammar action declares `my @vars`, which is the same name
as `Test.rakumod`'s file-scope `my @vars`, and the two share one env key. That
is exactly the case
`todo/tickets/module-file-scope-array-and-hash-still-share-the-caller.md`
predicted would be the one that matters; the measured instance is recorded
there. Renaming the *test's* `@vars` makes the file pass, so nothing else is
wrong with it.

Two things this batch adds to the campaign's method:

- **A file that only fails "because a module is loaded" may not be about
  anything the module *does*.** `cmp-ok` is never called in `advent2012-day14.t`
  and `_push_vars` is never called in `99problems-41-to-50.t`; in both cases it
  was enough that the routine exists. Look for compile-time global flags and
  name collisions, not just executed code.
- **Bisect the module by brace-balanced top-level chunks, not by line count.**
  Truncating `Test.rakumod` at a line number breaks parsing (or drops
  `_init_vars`, which file scope calls at line 41) long before it changes
  behaviour; every prefix cut gave a useless answer. Splitting into 248
  chunks and always keeping chunks 0-47 plus the `_init_vars` chunk made both
  bisects converge in about six runs. Two directions are worth having: keep
  `0-47, 220, a..b` when the file needs no `Test` routines, and drop `a..b` from
  the full set when it does.

### Six files closed on 2026-08-04, and none of the causes was in `Test`

| file | assertions | cause | fix |
| --- | --- | --- | --- |
| `S02-types/capture.t` | 46 | the atomic shared-array store seeded from an **undereferenced `ContainerRef`**, so it started EMPTY and dropped `Test.rakumod`'s whole `@vars` subtest stack the first time the file spawned a thread | `news/2026-08/shared-array-mutation-through-a-container-cell.md` |
| `S04-statements/given.t` | 54 | **three** independent bugs in a row (see below) | three PRs |
| `S04-statements/repeat.t` | 21 | `X::Syntax::Missing` had no `.what` | `news/2026-08/syntax-missing-and-unitscope-carry-what.md` |
| `S06-other/main-semicolon.t` | 10 | `X::UnitScope::*` had no `.what`, and a *soft* parse diagnosis dropped its structured exception | same |
| `S28-named-variables/init-instant.t` | 3 | `Instant` was not `Numeric`, so `is-approx` matched no candidate and fell through to the **native** provider's separate counter | `news/2026-08/instant-and-duration-do-real.md` |
| `S02-types/instants-and-durations.t` | 36 | same, plus the `Real.abs` the relation then needed | same |

`given.t` took three: an EVAL'd `sub` colliding with a routine only the registry
knew about (`news/2026-08/eval-sub-shadows-a-registered-routine.md`), the
lexotic-`return` bug that fix unmasked, and `my &f = ...` not shadowing an outer
`sub f` for a bare-name call
(`news/2026-08/lexical-amp-binding-shadows-a-routine.md`). **Budget three
independent general bugs per file, not one.**

Two measurement rules this batch produced, both learned the hard way:

- **Measure the whole file's assertion count, not its first `not ok`.** Widening
  `Instant`'s type relations took `instants-and-durations.t` from 38 assertions
  to 3 — the right `is-approx` candidate was finally selected and then died on a
  missing `Real.abs`. A first-failure check shows nothing there, because the
  file aborts before asserting.
- **Do not write a pin against `throws-like`'s named matchers.** mutsu's native
  `throws-like` does not check them, so a `throws-like …, X::…, what => …` pin
  passes without the fix. Read the attribute off the caught exception instead.

### Re-measured 2026-08-14, ten days of unrelated work later

No one had touched this ticket since 2026-08-04 (confirmed via `gh pr list
--search vendor-real-test-module`), but ~10 days of general interpreter work
had landed in the meantime, so the first step was re-measuring rather than
trusting the 2026-08-04 numbers. Full sweep on `51abd38c6` (release build,
`prove -j6`, the same command as the "Step 3" section above):

| | 2026-08-04 | 2026-08-14 |
| --- | --- | --- |
| regress under `-j6` (raw) | 190 (evening figure) | **157 / 1435** |
| genuine, re-run alone with 4x the per-file timeout budget | 119 | **90** |

So the general-purpose work of the last ten days — none of it aimed at this
ticket — closed roughly a third of the residue as a side effect. That is
consistent with the campaign's own thesis: most of what the real `Test`
module exposes is ordinary interpreter gaps, not `Test`-shaped ones, so
unrelated fixes keep chipping at it.

Classifying the 90 by first `not ok` again produces the same flat histogram
the 2026-08-04 entry warned about (no symptom bucket bigger than a couple of
files), so per that entry's own rule the right move is to regroup by
mechanism instead of symptom. One paid off:

**14 of the 90 files fail because a specific `throws-like …, X::Some::Class`
assertion gets `X::Syntax::Confused` instead** (`grep -l "Got:
X::Syntax::Confused"` over the per-file logs). The *expected* classes are all
over the map — `X::Syntax::CannotMeta`, `X::Syntax::Comment::Embedded`,
`X::Syntax::Signature::InvocantNotAllowed`, `X::Anon::Multi`,
`X::Comp::Group`, `X::Worry::Precedence::Range`, `X::Syntax::Malformed`, and
others — so this is *not* one shared parser gap; most of these constructs
genuinely need their own individual diagnosis work (the long tail the
2026-08-03/04 entries already describe). But one of them **was** a shared
mechanism: all 9 `X::Anon::Multi` assertions in `S06-multi/syntax.t`
(`only sub {}` / `multi sub {}` / `proto sub {}` / `multi sub (Int $x) {}`
with no name) failed the same way, even though
`src/parser/{stmt/sub/sub_decl.rs,primary/ident/identifier_call.rs}` already
raise exactly `X::Anon::Multi` with the right message at the point of the
error.

**Root cause: `PError::fatal(message)` already prepends the `"FATAL:"`
sentinel prefix** (`src/parser/parse_result.rs:93-99`) that marks a parse
error as non-recoverable, but all 4 of its `X::Anon::Multi` call sites (2 in
`sub_decl.rs`, 2 in `identifier_call.rs`) *also* wrote a literal `"FATAL:"`
at the front of their own message text. The stored message became
`"FATAL:FATAL:X::Anon::Multi: An anonymous routine may not take a …
declarator"`. `PError`'s `Display` impl only strips one `FATAL:` layer
(`parse_result.rs:352-357`), so the message `parser::parse_program()` puts on
the resulting `RuntimeError` was left with one residual `"FATAL:"` still
glued to the front — `"FATAL:X::Anon::Multi: …"` — which no longer
`starts_with("X::")`. `RuntimeError::split_typed_message_convention()`
(`src/value/error_construct.rs:162-177`) therefore couldn't recognize the
class, and `exception_value()` fell back to `untyped_exception_class()`,
which answers `X::Syntax::Confused` for anything carrying a parse-error code.
Confirmed with a direct repro before the fix:

```
$ mutsu -e 'multi sub {}'
Runtime error: FATAL:X::Anon::Multi: An anonymous routine may not take a multi declarator
```

— the literal leftover `"FATAL:"` in that output was the tell. Audited every
`PError::fatal`/`fatal_at`/`fatal_with_exception` call site in `src/parser/`
for the same double-prefix pattern (`grep -rn '"FATAL:' src/parser/`); the
other four sites (`return_type.rs`, `my_decl_dispatch.rs`,
`my_decl_helpers.rs`, `param_inner.rs`) use `PError::raw()`, which does
*not* auto-prepend `"FATAL:"`, so their single literal prefix is correct and
they were left alone. Only the 4 `X::Anon::Multi` sites had the bug, and all
4 are now fixed by deleting their redundant `"FATAL:"` literal.

Result: `mutsu -e 'multi sub {}'` now raises `X::Anon::Multi` cleanly (no
leftover sentinel in the message), `S06-multi/syntax.t` goes from 9 lost
assertions to **45/45 clean** under `MUTSU_REAL_TEST=1`, and it was already
whitelisted and still passes under the native provider unchanged (same
control flow — only the message text changed). Pinned by
`t/anon-multi-exception-class.t` (10 assertions, green under `raku` too). Full
`t/` suite (3154 files) and `cargo clippy -- -D warnings` (pinned toolchain
1.96.1) both clean. Re-running the whole sweep after the fix: `157 → 157` raw
under `-j6` (one file fixed, one unrelated `-j6`-load artifact —
`S17-lowlevel/cas-int.t`, a CAS/atomics test that is CPU-bound and passes
clean alone — took its place in the raw count), but the *genuine* diff is
exactly `S06-multi/syntax.t` removed and nothing else changed.

**Next lead for round N+1:** the other 13 `Got: X::Syntax::Confused` files are
a real long tail — each expected class (`X::Syntax::CannotMeta` for `6 >==
2`/`6 ~~= 2`, diffy comparison operators used as an assignment-metaop base;
`X::Syntax::Comment::Embedded` for a malformed `#\`(...)` embedded comment;
`X::Comp::Group` per the still-open role-membership design in
`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`; …) needs its
own parser-side diagnosis, and none of the remaining 13 shares a mechanism
with another the way the `X::Anon::Multi` cluster did. Before picking one,
re-run `grep -rn '"FATAL:' src/` **outside** `src/parser/` too (not done this
round — the audit above was parser-scoped, since that's where every current
`PError::fatal` call site lives, but worth a quick repeat check next round in
case new call sites were added elsewhere). The other 76 of the 90 (the ones
without an `Expected:`/`Got:` mismatch at all) are ordinary single-assertion
gaps in the same long-tail shape the 2026-08-03/04 entries already describe —
no new dominant cluster found there this round; `roast/S02-types/array.t`
looked like a second `TODO`/exit-status mechanism at first glance (all its
`not ok` lines are `# TODO`-marked yet the file still exits non-zero) but
turned out to be a `Died` mid-file in an unrelated `lives-ok` subtest
("reification of zen and whatever slices"), not a TODO-handling bug — false
lead, ruled out.

## Stop concluding "from here it is one at a time" (2026-08-04)

That sentence has been written into this file three times, from three separate
first-`not ok` histograms, and the very next session has disproved it every
time. On 2026-08-04 it was disproved hardest: **8 fixes closed 8 files, and
three of the 8 closed two files each.**

| fix | files it closed |
| --- | --- |
| `X::Syntax::Missing` / `X::UnitScope::*` gain `.what` | `S04-statements/repeat.t`, `S06-other/main-semicolon.t` |
| `Instant`/`Duration` are `Cool` + `does Real` | `S28-named-variables/init-instant.t`, `S02-types/instants-and-durations.t` |
| a closure's writeback sees a `does` mixin as a change | `S14-roles/anonymous.t`, `S14-roles/parameterized-mixin.t` |

The reasoning error is specific and repeatable, so name it rather than the
conclusion: **the first failing assertion is a symptom, and the histogram
buckets symptoms. Causes are shared one or two levels below it.** Two files
whose first `not ok` reads `right exception type (X::Syntax::Missing)` and
`... (X::UnitScope::Invalid)` land in different buckets and have one cause (no
parse-error class carried its attributes). Two files that abort with
`No such method 'cool'` and `No such method 'attr'` look like two missing
methods and are one comparison using the wrong equality.

So when the histogram flattens, that is a signal to **stop classifying by
symptom**, not a signal that leverage is exhausted. What actually predicts a
2-file fix:

- Two files whose diagnostics name **different members of one mechanism**
  (two exception classes; two methods of one role; two flavours of one
  writeback).
- A file that aborts on a *missing method of a type mutsu models wrong* — the
  type relation is the cause and it will be wrong for every sibling type too.
- Anything reached through `Test.rakumod`'s own routines (`is-approx`,
  `throws-like`, `subtest`): the module dispatches on Raku type constraints, so
  one wrong relation silently diverts *every* call of that shape to the native
  provider.

Write the next status note as "the largest *mechanism* cluster is N", and only
after trying to merge symptom buckets that share a mechanism. Do not write
"one at a time" again without that step.

## `X::Syntax::CannotMeta` for diffy comparison operators as an assign-metaop base (2026-08-15)

Picked up the named-but-not-yet-fixed example from the 2026-08-14 entry:
`roast/S03-operators/assign.t` failed two `throws-like …, X::Syntax::CannotMeta`
assertions under `MUTSU_REAL_TEST=1` (`6 >== 2`, `6 ~~= 2`) because the parser
had no diagnosis at all for a chaining/structural comparison operator used as
the base of the `=` assignment metaoperator — it fell through to the generic
"Confused" error instead. Fixed generally (`ComparisonOp::source_spelling()`
+ `diffy_assign_meta_dba()` in `src/parser/expr/precedence/ternary.rs`, wired
into every site that consumes a comparison or range operator during parsing),
not as a two-spelling special case — it now covers all chaining ops (`==`,
`!=`, `<`, `<=`, `>`, `>=`, `eq`, `ne`, `lt`, `le`, `gt`, `ge`, `eqv`, `~~`,
`!~~`, `before`, `after`, `===`, `=:=`, `=~=`, ...), all structural ops
(`cmp`, `leg`, `<=>`, `coll`, `unicmp`), and the range operators (`..`,
`..^`, `^..`, `^..^`), each verified message-for-message against
`raku -e '...'`. `NotDivisibleBy` (`!%%`) was deliberately excluded — rakudo
parses `6 !%%= 2` as METAOP_NEGATE of the compound-assignment operator `%%=`
instead, a different, still-unimplemented gap. Pin: `t/diffy-assign-metaop.t`
(70 assertions, green under `raku` too). `news/2026-08/diffy-comparison-assign-metaop-cannotmeta.md`.
`roast/S03-operators/assign.t` now passes under both the native `Test`
provider and `MUTSU_REAL_TEST=1`. This was the only file in the "14 files
fail on `Got: X::Syntax::Confused`" cluster whose expected class was
`X::Syntax::CannotMeta`; the other 13 (different expected classes:
~~`X::Syntax::Comment::Embedded`~~, `X::Syntax::Signature::InvocantNotAllowed`,
`X::Comp::Group`, `X::Worry::Precedence::Range`, `X::Syntax::Malformed`,
...) remain open, each needing its own individual parser diagnosis.

## `X::Syntax::Comment::Embedded` for `#\`` without an immediate bracket (2026-08-15)

Picked the `X::Syntax::Comment::Embedded` example named above. Two
independent gaps, not one: the parser's "Opening bracket required for #\`
comment" diagnosis (`src/parser/helpers.rs`'s `ws()`) was never spelled in
the `"X::Type: text"` convention (fixed: `PError::fatal_at` with the class
prefix, same treatment as the sibling "Couldn't find terminator" case a few
lines above), and — found only after fixing the first — the class was not
registered under `X::Syntax` in `runtime_init.rs` at all, so
`roast/S02-lexical-conventions/comments.t`'s three "no space/tab allowed"
variants (which check the looser `~~ X::Comp`, marked "no exception type
yet" in the roast source) regressed until the registration was added too.
Fixed both; `news/2026-08/comment-embedded-exception-class.md`, pin
`t/comment-embedded-exception-class.t`. `comments.t` goes from 4 to 1
remaining failure under `MUTSU_REAL_TEST=1` (the last is the unrelated
unspace-in-comment "sanity check" at line 167 — `#\`\  (comment) 32` reads
the backslash as unspace and produces a differently-wrong parse rather than
the `X::Syntax::Comment::Embedded` the roast assertion at line 157 wants;
not investigated this round); `misc2.t` goes from 7 to 6.

**Lesson for the next `Got: X::Syntax::Confused` file:** typing the message
correctly is necessary but not sufficient — always re-check the class's
`register_x` entry exists too, since `runtime_init.rs`'s X::Syntax family has
several gaps of exactly this shape (`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`
already covers the general problem; this was one concrete instance of it).

**Correction (2026-08-15): not every "Got: X::Syntax::Confused" file is a
registration gap.** `roast/S03-operators/range.t` (expected class
`X::Worry::Precedence::Range`) looked like the same shape but is not —
`X::Worry::Precedence::Range` is already correctly typed *and* registered,
and works end to end in isolation. The file's actual failure is a topic
(`$_`) leak: the first `throws-like` call (reached through the real, loaded
`Test.rakumod`) permanently overwrites the enclosing `for @opvariants { ...
}` loop's own `$_` with the caught exception's message, so every subsequent
statement in the loop — which builds its assertion's source string by
interpolating `$_` — parses garbage and reports the generic
`X::Syntax::Confused` several statements after the real corruption happened.
Root-caused as far as ruling out the CATCH/topicalization restore mechanism
itself (verified correct in isolation) without finding the exact write;
recorded as
`todo/deep/module-catch-default-topic-leaks-to-callers-for-loop.md` with a
5-line repro. Before trusting a "Got: X::Syntax::Confused" diagnosis on any
remaining file, check whether the file's *own* topic/loop variable could have
been clobbered by an earlier assertion in the same loop — the parse failure
may be several statements downstream of the actual bug.

## `X::Syntax::Signature::InvocantNotAllowed` and `X::Syntax::NoSelf` (2026-08-15)

Same shape again, found via `roast/S06-signature/errors.t` (not itself in the
"14 files" cluster list, but the same mechanism): both classes already had
correctly-named exceptions built in `src/parser/stmt/sub/traits.rs`, but
neither was registered under `X::Syntax` in `runtime_init.rs`, so `~~
X::Comp` failed. Also fixed a `.message`-attribute bug found on the way (both
sites stored the full `"X::Type: text"` string in the `message` attribute
instead of just `text`) and corrected `InvocantNotAllowed`'s wording to
rakudo's actual text. `news/2026-08/invocant-marker-exception-classes.md`,
pin `t/invocant-marker-exception-classes.t`. `errors.t` goes from 4 to 2
remaining failures under `MUTSU_REAL_TEST=1` — the other 2
(`-> $a: { }` / `-> $a: $b { }`) are a distinct gap: pointy-block signatures
don't parse the `:` invocant marker at all (a parse-level gap, not a missing
semantic check — `reject_invocant_in_sub` is only wired into `sub`
declarations, not the pointy-block param parser in
`src/parser/stmt/control/pointy_param.rs`), so the diagnosis is lost to
generic "Confused." before any check can run. Fixed the same day, next entry.

## `X::Syntax::Signature::InvocantNotAllowed` for a pointy block's `:` marker (2026-08-15)

Picked up the gap the previous entry left open. `sub foo($a:) { }` already
raised the right typed exception (the sub-signature parser's
`reject_invocant_in_sub` handles it), but `-> $a: { }` / `-> $a: $b { }` fell
through to the generic "Confused" parse error: `parse_pointy_param` (the
pointy-block per-parameter parser,
`src/parser/stmt/control/pointy_param.rs`) never looks for a trailing `:`
invocant marker at all — every `ParamDef` it returns hardcodes
`is_invocant: false` — so the arrow-lambda driver
(`src/parser/primary/misc/lambda.rs::arrow_lambda_inner`) was left holding a
literal `: { }` / `: $b { }` it had no branch for, and `parse_block_body`'s
`parse_char(input, '{')` failed on the leading `:`.

Fixed by checking for the marker directly in `arrow_lambda_inner`, both right
after the first parameter and after each subsequent parameter in the
multi-param comma loop, and raising the same
`X::Syntax::Signature::InvocantNotAllowed` class the sub path already used
(shared `invocant_not_allowed_error()` helper in
`src/parser/stmt/sub/traits.rs` — raku uses the same wording, "Can only use
the : invocant marker in the signature for a method", for both contexts, so
no context parameter was needed). A pointy block can never declare an
invocant (only a method can), so the check fires unconditionally. Verified
the fix does not regress legitimate colon uses in pointy signatures
(`:$named`, `where` clauses, `::T` type captures) — none of those reach this
code path, since they're consumed inside `parse_pointy_param` itself before
returning.

Pin: extended the existing `t/invocant-marker.t` (which already covered the
sub-side cases) with the two pointy-block assertions, both green under `raku`
too. `roast/S06-signature/errors.t` now passes fully under both the native
and the real `Test` module. Full `t/` suite (3171 files) and
`cargo clippy -- -D warnings` both clean.

## `X::Comp` missing `.pre`/`.post`, plus two more "typed but missing an
attribute" gaps (2026-08-15)

Continuing the campaign, picked `roast/S32-exceptions/misc.t` — still
regressing under `MUTSU_REAL_TEST=1` — and triaged it in full before starting
(per the "a file that fails at three unrelated layers" lesson above). Three
independent gaps, all the same shape as every other entry in this file: the
class was already typed correctly, but an attribute `throws-like` reads was
missing, so the assertion crashed the whole file with "No such method" instead
of just failing the one subtest.

1. **`X::Comp`'s whole family (`X::Syntax::Missing` and friends) had no
   `.pre`/`.post` at all.** rakudo's `X::Comp` base class carries these (the
   source text immediately around the parse failure's eject point) for every
   compile-time exception, but mutsu's generic "derive attrs from the typed
   `"X::Type: text"` message" path (`RuntimeError::exception_value_with_backtrace`,
   `src/value/error_construct.rs`) only derived `X::Syntax::Missing.what`. Fixed
   generally rather than per-class: `parser::parse_program()`
   (`src/parser/mod.rs`) is the one place that unambiguously has both the full
   original source and the failure offset for a SOFT (recoverable,
   typed-convention-message) parse diagnosis, so it now computes `pre`/`post`
   there (current-line-only, same convention as the pre-existing
   `source_span_at` helper `X::Syntax::Confused`'s "Missing semicolon" case
   already used) and stores them on two new `RuntimeError` cold fields
   (`pre_context`/`post_context`, `src/value/error.rs`). The generic attribute
   derivation then fills `pre`/`post` from those fields for any class that
   doesn't already carry them explicitly (`.entry(...).or_insert_with(...)`,
   so a call site that built its own exception with its own `pre`/`post`
   — e.g. the `modifier.rs` "Missing semicolon" site — is left alone).
   **Caveat, not fixed:** this does not attempt to reproduce rakudo's `pre`/
   `post` bug-for-bug — `roast/S32-exceptions/misc.t` line 92's own assertion
   is `#?rakudo todo`'d as "Wrong eject position" (rakudo issue #4431), i.e.
   rakudo itself gets this specific construct's eject point wrong, so no
   fix here could match both rakudo and the assertion at once. mutsu now
   computes its own eject point without crashing, which is strictly closer to
   correct than raising nothing.
2. **`X::InvalidType` had no `.typename`** on the `does`/`hides`-parent raise
   site (`src/runtime/registration_class_validate.rs`) — only the sibling
   `returns`/`of`-trait site (`src/runtime/registration_sub.rs`) set it. Fixed
   the same way as `X::Syntax::Missing.what`: rakudo's message IS `Invalid
   typename '{typename}'`, so `exception_value_with_backtrace` derives it from
   the message text rather than duplicating it at the raise site.
3. **`X::Syntax::Adverb` had no `.what`** at either of its two raise sites
   (`my $x :a` in `src/parser/stmt/decl/my_decl.rs`, `infix:(&)` in
   `src/parser/primary/ident/identifier_call.rs`). The first already builds a
   full exception object with the sigil+name in hand, so it now passes `what`
   as an extra attribute directly; the second only ever built a plain message
   string, so it goes through the same message-derivation mechanism as item 2
   (`"You can't adverb {what}"`).

None of these three needed a message-text change (the trailing-period
double-`X::Type:` prefix bug documented in the round-6-round-8
`bench-ctor`-adjacent sessions elsewhere in this repo was checked for at both
raise sites and not present here) — only additional attributes. Pin: extended
`t/typed-exception-attributes.t` (16 → 21 assertions) with the three new
cases, message text unchanged elsewhere in the file; all new assertions green
under `raku` too except the `X::Syntax::Missing` `pre`/`post` one, which is
annotated with the rakudo-bug caveat from item 1 instead. `misc.t` now passes
fully under both the native and the real `Test` module. Full `t/` suite (3172
files, 29568 tests) and `cargo clippy -- -D warnings` both clean.

**Next lead:** `misc.t` was fully closed, but two more gaps surfaced past it
while triaging (not fixed this round): `sub foo() returns !!!wtf??? { }`
(expects `X::Syntax::Malformed`, `what => 'trait'`) parses as generic
`X::Syntax::Confused` instead — the malformed-return-type name is not
recognized as its own diagnosis; and a stubbed-role-parameterization test
(`role Bottle[::T] { ... }; class Wine { ... }; say Bottle[Wine].new;`, around
line 29) raised an uncaught "The following packages were stubbed but not
defined: Wine" that aborted the file with exit 1 before the `pre`/`post` fix
made it possible to see past line 93. Also still open in this file's
neighbourhood: `PError::malformed()` (`src/parser/parse_result.rs`) has the
same double-prefix `message`-attribute bug the `X::Anon::Multi` and
`InvocantNotAllowed` fixes elsewhere in this file already found and fixed for
other classes — `X::Syntax::Malformed.message` currently reads
`"X::Syntax::Malformed: Malformed initializer"` instead of rakudo's plain
`"Malformed initializer"` — not yet fixed since no roast assertion in the
sweep currently reads `.message` on it, but worth doing alongside whichever of
the two gaps above is picked up next (both raise `X::Syntax::Malformed`).

## `PError::malformed()` double-prefix, and a caught stub error re-firing later (2026-08-15)

Picked up both items the previous entry left open — they turned out to be
independent, not the same fix.

1. **`PError::malformed()`'s double-prefix bug, fixed.** Exactly the shape the
   previous entry predicted:
   `crate::value::Value::str(message.clone())` stored the whole
   `"X::Syntax::Malformed: Malformed {what}"` string as the `.message`
   attribute. Now builds `format!("Malformed {}", what)` directly for the
   attribute, matching `raw_with_what`'s existing
   `split_typed_message_convention` strip. Verified against `raku -e 'my $x
   ='`'s actual `.message` ("Malformed initializer", confirmed via a direct
   `raku` repro) — no roast assertion in the sweep reads `.message` on this
   class yet, so this was found by re-deriving rakudo's wording by hand, not
   by a newly-passing file. Pin: extended `t/malformed-syntax-classes.t` with
   a direct `EVAL`+`CATCH` assertion (12th test) — native `throws-like` does
   not check `.message` or its named matchers, per this file's own earlier
   lesson, so a `throws-like` line alone would not have caught this.
2. **The stubbed-role-parameterization abort was not a `Test` gap at all — it
   was a stub double-report bug, fixed generally.** `Wine`'s
   `X::Package::Stubbed` correctly raised inside the `EVAL` and was correctly
   caught by the surrounding `try`/`CATCH` — but the registry entry for
   `Wine` is only ever removed when the stub is actually *defined*
   (`registration_class_validate.rs`/`vm_exec_dispatch.rs`), and a
   deliberately-never-defined stub like `Wine` here never is. So the
   top-level end-of-program `check_unresolved_stubs()` (`run.rs`,
   unconditional) found the same still-registered `Wine` stub and raised the
   identical error a **second time, uncaught**, well after the `CATCH` had
   already handled it — aborting the whole file past line 93.
   `check_unresolved_stubs_excluding` (`src/runtime/run_dist.rs`) now tracks
   every name it reports in a **new, separate** registry set
   (`reported_stub_errors`) and skips names already in it, so a stub is
   reported at most once per program — matching rakudo's own "raised once per
   compilation unit at CHECK time" semantics. **First attempt was wrong**:
   removing the reported name straight from `class_stubs`/`package_stubs`
   (rather than tracking it separately) broke `roast/S12-class/stubs.t`
   (regressed in CI, not caught locally the first time) — `class_stubs`
   membership is not just report-bookkeeping, it is the live "is this class
   still just a stub" flag every other class-system check reads (composition,
   "already a stub, allow re-stubbing"), so removing `A` from it after
   reporting made a *later* `class B is A {}` silently compose instead of
   raising `X::Inheritance::NotComposed` (`stubs.t`'s own next assertion,
   reusing the exact same name `A` in a separate `EVAL` right after). The
   separate-set fix keeps `class_stubs` semantically untouched; names are
   removed from `reported_stub_errors` wherever a stub is genuinely resolved,
   so a reused name can report its own fresh error later. Required widening
   `check_unresolved_stubs{,_excluding}` from `&self` to `&mut self`; both
   call sites (`run.rs`'s end-of-program check, `system.rs`'s EVAL check)
   already held `&mut self`. Pin: new `t/eval-stub-error-not-reraised.t`,
   green under `raku` too — plus the full `S12-class/stubs.t` and every other
   roast file found to touch stubs/`X::Package::Stubbed` re-run locally after
   the correction.
   **Lesson: a "remove once reported" fix on any registry set needs to ask
   first whether that set is pure bookkeeping or also a live semantic flag
   consulted elsewhere — CI (not local `t/`) is what caught this, so widen
   the local regression sweep to every roast file matching the touched
   mechanism's name before trusting a "fixed" write-up next time.**

Both fixes: `news/2026-08/malformed-message-prefix-and-stub-error-not-reraised.md`.
`roast/S32-exceptions/misc.t` progresses substantially further under
`MUTSU_REAL_TEST=1` (past the stub-abort point) but is not yet fully clean —
6 individual assertion gaps remain (`X::Inheritance::SelfInherit`,
`X::TypeCheck::Argument`, an `X::Comp::Group` shape for an undeclared type in
a `when` clause, `X::Parameter::BadType`, `X::ControlFlow::Return`, and the
still-open `sub foo() returns !!!wtf??? { }` malformed-return-type gap named
in the previous entry) — each its own individual diagnosis, not yet picked
up. Full `t/` suite (3176 files, 29594 tests) and `cargo clippy -- -D
warnings` both clean.

## Three more gaps closed in `misc.t`; `X::ControlFlow::Return` traced to an already-filed deep bug (2026-08-16)

Continued picking off `misc.t`'s remaining 6 individual assertion gaps
(under `MUTSU_REAL_TEST=1`). Down from 6 to 3 unresolved after this round:

1. **`X::Inheritance::SelfInherit` (`my class Foobar is Foobar { }`), fixed.**
   Raised via a bare `RuntimeError::new(format!(...))` — an untyped message,
   not a typed exception at all — so `throws-like`'s `name => "Foobar"`
   matcher read `.name` as `Nil`. Switched the throw site
   (`registration_class_validate.rs`) to `RuntimeError::typed` with a `name`
   attribute (verified against real `raku`'s `.name`/`.message` for this
   exact class) and registered the class via `register_x` in
   `runtime_init.rs` (it had no registry entry at all — every other
   `X::Inheritance::*` sibling did).

2. **`proto sub foo(Str) {*}; foo 42;` inside `EVAL`, fixed.** mutsu correctly
   raises `X::TypeCheck::Argument` for this shape when compiled as the
   program's own mainline — but under `EVAL` (what `throws-like` uses), it
   died with a false `X::Undeclared::Symbols: Undeclared routine: foo`
   instead. Root cause: `proto sub` parses to a *distinct* AST node,
   `Stmt::ProtoDecl` — not `Stmt::SubDecl` — and
   `system_eval_names.rs`'s `check_eval_undeclared_routines` (the
   EVAL-specific "is every called name declared here" pre-pass) only added
   `Stmt::SubDecl`/`Stmt::MethodDecl` names to its `declared` set, so a
   proto-only sub was invisible to it. (The sibling mainline check in
   `undeclared_routines.rs` already had a `Stmt::ProtoDecl` arm — this is
   why the exact same code worked outside `EVAL`.) Added the missing arm.

3. **`X::Parameter::BadType` (`my package A {}; sub foo(A $a) { }`) — the
   general fix landed, but the roast subtest still has an unresolved,
   order-dependent gap (see below).** The throw site
   (`registration_sub.rs`) already correctly built `RuntimeError::typed`
   with the right message and a `type` attribute — same "typed but never
   registered" shape as gap 1 — but `X::Parameter::BadType` had no
   `register_x` entry either. Registered it (parented on the pre-existing
   `X::Parameter`). Verified in isolation against real `raku` — fixed. BUT:
   the exact roast subtest at `misc.t` line 227 only reproduces the bug
   after the file's preceding ~226 lines/~47 subtests have run for real; a
   standalone repro of the same two lines (with or without the immediately
   preceding sibling `throws-like` in the file) passes cleanly. This smells
   like a *different*, order-dependent leak (something saturates or
   collides after enough `EVAL`s reuse the same short class/package names,
   `A` in this case) — filed separately as
   `todo/tickets/parameter-badtype-order-dependent-under-many-prior-evals.md`
   rather than chased further in this session, since the minimum repro
   needs the full accumulated state and none of the usual bisection tricks
   shrank it.

4. **`X::ControlFlow::Return` (`gather { return  1}` via `EVAL`) — traced,
   not fixed; already filed as a deep architectural bug in the previous
   session** (`todo/deep/return-outside-routine-uncatchable-inside-nested-run.md`):
   the escaping `return`'s conversion into a catchable
   `X::ControlFlow::Return` is gated on `nested_run_depth == 0`, which is
   never true inside `EVAL`'s nested run, so the raw control-flow signal
   passes straight through `try`/`CATCH` and aborts the program instead.
   Confirmed this is the same failure `misc.t` line 280 hits; no new work
   done here this round, left for that ticket's own "needs design" path.

**Remaining after this round:** the `X::Comp::Group` cases (an undeclared
type in a `when` clause, and a bare `5.` term needing a method name — both
rakudo parser-ambiguity diagnostics, not simple "detect X and throw Y"
fixes), the order-dependent `X::Parameter::BadType` leak (ticket above), and
`X::ControlFlow::Return` (deep ticket above) — 3 items, none picked up yet.

Full `t/` suite (3183 files, 29636 tests), `cargo build --release`, and
`cargo clippy -- -D warnings` all clean. Verified the 3 whitelisted files
touched by these changes (`roast/S32-exceptions/misc.t`,
`roast/S12-class/self-inheritance.t`, `roast/S02-types/WHICH.t`) still pass
under the *default* (native `Test`) mode too, since the `register_x`
registrations and `RuntimeError::typed` conversions are not gated on
`MUTSU_REAL_TEST`.

## `sub foo() returns !!!wtf??? { }` malformed-return-type gap, fixed (2026-08-16)

Picked up the malformed-return-type item this round's earlier entry left
open. A `returns`/`of` trait whose type-name expression fails to parse at
all (`!!!wtf???` doesn't start with an identifier character) propagated the
generic parse error unconverted, surfacing as `X::Syntax::Confused` instead
of rakudo's `X::Syntax::Malformed: Malformed trait`. Fixed in
`src/parser/stmt/sub/traits.rs`: added `malformed_trait()`, mirroring the
existing `malformed_initializer()` helper's contract
(`stmt/decl/my_decl_assign.rs`) — only converts an error that failed
*immediately* with no partial parse, and leaves an already-fatal or
structured-exception error alone — and wired it into both the `returns` and
`of` trait branches (verified against real `raku`: both give "Malformed
trait"; `-->` gives a different, unrelated "Missing block" error via a
different grammar path and was left untouched). Full `t/` suite (3183
files), `cargo build --release`, `cargo clippy -- -D warnings` clean;
targeted sweep of all 93 whitelisted `S06-signature`/`S06-traits` files
(the area most likely to exercise `returns`/`of` traits) plus
`S32-exceptions/misc.t` on release, all pass.

**Remaining: 3 items** (the two `X::Comp::Group` parser-ambiguity cases, the
order-dependent `X::Parameter::BadType` leak, and the deep
`X::ControlFlow::Return` ticket) — none picked up yet.

## The order-dependent `X::Parameter::BadType` leak, fixed (2026-08-16)

Picked up `todo/tickets/parameter-badtype-order-dependent-under-many-prior-evals.md`,
the one remaining item from the previous round's `misc.t` triage that looked
deep enough to file separately. Root-caused with a synthetic loop, per that
ticket's own "suggested next step" — reduced to a 4-line repro with no roast
file needed: `my class A {}` in a now-exited block, `EVAL 'my package A {};
my A $a;'` (fails), then a wholly separate `EVAL 'my package A {}; sub
foo(A $a) { }'` — the second `EVAL`'s own `X::Parameter::BadType` silently
stopped firing.

Not actually order-*dependent* once isolated — it reproduces with exactly two
`EVAL`s and no accumulated file state at all. The real shape: `my class`/`my
role` already lexically scope their bare name (suppressed once their block/
EVAL exits, via `register_lexical_class`/`pop_lexical_class_scope`), but `my
package`/`my module` never participated in that mechanism, and `EVAL` itself
never pushed its own lexical-class-scope frame around its body (only bare
`{ ... }` blocks did). So a `my package A` that shadows a stale out-of-scope
`my class A` (`shadow_suppressed_type_with_package`, which deliberately
un-suppresses `A` so the new package becomes active) never got RE-suppressed
when its own EVAL/block ended — permanently un-suppressing `A` for the rest
of the process, even from a snippet that itself later failed.

Fixed generally: `EVAL` (`parse_and_eval_with_operators`, `system.rs`) now
push/pops its own lexical-class-scope frame around the snippet body (mirrors
the bare-block cleanup in `vm_misc_scope.rs`, unconditional pop so a dying
snippet still cleans up), and `RegisterPackageMy` (`vm_exec_dispatch.rs`) now
calls `register_lexical_class` too, so a `my`-scoped package reuses the exact
same scope-exit re-suppression `my class` already had, for both a bare block
and `EVAL`'s new push/pop. `roast/S32-exceptions/misc.t` line 227 now passes
deterministically with the file's full preceding state, under both `Test`
providers. Pin: 3 new assertions in `t/lexical-type-scope-suppression.t`.
Full write-up: `news/2026-08/my-package-lexical-scope-leak.md`.

**A blind alley worth recording:** the first fix attempt gated the raw
`env.get(name)` bareword-resolution fallback (`vm_var_get_ops.rs`) on
`!is_name_suppressed(name)` directly — reasoning that a self-named
`Package("A")` env entry should never resolve while `A` is suppressed. That
regressed `t/class-body-type-scope.t`: a class body's OWN `my class Foo`
legitimately suppresses the bare name `Foo` globally while its scope is
active, but the *file-scope* `Foo` declared earlier is a completely different,
still-valid binding that happens to share the same suppressed name.
`suppressed_names` is a bare `HashSet<String>`, not tied to which declaration
produced the *current* env value, so "is this name suppressed" and "is this
specific env value stale" are different questions the flag cannot answer by
itself — gating on it wholesale is unsound. Reverted that part; the two
`register_lexical_class` fixes above turned out to be sufficient for the
actual reported bug without touching bareword resolution at all.

**Also found and deliberately left open:** a *non*-`my` `class`/`package`
declared inside `EVAL` (`EVAL 'class Foo { }'`) also stays bareword-visible
outside the `EVAL` in mutsu, unlike raku (`EVAL`'s own compilation-unit
boundary is stricter than a plain block's — a plain `{ package Foo { } }`
correctly SHOULD stay visible after the block, only `EVAL` should not). No
test in the current suite depends on this either way; fixing it would mean
touching `eval_eval_string`'s classes/roles snapshot-merge dance
(`system_eval_string.rs`), which is broader and riskier than this round's
fix. Left for a future round if a roast file ever needs it.

## Re-measured 2026-08-16 (round N+1): ternary `?? !!` diagnoses, and one more `X::Syntax::Adverb` gap

Re-ran the full sweep on a fresh `MUTSU_REAL_TEST=1` release build to find the
next mechanism cluster after the `misc.t`/`X::Anon::Multi` work above (no
dominant single-file blocker was left in the ledger, so this was a fresh
`grep -l "Got: X::Syntax::Confused"` sweep over the `-j6` residue, alone-rerun
to separate genuine failures from `-j6` contention per this file's own
established method). One 7-assertion cluster in a single file stood out:
`roast/S03-operators/ternary.t` failed 7 `throws-like …,
X::Syntax::ConditionalOperator::*` assertions, all landing on the generic
`X::Syntax::Confused` instead.

**Root cause: mutsu's ternary (`?? !!`) parser had no typed diagnoses at all
for the common ways rakudo's grammar reports a malformed then/else branch** —
not a registration gap this time (the file closing this round's earlier
entries were all "typed but not registered"; this one genuinely had zero
mechanism). Implemented four distinct diagnoses, each verified message-for-
message against `raku -e '...'`:

| shape | class | rakudo message |
| --- | --- | --- |
| `1 ?? 2,3 !! 4,5` (comma inside a branch) | `X::Syntax::ConditionalOperator::PrecedenceTooLoose` | "Precedence of `{op}` is too loose to use inside ?? !!; please parenthesize" |
| `1 ?? 3 :foo !! 2` (colonpair adverb inside a branch) | same class | same message, `op` = the spelled adverb (`:foo`, `:v`, ...) |
| `$a ?? $a = 1 !! $a = 2` (assignment inside a branch — already partly handled) | same class | same message, `op` = the spelled assignment operator (`=`, `+=`, ...) |
| `1 ?? 3 :: 2` / `1 ?? 3 : 2` | `X::Syntax::ConditionalOperator::SecondPartInvalid` | "Please use !! rather than `{second-part}`" |
| `1 ?? rt123115 !! 3` (a bareword listop call swallows the `!!`) | `X::Syntax::ConditionalOperator::SecondPartGobbled` | "Your !! was gobbled by the expression in the middle; please parenthesize" |

The existing `PrecedenceTooLoose` builder (`conditional_precedence_too_loose_error`,
`src/parser/expr/precedence/errors.rs`) already fired for the assignment case
but with a message that didn't match rakudo's wording at all ("Assignment
operators inside ?? !! are too loose; parenthesize them") and no `.operator`
attribute — discovered by checking real `raku`'s actual output for that case
too, not just the untested ones (`raku -e 'my $a=5; $a ?? $a = 42 !! $a =
43'` → "Precedence of = is too loose..."). Unified all three PrecedenceTooLoose
producers (comma, adverb, assignment) onto one parameterized builder taking
the exact spelled operator text, fixing a latent message-wording bug in the
same pass as adding the two missing classes.

**One trap, caught by testing against `raku` and not just by feel:** the
initial fix made the bareword-gobble diagnosis fire unconditionally whenever
a non-type bareword sat in then-position — reasoning that `1 ??
UNDECLARED_NAME !! 2` must be a gobble the same way `1 ?? rt123115 !! 3` (a
*declared* sub) is. Wrong on two counts, both caught only by running the roast
file's own existing assertion (`1 ?? b\n !! 2` → `X::Syntax::Confused`,
`roast/S03-operators/ternary.t` line 111) against real `raku` before trusting
the fix:

1. Declared-vs-undeclared doesn't matter — `raku -e '1 ?? b !! 2'` (a clean,
   *undeclared* bareword) also gobbles and reports `SecondPartGobbled`; rakudo
   always attempts the listop-call parse regardless of whether the name
   resolves.
2. The roast assertion's `1 ?? b\n !! 2` is not actually about an undeclared
   name at all — it's a **literal backslash-n** (two characters; the source
   is single-quoted inside `EVAL`, so `\n` is not a newline) glued directly
   onto the bareword with no separating whitespace, which is genuinely bogus
   trailing code, not a clean gobble.

The real, general rule (implemented in both `ternary_mode` and its
list-infix-layer twin `item_expr`, which duplicate this whole guard — see the
`item_expr` doc comment's own note that it "mirrors" `ternary_mode`): a
bareword then-branch is `SecondPartGobbled` only when the residual
immediately after it is whitespace or end-of-input; anything else (adjacent
non-whitespace garbage) falls through to the pre-existing generic Confused
path unchanged.

**A related, separate gap surfaced in the same file and was fixed alongside
it:** `1 ?? (3 :foo) !! 2` (a parenthesized adverbed *literal*) also failed —
not a ternary problem at all, but `(EXPR :adverb)` general paren-expression
parsing having no diagnosis for a colonpair directly following a bare literal
term. rakudo: `raku -e '(3 :foo)'` → `X::Syntax::Adverb`, "You can't adverb
3" (confirmed this is specifically about *literal* terms — `(1+2 :foo)`
legitimately attaches the colonpair as a named argument to the `infix:<+>`
call in rakudo, a separate, pre-existing, still-open gap left untouched since
no roast assertion needs it). Fixed narrowly for `Expr::Literal`/
`Expr::LiteralSrc` in `src/parser/primary/container/paren.rs`, matching only
the verified-correct shape rather than guessing at `Expr::Var`'s behaviour
too (which `raku -e 'my $x=5; ($x :foo)'` suggests behaves the same way, but
was not verified byte-for-byte and is not needed by any current test —
left as a documented possible follow-up, not assumed).

`roast/S03-operators/ternary.t` is now **28/28 clean** under
`MUTSU_REAL_TEST=1` (previously 21/28). Full `t/` suite (3186 files, 29686
tests) and `cargo clippy -- -D warnings` both clean. Pin:
`t/ternary-second-part-diagnoses.t` (18 assertions, all verified
byte-identical against `raku` including the two "stays Confused" negative
cases).

**Note found in passing, not investigated:** `roast/S32-io/spurt.t` test 36
fails when run standalone with `prove` directly (both on this branch and on a
clean `main` checkout via `git stash` — confirmed NOT a regression from this
round's work), but the whitelisted `make roast` run passes it. `make roast`
removes a stale `roast/temp-file-RT-126006-test` before starting (see
CLAUDE.md); this looks like the same class of leftover-file-from-a-prior-run
issue, not caught this round since `prove` was run directly for speed. If it
recurs as a genuine `make roast` failure, look there first.

## `MONKEY-TYPING` leaking into (and then failing to re-arm inside) `EVAL` (2026-08-16)

Follow-on from the ternary round above: re-ran the sweep and picked
`roast/S12-class/augment-supersede.t`'s remaining `Got: X::AdHoc` failure —
a `throws-like` expecting `X::Syntax::Augment::WithoutMonkeyTyping` instead
observed a *different* error (a method-clash `X::AdHoc`), meaning mutsu let
an `augment` inside an `EVAL`'d string succeed when it should have been
rejected outright.

**Root cause: an outer `use MONKEY-TYPING;` (active in the script that calls
`EVAL`) was leaking into the separately-`EVAL`'d string**, unlike real
`raku` — verified directly: `raku -e 'use MONKEY-TYPING; try { EVAL q[class C
{ method f {} }; augment class C { method f {} }] }; say $!.^name'` prints
`X::Syntax::Augment::WithoutMonkeyTyping`, not the method-clash error an
inherited pragma would reach. This differs from `fatal` (a genuine runtime
dynamic-scope pragma the caller's `EVAL` legitimately inherits — see the
`eval-does-not-leak-use-fatal` fix elsewhere in this repo, which is about the
*opposite* leak direction, EVAL-out-to-caller): `MONKEY-TYPING` gates a
compile-time check (`augment class Foo {}` is only legal *syntax* when it's
active), and `EVAL` is a fresh compilation unit for that check.

Fix: `eval_eval_string` (`src/runtime/system_eval_string.rs`) now saves and
resets `self.monkey_typing` to `false` before compiling+running the EVAL'd
unit (mirroring the existing `fatal_mode` save/restore skeleton, but with a
forced reset rather than a straight save/restore, since the two pragmas have
opposite inheritance semantics), and restores the caller's value afterward.

**This surfaced a second, previously-invisible latent bug**, caught only by
testing the fix against a full script sequence rather than a single isolated
snippet (per this file's own repeated lesson about order-dependent state):
resetting `monkey_typing` to false around `EVAL` meant an EVAL'd string's
*own* `use MONKEY-TYPING;` now had to actually re-arm the flag — and it
didn't, whenever the CALLER had already `use`d the same module once.
`use_module_with_tags_inner`'s `if self.loaded_modules.contains(module) { ...
}` fast path (`src/runtime/runtime_module.rs`) already re-arms `strict_mode`/
`fatal_mode` on a repeat `use` of an already-loaded module (a scope that
restored `env` wholesale since the original load may have lost the runtime
effect even though the module stays recorded as loaded) — but had no
matching arm for `MONKEY-TYPING`/`MONKEY`. This was invisible before because
`monkey_typing` was never independently reset anywhere, so the omission
never had a way to manifest. Added the missing arm, mirroring `strict`/
`fatal` exactly.

Pin: extended `t/monkey-typing-lexical.t` (4 → 6 assertions) with both the
leak-prevention case and the re-arm-after-reset case, verified against `raku`
too. `roast/S12-class/augment-supersede.t`'s TAP output is now 15/15 clean
under `MUTSU_REAL_TEST=1` (previously failed one, and a second went from
"passing only because a preceding failure happened to route through a
different, uninstrumented path" to genuinely passing) — its remaining
non-zero exit code under `MUTSU_REAL_TEST=1` is an unrelated, pre-existing
issue (a `class ::F { ... }` stub declared inside a deliberately-broken `try
EVAL '...'` in the file's own "used to crash rakudo" regression tests never
resolves and trips the end-of-program `check_unresolved_stubs()` check;
confirmed present on a clean `main` checkout too via `git stash`, not
investigated further this round). `roast/S32-exceptions/misc2.t` also
improved (6 → 5 remaining failures). Full `t/` suite (3185 files) and `cargo
clippy -- -D warnings` both clean.

## `trait_mod:<is>` no-candidate fallback missing for attribute/class/role traits (2026-08-16)

The same round found two more `Got: X::Multi::NoMatch` files —
`roast/S12-attributes/instance.t` and `roast/S12-class/inheritance.t` — and
both trace to the exact bug already fixed once for *variable* traits
(`news/2026-08/user-trait-mod-does-not-consume-every-trait.md`,
`todo/tickets/user-trait-mod-multi-shadows-builtin-traits.md`, closed as
#5689): a user-declared `trait_mod:<is>` multi (Test.rakumod itself exports
`multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!)`) shares its
dispatch with the built-ins, so a trait shape it doesn't match must fall
through to the built-in unknown-trait diagnosis — but that fallback had only
ever been wired into the *variable*-trait code path
(`vm_var_trait_ops.rs::exec_apply_var_trait_op`), not the two siblings:

- **Attribute traits** (`has $.a is bar`) — `apply_attribute_traits`
  (`src/runtime/methods_classhow_attribute.rs`) unconditionally propagated
  `call_result?` from the `trait_mod:<is>` dispatch, so a non-matching user
  candidate's `X::Multi::NoMatch` reached the caller instead of the
  `X::Comp::Trait::Unknown` built below it in the same function.
- **Class/role inheritance traits** (`class X is nosuchtrait { }`,
  `role R is nosuchtrait { }`) — `validate_class_parents`
  (`src/runtime/registration_class_validate.rs`) defers ANY lowercase parent
  name to custom-trait dispatch as soon as `has_proto("trait_mod:<is>") ||
  has_multi_candidates(...)` is true, regardless of whether that candidate's
  *signature* could ever match a `(Package, Pair)` call — and the two
  deferred-dispatch sites that actually run it (`exec_register_class_op`/
  `exec_register_role_op`, `src/vm/vm_typedecl_ops.rs`) likewise propagated
  the dispatch failure verbatim instead of falling back to
  `X::Inheritance::UnknownParent`.

Fixed both the same way as the variable-trait precedent: match on the
dispatch `Result`, and only when `Interpreter::is_trait_mod_no_candidate`
(the existing shared predicate) says "no candidate matched at all" does
execution fall through to the unknown-trait/unknown-parent error; a real
error raised from *inside* a candidate that DID match still propagates
unchanged. Extracted the `X::Inheritance::UnknownParent`-building code (used
at three call sites now — the immediate check, and the class/role deferred
paths) into a shared `Interpreter::unknown_parent_error` helper rather than
duplicating the suggestion/attrs logic three times.

Pin: extended `t/user-trait-mod-does-not-consume-every-trait.t` (6 → 10
assertions) with the attribute-trait case, both the class and role
inheritance cases, and a negative control (an error from inside a matching
class-level handler still propagates) — all verified against `raku` too.
`roast/S12-attributes/instance.t` and `roast/S12-class/inheritance.t` both
pass fully under both `Test` providers. Full `t/` suite (3187 files),
`cargo clippy -- -D warnings`, and the full roast whitelist under the native
provider (only the pre-existing unrelated `S32-io/spurt.t` artifact) all
clean.

**Lesson for the next `Got: X::Multi::NoMatch` file:** `trait_mod:<is>` has
(at least) four independent dispatch sites sharing this exact bug shape —
variable, attribute, class, and role. All four are now fixed, but any FUTURE
new `is`-trait dispatch site should wire in `is_trait_mod_no_candidate` from
day one rather than adding a fifth latent instance.

## `X::Parameter::RW` lost through the binding-error "enhancement" wrap (2026-08-16)

Another `Got: X::AdHoc` file: `roast/S06-traits/misc.t` expected
`X::Parameter::RW` for a literal/itemized-array argument passed to an `is rw`
parameter (`sub f ($x is rw) {}; f(1)`), but observed a generic
`X::AdHoc` whose message was the wrapped-and-buried original text:
`"Calling f(Int) will never work with declared signature ($x)\n
X::Parameter::RW: 'x' expects a writable variable argument"`.

**Root cause, same family as the `X::Anon::Multi` double-prefix bug earlier
in this file, different mechanism:** the RW-binding check
(`src/runtime/types/binding_signature.rs`, two call sites) spells its class
only via the `"X::Type: text"` message convention — `RuntimeError::new(...)`,
no `.exception` object attached. `Interpreter::enhance_binding_error`
(`src/runtime/calls.rs`) wraps every call-failure message in a "Calling
f(Int) will never work with declared signature (...)" prefix for
compile-flavored diagnostics, and it already has a precedent exclusion for
exactly this shape of problem — a subset/where constraint failure is
excluded because it "is a genuine *runtime* check in raku; it surfaces
verbatim, never as a compile-flavored 'will never work'" — but `is rw`
binding failure (equally a runtime check, and real `raku`'s own message for
it has no such prefix either) had no matching exclusion. Once wrapped, the
prefixed text no longer starts with `"X::Parameter::RW:"`, so the later
generic typed-message-convention parser can't recover the class and falls
back to `X::AdHoc`.

Fixed by adding a second exclusion, `err.message.starts_with("X::Parameter::RW:")`,
mirroring the existing subset/where one exactly. `roast/S06-traits/misc.t`
passes fully under both `Test` providers. Pin:
`t/rw-param-typed-exception-class.t` (4 assertions, verified against `raku`
too, including a negative control that a sigilless `\x` parameter — not `is
rw` — is unaffected).

**Lesson for the next `Got: X::AdHoc` file:** `enhance_binding_error`'s
message-prefix exclusion list is the first place to check whenever a binding
failure's class goes missing — a call site spelling its class only through
the message convention (no `.exception`) is invisible to the two branches
that DO preserve a class (`is_arity_error || is_type_only_mismatch`, and the
`else if let Some(ex) = err.exception` branch), so it silently loses its
typing the moment this wrap fires.

## One of the two remaining `X::Comp::Group` gaps closed: bare `5.` (2026-08-16)

Picked up the "3 items, none picked up yet" list from the `X::Inheritance::SelfInherit`
round above — down to 2 by then (`X::Parameter::BadType` leak and
`X::ControlFlow::Return` were both closed in later rounds; see above). Took
the `5.` half of the remaining pair: `throws-like '5.', X::Comp::Group,
sorrows => sub (@s) { @s[0] ~~ X::Syntax::Number::IllegalDecimal }`.

**Root cause:** the decimal-literal parser already backtracked cleanly when
`5.` had no fraction digit, but nothing downstream diagnosed the resulting
dead end — the postfix-`.` parser only special-cased *whitespace* after the
dot (`5. `) as the "Decimal point must be followed by digit" error; `5.`
immediately followed by end-of-input, `;`, `)`, `,`, `}`, `]`, `\n`, `=`, or a
lone `:` fell through to a method-call attempt with nothing to read, landing
on the generic `X::Syntax::Confused`. Also, even the existing whitespace case
raised a lone typed exception, not the `X::Comp::Group` (sorrow +
panic) rakudo actually throws — `PError::comp_group` already existed for
exactly this shape (see `check_bare_io_func`'s bare-`say` diagnosis and
`check_multi_underscore`'s underscore-run diagnosis, both in this file's
neighbourhood) but this call site wasn't using it.

Fixed in `src/parser/expr/postfix/loop_.rs`: a new
`illegal_decimal_point_error()` helper builds the `X::Comp::Group` (sorrow =
`X::Syntax::Number::IllegalDecimal`, panic class `X::Comp::AdHoc` labelled
"Confused" since rakudo's own second complaint varies by what follows the dot
— `Malformed postfix call`, `Unsupported use of . to concatenate strings`,
`Missing required term after infix`, depending on the exact next character —
and no roast assertion pins the panic's own class or the message's second
line, only `.sorrows[0]`'s class), reused by both the pre-existing whitespace
case and a new "nothing at all can follow" case.

**A broadened first attempt regressed `roast/S02-literals/numeric.t`,
caught before pushing:** treating any `:` after the dot as a dead end (unless
immediately followed by an identifier char) broke the legitimate
reified-operator postfixes `42.:<->`, `42.:«~»`, `42.:[...]`,
`42.:<<'~'>>` — `<`, `«`, `[` are not identifier characters either. Tightened
to only treat `:` as a dead end when the character *after* the colon is
ALSO one of the dead-end terminators (or end of input) — i.e. `5.:` alone
is illegal, `5.:<anything-that-could-start-a-postfix>` is not. This is the
same lesson as the `when`-gobbling attempt below: **verify a broadened parser
condition against every roast file that exercises the same punctuation
before trusting it, not just the one file that motivated the change.**

`roast/S32-exceptions/misc.t`'s `5.` subtest now passes under both `Test`
providers. Pin: `t/decimal-point-illegal-comp-group.t` (10 assertions,
verified byte-identical against `raku` for exception type/sorrow
count/sorrow class — `.message`'s *second* line was deliberately left
unpinned since it varies by construct, per above). Full `t/` suite (3189
files) and the six touched/neighbouring whitelisted roast files
(`S02-literals/numeric.t`, `S02-lexical-conventions/minimal-whitespace.t`,
`S32-exceptions/misc.t`, `misc2.t`, `S02-literals/radix.t`,
`S02-types/WHICH.t`) all clean; `cargo clippy -- -D warnings` clean.

**The other half — `when SomeUndeclaredType { }` — was tried and reverted,
not fixed.** Broadening `when`'s existing `X::`/`CX::`-only gobbling
detection (`src/parser/stmt/control/given_when.rs`) to any undeclared
bareword looked promising (raku genuinely gobbles the block for *any*
undeclared/forward-referenced/even-declared-sub bareword there, not just
`X::`/`CX::`-namespaced ones — verified directly against `raku`), but it
produces real false positives mutsu cannot rule out at parse time:
`when Kept { }` (`roast/packages/Test-Helpers/lib/Test/Util.rakumod` —
`Kept` is a builtin `PromiseStatus`-shaped constant mutsu represents as a
bare runtime string, not a registered enum value the parser can see) and
`when condition { }` (`roast/S04-statements/given.t` — `condition` is a
sigilless lexical, `\condition`, and the parser has no "is this name a
declared sigilless variable" check at all). Both broke multiple whitelisted
roast files transitively (a `Test::Util` compile failure takes down every
file that loads it) — caught locally before pushing, not by CI. A safe
general fix needs a registry of builtin enum-like constants and
sigilless-lexical name tracking in the parser first; left as a `todo/tickets/`
candidate for whoever picks this back up, not re-attempted this session.

## `when SomeUndeclaredType { }`: second attempt, reusing precedented infrastructure, also reverted (2026-08-16)

Came back to this same session with what looked like the missing prerequisite
in hand: `src/parser/expr/precedence/ternary.rs` and `list_infix_top.rs`
already solve the **identical** ambiguity (an undeclared bareword directly
before `!!`/end-of-item is a listop-call gobble, not a complete term) with a
mature, already-shipped exclusion set —
`is_known_type_constraint`/`is_known_compound_type`/`is_user_declared_type`
plus `is_builtin_enum_value`/`is_builtin_constant_term`/
`is_user_declared_value_term`/`is_user_declared_enum_value`. That last one,
`is_user_declared_value_term` (`src/parser/stmt/simple/user_ops.rs`), turned
out to already cover exactly the `condition` sigilless-param case from the
first attempt — `register_sigilless_terms` in `sub_decl.rs` already registers
a routine's sigilless params as term symbols when parsing its body, so the
"prerequisite" the first attempt's write-up asked for already existed; it
just wasn't being consulted at this call site. Added `Planned`/`Kept`/`Broken`
to `is_builtin_enum_value` (documented as representing mutsu's bare-string
`Promise.status`) to cover the other first-attempt false positive, and wired
`when_stmt` to the same guard set as the ternary sites (compound `::` names
still restricted to the `X::`/`CX::` check, since that half of the original
concern — forward-imported/not-yet-parsed compound type names — is
unaffected by any of this).

Verified both original false positives fixed (`Kept`, `condition`) — but a
**third and fourth** false positive surfaced on a *wider* regression sweep
(the first attempt was only checked against the specific files the first two
false positives came from; this round checked every whitelisted file
touching `Kept`/`Broken`/`Planned`/`ternary`/`given`/`ClassDecl`-in-a-block
patterns):

1. **`roast/S04-exception-handlers/catch.t`**: `class Naughty is Exception
   {}` declared inside a bare `{ ... }` block, then `when Naughty { }` used in
   a **later, separate** bare block after the first one exited. Plain
   (non-`my`) class declarations are supposed to stay bareword-visible after
   their declaring block ends (this file's whole point is testing exception
   superclass matching) — real `raku` treats `Naughty` as visible there. But
   `is_user_declared_type`'s `SCOPES` tracking only keeps a *bare* name
   (`register_user_type_verbatim`) in the scope that was active when
   registered; `register_user_type`'s "stays visible after the enclosing body
   ends" logic only promotes the **composed** (package-prefixed) spelling to
   the outermost scope, not the bare one, and bare blocks pop their own scope
   frame. So by the time the second block's `when Naughty` was parsed, no
   currently-active scope contained `"Naughty"` and the check misfired. This
   is a real, previously-latent gap in `is_user_declared_type`'s own model of
   Raku's non-lexical `class`/`package`/`role` visibility (already flagged,
   for the narrower EVAL-boundary case, at the end of the
   `X::Parameter::BadType leak, fixed` section above) — not something a
   `when`-local workaround should paper over.
2. **`roast/S32-exceptions/misc2.t`**: `given $bar { when Real { 1 } when Str
   { 2 } }` — `Real` is a genuine builtin Raku type but was simply **absent**
   from `is_known_type_constraint`'s list (verified: `grep '"Real"'` finds
   nothing). The roast assertion here doesn't even care about `when`'s
   semantics — it's testing an unrelated curly-brace hash/block
   disambiguation ("Strange text after block" in real `raku`) and only
   incidentally uses `Real`/`Str` as filler type names — but the missing
   registry entry let the new `when`-check misfire into a wrong diagnosis
   anyway.

Given TWO independent, previously-unknown gaps in the exclusion registries
surfaced on a regression sweep that only covered a few dozen files (not the
full corpus), the risk model from the first attempt's write-up holds:
**`is_known_type_constraint` is not provably complete, and `is_user_declared_type`
does not correctly model non-lexical class/package visibility across block
boundaries.** Reusing the ternary sites' exact guard is *necessary* but not
*sufficient* — those two sites happen not to be exercised by any roast file
that hits either gap, but `when` (used far more pervasively across the
corpus, including directly on `given`'s topic in loops over heterogeneous
values) is. Reverted `given_when.rs` back to the original `X::`/`CX::`-only
check and the `is_builtin_enum_value` addition (neither false positive
reproduces on the reverted tree; full `t/` suite and the ~20 touched roast
files all clean again). **Do not re-attempt this broadening without first
either (a) auditing `is_known_type_constraint` against the actual
`raku-doc/doc/Type/` type list for completeness, or (b) fixing
`register_user_type`'s scope model so a plain (non-`my`) declaration's bare
name outlives its declaring block the way its composed name already does —
whichever is cheaper turns out to be first will likely also fix the other's
symptom for free, since both are "the registry `when` would need to trust is
not yet trustworthy" instances of the same root problem.**

## Both prerequisites fixed (PR#6527) — and the `when` broadening *still* isn't safe (2026-08-16, same session)

Did (a) and (b) both, in full, same session: audited `is_known_type_constraint`
against every top-level (non-`::`-compound) type name documented under
`raku-doc/doc/Type/*.rakudoc` (`comm -23` between the two name lists) and
added every genuine gap found — `Real`, `Numeric`, `Rational`, `Callable`,
`Supply`, `Iterable`, `Iterator`, `PredictiveIterator`, `Associative`,
`Positional`, `PositionalBindFailover`, `Sequence`, `Stringy`, `Baggy`,
`Mixy`, `Setty`, `Dateish`, `Systemic`, `Encoding`, `Formatter`,
`ForeignCode`, `Collation`, `Proc`, `Signal`, `Order`, `Endian`,
`PromiseStatus`, `Scheduler`, `Telemetry`, `RaceSeq`, `RakuAST` (28 entries).
Fixed `register_user_type` (`pragma_preseed.rs`) to promote a plain
declaration's bare name to the outermost scope unconditionally, exactly
mirroring what it already did for the composed spelling — confirmed this is
safe because the function has never distinguished `my`/non-`my` at any of its
call sites, so the only behavioural change is that a `my`-scoped type now
also stays "known" to this *parse-time heuristic* after its block exits,
which is a strictly rarer and lower-consequence miss (a wrong exception
*class* at parse time) than the false positive being fixed (this registry is
never consulted by the actual runtime class registry, which has its own
correct lexical-scoping mechanism).

Re-applied the `given_when.rs` broadening from the previous entry with both
fixes in place. All five previously-known false positives (`Kept`,
`condition`, `Naughty` cross-block, plus a `Real`-shaped filler-type case)
now resolve correctly, and the same ~24-file roast sample used before passes
clean.

**Then a full local `prove -e target/debug/mutsu t/` (not just the roast
sample) surfaced 8 MORE previously-unknown false positives**, none
overlapping the earlier ones: `t/if-pointy-topic-under-given.t`,
`t/mustache-battery.t`, `t/pod-not-collected-from-heredoc.t`,
`t/pod-to-text-bundled.t`, `t/prelude-helper-not-block-lexical.t`,
`t/subst-smartmatch-topic-source.t`, `t/text-csv-battery.t`,
`t/when-value-through-block-local.t`. Confirmed by bisection (revert only
`given_when.rs`, keep the two registry fixes, re-run all 8) that every one of
these is caused by the `when`-broadening itself, not by either registry fix —
the registry fixes alone are clean against the full `t/` suite (3189 files)
and the roast sample.

**Final verdict, and why this ticket item stops here for now:** two
consecutive genuine architectural prerequisites, each fully fixed and each
independently valuable (shipped as PR#6527, the registry-only half), still
were not enough — a *third* round of false positives appeared on a wider
sweep, at roughly the same rate as the first two rounds (2-3 new ones per
~25-30 files newly exercised). This is a strong signal that the true
false-positive rate of "any undeclared/unknown bareword directly before `{`
in a `when` clause is a gobble" is NOT bounded by a finite, auditable set of
missing registry entries — `when`'s condition can legitimately be almost any
term shape (block-lexical `my` bindings the parser scope-tracking doesn't see
across certain block kinds, module-provided term symbols, battery-specific
helper names, ...) far more often than the two other sites (`?? then !!`,
list-infix) that this guard was borrowed from. **Extrapolating: fixing the
current known set would very plausibly uncover a 4th, 5th, ... round at the
same rate, each requiring a full-corpus (not sampled) sweep to catch — this
is not converging fast enough to be worth continuing within a single
session.** `given_when.rs` reverted to the original `X::`/`CX::`-only check
one more time (verified clean against the full `t/` suite again). The
registry fixes (PR#6527) are the net positive result of this investigation;
the `when` broadening itself stays unimplemented. If picked up again: run the
**full** `t/` suite (not a hand-picked roast sample) after every attempt,
budget for several rounds of whack-a-mole, and consider whether a
fundamentally different approach (e.g., a *runtime* fallback path in the VM
that still raises the right exception if the gobble genuinely happens, rather
than a parse-time static prediction) would be more tractable than trying to
make the static prediction complete.

## `&infix:«<»`/`»`/`<=`/`>=` called as a routine numified big rationals/bigints as 0 (2026-08-16)

Re-ran `scripts/test-module-sweep.sh` fresh (no code changes since PR#6527):
3156 pass under both providers, 16 regressed under the real `Test`, 2 pass
*only* under the real `Test`, 16 fail under both (pre-existing, out of this
ticket's scope). Picked `t/bigrat-sort-compare.t`'s regression —
`cmp-ok (-2**80 + 0.1).FatRat, '<', -0.5, ...` failed only under
`MUTSU_REAL_TEST=1`, reporting `got: FatRat.new(-12089258196146291747061759, 10)`
(the correct value) compared with `<` against `-0.5` and getting the wrong
boolean. Confirmed the plain infix `$x < -0.5` already returned the right
`True` — the difference is entirely in *how* `cmp-ok` invokes the comparator:
`Test.rakumod`'s `cmp-ok` (line 264) resolves the string `'<'` to
`&CALLER::LEXICAL::("infix:<$op>")` and then calls `$matcher($got,$expected)`
as an ordinary two-arg routine call, not a compiled infix expression.

**Root cause:** calling `&infix:«<»` as a sub routes through
`Interpreter::call_infix_routine` → `apply_reduction_op`
(`src/runtime/ops_reduction.rs`), whose `<`/`>`/`<=`/`>=` arms use a
**local, independently-reimplemented** `to_num`/`to_int` numeric-coercion
closure (distinct from the shared, already-correct
`crate::runtime::utils::to_float_value` that every *compiled* comparison
opcode uses — see `src/vm/vm_comparison_ops.rs`'s own `to_float_value`
wrapper). This local closure pattern-matched `Int`/`Num`/`Rat`/`FatRat`/
`Str`/`Bool`/`Enum`/`Array` and fell through to a silent `0.0`/`0` default
for anything else — but a numerator/denominator that overflows the inline
i64 `ValueView::FatRat(i64,i64)`/`ValueView::Rat(i64,i64)` view is boxed as
`ValueView::BigRat(&BigInt,&BigInt)` (and plain overflowing integers as
`ValueView::BigInt`), neither of which the closure had a case for. So
`(-2**80 + 0.1).FatRat` (an out-of-i64-range numerator) numified to `0.0`
when called as a routine, making `0.0 < -0.5` wrongly `False` (and the
reverse-direction comparison wrongly `True`). `==`/`!=`/`cmp` in the same
function were unaffected — they already route through the correct
`to_big_rat_parts`/`compare_big_rat_parts` helpers before ever reaching
`to_num`; only the `<`/`>`/`<=`/`>=` arms (line ~479-482) called it directly
with no such guard.

Fixed by adding `ValueView::BigInt(_) | ValueView::BigRat(..)` arms to both
closures in `apply_reduction_op`: `to_num` now delegates to the shared
`crate::runtime::utils::to_float_value` (avoiding a second reimplementation
of the arbitrary-precision-to-f64 scaling logic that already lives there),
and `to_int` does the analogous big-integer-division-then-clamp that its
existing `ValueView::BigInt` arm already did for the non-rational case.
Verified against `raku` directly (`&infix:«<»`/`»`/`<=`/`>=` called on big
`Int`/`Rat`/`FatRat` operands in both signs and both operand orders) — all
match byte-for-byte after the fix.

**A related, deeper gap noticed but deliberately NOT fixed in this round:**
`&infix:<div>`/`&infix:<mod>` called as routines on a `BigInt` that overflows
i64 (e.g. `&infix:<div>(10**30, 3)`) still give a wrong answer
(`3074457345618258602` instead of raku's exact
`333333333333333333333333333333`) — `to_int`'s pre-existing `BigInt` arm
clamps to `i64::MAX`/`i64::MIN` rather than doing true arbitrary-precision
integer division and returning a `BigInt` `Value`. This is a *different*
mechanism (not "unhandled `_ => 0` default", but "the whole `div`/`mod`
branch is i64-only by construction") and is not exercised by any current
regression — noted here for whoever next touches this function, not filed as
a separate ticket since it wasn't blocking anything in this sweep.

Pin: `t/bignum-infix-sub-comparison.t` (16 assertions: big `Int`, big `Rat`,
and big `FatRat` operands, both signs, both operand orders, across `<`/`>`/
`<=`/`>=`, plus the exact `cmp-ok` shape that started the investigation) — all
verified byte-identical against `raku`. `t/bigrat-sort-compare.t` now passes
under both `Test` providers. Full `t/` suite (3191 files, 29730 tests) and
`cargo clippy -- -D warnings` both clean; also spot-checked the relevant
whitelisted roast files (`S02-types/fatrat.t`, `S02-types/num.t`,
`S32-num/cool-num.t`, `S32-num/fatrat.t`, `S03-metaops/infix.t`,
`S03-operators/infixed-function.t`, `S06-operator-overloading/infix.t`)
clean on a release build.

## Re-measured 2026-08-18: t/ residue down to 17, and the single largest roast cluster is `S03-metaops/infix.t`

Fresh `scripts/test-module-sweep.sh` run (debug build): `t/` is down to
**3175/3209 clean, 17 regressed under the real `Test`** (mostly individual
gaps already named earlier in this file — `is-lazy-io-lines.t`,
`proxy-list-transparency.t`, `subscript-adverbs.t`,
`throws-like-gather-sink.t`, `whatever-code-fixes.t`,
`undeclared-when-type.t` — plus several new small ones not yet triaged:
`emit-done-controlflow.t`/`take-without-gather.t` (`emit without supply or
react`), `error-reporting-quality.t`, `exception-methods.t`,
`exception-role-membership.t`, `for-modifier-placeholder-scope.t`,
`malformed-syntax-classes.t`, `proto-new-no-match.t`,
`undeclared-symbol-exception-class.t`,
`user-trait-mod-does-not-consume-every-trait.t`,
`warn-resumes-at-the-raise-site.t`).

A full `roast-whitelist.txt` sweep (1436 files, release build) followed by an
alone/4x-timeout re-check of every raw regression: **141/1436 genuine
regressions** — up from the 90 last recorded here on 2026-08-14, despite
several individual fixes landing in between (2026-08-15/16 rounds above).
Not investigated further this session (the user explicitly said not to chase
why the count moved) — worth a fresh look next time this ticket is picked
up, since a rising count while unrelated general-interpreter work lands
implies something is regressing under `MUTSU_REAL_TEST=1` that nothing
currently monitors (this mode is not in CI).

Sorted by failed-subtest count, one file dominates: **`roast/S03-metaops/infix.t`,
171 of 2086 tests failing** (the file aborts mid-run on an unrelated,
pre-existing `Attempt to divide 4 by zero` a bit further in — same abort
point as before this session, not a new regression). Second place is
`S03-operators/range.t` at 40/181; nothing else exceeds 10.

Root-caused and partially fixed — full writeup, including a fix that was
tried, verified to make the file pass 5076/5076, and then **reverted** for
regressing 6 `t/` files, is in
`todo/deep/hash-pointy-param-writeback-loses-object-hash-identity.md`. Short
version: a `for`-loop hash pointy-block parameter aliased to an object hash
(`%ao{Any}`) loses its object-hash identity (`key_type`, `.WHICH`-keying) the
moment anything stores back into the loop's own bare name, because two
independent "materialize a fresh plain hash" coercion passes
(`coerce_hash_var_value` and the always-unconditional
`coerce_typed_container_assignment`) both assume every `%name = value` store
is a brand-new `my`-style declaration, with no way to recognize a write-through
re-store of an existing bound alias. The one genuinely *shipped* fix from
this investigation — `hyper_op_pair`'s hash-scalar branches (the *symbolic*
`%h{Any} >>op>> scalar` path, `vm_hyper_ops.rs`) dropping object-hash
identity — is unrelated to `S03-metaops/infix.t` itself (which always calls
through a lexical `&op`/`&metaop`, a different code path that was already
metadata-correct) but is a real, independently-pinned bug
(`t/hyper-hash-scalar-object-hash-type.t`).

The deep-ticket writeup names the actual fix shape needed: a hash analogue of
`Interpreter::quanthash_store_preserving_identity` (container-identity
in-place write via the audited `gc_contents_mut` unsafe primitive, ADR-0013
§8), gated the same way `inplace_old_hash` already is. Deferred to a
dedicated session — the `unsafe` aliasing contract needs careful auditing
against every hash-value caller, not a fold-in alongside an unrelated fix.

## Second-place regression closed: `S03-operators/range.t`'s topic corruption (2026-08-18)

The 40/181 second-place regression named above (`S03-operators/range.t`) is
fixed, and it was NOT a `Test`-shape problem at all — it was the same general
topic-corruption bug root-caused (correctly, this time) in
`news/2026-08/bind-topic-does-not-splice-into-ancestor-frames.md`:
`Test.rakumod`'s `throws-like` does `my $ex := $_;` inside its
`CATCH { default { ... } }`, and mutsu's `:=` bind machinery treated the
topic — chain-visible from nearly every frame on the call stack, since every
routine writes a fresh `$_` into its own env on entry — as a genuine outer
lexical worth splicing a shared cell into every ancestor frame. Fixed at the
five "propagate a promoted cell to ancestor frames" sites; `range.t` now
passes fully under both `Test` providers. This closes out the previously
open `todo/deep/module-catch-default-topic-leaks-to-callers-for-loop.md`
ticket, whose own diagnosis ("needs a real topic stack") was wrong — worth
noting since its "Other roast files combining a `for` loop with
`throws-like`/`eval-lives-ok`/`eval-dies-ok`" candidates list is now the
right starting point for re-measuring the residue with this fix in.

## `warn-resumes-at-the-raise-site.t` investigated, not yet fixed (2026-08-18)

Picked up the next item from the 2026-08-18 `t/` residue list above. Not the
topic-splice bug either, and not a `Test`-shape problem: a `sub` with a
`CONTROL { when CX::Warn { ...; .resume } }` handler, whose caller re-assigns
its multi-value return into already-declared scalars (`($x, $y, $z) = f(...)`
called more than once), gets a stale (function-entry-default) value for the
**first** target on the *second and later* calls — but only once `use Test;`
has loaded the real, large vendored module; an empty synthetic module does
not trigger it, ruling out "any module load" as the cause. Root cause not
yet found (JIT and bare-name collision were both ruled out). Full repro and
findings: `todo/deep/control-warn-resume-list-assign-first-target-stale-on-repeat-call.md`.

## `t/exception-methods.t` closed: `$!` was not lexical in a `&`-param block (2026-08-18)

Same investigation session, next residue item. `dies-ok { $!.message }` (the
real `dies-ok`'s own shape: `sub dies-ok(Callable $code, ...) { ...; try {
$code(); $death = 0 } ... }`) silently reported "did not die" instead of
dying — `Nil.message` doesn't raise the way `Any.message` does, and the block
was reading `Nil` instead of the caller's real `$!`.

Root cause: `vm_closure_dispatch.rs`'s closure-entry merge already force-
installs a captured `$_` over the don't-overwrite default (`entry_or_insert_sym`,
which uses the chain-walking `contains_key_sym`) for exactly this reason — see
the comment there — but had no matching case for `$!`. A block bound to a
`&`-sigiled parameter and called from inside a `sub` therefore saw the
*caller sub's own* fresh `$!` (reset to `Nil` on that sub's own entry,
visible through the parent chain) instead of its own captured value from its
creation scope. Fixed by adding `$!` to the same force-install branch as
`$_`, and separately corrected an unrelated but adjacent bug in the same
function: the "`$!` is scoped per routine" reset just below was gated on
`!data.name.is_empty()` (true for a `&`-bound block, which picks up its
parameter's name for introspection) instead of `cc.is_routine` (matching the
`$_` reset's own, correct guard) — harmless once the capture fix landed, but
still worth fixing for the same reason. Pin:
`t/closure-captured-bang-var-through-code-param.t`. `t/exception-methods.t`
now passes fully under both `Test` providers; full local `t/` suite (3222
files) and `cargo clippy -- -D warnings` both clean.

## `exception-role-membership.t` and `malformed-syntax-classes.t` triaged, not fixed (2026-08-18)

Two more residue items, both already-known gaps rather than new findings:

- `exception-role-membership.t`'s one failure is the `when SomeUndeclaredType
  { }` gobbling gap — already investigated and reverted twice above ("Both
  prerequisites fixed... and the `when` broadening *still* isn't safe").
  Nothing new to add; not re-attempted.
- `malformed-syntax-classes.t`'s one failure (`my @a = 1, => 2` expecting
  `X::Syntax::InfixInTermPosition`) is a genuinely new gap: the class is
  registered but nothing in the parser ever raises it — not the usual
  "diagnosis exists but gets flattened" shape this campaign has fixed several
  times. Filed as `todo/tickets/infix-in-term-position-not-diagnosed.md`
  (also blocks the same subtest in `roast/S32-exceptions/misc2.t`); deferred
  rather than attempted inline given this repo's parser needs the same
  full-corpus verification discipline as the `when`-broadening attempts
  above, which is more than a single-session slice.

## `emit-done-controlflow.t` and `take-without-gather.t` closed: bare `emit`/`done` were an uncatchable Rust-level panic under the real module (2026-08-18)

Both residue files aborted with `exit 255` under the real `Test` (`emit-done-controlflow.t`: "You planned 4 tests, but ran 0" then `Runtime error: emit without supply or react`; `take-without-gather.t`: the same shape). Root cause was NOT in `Test.rakumod` at all — mutsu's own `try`/`CATCH` machinery let a bare `emit`/`done` (no enclosing supply/react anywhere) escape uncaught past every `try`, all the way to the top of the program. `emit_signal()`/`react_done_signal()` both pre-set an `X::ControlFlow` exception (so `throws-like`/`CATCH` *would* report the right type if reached), but `RuntimeError::is_illegal_control()` — the predicate `vm_try_catch_ops.rs`'s `try` consults to decide "must this control signal keep propagating, or is there nothing left to consume it" — never matched `Control::Emit`/`Control::ReactDone`, only `Next`/`Last`/`Redo`/`Take`. So `try` always forwarded an emit/done signal, correct for the legitimate "there's a supply/react up the dynamic chain" case but wrong for the "nothing will ever consume this" case, which then had nowhere left to go and surfaced as an uncaught Rust-level error. mutsu's *native* `Test` provider's `throws-like` masked this (it special-cases control signals rather than routing through an ordinary `try`), which is exactly why this only showed up once the real module's `try`-based `dies-ok`/`throws-like` were driving it.

`Control::Emit` was safe to add to `is_illegal_control()` unconditionally: `emit_signal()` is *only ever* constructed when the raise site (`builtins.rs`'s `"emit"` arm) has already checked `active_supply_emitters`/`supply_emit_buffer` and found neither — the same "only ever illegal" invariant `Control::Take` already relies on there.

`Control::ReactDone` needed more care, because — unlike `emit`/`take` — the `OpCode::ReactDone` opcode (bare `done`) was raised **unconditionally**, with no such gate at the raise site: a legitimate `done` (with a real react/supply drive loop somewhere up the dynamic chain) has to keep propagating through nested `try`s exactly like `next`/`last`/`redo` do, so it cannot always be "illegal". Fixed the same way `loop_handler_depth.rs` fixed `next`/`last`/`redo`: split `react_done_signal()` (exception set, matches `is_illegal_control()`, used only when nothing dynamically active would consume it) from a new `done_signal()` (no exception, mirrors `last_signal()`/`next_signal()`/`redo_signal()`, used when something would). The `OpCode::ReactDone` raise site picks between them by consulting a **new thread-local depth guard**, `runtime::react_done_handler_depth` (`ReactDoneHandlerGuard`) — deliberately a thread-local rather than an `Interpreter` field (`react_active`/`supply_emit_buffer`), because the first attempt at this fix used those two fields directly and regressed `try { emit $v; done if $v == 2 }` inside a `react { whenever ... }`: the `whenever` callback runs on a scheduler **worker thread**, whose freshly-cloned `Interpreter` (`runtime_thread.rs`) starts both fields at empty/0 regardless of what the main thread set. A thread-local guard, held for the *dynamic extent of whichever thread actually runs the callback body*, is the only mechanism that is correct across that thread boundary — same reasoning `loop_handler_depth.rs`'s own doc comment gives for `next`/`last`/`redo`.

**The sweep had to stay complete** (same warning `loop_handler_depth.rs` gives): a bare `done` reaches `OpCode::ReactDone` through several *independent* body-dispatch choke points, not one, and each needed its own guard — found by constructing one repro per plausible dispatch shape and checking it against `raku` byte-for-byte, not by guessing:

| dispatch path | function | why it's separate |
| --- | --- | --- |
| `react { whenever ... }`, and any `whenever`/`LAST`/`QUIT`/`CLOSE` callback the live drive loop runs | `Interpreter::call_react_callback` (`vm_react_loop.rs`) | the single documented dispatch point for the live drive loop; covers `vm_react_subscriptions.rs`/`vm_react_supply_helpers.rs`/`vm_react_loop.rs` callers for free |
| `supply { whenever Supply.from-list(...) { ... } }.tap(...)` — a cold, supplier-less, channel-less replay | `Interpreter::replay_cold_whenever_capture::run_capture` (two near-duplicate closures, `supply_promise.rs`) | bypasses the live drive loop entirely; documented as its own mechanism in that function's own doc comment |
| `whenever $supplier -> $v { ... }` on a *live* `Supplier`-backed supply | `Interpreter::call_supply_tap` (`supply_promise.rs`) | the emit-time tap dispatcher; already handles `is_react_done()` itself (converts a stamped whenever's `done` to `$emitter.done()`) but had no guard around its own `call_sub_value` |
| the react body's own top-level statements (a bare `done` not inside any `whenever`) | `exec_react_scope_op` (`vm_scope_ops.rs`) | runs before the drive loop starts; distinct from every callback path above |
| `(1..Inf).Supply.tap({ ...; done if ... })` — a plain values-array tap, no `whenever` involved | the tap loop in `native_supply_mut_methods.rs` | already had its own `is_react_done()`/`is_last()` arm, no guard |
| a live-streaming consumer registered via `supply_stream_consumers` | `Interpreter::try_stream_emit` (`subtest.rs`) | already had its own `is_react_done()` arm, no guard |
| a `whenever <Promise>` marker's replay loop | `supply_promise.rs` (the `cbs.into_iter()` loop) | separate from the cold-replay closure above despite living in the same function |
| a `.lines`/decode background reader thread's per-chunk tap dispatch | `native_methods/encoding.rs` | runs on its own spawned reader thread, same cross-thread reasoning as the react-worker-thread case |

Each was found by constructing the smallest repro for that specific shape (`Supplier.new` + live `.emit`, `react { whenever }`, a plain on-demand `.tap()`, ...), confirming it diverged from `raku`, then tracing the exact `call_sub_value`/`call_react_callback` site with a `rust-gdb -batch` breakpoint + backtrace (never `eprintln!`) rather than guessing. `invoke_done_callback`'s own `done => { ... }` tap-consumer-callback dispatch (`native_supply_methods.rs`) was deliberately left unguarded — a `done` written *inside* a tap's own `done =>` handler is an edge case with no known real-world use and no test exercising it; if it turns out to matter, `roast`/CI will catch it as a deterministic failure, per this repo's stated "CI + roast are the safety net" policy, rather than being guessed at now.

Verified against `raku` byte-for-byte for every dispatch path above (`try { emit 1 }` / `try { done }` both bare and nested three shapes deep). Pin: `t/react-done-catchable-outside-supply.t` (5 assertions: bare catchability, plus a regression guard per dispatch shape — cold on-demand `.tap()`, `react { whenever }`, live `Supplier`-backed `whenever`). Full local `t/` suite (3224 files) and `roast/S17-supply/*.t` (58 files) both clean under both `Test` providers; `cargo clippy -- -D warnings` clean.

A separate, pre-existing (not a regression — reproduces identically on `main`) gap surfaced while constructing repros and is left unfixed: `supply { sub d() { emit 1; done }; d(); emit 2 }` — a bare `done` reached via a *nested sub called directly from the supply body's own top level* (not through any `whenever`) — still escapes uncaught. `run_on_demand_body` (`supply_promise.rs`) is the relevant dispatch point if this is picked up later; it was not touched here.

## `undeclared-symbol-exception-class.t` closed: `True()`/`False()`/`Inf()`/`NaN()` under EVAL had the wrong exception class (2026-08-18)

The one remaining failure ("True() likewise") was **not new** — it is the same bug `news/2026-08/undeclared-variable-is-not-undeclared-symbols.md` fixed for `e()`/`pi()`/`tau()`/`i()` (a CORE term constant called as a routine is `X::Undeclared` naming `&name`, not `X::Undeclared::Symbols`), just not fully applied. That fix touched two independent places that both have to agree: the *runtime* fallback (`CORE_TERM_CONSTANTS` in `undeclared_routines.rs`, consulted by `builtins_operators_fallback.rs`'s last-resort call handler) already listed all eight names (`e`, `i`, `pi`, `tau`, `Inf`, `NaN`, `True`, `False`) and answers correctly when reached directly (`mutsu -e 'try { True() }; say $!.^name'` → `X::Undeclared`, already correct before this fix). But `EVAL "True()"` goes through a *second*, earlier gate first: `check_eval_undeclared_routines`'s CHECK-time pre-pass (`system_eval_names.rs`) has its own, separately-maintained exemption list, `EVAL_KNOWN_ROUTINE_NAMES` — and it had `e`/`i`/`pi`/`tau` but not `Inf`/`NaN`/`True`/`False`, so those four got flagged as a generic undeclared routine (wrong class, `X::Undeclared::Symbols`) by the pre-pass before the correct runtime path was ever reached. Found by a `rust-gdb -batch` breakpoint on the runtime fallback's `CORE_TERM_CONSTANTS` check confirming it *was* reached (and answered correctly) for a direct call but never fired for the `EVAL`-wrapped one — pointing straight at an earlier interception rather than a bug in the fallback itself.

Fixed by adding the missing four names to `EVAL_KNOWN_ROUTINE_NAMES`, keeping the list's existing alphabetical order. Verified byte-for-byte against `raku` (`.^name`/`.symbol` match; `.message` text differs slightly from `pi`/`e`'s own pin, matching the pre-existing wording gap already accepted there, not a regression introduced here). Pin: `t/eval-core-term-constant-call-undeclared-class.t`. Full local `t/` suite (3225 files) clean under both `Test` providers; spot-checked `roast/S04-exceptions/exceptions-alternatives.t`, `roast/S32-exceptions/misc.t`, `roast/S32-exceptions/misc2.t`; `cargo clippy -- -D warnings` clean.

## `user-trait-mod-does-not-consume-every-trait.t` closed: two byte-identical `multi` candidates should not be ambiguous (2026-08-18)

The one remaining failure in this file ("the user trait_mod:<is> still accepts its own trait") was a **general multi-dispatch bug**, not a `Test`-shape problem. `Test.rakumod` itself exports `multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) is export` (verified in the vendored copy), and this test file — deliberately, per its own comment — declares an **identical** candidate itself. Confirmed against real `raku` that this is not accidental: `raku -e 'multi sub foo(Int $x, :$bar!) {"first"}; multi sub foo(Int $x, :$bar!) {"second"}; foo(1,:bar)'` prints `first`, not an ambiguity error — Rakudo silently keeps whichever declaration wins the sort-by-declaration-order tie-break, whether it is a self-import shadow or a plain duplicate. mutsu's `choose_best_matching_candidate` (`dispatch_candidates.rs`) already computes that same first-declared candidate for its `matches[0]`, but its ambiguity check downstream (`tied.len() > 1` after narrowness-tie-breaking) had no exemption for the case where the tie is a genuine duplicate, so it always raised `X::Multi::Ambiguous` once two candidates tied on every narrowness axis it tracks.

Fixed by adding one more check before raising the error: if every tied candidate's **full parameter list** (`FunctionDef::param_defs`, not just the narrowness-relevant [`candidate_dispatch_shape`] subset) is identical to the winner's, it is a duplicate declaration and `matches[0]` (first by `decl_order`, already sorted) is returned directly. Two things `candidate_dispatch_shape` alone could not distinguish, both needed for correctness:

- **Named-parameter names.** The shape tuple carries a named param's *type* but not its *name*, so `:$bar!` and `:$baz!` look identical there — comparing the full `param_defs` catches the difference (`:$bar!` vs `:$baz!` is a real overload, not a duplicate).
- **Params after a `;;` long-name separator.** [`Self::dispatch_visible_params`] filters to only `multi_invocant` params, so two candidates that agree on everything dispatch-relevant but differ after `;;` — `multi f(;; Any $v)` vs `multi f(;; Int $v)`, exercised by `t/multi-sig.t`'s "types after `;;` do not make one candidate narrower" — have *empty* dispatch-visible lists and would trivially "match" if compared that way. Comparing `param_defs` (the full list, `;;`-only params included) keeps this case genuinely ambiguous, as Rakudo requires. Caught this by running the full `t/` suite before committing, not by reasoning it out — `t/multi-sig.t`'s existing pin failed on the first attempt (which compared `dispatch_visible_params`) and pointed straight at the gap.
- One positional-parameter subtlety fixed en route: a parameter's own *variable name* (`$x` vs `$y`) is never dispatch-significant in Raku for a positional param, only for a named one — the comparison only checks `x.name == y.name` for named params, else two candidates differing solely in a positional variable name would be wrongly reported as distinct (still ambiguous) instead of being recognized as the same declaration.

Verified against `raku` for every case in the new pin. Pin: `t/multi-identical-signature-not-ambiguous.t`. Full local `t/` suite (3225 files), `roast/S06-multi/*.t`, `roast/S12-methods/*.t`, `roast/S32-exceptions/*.t` all clean under both `Test` providers; `cargo clippy -- -D warnings` clean. Landed as its own PR, separate from the `emit`/`done` fix above — an unrelated area of the interpreter, just found while triaging the same residue list.

A related but **out of scope** pre-existing gap noticed while writing the pin (reproduces identically on `main`, not touched here): `multi sub f(Int $x, :$a) {...}; multi sub f(Int $x, :$b) {...}; f(1)` — two candidates differing only in an *untyped, optional* named parameter's name, called with neither named arg supplied — is wrongly `X::Multi::Ambiguous` in mutsu; `raku` picks the first-declared candidate (same declaration-order rule the fix above formalizes, just not extended to this narrower "both apply, differ only in an unused optional named" case).

**Correction, caught by CI (`make roast`) before merge, not locally:** the first version of this fix was too broad. It applied the "byte-identical signature is not ambiguous" leniency unconditionally, but Rakudo does **not** extend it to two purely *positional* duplicate declarations with no named parameter at all — `roast/integration/advent2011-day24.t` (whitelisted) deliberately pins `multi sub Slurp($filename) {...}` declared twice as genuinely `X::Multi::Ambiguous`, and the broad version silently ran the first one instead, which meant `throws-like { EVAL $ambiguous ~ ... }` never threw and the extra `fail "Yuck!..."` TAP line threw off every following subtest's numbering (`Failed tests: 6, 9`, `planned 8 but ran 9`). Confirmed against `raku` directly that the presence of *any* named parameter (typed or not, required or not) is the actual dividing line — `multi sub f(Int $x, :$bar!) {...}` twice: first wins, not ambiguous; `multi sub g(Int $x) {...}` twice: `X::Multi::Ambiguous` — matching the existing `candidate_declares_named`/`best_has_named` narrowness dimension already computed a few lines above in the same function. Gated the exemption on `best_has_named`, added a fourth pin assertion for the positional-duplicate case, and re-verified `roast/integration/advent2011-day24.t` passes again. Locally `make test`/the targeted roast files this session ran were not enough to catch this — worth remembering `roast/integration/*` (real-world code samples, not spec-focused) exercises shapes the topic-focused suites (`S06-multi`) don't happen to hit; a broader `roast/integration/*.t` sweep is cheap and worth doing by default for any dispatch-adjacent change.

## `for-modifier-placeholder-scope.t` closed: a bare-name-before-placeholder conflict was raised as a plain string, not a typed instance (2026-08-18)

Next `t/` residue item from the 2026-08-18 list above. The one failing subtest
(`a bare '$b' in a statement before its '$^b' is undeclared`) reported the
right *message* (`X::Undeclared: Variable '$b' is not declared. Did you
mean '$^b'?`) but the wrong *class*: `.^name` came back `X::AdHoc`, not
`X::Undeclared`. mutsu's native `throws-like` masked this — it matches on
`err.message.contains("X::Undeclared")` (`test_functions/throws_like.rs`),
so the message-embedded class name alone was enough to pass under the native
provider — but the real module's `throws-like` does a genuine `.^name` /
`~~` check.

**Root cause, same shape as the several `typed-exception-class-from-the-
message-convention` fixes earlier in this file:**
`Compiler::check_placeholder_conflicts` (`helpers_call_args.rs`) has two
branches that detect a bare `$name` used before/instead of its own `$^name`
placeholder. One neighbor branch a few lines above already builds a real
`Value::make_instance(Symbol::intern("X::Placeholder::NonPlaceholder"),
attrs)`; both of *these* two branches instead built a bare `Value::str(format!
("X::Undeclared: ..."))` — a plain string die payload, which the `Die`
opcode wraps as `X::AdHoc` with that string as its `payload`, not a
typed exception at all. Confirmed via `rust-gdb -batch` breakpoints on both
`vm_value_helpers.rs`'s `strict_undeclared_error` and
`system_eval_vars.rs`'s `check_eval_undeclared_vars` that *neither* of those
(already-correctly-typed) undeclared-variable paths was even reached for this
repro — the string literally came from `helpers_call_args.rs` instead, found
by grepping for the exact message text once the two likely candidates ruled
themselves out.

Fixed by building a real `X::Undeclared` instance in both branches (`name`,
`symbol`, `post`, `highexpect`, `suggestions`, `message` attrs, matching the
shape `RuntimeError::undeclared_variable`/`strict_undeclared_error` already
use elsewhere). While fixing the first branch, also caught and fixed an
independent message-wording bug found by checking against `raku` directly:
that branch said `"Did you mean '$^b'?"`, but `raku -e 'EVAL q[my $f = { say
$b; say $^b }; $f(1)]'` never offers `$^b` as a suggestion for the bare `$b`
read — real Raku's suggestion mechanism doesn't consider placeholders as
candidates for a scalar typo-suggestion, so it falls to the same default
`"Perhaps you forgot a 'sub' if this was\nintended to be part of a
signature?"` wording the sibling branch already used. Both branches now emit
that message, byte-identical to `raku` in both repro shapes.

Pin: `t/undeclared-bare-var-before-placeholder-typed-exception.t` (4
assertions: `isa-ok`/`.message` for both branches — the bare-name-precedes-
own-placeholder shape, and the bare-name-shadowed-by-a-nested-block's-own-
placeholder shape, the latter needing the `{ ... }()` immediate-call form to
actually route through the fixed branch rather than the unrelated,
already-correctly-typed runtime fallback that a bare `{ ... }` statement
block falls to when it has no placeholder of its own). Full local `t/` suite
(3239 files, 30012 tests) and the placeholder-related roast files
(`S06-signature/{named,mixed,positional,slurpy}-placeholders.t`,
`S04-declarations/implicit-parameter.t`,
`S04-blocks-and-statements/pointy.t`) clean under both `Test` providers;
`cargo clippy -- -D warnings` clean. `t/for-modifier-placeholder-scope.t` now
passes fully under `MUTSU_REAL_TEST=1`.

## `proto-new-no-match.t` closed: a mutsu-only test-file bug, not an interpreter gap (2026-08-18)

Next residue item. `throws-like`'s second call declared `class Polar { ... }`
a second time (a *different* string, same class name as the first
`throws-like` call a few lines above). Under the real module this failed
with `Got: X::Redeclaration` instead of the expected `X::Comp::BeginTime`.
**Verified directly against `raku` with the real `Test` module** that this
is not a mutsu bug at all: `raku`'s own `EVAL` is not isolated between
separate `throws-like` calls either, so a second top-level `class Polar { }`
genuinely collides with the first `EVAL`'s declaration and raises
`X::Redeclaration` there too — reproduced byte-for-byte, including the exact
"Got: X::Redeclaration" / "Exception message: Redeclaration of symbol
'Polar'." wording. mutsu's *native* `throws-like` masked this because it
spins up a fresh nested `Interpreter` per call (per the "Where the alias
stands" section far above), so the two `class Polar` declarations never
actually shared a registry there — an isolation the real module's `EVAL`
genuinely does not provide.

Fixed by renaming the class in the second `throws-like`'s EVAL string
(`PolarConst`, verified against `raku` to restore the intended
`X::Comp::BeginTime` wrapping `X::Multi::NoMatch` behavior with zero
interpreter changes). No pin needed beyond the existing file, which now
passes under both `Test` providers. This is the same "test file to correct"
category several `t/` residue items have fallen into before — always check
the exact repro against `raku` (with the real module, not just plain
`EVAL`) before assuming an interpreter gap.

## Re-measured 2026-08-18 (evening): `t/` residue down to 9, `error-reporting-quality.t` already fixed as a side effect

Fresh `scripts/test-module-sweep.sh` run (debug build, 8-way): **9 / 3224
regressed** (down from 17 at the top of this section). `error-reporting-quality.t`
— one of the "new, not yet triaged" files named there — is no longer in the
regression list at all: both `Test` providers pass it cleanly now (confirmed
directly, no code change needed), almost certainly a side effect of one of
the other general fixes landed the same day (topic-splice / multi-dispatch /
placeholder fixes above). Nothing to do there.

The remaining 9, none newly investigated this pass except the one below:
`exception-role-membership.t`, `is-lazy-io-lines.t` (×2 assertions),
`malformed-syntax-classes.t`, `proxy-list-transparency.t`,
`subscript-adverbs.t` (×2), `throws-like-gather-sink.t`, `undeclared-when-type.t`,
`warn-resumes-at-the-raise-site.t` (×2) — all already named/triaged/ticketed
earlier in this file — **plus one new file**: `has-attr-binding.t`, whose one
failing assertion ("binding $!x tracks source changes") is a genuine, deep
VM bug, not a `Test`-shape difference — full writeup, root cause trace, and
suggested fix shape in
`todo/deep/attr-bind-source-write-lost-through-nested-sub-call-chain.md`.
Short version: a `$!x := $var` attribute bind's shared cell is silently lost
when the write to `$var` happens back through a multi-frame sub/closure call
chain (`lives-ok { $obj.bind() }` specifically) — `VmCallFrame` has no
per-frame reference to its own `CompiledCode`, so the ancestor-frame
propagation loop in `vm_var_assign_set_local.rs` that's supposed to splice
the freshly-bound `ContainerRef` back into every frame that owns the source
variable can never correctly index into `saved_locals` for a cross-frame free
variable. Possibly the same root cause as the (now-fixed, see below)
`warn`-resume caller-var collision bug — same "only reproduces through a
real, multi-frame `Test` module call chain" shape — worth investigating
together.

## `warn-resumes-at-the-raise-site.t` root-caused and mostly fixed (2026-08-19)

Picked back up the `warn-resumes-at-the-raise-site.t` investigation from
above. Root-caused and fixed:
`news/2026-08/control-warn-resume-caller-var-name-collision.md`. Short
version: `Interpreter::call_compiled_closure_with_topic`'s closure-return env
writeback has a guard meant to stop a closure's own untouched captured
binding from leaking into a same-named caller lexical (`captured_names`
present + live value identical to the closure's capture-time snapshot ⇒
skip). That guard is a false positive when the "no change" is coincidental —
specifically when a resume-safe `CONTROL` handler
(`try_resume_safe_control_inline`) writes an ANCESTOR frame's lexical
in-band during the call, and that write happens to reproduce a value the
closure's own blanket env-capture snapshot already held (e.g. the caller
variable already equals the `CONTROL` handler's target value from an earlier
call in the same program). `inline_control_env_writes` changed from a
counter to a `Vec<Symbol>` log of the names actually written, so the
writeback scan can exempt them from the skip. Pin:
`t/control-warn-resume-caller-var-name-collision.t` (spawns a subprocess with
`MUTSU_REAL_TEST=1` — a synthetic module with many declared subs does not
reproduce it, only the real vendored `Test.rakumod` does, so the trigger
needs the real switch on even though the bug itself is a general interpreter
bug unrelated to `Test`).

This does **not** fully close the file: test 8 has a *different*, deeper bug
that survives the fix. Root-caused (2026-08-19) as unrelated to method
dispatch (the original suspicion) — it reproduces with three plain `warn`
calls sharing the caller's variable names, with no method dispatch involved
at all: a *sunk, statement-level list reassignment* (`($x,$y,$z) = f(...);`,
as opposed to a `my (...) = f(...);` declaration) emits a discarded
alias-array construction whose `WrapVarRef` handling boxes the reassigned
names into shared `ContainerRef` cells and writes them into the flat,
cross-frame `env` store — even though the array is immediately thrown away.
That leaks a stale `ContainerRef` into `env["x"]`/`env["y"]`/`env["z"]`,
corrupting the *next* call's env-by-name reads (specifically
`try_resume_safe_control_inline`'s handler-locals seed) once the caller and
callee share those bare names. Filed as
`todo/deep/sunk-list-reassign-leaks-containerref-into-shared-env.md`.
`t/warn-resumes-at-the-raise-site.t` therefore stays in the `t/` residue list
below pending that ticket.

## `t/warn-resumes-at-the-raise-site.t` fully closed: the sunk-list-reassign leak (2026-08-19)

The ticket above is fixed —
`news/2026-08/sunk-list-reassign-containerref-leak.md`. Root cause confirmed
with `rust-gdb -batch` breakpoints (conditional on the declaring statement's
local slot index, then backtraces off `Env::insert`/`insert_sym` hits, no
rebuild needed per hypothesis): a bare (sunk) list-reassignment statement's
own discarded rvalue was compiled unconditionally (`WrapVarRef`+`MakeArray`,
boxing each target into a shared `ContainerRef` written into the flat `env`),
even though a `SinkPop` immediately discards it. The stale cell then got
picked up by the next unrelated closure literal sharing the same env keys,
because `capture_closure_env` under `reflective_name_access_possible()`
snapshots a closure's *entire* flat env at creation time rather than just its
real free variables — so `{ warn "boom3" }`, which never references
`x`/`y`/`z`, still captured them, and its closure-entry merge force-
overwrote `f`'s own freshly-declared locals with the stale cell.

Fixed at the compiler: `Stmt::Expr` now recognizes a top-level list-
reassignment to existing variables and sets a one-shot flag consumed
immediately inside the list-assign compilation (before the RHS or any
chained/nested assignment is compiled), skipping the discarded result-list
construction entirely. Verified against `raku` that the non-sunk cases
(consumed as an argument, `my $l = (...) = ...`) still preserve list-element
container aliasing. Pin:
`t/sunk-list-reassign-does-not-leak-containerref.t`. Full local `t/` suite
(3244 files, 30044 tests) and `cargo clippy -- -D warnings` both clean; the
two unrelated `t/io-socket-async-*.t` failures in the same run reproduced
identically on the pre-fix binary (confirmed by `git stash`-ing the change
and rebuilding) and pass individually — pre-existing parallel-load flakes,
not a regression. `t/warn-resumes-at-the-raise-site.t` now passes 8/8 under
`MUTSU_REAL_TEST=1`.

`t/` residue is now down to 8 (from the 9 named in the 2026-08-18 evening
re-measure above, minus this file): `exception-role-membership.t`,
`is-lazy-io-lines.t` (×2 assertions), `malformed-syntax-classes.t`,
`proxy-list-transparency.t`, `subscript-adverbs.t` (×2),
`throws-like-gather-sink.t`, `undeclared-when-type.t`, and
`has-attr-binding.t` — all already triaged/ticketed above. Worth a fresh
`scripts/test-module-sweep.sh` run next session to confirm and re-measure the
roast side (`S03-metaops/infix.t` and the other roast regressions named in
the 2026-08-18 sections above were not re-swept this session).

## Re-measured 2026-08-19: residue down to 6 after main picked up `has-attr-binding.t`'s fix; `proxy-list-transparency.t` closed

`main` had already picked up the SetGlobal bind-source fix
(`news/2026-08/attr-bind-source-write-tracked-through-nested-call-chain.md`,
PR #6675) and the loop-body keep/undo fix since the last measurement in this
file; pulling it and re-running `scripts/test-module-sweep.sh` (debug, 8-way)
dropped the residue to **6 / 3225** with no code change:
`exception-role-membership.t`, `is-lazy-io-lines.t` (×2 assertions),
`proxy-list-transparency.t`, `subscript-adverbs.t` (×2),
`throws-like-gather-sink.t`, `undeclared-when-type.t`.
`malformed-syntax-classes.t` and `has-attr-binding.t` are gone from the list
without further work here.

`proxy-list-transparency.t` is now closed too. Root cause: `Value::eqv` is a
pure, interpreter-free comparison (no `&mut Interpreter`), so it cannot call
a `Proxy` element's FETCH callback. `eval_binary_with_junctions` already
auto-FETCHes a *top-level* Proxy operand before calling `eqv`
(`vm_helpers_junction.rs`), but a Proxy nested one level down — inside an
Array/List/Hash/Pair, e.g. `(1, 2).map({ Proxy.new(...) }).List` — was passed
through untouched, so `$got eqv $expected` (what the real `Test.rakumod`'s
`is-deeply` reduces to, `_is_deeply` at line 713-715) compared the raw
`Proxy` objects instead of their fetched values and always returned `False`.
Fixed in `exec_eqv_op` (`vm_comparison_order_ops.rs`) by calling the existing
`resolve_proxies_in_value` (added for `t/`'s native-provider test-argument
path, `builtins_lvalue.rs`) on both operands before comparing — a cheap
no-allocation scan in the common Proxy-free case, deep-FETCHing every nested
Proxy otherwise. Pin: `t/eqv-fetches-nested-proxy-elements.t` (top-level
Proxy still FETCHes, nested inside Array/List/Hash/Pair now FETCHes too, and
a genuinely-different nested-Proxy list correctly stays not-`eqv`) — verified
byte-for-byte against `raku`. Full local `t/` suite and `cargo clippy -- -D
warnings` clean; `t/autoviv-index-guard.t` timed out under `-j` load during
the same `make test` run but reproduces identically on `main` with the fix
`git stash`-ed out (confirmed directly, not this change) — pre-existing,
unrelated to `eqv`/`Proxy`.

## `throws-like-gather-sink.t` partly closed (1 of 4 subtests), same 2026-08-19 session

Its first
subtest — `throws-like 'gather { return 1 }', X::ControlFlow::Return` —
turned out not to need the frame-depth mechanism this ticket's own
`eval-context-frame-owns-the-return-target.md` describes at all. The real,
narrower bug: a bare `EVAL '...';` statement never *forces* a deferred
`gather`/lazy-IO-lines result at all, regardless of `context`. `SinkPop`
(the bytecode a bare non-`EVAL` statement's discarded value goes through)
already forces a `LazyList`/`LazyIoLines`, but `EVAL` compiles to a
different, "statement-level call, no return value kept" form
(`OpCode::ExecCall`/`ExecCallPairs`) that never reaches `SinkPop` at all —
confirmed with `rust-gdb -batch` breakpoints: a `SinkPop` breakpoint fires
for a bare top-level `gather {...};` statement but never fires for `EVAL
'gather {...}';`. So `EVAL 'gather { return 1 }';` silently never ran the
gather body, and `throws-like`'s own `EVAL $code, context => $ctx;` (a bare,
named-arg, mid-body statement — exactly this shape) never saw the `return`
at all. Fixed by adding the same `LazyList`/`LazyIoLines`-forcing arms
`SinkPop` has to `sink_discarded_call_value`
(`vm_call_exec_ops.rs`), the shared helper both `ExecCall` and
`ExecCallPairs` already used for the Failure-exploding half of sink
semantics. Pin: `t/eval-statement-sinks-lazy-result.t`, verified against
`raku`. `news/2026-08/eval-statement-sink-forces-lazy-result.md`.

The other 3 subtests of `t/throws-like-gather-sink.t` (`return;`/`for ^5 {
return; }` at mainline, both wrapped in `throws-like`'s `context =>
$caller-context`) still need the actual frame-depth mechanism —
`todo/deep/eval-context-frame-owns-the-return-target.md` is unchanged and
still the right next step there; do not assume the sink-forcing fix above
covers it (it only fixed the "body never even ran" half of the file's first
subtest).

Residue after this session: **5 files, 5 assertions**:
`exception-role-membership.t`, `is-lazy-io-lines.t` (×2), `subscript-adverbs.t`
(×2), `throws-like-gather-sink.t` (3 remaining subtests, needs
`eval-context-frame-owns-the-return-target.md`), `undeclared-when-type.t`.
`exception-role-membership.t` and `undeclared-when-type.t` share the exact
same underlying gap: `when SomeUndeclaredType { ... }` should be a
compile-time `X::Comp::Group`/"needs parens to avoid gobbling block" parse
ambiguity (raku's parser cannot tell whether an undeclared bareword followed
by `{` is a type smart-match or a routine call taking the block as its sole
argument), which mutsu's parser does not diagnose at all today — a genuine
parser feature gap.

Investigated 2026-08-19: the obvious fix (drop `when_stmt`'s existing
`X::`/`CX::`-only "gobbled block" diagnosis down to any bareword) is unsafe
as-is — it would misdiagnose real cross-file `when SimpleTypeName { ... }`
usages in the batteries corpus (e.g. `Cro::HTTP::ResponseParser`'s `when
Header { ... }`, declared in a sibling file) as a parse error. Split out to
its own ticket:
`todo/deep/when-undeclared-bareword-gobbles-block-needs-cross-file-type-index.md`.

## Re-measured 2026-08-20 — and `scripts/test-module-sweep.sh` has been under-reporting the `t/` residue by ~4x

Picked the ticket up cold and re-measured both sides on `3d9c3bdd3`
(`origin/main`, release build, 12-core box) before trusting any number in this
file. Two of the three results are good news; the third invalidates every `t/`
residue count recorded here since 2026-08-02.

**The vendoring itself is intact.** `modules/Rakudo-Core/lib/Test.rakumod` is
still the unmodified 953-line upstream file (md5
`f34dec45d52ad099c37f42fdbd93e277`, `unit module Test;` unrenamed), the
`MUTSU_REAL_TEST` switch is still `Interpreter::real_test_module_enabled()`
(`src/runtime/runtime_module.rs:16`), and `t/vendored-real-test-module.t` still
pins the functional half in ordinary CI. Nothing about step 2 has rotted.

### The measurement bug: a mid-file abort emits no `not ok`, so the sweep scores it as a pass

`scripts/test-module-sweep.sh` classifies a run with

```sh
passes() { ! grep -qE '^(not ok|Runtime error|Parse error|===SORRY)' "$1"; }
```

A file that aborts on the real module's own END plan check prints **only**
`# You planned N tests, but ran M` and exits 255 — no `not ok`, no
`Runtime error`. The predicate calls that a pass. That is precisely the
"mid-file abort" failure shape this file's *roast*-side sections have tracked
since 2026-08-02 (the roast sweeps classify by exit status, so they were never
affected) — but the `t/`-side numbers all came from this text predicate.

Measured both ways over the same corpus, same binary, same run conditions:

| `t/` classification | regressed under the real `Test` |
| --- | --- |
| `scripts/test-module-sweep.sh` (grep for `not ok`) | 4 |
| exit status of the same two runs | **24** |
| of those 24, reproduced from the repo root | **20** |

(The 4 that do not reproduce — `any-type-object-int-coercion.t`,
`bound-nil-method-warn.t`, `type-object-numeric-coercion.t`, `warns-like.t` —
fail only inside the sweep's `tmp/test-module-sweep/` working copy, i.e. they
are artifacts of that harness's cwd, not of the real module. Note the same
harness inflates its "fail under both" bucket for the same reason: 17 by the
text predicate, 76 by exit status, mostly files that need the repo root as cwd.
Only the *regressed* column — native exit 0, real exit non-zero — is meaningful
either way.)

So the "residue: 5 files, 5 assertions" line at the end of the 2026-08-19
section is wrong; the real figure at that point was of the same order as the 20
below. **The first slice for whoever picks this up is fixing the predicate**, or
every future re-measure repeats the error:

```sh
# in run_one, capture the status of each run:
( cd "$WORK" && MUTSU_REAL_TEST= timeout 90 "$MUTSU" ... ; echo "$?" > "$WORK/$name.native.st" )
( cd "$WORK" && MUTSU_REAL_TEST=1 timeout 90 "$MUTSU" ... ; echo "$?" > "$WORK/$name.real.st" )
# ...and classify on those two numbers instead of calling passes().
# Also add `# You planned` to the detail grep, or the regression report is blank
# for exactly the files this bug was hiding.
```

`tmp/t-sweep-status.sh` in the working tree of this investigation is that
script; it was deliberately not committed, since this pass was
investigation-only.

### The honest `t/` residue: 20 files

Confirmed one by one from the repo root (native run exits 0, real run does not):

| file | real exit | shape |
| --- | --- | --- |
| `bare-precedes-placeholder-nested-scope.t` | 255 | planned 11, ran 5 |
| `exception-role-membership.t` | 1 | `X::Undeclared::Symbols` vs `X::Comp::Group` (already triaged) |
| `exec-call-mixed-block.t` | 255 | planned 2, ran 0 |
| `exec-call-pairs.t` | 255 | `Unknown call: dies-ok` — the real `dies-ok`'s signature rejects the pin's argument shape |
| `exits-ok.t` | 4 | planned 13, ran 0 |
| `failure-sink-handled.t` | 255 | planned 4, ran 3 |
| `io-cathandle-lazy.t` | **134** | Rust stack overflow, `SIGABRT` |
| `is-lazy-io-lines.t` | 2 | already triaged (deferred-`Seq` reification) |
| `malformed-syntax-classes.t` | 255 | back in the list after being reported gone on 2026-08-19 |
| `pair-improvements.t` | 255 | planned 10, ran 7 |
| `parametric-role-of-type.t` | 255 | planned 14, ran 5 |
| `signature-introspection-gaps.t` | 255 | planned 8, ran 7 |
| `skip-list-vs-test.t` | 255 | the real `skip` rejects the pin's argument (`non-integer number of tests`) |
| `skip-user-multi-shadows-test.t` | 1 | |
| `subscript-adverbs.t` | 2 | already triaged (`:p` snapshot Pair) |
| `throws-like-gather-sink.t` | 255 | already triaged; **the sweep now scores it a PASS** — it is the exact file that exposed the predicate bug |
| `two-terms-in-a-row-initializer-listop.t` | 255 | |
| `undeclared-when-type.t` | 1 | already triaged (`when` bareword gobble) |
| `vm-panic-boundary.t` | 255 | planned 9, ran 6 |
| `whenever-out-of-scope.t` | 255 | planned 8, ran 4 |

Five of these were the previously-recorded residue; **fifteen were invisible**.
Several are mutsu's own pins whose assertions were written against the native
provider's looser signatures (`exec-call-pairs.t`, `skip-list-vs-test.t`) — but
this file has recorded three separate times that a "just re-point the pin" label
was wrong, so **run each under `raku` before believing it**.

### roast: 76 genuine regressions, down from 141

Method as in the "Step 3" section, except that only the real-module side needs
running: the whitelist *is* the native-provider baseline, since `main` is
protected by a green `make roast`. Release build, `-j4`, then every raw failure
re-run **alone** with `MUTSU_ROAST_TIMEOUT_SCALE=4`.

| | files |
| --- | --- |
| whitelisted | 1436 |
| raw failures under `-j4` | 82 |
| pass when re-run alone (load artifacts) | 6 |
| **genuine regressions** | **76** |

Trend: 90 (2026-08-14) → 141 (2026-08-18) → **76** (2026-08-20). The 2026-08-18
entry flagged the rise to 141 as evidence that "something is regressing under
`MUTSU_REAL_TEST=1` that nothing currently monitors". That is not what happened:
the count came back down without anyone working this ticket, so the 141 was
either measurement conditions or ordinary residue that the general-interpreter
work of the last two days closed. The six load artifacts this round were all in
the known slow families (the four `6.d/S32-str/sprintf-*.t` and both
`S03-buf/*-bits/int.t`), consistent with the 2026-08-03 finding that those files
are simply 4000+ interpreted assertions each.

Shape of the 76: **9 abort mid-file, 67 lose individual assertions**. No
TODO-handling cluster hides in there — every one of the 76 has at least one
non-`# TODO` failure or a hard abort (checked explicitly, because several files'
*first* `not ok` is a `__mutsu_backend_todo__` line and reads like one).

### Three roast files abort with a Rust stack overflow — the largest shared mechanism left

`roast/S16-io/words.t`, `roast/S32-io/io-cathandle.t` and
`roast/S32-list/tail.t` all die with

```
thread 'mutsu-main' has overflowed its stack
fatal runtime error: stack overflow, aborting
```

(exit 134), and so does `t/io-cathandle-lazy.t` on the other side. Four files,
one mechanism, and a Rust-level abort is the highest-priority class the project
recognises. **Root-caused 2026-08-20** in
`todo/deep/seq-cache-does-not-narrow-to-list-stack-overflow.md` (renamed from
`cathandle-real-test-is-deeply-infinite-recursion.md`, whose single-file framing
was indeed too narrow — and whose CatHandle framing was wrong: two of the four
files involve no CatHandle at all). It is one mechanism: `Seq.cache` must return
a `List`, and mutsu returns something that still binds `Seq:D`, so the real
`Test.rakumod`'s `is-deeply` Seq-narrowing candidates re-select themselves
forever. Design in
[ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md).

### A category this campaign had not named: files that pass only because the native provider is *wider* than upstream `Test`

Not every regression is an interpreter gap. Two of the 76 are whitelisted only
because mutsu's native provider offers surface the genuine upstream module does
not have at all:

- `roast/S24-testing/2-force_todo.t` calls `force_todo(...)`. Upstream
  `Test.rakumod` has no such routine (`grep` finds nothing), and `raku` on the
  same file answers `Undeclared routine: force_todo used at line 7`. mutsu
  implements it natively, so the file passes today. The file guards the call
  with a `#?rakudo eval "Module Test doesn't implement force_todo yet"` fudge
  directive that mutsu's `MUTSU_FUDGE` does not implement — so **implementing
  the `#?rakudo eval` fudge directive is the fix**, not implementing
  `force_todo` on the real module.
- `roast/S24-testing/6-done_testing.t` calls `ok 0, :todo(1)`. The upstream
  `multi sub ok(Mu $cond, $desc = '')` has no `:todo` named parameter, so the
  call is `Unknown call: ok`; raku's own baseline for this file is `ABORT`.

Neither is fixable by growing the interpreter, and neither should be. At step 3
they get un-whitelisted or fudged. Expect more of this shape once the
interpreter gaps thin out — **before filing a residue file as an interpreter
gap, check whether the assertion depends on a native-provider extension**.

### Verdict on the ticket's own question (retirability), 2026-08-20

Unchanged, and now measured rather than assumed: the real module is **the right
answer and the campaign is working**. `Test` is nothing like
`NativeCall` (`todo/deep/nativecall-cannot-be-vendored.md`, which needs
`use QAST:from<NQP>`, MoarVM dispatch programs and 61 missing `nqp::` ops) —
the genuine upstream file parses, loads, and answers every assertion it
exports, and has done since 2026-08-01. There is no open architectural fork
here, so **this ticket does not want an ADR**; it wants the residue worked
down. Roughly 95% of roast and (by the corrected count) 99.4% of `t/` already
pass under the real module, and essentially every fix the campaign has produced
was a general interpreter bug that the strict module merely exposed.

What still blocks step 3 (flipping `runtime_module.rs`), in priority order:

1. **Fix `scripts/test-module-sweep.sh`'s pass predicate** (above). Everything
   else in this list is measured through it.
2. **The four stack-overflow aborts** — one mechanism, a Rust-level abort,
   now root-caused and designed:
   `todo/deep/seq-cache-does-not-narrow-to-list-stack-overflow.md` /
   [ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md).
   Ready for implementation; its phase 2 is ~2 lines and clears two of the four.
3. The 20 `t/` files and 76 roast files, most of them one general interpreter
   gap each — the same long tail this file has been grinding down since
   2026-08-01, at an observed rate of roughly one file per fix and occasionally
   two.
4. Un-whitelist (or fudge) the native-provider-only files named above; implement
   the `#?rakudo eval` fudge directive while doing it.
5. `todo/perf/interpreter-call-path-in-hot-loops.md` — still the perf blocker
   the 2026-08-03 measurement named (`S04-declarations/state.t`: 3.7 s native,
   61.8 s real, 0.9 s raku). Not re-measured this round.

One process note, since the 2026-08-18 entry raised it: **this mode is still not
in CI**, so nothing detects a `MUTSU_REAL_TEST` regression between manual
sweeps. Gating it properly means a second full roast pass (~2x the roast CI
cost) and is not worth it while 76 files are red; the cheap interim is to re-run
the corrected `t/` sweep at the start of every session that touches this ticket,
which costs a few minutes and would have caught the fifteen invisible `t/` files
much earlier.

## 2026-08-20 (later same day): `scripts/test-module-sweep.sh`'s predicate is fixed

Item 1 of the priority list above, done. `run_one()` now captures each run's
exit status to a sidecar `.st` file (`$name.native.st` / `$name.real.st`), and
`passes()` requires exit status 0 *and* no failure marker in the text — the
text predicate gained `# You planned` (the truncated-plan abort line the old
version missed entirely) alongside the pre-existing `not ok` / `Runtime
error` / `Parse error` / `===SORRY` set. The regression-detail block also now
greps `# You planned` so a mid-file abort shows up in
`regressions.txt` instead of leaving it blank for exactly the files this bug
was hiding. Interface and output format are unchanged (same summary lines,
same `regressions.txt` path).

Re-ran the fixed script (release irrelevant here — debug build, `-j8`, from
the repo root, on top of `e13d278ff` / `origin/main`):

```
pass under both:                   3159
regressed under the real Test:     22
passes only under the real Test:   0
fail under both (pre-existing):    83
```

(3159 + 22 + 0 + 83 = 3264 = `ls t/*.t | wc -l`.)

That is the fix working: 22, not the old predicate's 4 — roughly the same
order of magnitude as the 24 raw / 20 confirmed the 2026-08-20 hand
investigation above found. The small remaining gap from 24 was checked by
hand, not guessed:

- **4 of the 22 do not reproduce from the repo root** — same finding as
  before, same four files (`any-type-object-int-coercion.t`,
  `bound-nil-method-warn.t`, `type-object-numeric-coercion.t`,
  `warns-like.t`): they only fail inside the sweep's
  `tmp/test-module-sweep/` working copy (a cwd artifact of that harness, not
  of the real module). Re-run standalone from the repo root, both runs exit 0
  and every subtest passes. **18 of the 22 are genuine, reproduced
  individually from the repo root** (native exit 0, real exit non-zero in
  every case: 255 ×13, 1 ×3, 2 ×2, 134 ×1, one already-triaged 124/timeout for
  `io-cathandle-lazy.t`'s stack overflow).
- **A second, narrower gap the exit-status fix does not close**: two files
  from the hand-investigated 20 (`exits-ok.t`, `failure-sink-handled.t`) are
  *still* invisible to the script, for a different reason than the one this
  ticket was fixing. Both have **legitimate `# TODO`-annotated `not ok` lines
  in their own native-provider baseline** (`exits-ok.t` tests 10/12,
  `failure-sink-handled.t` test 4) — ordinary, expected TAP `not ok` lines
  that happen to carry a `# TODO` comment. `passes()`'s text grep does not
  distinguish a TODO `not ok` from a real one, so it scores the *native* run
  itself as "not passing" and the file falls into the "fail under both"
  bucket instead of "regressed" — even though the real run fails a
  *different, non-TODO* way (verified by hand:
  `exits-ok.t` real: `# You planned 13 tests, but ran 0`, exit 4;
  `failure-sink-handled.t` real: `# You planned 4 tests, but ran 3` then
  `Unknown call: is-approx`, exit 255). This is a pre-existing quirk of the
  original text predicate (it never understood TODO annotations, on either
  side of the comparison) and out of scope for this fix — recorded here so
  the next person does not re-discover it from scratch. A real fix would need
  the predicate to parse the TAP `# TODO` suffix rather than grep raw `not
  ok`, which is more than "capture exit status" and is follow-up work.
  **Fixed 2026-08-23 — see the dated entry at the end of this file.**

So the honest current count is **20 confirmed regressions when checked by
hand** (the 18 the script now surfaces, plus the 2 masked by the TODO-line
quirk above) — unchanged from the 2026-08-20 hand count, and the script now
gets within 2 of it automatically instead of undercounting by 4x.

Newly-visible regressions this fix surfaces (i.e. present in
`regressions.txt` for the first time, previously silently scored a pass):
`bare-precedes-placeholder-nested-scope.t`, `exception-role-membership.t`,
`exec-call-mixed-block.t`, `exec-call-pairs.t`, `io-cathandle-lazy.t`,
`is-lazy-io-lines.t`, `malformed-syntax-classes.t`, `pair-improvements.t`,
`parametric-role-of-type.t`, `signature-introspection-gaps.t`,
`skip-list-vs-test.t`, `skip-user-multi-shadows-test.t`,
`subscript-adverbs.t`, `throws-like-gather-sink.t`,
`two-terms-in-a-row-initializer-listop.t`, `undeclared-when-type.t`,
`vm-panic-boundary.t`, `whenever-out-of-scope.t` (18 files — the same set the
2026-08-20 hand investigation already named individually above; this just
confirms the script now finds them on its own). `exits-ok.t` and
`failure-sink-handled.t` still require the TAP-TODO-aware follow-up described
above to surface automatically.

No interpreter code was touched in this pass — this was a test-harness fix
only. **The 20-file `t/` residue and the 76-file roast residue are unchanged
and are the next work**, per the priority list above (items 2-5).

## 2026-08-22: `throws-like-gather-sink.t` fully closed by ADR-0037 Slices 4-5; `t/` residue re-measured at 19

`throws-like-gather-sink.t`'s remaining 3 subtests (the ones needing the
actual return-targeting mechanism, past the sink-forcing fix that already
closed subtest 1) are fixed —
[ADR-0037](../../docs/adr/0037-eval-context-frame-owns-the-return-target.md)
Slices 4 and 5 landed. `EVAL ..., context => $ctx`'s `return` now targets the
routine `$ctx` names specifically (past any intervening routine boundary),
not just the frame that happened to call `EVAL`. Full details, including a
second independent bug this surfaced (the two "light" call dispatch paths
caught any return signal unconditionally, never checking whether it was
actually meant for them), are in the ADR's own "Implementation status" and in
`news/2026-08/eval-context-frame-owns-the-return-target.md`, which retires
this ticket's origin,
`todo/deep/eval-context-frame-owns-the-return-target.md`.

Re-ran `scripts/test-module-sweep.sh 6` (debug build, from the repo root, on
top of this fix):

```
pass under both:                   3243
regressed under the real Test:     19
passes only under the real Test:   0
fail under both (pre-existing):    91
```

**`throws-like-gather-sink.t` is confirmed gone from `regressions.txt`** —
the specific thing this session's fix targeted. The count moved from the
2026-08-20 measurement's 20 (18 script-visible + 2 masked by the TAP-TODO
quirk) to 19 (still + the same 2 masked), but not by a clean -1: several
other files closed or newly appeared between the two measurements from
unrelated work on `main` in between, not investigated further here (out of
scope for this ADR) —
`exception-role-membership.t`, `subscript-adverbs.t`, `undeclared-when-type.t`
and `vm-panic-boundary.t` are gone (already fixed elsewhere, consistent with
this file's 2026-08-19 residue-count trend); `placeholder-scope-rejecting.t`
and `user-class-shadows-immutable-builtin.t` are new and untriaged.
`any-type-object-int-coercion.t`, `bound-nil-method-warn.t`,
`type-object-numeric-coercion.t` and `warns-like.t` reappear in the raw
`regressions.txt` but are the same sweep-harness cwd artifact the 2026-08-20
entry already named ("do not reproduce from the repo root") — not real
regressions. `exits-ok.t` and `failure-sink-handled.t` remain masked by the
TAP-TODO predicate quirk (confirmed by hand: still non-zero real exit status,
4 and 255 respectively) — unchanged, still open, still needs the
TAP-TODO-aware predicate follow-up noted above.

`emit-done-controlflow.t`, the other file the ADR's origin ticket named, was
already closed by an unrelated earlier fix
(`news/2026-08/emit-done-controlflow-illegal-control.md`, 2026-08-18, "closed:
bare `emit`/`done` were an uncatchable Rust-level panic under the real
module" above) before this session began; re-confirmed passing 5/5 under
`MUTSU_REAL_TEST=1` as part of this sweep, not newly fixed by it.

roast side not re-swept this session (unrelated to the fix; the priority list
item 3 above still applies).

## 2026-08-23: `test-module-sweep.sh`'s `passes()` predicate now understands TAP `# TODO`

Fixed the narrow follow-up recorded above: `passes()` in
`scripts/test-module-sweep.sh` scored a `not ok N ... # TODO ...` line the
same as a genuine `not ok`, so a file whose *native*-provider baseline
legitimately carries a `# TODO`-annotated failure (an expected failure, per
standard TAP and mutsu's own `todo()` — see `raku-doc/doc/Type/Test.rakudoc`)
was scored as "not passing" on the native side too. That masked a real
regression on the real-Test side by dropping the file into "fail under both"
instead of "regressed".

`passes()` now treats `^not ok` lines as a genuine failure only when they lack
a case-insensitive `# TODO` suffix:

```sh
grep -E '^not ok' "$out" | grep -qvi '#[[:space:]]*todo' && return 1
```

Verified against the two files this ticket named by hand (debug build, repo
root):

- `t/exits-ok.t`: native exits 0 with two legitimate TODO `not ok` lines (its
  own negative-case tests 10 and 12) -> now scores as **passing**. Real
  (`MUTSU_REAL_TEST=1`) exits 4 with `# You planned 13 tests, but ran 0` -> still
  scores as **failing**. The file now correctly lands in "regressed", not
  "fail under both".
- `t/failure-sink-handled.t`: native exits 0 with one legitimate TODO `not ok`
  (test 4) -> now scores as **passing**. Real exits 255 with `# You planned 4
  tests, but ran 3` then `Unknown call: is-approx` -> still scores as
  **failing**. Same reclassification.

This is a `scripts/`-only test-harness fix; no interpreter code was touched.
The 22-regression sweep count from the 2026-08-21 entry above should now read
24 once someone re-runs the full sweep (the two previously-masked files
surfacing), consistent with the "20 confirmed by hand" / "24 raw" figures
already recorded there -- not re-run here to keep this change scoped to the
predicate fix itself.

## 2026-08-28: `t/` residue 20 -> 9 genuine; six general interpreter bugs fixed; the residue is mostly NOT interpreter gaps any more

Re-ran the sweep first, per the process note above (debug build, `-j6`, from
the repo root, on top of `6e426bcb3` / `origin/main`):

```
pass under both:                   3403      (before)
regressed under the real Test:     20
passes only under the real Test:   0
fail under both (pre-existing):    85
```

and after this session's fixes:

```
pass under both:                   3414      (after)
regressed under the real Test:     13
passes only under the real Test:   0
fail under both (pre-existing):    85
```

Four of the 13 are the long-standing sweep-harness cwd artifact the 2026-08-20
entry named (`any-type-object-int-coercion.t`, `bound-nil-method-warn.t`,
`type-object-numeric-coercion.t`, `warns-like.t`) -- re-confirmed this session:
run standalone from the repo root both providers exit 0. **So the honest count
is 20 genuine before, 9 genuine after** (16 raw / 12 genuine after the
interpreter fixes alone, then three more closed by the local-test rewrites
described below).

### The priority list at the head of this file was stale in both directions

Two of its five items were already done and one of its assumptions was wrong:

1. ~~Fix `scripts/test-module-sweep.sh`'s pass predicate~~ -- **done**
   (2026-08-20 exit-status sidecars, 2026-08-23 TAP `# TODO` awareness).
2. ~~The four stack-overflow aborts~~ -- **done**: ADR-0038 is
   `Accepted -- implemented`, all four phases landed, and
   `todo/deep/seq-cache-does-not-narrow-to-list-stack-overflow.md` was closed
   out to `news/2026-08/seq-cache-returns-list-fixes-is-deeply-stack-overflow.md`.
   No file in the current sweep aborts with a stack overflow.
3. The `t/` and roast residue -- still the head of the list, but see the
   reclassification below: it is no longer "one general interpreter gap each".

### The biggest finding: most of what is left is NOT an interpreter gap

The 2026-08-20 entry named the category "files that pass only because the
native provider is *wider* than upstream `Test`" and predicted more of them
would surface as the interpreter gaps thinned out. That has now happened, and
it is the *majority* of the residue. Of the 12 genuine `t/` regressions left
after the interpreter fixes, **seven are local tests that encode
mutsu-native-provider-only behaviour and do not even compile or dispatch under
real raku** -- verified individually by running each construct under `raku`,
not assumed:

| file | construct | raku's own answer |
| --- | --- | --- |
| `exec-call-mixed-block.t` | `dies-ok { ... }, 'd', :todo(False)` | `Cannot resolve caller dies-ok(Block:D, Str:D, :!todo)` |
| `exec-call-pairs.t` | same, plus `ok 1, 'x', :todo(False)` | `Cannot resolve caller ok(Int:D, Str:D, :!todo)` |
| `bare-precedes-placeholder-nested-scope.t` | `lives-ok '<source string>'` | `===SORRY!=== Calling lives-ok(Str, Str) will never work` |
| `whenever-out-of-scope.t` | same | same |
| `pair-improvements.t` | `cmp-ok $p, 'eqv', :foo<bar>, 'd'` | `Cannot resolve caller cmp-ok(..., :foo(Str))` -- a colonpair in an argument list is a NAMED argument |
| `skip-list-vs-test.t` | `skip(2, <a b c d e>)` with `Test` loaded | raku ALSO routes to `Test`'s `skip` and dies "was passed a non-integer number of tests" -- the pinned behaviour is what mutsu's native provider does, not what raku does |
| `skip-user-multi-shadows-test.t` | `my proto sub skip(Mu, |) {*}` + `my multi sub skip(...)` | `===SORRY!=== Redeclaration of routine 'skip'` -- the file is not valid raku at all |

**Three of those seven were rewritten this session** to spell the same
intention in a way both providers accept, which does not weaken what they pin:
the two `lives-ok '<string>'` files now use `eval-lives-ok` (the routine that
actually takes a Str), and `pair-improvements.t` passes its expected Pair as
`('foo' => 'bar')` instead of the colonpair. All three now pass under both
providers and under `raku` itself.

The remaining four are genuinely provider-coupled and should be retired
*with* the native provider rather than rewritten now:
`exec-call-mixed-block.t` / `exec-call-pairs.t` exist to pin the pair-encoded
named-argument exec-call path and use `:todo` on the native `ok`/`dies-ok`
signature to do it (they want a user-defined sub with a `:todo` parameter
instead); `skip-list-vs-test.t` / `skip-user-multi-shadows-test.t` pin a
core-`skip`-vs-`Test`-`skip` disambiguation that only exists because mutsu's
native provider is not a lexical import -- under the real module raku's own
answer is the opposite, so the disambiguation *should* disappear at step 3.

### Six general interpreter bugs fixed (four `t/` files closed, one improved)

Every one is a plain language bug with a `raku` oracle; none is Test-specific.

1. **A deferred `Seq` reaching a string context answered `(...)`.**
   `~$fh.lines` and `$fh.lines eq <A B C>` go through the operand coercion
   (`coerce_stringy_operand`) and the `StrCoerce` opcode, not through method
   dispatch, so the `.Str` reify guard never ran and the pure stringifier fell
   back to the opaque `IO::Handle.lines` placeholder. Both sites now route a
   still-deferred Seq through `reify_or_consume_seq_target(v, "Str")` --
   `"Str"` is not a `seq_method_consumes` entry, so this reifies without
   consuming, matching rakudo's `multi method Str(Seq:D:) { self.cache.Str }`
   (measured: `~$s; ~$s; $s.List` all work). Both sites are **tag-probed**
   (`is_seq_value()`) before the `view()`, because an unconditional `view()`
   there materialises every lazy Match in grammar-action code -- caught by
   `tests/lazy_match_no_eager_materialization.rs`, which is exactly what that
   guard exists for. Closes `is-lazy-io-lines.t`.
   Pin: `t/seq-string-context-forces-deferred.t`.
2. **The mut method-dispatch path ignored a lazy list's List view.**
   `vm_call_method_ops.rs` renders a forced `LazyList` as `Value::array` when
   `in_list_context()` and `Value::seq` otherwise; `vm_call_method_mut_ops.rs`
   unconditionally built `Value::seq`. So whether `(gather ...).List` rendered
   as a List or a Seq depended purely on which opcode the *later* method call
   compiled to -- `CallMethod` for an inline receiver, `CallMethodMut` for a
   named-variable one. `my $a = (gather takes-two()).List; $a.raku` gave
   `(1, 2).Seq`, while the two-statement spelling gave `$(1, 2)`.
3. **`eqv` had no `LazyList` arm at all.** `Value::eqv` (`value/types_eqv.rs`)
   only pairs a LazyList with another LazyList, by identity, so
   `(gather takes-two()).List eqv (1, 2)` answered False *without ever running
   the gather body*. `reify_or_consume_eqv_operand` now forces a non-hanging
   LazyList operand into the same view its own dispatch would produce.
4. **`eqv` threw `X::Cannot::Lazy` on finite `.map`/`.grep` pipes.**
   `LazyList::eqv_would_hang()` treated *any* `lazy_pipe` as unsafe, so
   `(gather {take 1}).map(*+1) eqv (gather {take 1}).map(*+1)` died where raku
   answers `True`. It now excludes a pipe whose source chain
   `pipe_bottoms_out_finite()` -- the same predicate the forcing dispatch paths
   already use. (2)-(4) together close `take-without-gather.t` and take
   `io-cathandle-lazy.t` from 2 failures to 1.
   Pin: `t/lazy-list-eqv-and-list-view.t`.
5. **`exit` did not honour the dynamic `&*EXIT` hook.** rakudo's `exit` calls
   `&*EXIT($status)` when one is in dynamic scope and then *returns normally*
   (measured: `sub f { my &*EXIT = -> $c {say "trapped $c"}; exit 7; say
   "continues" }` prints both lines). mutsu always terminated the process.
   The real `Test.rakumod`'s `exits-ok` is built entirely on this hook, but it
   is a plain language feature. Guarded so a declared-but-unassigned
   `my &*EXIT;` (a `Callable` type object) does not swallow the exit.
   Closes `exits-ok.t`. Pin: `t/exit-dynamic-hook.t`.
6. **`:name<word>` lost the allomorph on a listop-style call to a routine the
   compiler cannot see statically.** `<90>` is quote-words, so it yields an
   `IntStr` -- but the *statement-call* argument parser
   (`src/parser/stmt/args.rs`, the `CallArg::Named` path taken for a listop
   call to an imported routine) minted a bare `Value::str(word)` instead of
   calling the shared `parser::angle_word_value`. So `is-approx 1, 10,
   :abs-tol<90>, :rel-tol<.5>` could not bind the `Numeric :$abs-tol`
   parameter and the whole multi became `Unknown call: is-approx`, while
   `f(:abs-tol<90>)` compiled through `Expr::Call` and worked. Reduced to a
   Test-free repro (`t/lib/AngleNamedArg.rakumod`) before fixing.
   Closes `failure-sink-handled.t`. Pin: `t/angle-named-arg-allomorph.t`.

A note on method, since this file's own history keeps re-learning it: the
`is-approx` bug looked for a long time like a *dispatch* bug (multi candidate
selection with two required nameds), and a hand-written local reproduction of
the exact five `is-approx` candidates passed. Only bisecting caller *spelling*
(`:x<90>` vs `:x(<90>)`, identical AST and identical `--dump-bytecode` output)
against caller *kind* (local sub vs imported sub) found it, and the confirming
evidence was `MUTSU_VM_STATS=1` showing `ExecCallPairs`/`MakeNamedArg` where
`--dump-bytecode` had shown `CallFuncNamed` -- i.e. **`--dump-bytecode` does
not always show the bytecode that actually runs**, which is worth remembering
the next time a dump and an observed behaviour disagree.

### The 9 genuine `t/` regressions that remain

Five are real interpreter gaps and are the actual next work:

- `io-cathandle-lazy.t` (test 10 only) -- a `.map` pipe over
  `IO::CatHandle.handles` never forces, so `.raku`/`eqv` see `(...)`. Filed as
  `todo/tickets/cathandle-handles-map-pipe-never-forces.md` with a Test-free
  repro; likely `needs_vm_lazy_dispatch()` plus `pipe_bottoms_out_finite()` on
  a `cat_pull`-rooted chain.
- `user-class-shadows-immutable-builtin.t` (5 subtests) -- a user class named
  `Map`/`Set`/`Bag`/`Mix` permits element assignment but stores nothing
  (`got: (Any)`).
- `parametric-role-of-type.t` -- aborts at line 34 with `No such method 'x'
  for invocant of type 'R1[Int]'`, after five passing subtests.
- `signature-introspection-gaps.t` -- runs 7 of 8 and stops with no error
  printed at all (worth a `rust-gdb` look; a silent stop is unusual).
- `placeholder-scope-rejecting.t` (test 13 only) -- `INIT { $^c }` does not
  die under the real module, though every other phaser in that 27-subtest file
  does.

The other four are the provider-coupled files listed in the table above
(`exec-call-mixed-block.t`, `exec-call-pairs.t`, `skip-list-vs-test.t`,
`skip-user-multi-shadows-test.t`). The remaining four rows in
`regressions.txt` are the cwd artifacts, which are a sweep-harness bug, not a
regression -- **worth fixing the harness for**, since they have now cost three
separate sessions a re-verification pass each.

### Corrected priority list

1. **The five real interpreter gaps above** -- the genuine head of the queue,
   two of them already reduced to standalone tickets.
2. **Re-sweep the roast side.** The last roast measurement is 2026-08-20's
   "76 regressions, 9 of them mid-file aborts", taken *before* ADR-0038 landed
   and before this session's six fixes, so it is certainly stale. Spot-checked
   rather than assumed: the three roast files that entry called "the largest
   shared mechanism left" (`S16-io/words.t`, `S32-io/io-cathandle.t`,
   `S32-list/tail.t`) **no longer abort** under `MUTSU_REAL_TEST=1` -- the
   `exit 134` stack overflow is gone in all three, which is exactly what
   ADR-0038 promised -- but all three still regress in ordinary ways (exit 4,
   1, 1). So expect the abort count to have dropped and the total to have
   moved less than that; measure it rather than guessing, and do it before
   picking any individual roast file.
3. **Un-whitelist or fudge the native-provider-only files**, and implement the
   `#?rakudo eval` fudge directive while doing it (unchanged from the old item
   4; `roast/S24-testing/2-force_todo.t` and `6-done_testing.t` are still the
   named examples, and the four `t/` files above join them). With the `t/`
   residue this thin, this is now a bigger share of what stands between here
   and step 3 than the interpreter gaps are.
4. **Fix the sweep harness's cwd artifact** so the four false positives stop
   costing a manual re-check every session. They only fail inside
   `tmp/test-module-sweep/`; the working copy needs whatever those four read
   relative to cwd.
5. `todo/perf/interpreter-call-path-in-hot-loops.md` -- unchanged, still last,
   and TRIAGE records it as mostly resolved (13.8x -> ~2x).

Verification for this entry: `make test` green (3512 files, 34944 tests), a
23-file targeted roast sweep chosen from the consumers of what changed
(`S03-operators/eqv.t`, `S04-statements/gather.t`, `S02-types/lazy-lists.t`,
`S16-io/lines.t` + `words.t`, `S32-list/seq.t`, `S29-context/exit.t` +
`exit-in-if.t`, `S04-phasers/exit-in-check.t`, the `S05`/`S06` named-argument
and colonpair files, `roast/t/test-util/01-is-eqv.t`, ...) all green, and the
full `make roast` delegated to CI.

## 2026-08-28 (later the same day): the ROAST side re-measured for the first time since 2026-08-20 — 76 -> 67, and the mid-file aborts are gone

Priority item 2 of the list above, done. The roast half of this campaign had
not been measured since 2026-08-20, before ADR-0038 and before a great deal of
other work; the 76 was stale, and nothing detects a `MUTSU_REAL_TEST`
regression between manual sweeps.

### Tooling: the existing sweep only covers `t/`, so there is now a roast one

`scripts/test-module-sweep.sh` sweeps `t/*.t` only. Rather than hand-pick roast
files (which would not be a measurement), this session added
**`scripts/roast-test-module-sweep.sh`** — the same two-runs-per-file
comparison and the same `passes()` predicate (exit status 0, no failure marker,
TAP `# TODO` treated as an expected failure), over every entry in
`roast-whitelist.txt`. Two deliberate differences from the `t/` sweep:

- files run **in place from the repo root through `scripts/run-roast-test.sh`**,
  so they inherit its per-file timeouts, its `MUTSU_FUDGE=1` export (roast needs
  it) and its `roast/`-cwd special cases. There is no working copy, so the `t/`
  sweep's cwd-artifact class *cannot occur here* — a small but real advantage of
  this harness over that one;
- a **release** build, since the whitelist is 1436 files run twice.

### The measurement (release build, `-j6`, on top of `003b69a35` / `origin/main`)

```
pass under both:                   1369
regressed under the real Test:     67
passes only under the real Test:   0
fail under both (pre-existing):    0
```

(1369 + 67 + 0 + 0 = 1436 = `wc -l roast-whitelist.txt`. "Fail under both" is
zero, as it must be: the whitelist is exactly the set that passes natively.)

**76 -> 67.** But the shape moved far more than the count:

### The mid-file aborts are gone: 9 -> 1

The 2026-08-20 entry recorded "**9 abort mid-file**, 67 lose individual
assertions". Counting truncated plans across the 67 real-provider logs now
finds exactly **one** (`S24-testing/2-force_todo.t`, which is a known
native-provider-only file, below). That is ADR-0038 landing, and it is the
single biggest structural change in this campaign since the last roast
measurement.

Spot-checked rather than assumed: the three files that entry called "the
largest shared mechanism left" (`S16-io/words.t`, `S32-io/io-cathandle.t`,
`S32-list/tail.t`) no longer abort with `exit 134`; all three now merely lose
1-4 individual assertions.

### 7 of the 67 are not correctness regressions at all — they are the real module being slower

Every `exit 124` row (`6.d/S32-str/sprintf-{d,f,x}.t`, `S03-buf/read-write-bits.t`,
`S03-buf/write-int.t`, `S32-str/sprintf-{b,d}.t`) is a **timeout**, and all
seven were re-run individually with a 900 s budget: **every one exits 0 with
zero non-TODO failures**, in 12-67 s. They are the assertion-heavy families the
2026-08-20 entry already named — several thousand assertions each, answered
through Raku-level code instead of the native provider's Rust — and under a
6-way parallel sweep they simply do not fit `run-roast-test.sh`'s 30 s budget.

**So the honest correctness count is 67 - 7 = 60.** (The sweep script now
documents this so the next person does not re-derive it; an `exit 124` row
means "re-run with headroom before counting it".)

**This is not a nicety — the raw count is noisy by about +-6 because of that
family alone.** The post-fix sweep at the end of this session came back with
only **one** `exit 124` (`S03-buf/write-int.t`, the slowest of the seven at
67 s standalone): the other six simply happened to fit the budget on a
less-contended run. Nothing about them changed. **Always quote the
timeout-excluded number**, or a sweep will appear to have gained or lost half a
dozen files that nobody touched.

### Classification: the residue is a long tail of ONE-subtest gaps, not shared mechanisms

Counting non-TODO `not ok` lines per file across all 67:

| failing subtests | files |
| --- | --- |
| 0 (fails on exit status / truncated plan only) | 12 (7 of them the timeouts) |
| 1 | 39 |
| 2 | 6 |
| 3 | 3 |
| 4 | 2 |
| 10 | 2 |

**39 files lose exactly one assertion.** That is the opposite of the `t/` side's
shape and it changes what "work the residue down" means here: on roast there is
no large shared mechanism left to find, and the observed rate really is about
one fix per file. The two 10-failure files (`S09-typed-arrays/native-int.t`,
`native-shape1-int.t`) are one assertion repeated across ten integer widths, so
they are one mechanism, not ten.

### A warning about the obvious classification shortcut

The tempting way to separate "native provider is wider" from "genuine
interpreter gap" on roast is the `raku_status` column of
`TODO_roast/raku-baseline.tsv`, and 22 of the 67 are not `PASS` there. **Do not
use it that way.** That baseline's own header says why: it runs `raku` on the
**raw, UNFUDGED** `.t` file, because applying roast's fudge would mean writing
into the read-only `roast/` tree — so "a raku FAIL/SORRY on a whitelisted file
is usually a fudge/version artifact, not raku being worse than mutsu". It is a
useful hint and nothing more; each candidate still has to be checked on its own
failing assertion. (Checked by hand this round: `S24-testing/10-is-approx.t`,
`14-like-unlike.t` and `3-output.t` all have raku_status `PASS` and are genuine
mutsu gaps, while `2-force_todo.t` and `6-done_testing.t` really are the
native-provider-only pair already documented on 2026-08-20.)

### Fixed this round: two general interpreter bugs, three roast files closed

Both were found by looking for *shared mechanisms* among the files that fail on
exit status with no visible failing assertion — the most information-dense
corner of the report, because a file that runs every test and still exits
non-zero is usually one mechanism rather than one assertion.

1. **An `EVAL`'s unresolved package/class stubs leaked into the enclosing
   program.** `src/runtime/system.rs` only ran the EVAL's own end-of-unit stub
   check when the snippet *succeeded*, so an EVAL that died first — e.g.
   `class A { ... }; class B does A { }`, which dies composing against the
   still-open stub — left `A` in the outer registry. The program's end-of-run
   check then reported "The following packages were stubbed but not defined:
   A" for a name the outer program never mentioned, and exited non-zero *after
   every test had passed*. raku prints nothing there (measured).

   The first attempt **removed** the leaked names from `class_stubs`, and the
   targeted roast sweep immediately caught the consequence:
   `roast/S12-class/stubs.t` test 7 went green-to-red, because `class_stubs` is
   also what answers "is this name still an open stub" — deleting the entry made
   a *later* `EVAL 'class A { ... }; class B is A {}'` see a fully-defined `A`
   and stop raising `X::Inheritance::NotComposed`. The shipped fix marks them in
   `reported_stub_errors` instead, which is precisely the distinction that field
   was introduced for: the name stays a stub for every class-system purpose,
   only its *error* is spent. Closes `roast/integration/error-reporting.t` and
   `roast/S12-class/augment-supersede.t`. Pin: `t/eval-stub-package-does-not-leak.t`
   (which pins the re-stub case too, so the first attempt's regression cannot
   come back). `roast/S32-exceptions/misc2.t`, which also lives in this
   EVAL-registry family, was checked and is **not** fixed by it — its three
   `X::Placeholder::Mainline` failures are a separate gap.

2. **`our @array` / `our %hash` declared in a nested scope was never readable
   through the package.** `{ our @a = 1..3 }` then `@OUR::a` answered `[]`;
   `{ our $s = 5 }` then `$OUR::s` answered `5`. The value really was published
   — `set_our_var` runs — but the **read** side diverged by sigil: `GetGlobal`
   (scalars) consults `our_pseudo_var_read`, which reads the `our` store before
   env, while `GetArrayVar`/`GetHashVar` resolve only through
   `get_env_with_main_alias`, and block exit deliberately drops the bare env
   alias (an `our` declared only inside a block keeps its lexical alias
   block-scoped). So the `@`/`%` read had nothing left to find. The fix makes
   the pseudo-package branch of `get_env_with_main_alias` fall back to the `our`
   store, purely additively — every lookup that already succeeded through env
   still does. Also fixes it inside a routine and for declare-then-assign.
   Closes `roast/S04-declarations/our.t`.
   Pin: `t/our-array-hash-in-nested-scope.t`.

### After the fixes

```
pass under both:                   1378
regressed under the real Test:     58
passes only under the real Test:   0
fail under both (pre-existing):    0
```

Diffing the two regressed-file lists, exactly nine files left the list: the
three this session fixed (`integration/error-reporting.t`,
`S12-class/augment-supersede.t`, `S04-declarations/our.t`) and six of the seven
timeouts, which merely fit the budget this time. So the headline, stated the
only way that is stable:

**roast correctness regressions: 60 -> 57** (67 -> 58 raw, minus 7 -> 1
timeouts). `make test` green throughout (3515 files, 34982 tests), and a
189-file targeted roast sweep over the consumers of what changed
(`S10-packages`, `S12-class`, `S12-construction`, `S12-coercion`, `S12-enums`,
`S32-exceptions`, `S04-declarations`, `S02-names*`, `S02-magicals`,
`S02-types`, `S11-modules`, `S06-other`) is green — that sweep is what caught
the `S12-class/stubs.t` regression described above, at a point where `make test`
alone was still green. The `t/` sweep was re-run afterwards and is unchanged at
13 raw / 9 genuine, so neither fix cost anything on that side.

### Five tickets filed for the gaps left behind

All five have a **Test-free repro with a `raku` oracle**, because in every case
the real `Test.rakumod` turned out to be only the *shape* that exposed the bug,
not part of it:

- `todo/tickets/eval-declared-my-role-leaks-and-shadows-a-later-lexical-role.md`
  — the role-registry sibling of the stub leak fixed above:
  `try EVAL 'my role R1[::T] { }'` makes a *later* lexical
  `my role R1[::T] { method x { T } }` resolve to the EVAL's method-less
  version. Blocks `t/parametric-role-of-type.t`. Note the tell recorded there:
  if the two declarations happen to have the same methods the leak is invisible,
  which is how a first attempt at the repro wrongly appeared to pass.
- `todo/tickets/sunk-lazy-seq-failure-escapes-try-and-aborts-the-routine.md` —
  a `Failure` produced by sink-forcing a lazy `Seq` at the end of a `try` block
  escapes the `try`, skips the rest of the enclosing routine and becomes its
  return value, silently. This is the "runs N of M tests and prints no error"
  signature of `t/signature-introspection-gaps.t`.
- `todo/tickets/user-class-instance-element-write-lost-through-closure-call.md`
  — `$userClassInstance<k> = 1` inside a closure another routine invokes is
  lost, while the same write into a builtin `Hash.new`, a `%h`, an `@a` or a
  `$scalar` is not. Records five hand-written twins of `lives-ok` that do NOT
  reproduce it, so the next person does not re-derive them.
- `todo/tickets/init-phaser-does-not-reject-a-placeholder-parameter.md` —
  `INIT { $^c }` does not raise `X::Placeholder::Block`, though `BEGIN`,
  `CHECK`, `PRE` and fifteen other block kinds do. Almost certainly a one-line
  addition.
- `todo/tickets/begin-selective-import-of-code-sigil-lexicals.md` —
  `BEGIN my (&plan, &is) = do { use Test; (&plan, &is) }` does not bind, so
  `roast/S32-list/skip.t` dies on `Unknown function: plan` and none of its 55
  tests run. roast uses this idiom specifically to avoid `Test`'s `skip`
  shadowing the core list `skip`.

### Two method notes worth keeping

- **`--dump-bytecode` does not always show the bytecode that actually runs.**
  The 2026-08-28 `:name<90>` allomorph bug had byte-identical `--dump-ast` *and*
  `--dump-bytecode` output for the working and the broken spelling; what cracked
  it was `MUTSU_VM_STATS=1` showing `ExecCallPairs`/`MakeNamedArg` where the
  dump claimed `CallFuncNamed`. When a dump and an observed behaviour disagree,
  suspect the dump.
- **A hand-written reproduction of "the same thing" can lie.** That same bug
  looked like multi-dispatch for a long time because a local re-declaration of
  the exact five `is-approx` candidates passed; only bisecting caller *spelling*
  against caller *kind* found it. The same trap fired twice more this round:
  five separate hand-written twins of `lives-ok` all failed to reproduce the
  closure-writeback bug, and the role-leak repro passed until the EVAL'd role
  was given *different* methods from the outer one.

### Corrected priority list

1. **The `t/` residue's five real interpreter gaps and the five tickets above**
   — now all written down as `todo/tickets/` files with standalone repros, so
   they can be handed to independent agents. The two EVAL-registry ones
   (`my role`, and the already-fixed stub half) are the same mechanism seen
   twice, so the role half is the natural next fix.
2. **Un-whitelist or fudge the native-provider-only files, and implement the
   `#?rakudo eval` fudge directive.** Unchanged in substance, but now clearly
   the *smaller* half of the work on the roast side: only 2 of the 67 roast
   regressions are confirmed native-provider-only, against the `t/` side's 4.
   The campaign's framing has moved again — on `t/` the residue is mostly
   provider-coupled local tests, on roast it is mostly a genuine one-assertion
   long tail.
3. **Work the roast long tail, cheapest-first.** With 39 single-assertion files
   and no large shared mechanism left, this is now a volume problem rather than
   a design problem, and it parallelises across agents better than anything
   else in this ticket. `tmp/roast-real-sweep/regressions.txt` (regenerated by
   the new script) names the failing assertion for each.
4. **Fix the `t/` sweep harness's cwd artifact** (four permanent false
   positives). The roast sweep shows the cheap fix: run the files in place from
   the repo root rather than from a working copy.
5. `todo/perf/interpreter-call-path-in-hot-loops.md` — unchanged, still last.
   Note the seven timeouts above are its most visible symptom in this mode: the
   real module is several times slower per assertion, which is exactly what that
   ticket is about.

One caveat to carry forward: this sweep takes ~25 minutes on a release build
and is still not in CI, so it remains a manual ritual. Gating it means a second
full roast pass; with 60 correctness regressions left that is still not worth
2x the roast CI cost, but the gap is narrowing.

## 2026-08-28 (third entry that day): the "swallowed EVAL failure" was two context-dependent error suppressions, both fixed; the rest of that candidate list is a wrong-exception-class long tail

The starting hypothesis was a single shared mechanism: "`EVAL` of a snippet that
fails to parse silently does not throw when the `EVAL` is inside a block that
another routine invokes", with eleven roast files from the 60-file regression
list nominated as likely consumers. Investigated with `rust-gdb` rather than
`eprintln!`, the hypothesis turned out to be **two unrelated suppressions with
the same shape** — and to explain **only two of the eleven files**. Both
suppressions are now fixed; the other nine are a different family and are filed
as their own tickets.

### The discriminating pair in the repro was the whole diagnostic

`EVAL '10_'` threw and `EVAL '10_.0'` did not, from the same call site, with the
same exception class. That rules out both "it is about the exception type" and
"it is about `EVAL`-in-a-block". Breaking on the parse showed why: **neither
snippet is a parse error in mutsu.** `parse_program` returns `Ok` for `10_.0` in
*both* contexts, with a byte-identical tree —
`Expr::InfixFunc { name: "_", left: 10, right: [0.0] }`. mutsu's parser accepts
any non-reserved word as a *speculative* `infix:<word>` at the loosest
precedence level (`parse_custom_infix_word`, deliberately permissive because an
infix can be installed at runtime), so `10_ .0` reads as `10 infix:<_> .0`. The
"parse error" is raised much later, by the runtime's unresolvable-infix fallback
(`call_infix_fallback` -> `X::Syntax::Confused: "Two terms in a row"`).

### Mechanism 1: the topic occupies the identifier `_`, so bare-name type lookups resolved `_` to `Any`

`$_` is stored in `env` under the **sigil-less key `_`** (CLAUDE.md's
debugging section already warns about this key). Entering a compiled routine
seeds the implicit topic with the `Any` **type object**, i.e. `Value::Package`.
`resolve_bare_type_name` resolves a short type name through exactly such an
`env` binding, because that is how a lexical `my class Foo` is reachable (it
registers under a mangled storage name and `env["Foo"]` points at it). So inside
any routine, `env.get("_")` answered `Package(Any)` and `_` resolved to the type
`Any` — after which `call_function_fallback`'s coercion arm turned the
unresolvable `_(10, 0.0)` into `Any(10, 0.0)`, which type-matches and **returns
its argument list**. `EVAL '10_.0'` therefore evaluated to `(10, 0.0)` and threw
nothing. At mainline there is no such env entry, the fallback ran out of options,
and the error appeared — which is the entire top-level-vs-routine divergence.

The bug is general, not EVAL-specific (all measured against `raku`):

```
sub f() { say _(1,2) }; f()          # was: (1 2)     raku: Undeclared name: _
sub f() { say 1 _ 2 }; f()           # was: (1 2)     raku: Two terms in a row
sub f() { my _ $x = 3; say $x }; f() # was: 3         raku: Type '_' is not declared
```

Fixed by `crate::env::is_magic_sigilless_key` — a documented, one-name guard
applied at the four bare-name type/role alias lookups (`resolve_bare_type_name`,
`is_declared_package`, `type_registry`'s short-import-alias arm,
`resolve_role_key`). `_` is the only sigil-less magic env key that is also a
legal bare identifier (`/`, `!`, `?FILE`, `0`, `<n>`, `*x` are not), so the guard
is exactly one name. The real fix — not storing the topic in the identifier
namespace — is recorded as a `TODO:` at the helper; until then a genuine
`class _ { }` stays unreachable through these lookups, which it already was.

### Mechanism 2: EVAL's `&?ROUTINE` check consulted the CALLER's routine stack

```rust
if code.contains("&?ROUTINE") && self.routine_stack.is_empty() { ...Undeclared... }
```

`&?ROUTINE` is resolved **lexically at compile time**, and an `EVAL`'d string is
its own compilation unit, so the caller's runtime stack is irrelevant. The gate
was wrong in both directions (both measured against `raku`): it accepted a
mainline `&?ROUTINE` in the snippet whenever the `EVAL` happened to sit inside a
`sub` (the reported symptom), and it **rejected** `EVAL 'sub g { &?ROUTINE.name }; g()'`
called from mainline, which raku accepts.

Replaced with a structural post-parse walk over the snippet's own statements
(`src/runtime/eval_routine_magicals.rs`, modelled on `parser::whenever_scope`),
run alongside the other `check_eval_*` passes. `sub`/`method`/`token`/`rule`/
`proto` and an anonymous `sub { }` open a routine scope; a bare block, a
`class`/`role` body and control flow preserve it. One documented limitation: a
pointy `-> { }` and a parameterised `sub ($x) { }` both lower to
`Expr::AnonSubParams`/`Expr::Lambda` with nothing left to tell them apart, so
both are treated as routine boundaries — per the walker's conservatism rule that
can only *miss* an offending pointy-block use, never invent one. (`raku` rejects
`EVAL 'my $z = -> { &?ROUTINE }; $z()'`; mutsu now does not. Every other measured
shape matches.)

### Measured, file by file (debug build, `scripts/run-roast-test.sh`, both providers)

| file | before (real) | after (real) | native |
| --- | --- | --- | --- |
| `S02-literals/underscores.t` | 1 failure ("Underscore before . fails") | **PASS** | still PASS |
| `S02-magicals/subname.t` | 1 failure ("&?ROUTINE not available outside of a routine") | **PASS** | still PASS |
| `S02-lexical-conventions/minimal-whitespace.t` | 3 | 3 (unchanged) | PASS |
| `S02-lexical-conventions/comments.t` | 1 | 1 (unchanged) | PASS |
| `S02-literals/quoting-unicode.t` | 1 | 1 (unchanged) | PASS |
| `S03-operators/context.t` | 2 | 2 (unchanged) | PASS |
| `S06-signature/optional.t` | 1 | 1 (unchanged) | PASS |
| `S06-signature/positional-placeholders.t` | 1 | 1 (unchanged) | PASS |
| `S02-types/whatever.t` | 1 (+2 TODO) | 1 (unchanged) | PASS |
| `S12-enums/misc.t` | 1 | 1 (unchanged) | PASS |
| `S32-exceptions/misc2.t` | 3 | 3 (unchanged) | PASS |

**So: roast correctness regressions 57 -> 55.** The full sweep was not re-run
(it is ~25 minutes and its raw count is noisy by about +-6 because of the
timeout family); the per-file before/after above is the measurement.

### The other nine are NOT this mechanism — they are wrong exception classes

Each of the nine remaining assertions *does* throw in both contexts; it throws
the **wrong class**, and mutsu's native `throws-like` hides that on purpose:
`src/runtime/test_functions/throws_like.rs` accepts any error whose message
contains `"Confused"`/`"parse error"` whenever the expected class starts with
`X::Syntax`, plus a similar `X::Comp`/`X::Comp::Group` widening. The real
module's `$_ ~~ $expected` does not. Filed with the full measured table as
`todo/tickets/parse-errors-collapse-to-x-syntax-confused.md` (nine rows,
independent of each other, so it parallelises well). Two rows are *not* in that
family and are called out there: `S12-enums/misc.t` (right class, empty `.enum`
attribute) and `S32-exceptions/misc2.t` (`X::Placeholder::Mainline`, already a
known separate gap).

One more general bug fell out of that triage and is filed separately:
`todo/tickets/eval-write-to-outer-lexical-lost-inside-a-closure-or-routine.md`
— `EVAL '$a = 32'` writes through at mainline and inside a bare block, but the
write is **silently lost** once the `EVAL` runs inside an invoked closure or a
`sub` (raku: 32, mutsu: `Any`). That is `comments.t` #41, and it is a dual-store /
closure-writeback problem rather than an exception one.

### A correction to carry forward

The 2026-08-28 roast entry's classification ("39 files lose exactly one
assertion … no large shared mechanism left") is right about the *shape* but the
reason matters: a large part of that long tail is not missing behaviour, it is
**mutsu raising a generic parse exception where rakudo raises a specific one**,
masked by the native provider's deliberate leniency. Counting those as
"one fix per file" overstates the work — several of them will fall to one
parser change each, and the `X::Obsolete`-in-interpolation pair is one change
for two assertions.

Verification for this entry: the four-line repro now matches `raku` exactly;
`t/eval-parse-failure-propagates-through-block-call.t` (14 assertions, green
under `raku`, under mutsu's native provider and under `MUTSU_REAL_TEST=1`);
`make test` green (3518 files, 35036 tests); and an 823-file targeted roast
sweep over every whitelisted file in the synopses that consume bare-name type
resolution and EVAL plumbing (`S02`-`S06`, `S09`-`S12`, `S14`, `S32-exceptions`,
`integration`) is green on a release build.

## 2026-08-28 (fourth entry that day): the 5-file native-typed-array roast cluster closed — two `ContainerRef`-blind read sites

The largest single cluster among the 60 correctness regressions of the entry
above was five `S09-typed-arrays` files. Both of its bugs turned out to be the
same *shape* of mistake and neither is Test-specific: a read site that inspects
a variable's stored `Value` without reading **through** a shared `ContainerRef`
cell. The real `Test.rakumod` only supplied the context that puts the cell
there — its `is` / `is-deeply` are Raku subs, so the roast files' `is (@arr =
()), ...` and `is (@arr := array[$T].new(...)), ...` pass an array variable
through *argument binding*, which boxes the variable's slot into a cell.

### Bug 1 — a shaped array lost its shape once the variable had been shared

```raku
sub peek(Mu $got) { }
my @a := array[str].new(:shape(4), "a","b","c","d");
peek(@a);            # or: my $s = @a;   or: my $s := @a;
@a = "x","y";
say @a.join(":"), " elems=", @a.elems;
# raku:  x:y:: elems=4      mutsu (before): x:y elems=2
```

The prompting analysis had this as "the assignment's *result* being bound as a
sub argument". It is broader than that: **any** promotion of the variable to a
shared cell does it — a plain `peek(@a)` with no assignment at all, `my $s =
@a` (`MarkArrayShareSource` → `array_share_assign`, which writes
`self.locals[source_idx] = container`), `my $s := @a`, and an rw/`\(...)`
argument capture (`WrapVarRef` → `capture_var_cell_inner`). It is also not
native-array-specific: a plain `my @d[4]` loses its shape the same way.

Root cause: `runtime::utils::shaped_array_shape` requires
`ValueView::Array(_, ArrayKind::Shaped)` and answers `None` for anything else,
including a `ContainerRef` **holding** such an array. All three whole-array
assignment paths compute their `lhs_shape` from the raw stored value:

- `vm/vm_var_assign_set_local.rs` (`SetLocal`, the statement form),
- `vm/vm_var_assign_local.rs` (`AssignExprLocal`, the expression form),
- `vm/vm_misc_assign.rs` (`AssignExpr`, the by-name form).

With `lhs_shape == None` the shape-refill block is skipped, the unshaped RHS is
stored straight through the cell, and the array silently shrinks. The tell that
made this confusing: `@a.shape` still answered `(4,)` *between* the call and the
assignment, because that read goes through a different store — only the
assignment's `locals[idx]` read saw the cell.

The fix reads all three through the cell (`Value::with_deref`), and inside the
refill block reads the *old* container through it too, so the padding default
(`typed_container_default`) and the `array[T]` metadata
(`container_type_metadata`) are still recovered from the real array. Confirmed
with `rust-gdb -batch` breaking on the `lhs_shape` line rather than by guessing:
good case `Some([4])`, bad case `None`, same line, one statement apart.

### Bug 2 — `.squish` on a shared variable squished the cell, not the array

```raku
sub peek(Mu $got) { }
my @j := array[str].new("m","e","a","t");
peek((@j := array[str].new("nn","nn","bb","uu")));
say @j.squish.List.raku;
# raku:  ("nn", "bb", "uu")     mutsu (before): (["nn", "nn", "bb", "uu"],)
```

The prompting analysis expected this might be a separate, large problem; it is
neither. `runtime/methods_mut_dispatch.rs`'s `"squish"` arm re-reads its
receiver by name — `self.env.get(&key).cloned().unwrap_or(target.clone())` —
and after the rebind-in-an-argument the env entry is the `ContainerRef`.
Squishing a cell yields a one-element result whose single element is the whole
array. `.unique` / `.repeated` pass right beside it because they never take
this by-name re-read path (`squish` and `tail` are the two methods
`vm_native_dispatch.rs` unconditionally bypasses to the interpreter). One
`.map(Value::deref_container)` fixes it.

Note for the record: the standalone repro *was* findable, contrary to the
expectation carried into this slice — but only by bisecting the roast file, as
this ticket's own "a hand-written reproduction of 'the same thing' can lie"
note predicts. Six hand-written twins (plain `my str @arr`, the mutating
`for`/`map` pair, an element assign, a `for (int,) -> $T` loop, a `multi sub`
with `Mu` params) all passed. The one ingredient none of them had was the
`:=` **rebind used as a sub argument**, which comes from the roast file's
`is (@arr := array[$T].new("m","e","a","t")), ...` sixty lines earlier.

### Per-file before/after (release build, both providers)

| file | real Test before | real Test after | native before | native after |
| --- | --- | --- | --- | --- |
| `S09-typed-arrays/native-shape1-str.t` | 1 failure | pass | pass | pass |
| `S09-typed-arrays/native-shape1-num.t` | 5 failures | pass | pass | pass |
| `S09-typed-arrays/native-shape1-int.t` | 10 failures | pass | pass | pass |
| `S09-typed-arrays/native-int.t` | 10 failures | pass | pass | pass |
| `S09-typed-arrays/native-str.t` | 1 failure | pass | pass | pass |

So **roast correctness regressions: 57 -> 52** against the 2026-08-28 baseline
of that entry (the full sweep was not re-run; these are per-file measurements,
which is what that entry recommends given the `exit 124` noise). This slice ran
concurrently with the EVAL-suppression entry above, which took the same 57
baseline to 55 on a disjoint pair of files; composed, the two land at
**57 -> 50**. Worth
recording about the timeout family: on the **debug** build `native-int.t` still
exits 124 under `MUTSU_REAL_TEST=1` with zero failing assertions — the same
"re-run with headroom before counting it" artifact, now visible per-file and
not just under the parallel sweep.

Pin: `t/shaped-native-array-survives-sub-argument.t` (24 assertions, verified
green under real `raku` as well as mutsu — it covers the sub-argument, the
`$s = @a` share, the `$s := @a` bind, all three parameter shapes, the native
`int`/`num`/`str` element defaults, `.squish` through both promotion routes,
and the negative case that a *plain* array still shrinks and still reports no
fixed dimension).

Verification: `make test` green (3519 files, 35071 tests), two targeted roast
sweeps chosen from the consumers of what changed (181 files across
`S09-typed-arrays`, `S09-subscript`, `S32-array`, `S32-list`, `S02-types`,
`S06-signature`; then 107 more across `S02-names-vars`, `S03-operators`,
`S04-declarations`, `S06-other`, `S09-*`, `S32-container`, `S12-construction`)
both green, and `scripts/battery-testsuite.sh` on a **release** build with an
idle machine: `GATE PASSED`, 273/297 — unchanged.

### What this suggests for the rest of the residue

Both bugs are instances of one class: **a read site that inspects a variable's
stored `Value` directly instead of through `Value::with_deref`**. Every such
site is latent until something promotes the variable to a shared cell, and
passing the variable to a Raku-level routine is exactly such a promotion — which
is why this class shows up disproportionately in the `MUTSU_REAL_TEST=1`
regression list and is invisible under the native provider, whose `is` is a Rust
builtin that never binds an argument. When triaging the remaining roast
regressions, "does this read go through `with_deref`?" is worth checking early:
it is cheap, and it found two of the five files in this cluster from the same
grep.

## 2026-08-28 (fifth entry that day): the `%`-sigil half of the same `ContainerRef`-blind class — hash `push`/`append` and `:delete`-with-adverb

The entry above closed the `@`-sigil instances of "a read site that inspects a
variable's stored `Value` instead of going through `with_deref`" and explicitly
left the `%`-sigil siblings for a follow-up. This slice took that follow-up.
The prompting analysis pointed at `runtime/methods_mut_dispatch.rs`'s
`self.env.get(&key).cloned()` shape; that site is indeed blind and is fixed
here, but it is **not** what the headline repro was hitting. `rust-gdb -batch`
settled it in three breakpoints and zero rebuilds:

### The headline bug was a fast-path delegating to a weaker implementation

```raku
sub peek(Mu $got) { }
my %h;
%h.push: 'b', 2, 'a', 1, 'c', 3;
peek(%h);                       # boxes %h's slot into a shared ContainerRef cell
%h.push: (:a(4), :a(5));
say %h.raku;
# raku:  {:a($[1, 4, 5]), :b(2), :c(3)}     mutsu (before): {:a(1), :b(2), :c(3)}
```

The second `.push` was lost outright — not merely invisible to the read, as the
prompting analysis had it: a plain `say %h.raku` right after it was stale too.

`vm/vm_call_method_mut_ops.rs`'s `try_native_hash_mut_bound` intercepts
`%h.push` / `%h.append` whenever the name's env entry is a `ContainerRef`,
unwraps the cell, and delegates to `call_method_with_values` on the inner hash
so the write lands through the cell. That routing is right. The problem is
where it lands: `runtime/methods_call_dispatch.rs`'s by-value `Hash.push` arm was
an inline hand-rolled duplicate of the `%`-sigiled lvalue arm, and it only
understood a bare `ValuePair` argument. Everything else silently vanished:

- an alternating `'k', $v, 'k2', $v2` list,
- a parenthesised list / `Seq` / `Slip` / `Hash` argument,
- `append`'s array-flattening semantics (it applied `push` semantics to both),
- the element itemization at the store.

Nothing noticed because the by-value arm was only reachable from routes that
happened to pass plain pairs — until `try_native_hash_mut_bound` started
funnelling every cell-boxed `%h.push` through it, and passing a hash to *any*
Raku-level routine boxes it. Under the native provider `is`/`is-deeply` are Rust
builtins that never bind an argument, so the whole family was invisible there;
under the real `Test.rakumod` they are Raku subs, and the first `is %h, ...` in a
file arms the bug for every later `.push` in that file.

The fix deletes the duplicate: the by-value arm now calls the same
`hash_push_collect_pairs` / `hash_push_insert` helpers the lvalue arm uses, so
the two implementations of `Hash.push`/`Hash.append` cannot drift apart again.

### Bug 2 — the `%`-sigil lvalue arm really is `with_deref`-blind

Separately observable, and exactly the shape the entry above predicted:

```raku
sub peek(Mu $got) { }
my %a; %a<x> = 1;
my %r := %a;
peek(%a);
push %a, 'y', 2;
say %a.raku, " ", %r.raku;
# raku:  {:x(1), :y(2)} {:x(1), :y(2)}      mutsu (before): {:x(1), :y(2)} {:x(1)}
```

The listop and `%h."$name"()` routes reach `call_method_mut_with_values` without
passing `try_native_hash_mut_bound`, so they land in the `%`-sigil arm, whose
`hash_present` predicate and both write sites used a raw `self.env.get(&key)` /
`self.env.get_mut(&key)`. Against a `ContainerRef` those never match
`ValueView::Hash`, so the arm fell through to its "create from target value"
fallback, which rebuilds a **detached** hash and overwrites `env[key]` with it —
severing the cell, so `%r` never sees the push again. The mutation still looked
right through `%a` itself, which is why it survived so long. Every read and both
writes now go through `env_root_descended_mut`, the same cell-descending
chokepoint the array mutators use; reading and writing through the identical
resolution is also what keeps the `.unwrap()`s sound.

### Bug 3 — `:delete` combined with a `:k`/`:v`/`:p`/`:kv` adverb

Found by auditing the class rather than the one call site — a 27-case
mutsu-vs-`raku` diff of every hash/array mutator run after a `peek()`:

```raku
sub peek(Mu $got) { }
my %h = a=>1, b=>2; peek(%h); %h<a c>:delete:p; say %h.raku;
# raku:  {:b(2)}      mutsu (before): {:a(1), :b(2)}
my @a = 1,2,3;      peek(@a); @a[0,1]:delete:p; say @a.raku;
# raku:  [Any, Any, 3]  mutsu (before): [1, 2, 3]
```

A bare `:delete` lowers to the `DeleteIndexNamed` opcode, which already descends
cells. Combined with a `:k`/`:v`/`:p`/`:kv` adverb the compiler instead routes it
to `__mutsu_subscript_adverb` with a `delete => True` flag, and **both** of that
builtin's delete companions in `runtime/builtins_multidim_subscript.rs` (the
associative one and the positional one) used a raw `self.env.get_mut(var_name)`.
Against a cell, `with_hash_mut` / `with_array_mut` return `None`, and neither
site has a fallback — so the `:p`/`:k` half answered correctly while the
`:delete` half was silently dropped. This is what actually failed
`advent2013-day12.t` test 24; the prompting analysis's guess that the two
`integration/` files were the same `push` mechanism was right for
`advent2010-day08.t` and wrong for `advent2013-day12.t`.

`vm/vm_call_method_mut_ops.rs`'s `DELETE-KEY` in-place removal had the same raw
`env_mut().get_mut(target_name)`; it does have a fallback, but the fallback is
the alias-severing rebuild, so it was fixed for the same reason.

### Sites audited

| site | verdict |
| --- | --- |
| `runtime/methods_call_dispatch.rs` by-value `Hash.push`/`append` | duplicate implementation, replaced by the shared helpers |
| `runtime/methods_mut_dispatch.rs` `%`-arm: `key_constraint` read | blind, now `env_root_descended_mut` |
| `runtime/methods_mut_dispatch.rs` `%`-arm: `existing` snapshot | blind, now `env_root_descended_mut` |
| `runtime/methods_mut_dispatch.rs` `%`-arm: typed `hash_present` + write | blind, now `env_root_descended_mut` |
| `runtime/methods_mut_dispatch.rs` `%`-arm: untyped `hash_present` + write | blind, now `env_root_descended_mut` |
| `runtime/builtins_multidim_subscript.rs` associative `:delete` companion | blind, now `env_root_descended_mut` |
| `runtime/builtins_multidim_subscript.rs` positional `:delete` companion | blind, now `env_root_descended_mut` |
| `vm/vm_call_method_mut_ops.rs` `DELETE-KEY` in-place removal | blind, now `env_root_descended_mut` |
| `runtime/methods_mut_dispatch.rs` sigilless-array `unshift`/`prepend` (`env.get_mut`) | reachable only behind `try_native_array_mut`, which already descends; measured green in the 16-case sigilless/bound audit, left alone |
| `vm/vm_var_index_tracking.rs`, `vm_var_assign_element.rs`, `vm_var_assign_index_named.rs`, `vm_var_assign_post_incdec.rs` `env_mut().get_mut` sites | measured green for every element-assign / incr / slice-assign / nested-assign case in the audit, left alone |

### Per-file before/after (release build, both providers)

| file | real Test before | real Test after | native before | native after |
| --- | --- | --- | --- | --- |
| `S32-hash/push.t` | 2 failures (#3, #5) | pass | pass | pass |
| `integration/advent2010-day08.t` | 1 failure (#7) | pass | pass | pass |
| `integration/advent2013-day12.t` | 1 failure (#24) | pass | pass | pass |

So **roast correctness regressions: 50 -> 47** against the 2026-08-28 baseline
(the 57 of the sweep entry above, minus PRs #7078/#7079). Per-file
measurements, as that entry recommends given the `exit 124` noise. A real-Test
re-run of all 285 whitelisted files under `S32-hash` / `S09-hash` / `S32-list` /
`S02-types` / `S06-signature` / `integration` reports exactly the baseline's
failure set for those directories minus these three files — no new real-Test
regression.

Pin: `t/hash-mutation-visible-after-sub-argument.t` (28 assertions, verified
green under real `raku` as well as mutsu — it covers the argument-binding
promotion, the `:=` rebind, both `%`- and `$`-named bind targets, all five call
forms of `push`/`append` (colon-listop, parenthesised, listop, `."$name"`,
`append`), alternating / pair-list / hash / duplicate-key arguments, alias
visibility, and `:delete` with `:p`/`:k` on both an associative and a positional
slice).

Verification: `make test` green (3521 files, 35113 tests), two targeted roast
sweeps chosen from the consumers of what changed (285 files across `S32-hash`,
`S09-hash`, `S32-list`, `S02-types`, `S06-signature`, `integration`; then 139
more across `S09-subscript`, `S32-array`, `S32-container`, `S02-names-vars`,
`S03-operators`, `S12-construction`, `S09-typed-arrays`, `S06-other`,
`S04-declarations`) both green, and `scripts/battery-testsuite.sh` on a
**release** build with an idle machine: `GATE PASSED`, 273/297 — unchanged.

### Note for the rest of the residue

The `with_deref` heuristic held up, but with a twist worth carrying forward: two
of the three bugs here were *write* sites, not read sites, and the headline one
was a **fast path delegating to a second, weaker implementation of the same
operation**. When a cell-boxed receiver misbehaves, check not only "does this
read go through `with_deref`?" but also "does this interception land in the same
implementation the non-intercepted path uses?" — a duplicated implementation is
latent divergence that only the cell case exercises.

### Follow-up the same day: `%`-sigiled names must NOT take the bound-hash fast path

Landing the above exposed one more consequence of routing a cell-boxed `%h`
through the by-value implementation: the by-value arm carries none of the
*richer* hash-push semantics the `%`-sigiled lvalue arm does. Measured against
`raku` right after the fix:

```raku
sub peek(Mu $got) { }
my %h{Int};  peek(%h); %h.push(1, 'x'); say %h.raku;
# raku: (my Any %{Int} = 1 => "x")    mutsu: (my Any %{Int} = "1" => "x")
my Int %h = a=>1; peek(%h); %h.push('b', 'not-an-int');   # raku dies, mutsu did not
```

The object hash's `.WHICH` key encoding with its `original_keys` record, the
typed-hash key/value type checks and the duplicate-key array-conflict check all
live in the `%` arm. Before this campaign the fast path hid that because it
dropped the push entirely; afterwards it performed the push, with the wrong key
representation and no type check.

Since the `%` arm now descends the cell on its own, the fast path is simply
redundant for a `%`-sigiled name, so `try_native_hash_mut_bound` bails on one.
It still owns the case the `%` arm cannot see — a *scalar*-named bind
(`my $r := %g; $r.push(...)`), which is what it was written for. Pinned by five
more assertions in `t/hash-mutation-visible-after-sub-argument.t` (33 total,
green under real `raku`).

The general lesson, sharper than the one above: when a fast path "delegates to
the interpreter", check *which* interpreter arm it lands in. Two arms
implementing one operation will differ, and the intercept silently picks the
poorer one.

## 2026-08-28 (sixth entry that day): the nine wrong-exception-class rows are all fixed, and the native `throws-like`'s parse-error leniency is now GONE

The 2026-08-28 third entry filed
`todo/tickets/parse-errors-collapse-to-x-syntax-confused.md` — nine assertions
across seven whitelisted files where mutsu raised a generic
`X::Syntax::Confused` / `X::AdHoc` and only the native `throws-like`'s message
sniffing kept them green. All nine now raise the class `raku` raises, each
re-derived against `raku` before and after the fix.

| snippet (inside `EVAL`) | before | after (= raku) | root cause |
| --- | --- | --- | --- |
| `@arr [0]` | `X::Syntax::Confused` | `X::Syntax::Missing` (`what => 'infix inside []'`) | a `[...]` in *infix* position is the reduce metaoperator, so its content must name an infix; the user-infix branch of `parse_list_infix_loop_impl` reported only "expected expression after bracket user infix op" |
| `42.:all` | `X::Syntax::Confused` | `X::Syntax::Number::IllegalDecimal` | `.:name` is a valid reified-operator postfix, so rakudo's decimal-point *sorrow* is its only complaint and is thrown alone; mutsu let `.:` + identifier through as a method-call attempt instead |
| `say 42.:all` | `X::Syntax::Confused` | `X::Syntax::Number::IllegalDecimal` | same |
| `"${$scalar}"` | `X::AdHoc` | `X::Obsolete` | the interpolation path injected `die "X::Obsolete: …"` — a *string*, so `$!` saw an `X::AdHoc`; it now embeds the real `X::Obsolete` instance (with `.old`/`.replacement`) |
| `"@{$array}"` | `X::AdHoc` | `X::Obsolete` | same |
| `rt54804( 1, , 3, )` | `X::Syntax::Confused` | `X::Syntax::InfixInTermPosition` | `primary()` seeded that diagnosis for `=>` only; `,` is the same story and no term can begin with it |
| `{my $foo; $^foo;}(1)` | `X::AdHoc` | `X::Redeclaration` | `check_placeholder_conflicts` returned a `"X::Type: text"` *string* for this one branch while its two sibling branches already returned instances |
| `{*.{}}()` | `X::Syntax::Confused` | `X::Syntax::Malformed` | two gaps: a bare `{ *-curry }` was accepted at all (rakudo: "Malformed double closure"), and `.{}` / `.[]` — the dotted zen slice — did not parse, so the body collapsed to "Confused" before the diagnosis could fire |
| `'RT' ~~ m\c[SNOWMAN].\c[COMET]` | `X::Syntax::Confused` | `X::Comp::Group` | `delim_commits_to_regex` committed only for `/ { [ ( <`, so an unterminated non-ASCII-delimited `m☃…` backtracked out to "Bogus postfix: ☃" instead of the existing "Regex not terminated" group |

Three general improvements fell out that were not asked for by any assertion:

* `42. i` / `42. foo` now raise a lone `X::Syntax::Number::IllegalDecimal` and
  `42.` / `42.,` / `42.:` / `42.:1` an `X::Comp::Group`, exactly as rakudo
  splits them (the sorrow stands alone iff the retried `.` still forms a valid
  postfix). That also fixes `minimal-whitespace.t` #9, which expects `X::Comp`
  and was getting an `X::Comp::Group` — not a subclass of it in rakudo.
* `%h.{}` / `@a.[]` parse (they were "Confused"), and `*.{}` / `*[]` curry into
  a `WhateverCode` that actually returns the container.
* `.old` / `.replacement` on the `${…}` / `@{…}` `X::Obsolete` now name the
  construct as written, matching rakudo's text.

### Measured, file by file (release build, `scripts/run-roast-test.sh`, both providers)

| file | before (real) | after (real) | native |
| --- | --- | --- | --- |
| `S02-lexical-conventions/minimal-whitespace.t` | 3 failures | **PASS** | still PASS |
| `S03-operators/context.t` | 2 failures | **PASS** | still PASS |
| `S06-signature/optional.t` | 1 failure | **PASS** | still PASS |
| `S06-signature/positional-placeholders.t` | 1 failure | **PASS** | still PASS |
| `S02-types/whatever.t` | 1 failure (+2 TODO) | **PASS** (2 TODO only) | still PASS |
| `S02-literals/quoting-unicode.t` | 1 failure | **PASS** | still PASS |
| `S12-enums/misc.t` | 1 failure | 1 (unchanged — the empty `.enum` attribute, a different bug) | still PASS |

**So: roast correctness regressions 55 -> 46** against the baseline of the third
entry that day (nine assertions across six files; `S12-enums/misc.t` is
untouched and stays on the list). This slice ran concurrently with the two
`ContainerRef`-blind entries above, which took the same 57-file baseline down to
47 on a **disjoint** file set (`S09-typed-arrays`, `S32-hash`, `integration`);
composed, the three land at **47 -> 38**.

### The narrowing experiment: the leniency is retired, and the real bug was reading the WRONG OBJECT

The ticket asked, once the classes were right, whether the native provider's
widening could be removed. Deleting the message-substring branches outright
broke **20 whitelisted files** — every one of them on `right exception type
(X::Comp)` or `(X::Syntax::Confused)`. Reading the failures showed why, and it
was not "mutsu still raises the wrong class": those errors carry **no
structured exception object at all** in `RuntimeError::exception`, so
`ex_class` was `None` and the type check skipped the class/MRO/role branches
entirely.

But `throws-like` was already computing the right object two hundred lines
lower: the *named matchers* answer off `err.exception_value()`, which derives a
real instance from the `"X::Type: text"` convention or the parse code (that is
why `$!.^name` said `X::Syntax::Confused` all along, while the type check saw
`None`). The type check simply read a different field than the matchers did.

Pointing both at `exception_value()` makes the widenings dead, and they are
deleted:

* the `X::Syntax::Confused` message-substring branch,
* the `starts_with("X::Syntax")` branch (message substring + `parse error` +
  parse-code),
* the `X::Comp` / `X::Comp::Group` message-substring branch,
* the `expected == "X::Comp::Group" && class_does_role(cls, "X::Comp")`
  broadening inside the class branch,
* and the `expected == "X::AdHoc"` "matches any ad-hoc error" catch-all.

**Measured: a full sweep of all 1436 whitelisted roast files is green with all
five removed** (release build). `make test` is green too. So the native
provider no longer has any parse-error leniency left to pay back — its type
check is now the same class/MRO/role question the real `Test.rakumod` asks,
against the same object.

Verification for this entry: `t/parse-error-exception-classes.t` (20
assertions, green under `raku` unchanged and under mutsu); `make test` green
(3520 files, 35081 tests); the seven ticket files green under BOTH providers;
and a full 1436-file whitelisted roast sweep green on a release build.

## 2026-08-28 (end of day): the composed sweep re-run — 57 -> 40 correctness regressions

The five entries above each measured against a partly-overlapping baseline
(the third and sixth entries both quote a "55", from different sides), which
makes the running arithmetic hard to trust. So the sweep was re-run once at the
end of the day, on `main` @ `592e04f3a` (after #7078, #7079, #7080, #7081 and
#7082 had all landed), release build, `-j6`, same
`scripts/roast-test-module-sweep.sh`:

```
pass under both:                   1394
regressed under the real Test:      42
passes only under the real Test:     0
fail under both (pre-existing):      0
```

Two of the 42 are `exit 124` (`S03-buf/read-write-bits.t`,
`S03-buf/write-int.t`) — the performance artifact this file has documented
twice, not correctness. **So: 40 correctness regressions, down from the 57 this
day opened with.** That is exactly 57 minus the 17 files the five PRs closed
(2 + 5 + 3 + 7), so the per-entry arithmetic above was right after all; this
run is the independent confirmation of it.

Note that the morning sweep had *three* timeouts and this one has two, with
nothing about those files changed — the same ±6 noise in the raw count that
this file warns about. The timeout-excluded number is the only one worth
quoting.

The per-file reports for both runs are preserved outside the repo at
`tmp/real-test-regressions-2026-08-28.txt` (morning, the 60-file baseline) and
`tmp/real-test-regressions-2026-08-28-eod.txt` (evening, the 42-file residue).

### What the residue looks like now

The three largest clusters closed today were all found by *classifying the
report* rather than by picking files off it one at a time, and the
classification that paid was **not** "which synopsis is it in" — it was "what
shape of mutsu bug can only be seen through a Raku-level `is`". Two of the four
slices reduced to the same root class (a read or write site that inspects a
variable's stored `Value` instead of going through `with_deref`), and that
class is invisible under the native provider by construction, because the
native `is`/`is-deeply` are Rust builtins that never bind an argument.

For whoever picks this up next: start from that axis, not from the file list.
The remaining named clusters are `S24-testing/{10-is-approx,14-like-unlike,3-output}.t`
(the real module's own spec tests, confirmed genuine mutsu gaps),
`S32-io/{slurp,spurt}.t` plus `S16-io/words.t` and `S32-io/io-cathandle.t`
(Buf/handle I/O), and `S32-list/{seq,tail}.t`.
`S24-testing/{2-force_todo,6-done_testing}.t` remain the native-provider-only
pair, which needs the `#?rakudo eval` fudge directive or an un-whitelisting
rather than an interpreter fix.

## 2026-08-29: the session-opening sweep reproduces 40, and `S24-testing/14-like-unlike.t` closes

Per this file's own process note, the day opened with a fresh
`scripts/roast-test-module-sweep.sh` on `main` @ `139aa395f` (release, `-j6`):

```
pass under both:                   1394
regressed under the real Test:      42
passes only under the real Test:     0
fail under both (pre-existing):      0
```

Two of the 42 are the familiar `exit 124` performance artifact
(`S03-buf/read-write-bits.t`, `S03-buf/write-int.t`), so **40 correctness
regressions** — and the regressed *file set* is byte-identical to the previous
evening's, so the count is reproducible rather than noisy. Report preserved at
`tmp/real-test-regressions-2026-08-28-round151-start.txt`.

### `S24-testing/14-like-unlike.t`, and the value of reading the diagnostic

The failing assertion is
`like class { method Str { 'foo' } }, /foo/, '...'`, and the obvious first
reading — "mutsu's regex smartmatch does not stringify a non-`Str` object" — was
**wrong**, even though probing it *did* turn up a real divergence. The
diagnostic is what corrects it:

```
# expected a match with: /foo/
#                   got: ""
```

`got: ""` means the argument was already `""` before any matching happened, and
rakudo's `like` declares **`Str() $got`** — a coercion-type parameter. mutsu's
`try_coerce_value_with_method` dispatched the target-named method for an
`Instance` but had no branch for a *type object*, so a class defining
`method Str` coerced to `""`. That, not the smartmatch, is what failed the file.

The smartmatch divergence found on the way in is real too and is fixed in the
same PR: `regex_match_text` matched a type object against its own type NAME, so
`Int ~~ /Int/` was True (rakudo: False, with the uninitialized-value warning)
and `C ~~ /foo/` was False for a `C` defining `method Str` (rakudo: True). Both
are in `news/2026-08/type-object-string-coercion-dispatches-its-own-str.md`,
pinned by `t/regex-smartmatch-type-object.t` (23 assertions) and
`t/coercion-param-type-object-user-method.t` (14), both green under real `raku`.

The smartmatch half also cost a lesson worth recording: making the coercion
*warn*, as rakudo does for `Any ~~ /a/`, regressed `roast/S05-metasyntax/regex.t`
test 51 in the targeted sweep, because a **bare** `/a/` is silent in rakudo where
the written-out `$_ ~~ /a/` warns. That is a compile-time distinction — the
compiler synthesizes the `$_` LHS in `compile_match_regex` — so it is now carried
on `SmartMatchLhs::Var` as an `implicit_topic` flag. **Run the targeted sweep
before believing a spec-fidelity addition is free**: this one looked like a pure
improvement and broke a whitelisted file.

| file | real Test before | real Test after | native before | native after |
| --- | --- | --- | --- | --- |
| `S24-testing/14-like-unlike.t` | 1 failure (#2) | **PASS** | PASS | PASS |

**So: 40 -> 39 correctness regressions** from this slice alone. Two other slices
ran concurrently on disjoint files (`S32-io/{slurp,spurt}.t`;
`S02-types/subset-6e.t` + `6.c/S02-types/subset-6c.t`) and are measured in their
own entries.

### Two clusters classified for whoever goes next

Probing the residue turned up one cluster that is worth taking as a unit,
because three files reduce to a single root cause:

**A runtime, name-resolved write to an outer lexical is lost as soon as it
happens inside an *invoked* closure or a routine.** Both `EVAL` and symbolic
dereference show the identical shape, and neither involves `Test`:

```raku
my $z = 1; $::('z') = 11;                              # 11   OK
my $z = 1; { $::('z') = 22 };                          # 22   OK  (bare block)
my $z = 1; my $c = { $::('z') = 33 }; $c();            #  1   WRONG (raku: 33)
my $z = 1; sub w(&f) { f() }; w({ $::('z') = 44 });    #  1   WRONG (raku: 44)
my $z = 1; sub w2() { $::('z') = 55 }; w2();           #  1   WRONG (raku: 55)
```

`todo/tickets/eval-write-to-outer-lexical-lost-inside-a-closure-or-routine.md`
already records the `EVAL` half and names
`roast/S02-lexical-conventions/comments.t` (test 41). The sweep says it is worth
more than that one file: `roast/S06-signature/sigilless.t` (test 5,
`lives-ok { EVAL 'swap($a, $b)' }` with sigilless rw parameters) is the same
`EVAL` half, and `roast/S02-names/symbolic-deref.t` (tests 3 and 14) is the
`$::(…)` half — **three files, one root cause**. It only surfaces under the real
module because both are written inside a `lives-ok { … }`, and the real
`lives-ok` is a Raku sub that *calls* the block where the native one does not.

The second, smaller observation: `S24-testing/{2-force_todo,6-done_testing}.t`
remain the native-provider-only pair described above, and
`S24-testing/3-output.t` is not a `Test` gap either — it compares `diag` output
and mutsu's parse-error text is both more verbose and internally duplicated
("expected expected statement …", "— near: X — near: X"), which is a message
-quality bug rather than a behavioural one.

## 2026-08-29: the Buf/handle-I/O cluster, part 1 — `infix:<eq>` across Blob types (40 -> 38)

Taking the `S32-io/{slurp,spurt}.t` pair named in the residue above. Both
regressed for one reason, and it was a general interpreter bug rather than
anything about I/O: **`eq`/`ne` between two Blob values of *different* Blob
types answered the wrong result.**

The two assertions are `is slurp($path, :bin), $test-contents.encode` and
`is slurp($path, :bin), $buf` / `($buf ~ $buf)` — in each case a `Buf` coming
back from `slurp :bin` compared against a `utf8` produced by `.encode`. Rakudo's
`(Blob:D, Blob:D)` `eq` candidate compares the bytes whatever the two Blob types
are (measured: `"hi".encode eq Buf[uint8].new(104,105)` is `True`, as are the
swapped and `Blob[uint8]` forms), so the assertion holds there. mutsu answered
`False` for every such pair. The native `is` never surfaced it because it
byte-compares Bufs itself (`runtime/test_functions/basic.rs`), bypassing
`infix:<eq>` entirely.

Root cause: `coerce_str_compare_operands`
(`src/vm/vm_comparison_order_ops.rs`) decoded a `utf8` operand to a `Str`
**per operand**, before the comparator body ran. Every comparator already had
the correct `is_buf_value(&l) && is_buf_value(&r)` byte branch — it was just
unreachable for a mixed pair, because the decode had already dissolved the Blob
pair. The surviving Buf then stringified to its gist (`Buf[uint8]:0x<68 69>`),
which no decoded text ever equals. Fixed by deciding the decode for the *pair*:
skip it when both operands are Blobs, keep it otherwise (so
`"hi".encode eq "hi"` still holds).

Per-file, release build, `MUTSU_BIN` set both ways:

| file | native, before | real Test, before | native, after | real Test, after |
| --- | --- | --- | --- | --- |
| `roast/S32-io/slurp.t` | PASS 21/21 | FAIL, test 12 | PASS 21/21 | PASS 21/21 |
| `roast/S32-io/spurt.t` | PASS 62/62 | FAIL, tests 1, 4, 12, 15 | PASS 62/62 | PASS 62/62 |

(The spurt helper `all-basic` runs twice, once per path form, which is why the
two failing assertions show up as four subtests.)

**Count: 40 -> 38 correctness regressions.** Pinned by
`t/blob-comparison-across-types.t` (33 assertions, green under real `raku` as
well as under mutsu). Two adjacent divergences were deliberately left out of
scope because they require mutsu to start *throwing* where it currently answers
(`Buf[uint8] eq "hi"` should be `X::Buf::AsStr`; mixed-Blob-type `lt`/`cmp`
should be a type-check failure); they are written up with the dozen other
gist-comparing sites they would have to move with in
`todo/tickets/blob-comparison-should-die-instead-of-answering.md`.

Remaining in this cluster: `S16-io/words.t` and `S32-io/io-cathandle.t`.

## 2026-08-29: the `where`-constraint scope pair (`subset-6c.t` / `subset-6e.t`)

Two more files off the residue — **the two this slice closes; see the note on
counting below before adding them to any running total** — and, as the
classification entry two sections up predicted for this residue in general, the
bug had nothing whatsoever to do with `Test`. It reproduced identically under
the native provider; it only *surfaced* under the real module because
`lives-ok`'s Raku-level implementation actually observes the exception the
native builtin swallowed.

| file | test | before (real Test) | after (real Test) | native provider |
| --- | --- | --- | --- | --- |
| `roast/S02-types/subset-6e.t` | 39, `where-constraint picks up the right lexical (+)` | FAIL (1/60) | PASS (60/60) | PASS before and after |
| `roast/6.c/S02-types/subset-6c.t` | 38, same assertion | FAIL (1/51) | PASS (51/51) | PASS before and after |

The bug: a parameter's `where` constraint (and its default value) was evaluated
against the *calling* frame instead of the scope the signature was written in.
The roast assertion's shape makes that unmissable — `bar(2)` died and `bar(3)`
lived, `3` being exactly the enclosing block's shadowed binding. Two independent
root causes, both general and both now fixed: the compiler never recorded a
signature's declaration-time reads as captures (they live on the `ParamDef` AST
and never reach the opcode scan `compute_free_vars` performs), and a named `sub`
escaping as its declaring routine's return value captured a flattened env that
could not see that routine's local *slots*. Full write-up:
`news/2026-08/where-constraint-declaration-scope-capture.md`; pin:
`t/where-constraint-lexical-scope.t` (23 assertions, also green under real
`raku`).

Verification: both files green under BOTH providers; `make test` green; the full
1436-file whitelisted roast suite green on a release build; and
`scripts/battery-testsuite.sh` green (the change touches closure-capture
representation, which the note in CLAUDE.md warns roast alone will not catch).

**Method for whoever continues here.** The previous entry's advice — classify by
"what shape of mutsu bug can only be seen through a Raku-level assertion", not by
synopsis — held up again, with one refinement worth writing down: for a file
whose regression is a *single* assertion, read that assertion's expected/actual
values before reading any code. Here, "the value that wrongly passed is precisely
the shadowed outer binding" identified the failure as a wrong-frame lookup (not a
missing one) in one step, and a handful of probe shapes then separated the two
root causes without a single instrumented build.

**A note on counting, since several slices are in flight at once.** This entry
deliberately quotes only its own per-file before/after. A fresh session-opening
sweep on `139aa395f` measured 42 raw regressions minus 2 `exit 124` timeouts =
**40 correctness regressions**; the `infix:<eq>` entry above took that to 38;
this slice closes 2 more; and PR #7086 closes `S24-testing/14-like-unlike.t`
independently. Because those landed concurrently, no single running total in
this file is trustworthy as arithmetic — subtract "the N files each entry names"
from a *re-measured* baseline instead, and re-run
`scripts/roast-test-module-sweep.sh` when you need an authoritative number.

## 2026-08-29: `return()` is a zero-argument call (`S04-statements/return.t`)

`roast/S04-statements/return.t` regressed on tests 2 and 5 —
`is(bar2(), Nil, ...)` for `sub bar2 { return() }` and
`sub foobar2 { return() if 1 }`. mutsu returned an empty list where rakudo
returns `Nil`, and the cause is one of Raku's oldest whitespace rules that the
parser dropped: an argument list attached with **no** space is the routine's
argument list, so `return()` passes zero arguments and is exactly a bare
`return`, while `return ()` passes the empty list as a term and really does
return `()`. `return_stmt` called `ws` immediately after the keyword, so both
spellings produced a byte-identical `Return(ArrayLiteral([]))`.

Measured on a rebuilt pre-fix binary: the **native** `is` accepted the wrong
value (`ok 1`), the real module's rejected it. Another instance of this file's
recurring shape — the answer was already wrong under the native provider, and
only the strict module asks a question sharp enough to see it.

| file | real Test before | after | native before | after |
| --- | --- | --- | --- | --- |
| `S04-statements/return.t` | 2 failures (#2, #5) | **PASS** | PASS | PASS |

Fix and pin: `news/2026-08/bare-return-with-parens-is-nil.md`,
`t/bare-return-with-parens.t` (17 assertions, green under real `raku`).
Verification: `make test` green (3526 files, 35226 tests); a 523-file targeted
roast sweep across `S04-statements`, `S04-blocks*`, `S06-*`, `S02-names`,
`S32-list`, `S05-*`, `S12-*` and `integration` green on the native provider.

Per the counting note above: this closes **one** named file. It composes with
#7084 (2 files), #7085 (2) and #7086 (1) against the same re-measured 40
baseline, so a fresh sweep should read 35 — measure it rather than trusting that
arithmetic.

## 2026-08-29: the Seq/List cluster — `tail.t`, `seq.t`, `words.t` (closes 3)

The three files the end-of-day sweep grouped as `S32-list/{seq,tail}.t` plus
`S16-io/words.t` are **two** root causes, not one, and both sit in
`infix:<eqv>` — which is exactly why the native provider never saw them: the
real `is-deeply` narrows `Seq` arguments with `.cache` and then compares with
`eqv`, and the real `cmp-ok` reaches an operator only through the ROUTINE form
`&CALLER::LEXICAL::("infix:<$op>")`.

1. **The `SeqView::List` handle was a `List` only by name.** ADR-0038 gave
   `.cache` on a not-yet-reified `Seq` a second handle tagged `SeqView::List`
   and taught `value_type_name` to read the tag — enough for `is-deeply`'s
   narrowing to terminate (the ADR's stack overflow), but the value stayed a
   `ValueView::Seq`, so `eqv` (type-strict) and `.raku` (renders the type) both
   still saw a `Seq`. `$d.cache eqv ('a','b','c')` answered False and
   `$d.cache eqv <a b c>.Seq` answered True — both backwards. Fixed by
   normalising through one new helper, `Value::seq_list_view_as_list`, read by
   `Value::eqv`, the `.raku` renderer, and `reify_or_consume_eqv_operand` (which
   used to rebuild a taken List-view handle as a plain `Seq`). That is
   `tail.t` 57 and all four `words.t` failures.
2. **`&infix:<eqv>` was not the `eqv` operator.** `a eqv b` runs `OpCode::Eqv`,
   whose handler owns the lazy-iterable rules, `Proxy` element FETCH, the
   same-Seq identity fast path and the Seq reify/consume protocol that raises
   `X::Seq::Consumed`. The routine form fell through `call_infix_routine` to the
   pure `apply_reduction_op` fold (just `Value::eqv`), so
   `cmp-ok $consumed1, 'eqv', $consumed2` silently answered False and emitted
   its own TAP line inside the `throws-like` subtest. The operator body is now
   `Interpreter::eqv_values` and `call_infix_routine` routes `eqv` through it,
   the same way it already routes `~~`. That is `seq.t` 34.

A THIRD route to `eqv` had the same defect and was fixed in the same move:
`eval_reduction_operator_values` (which serves `[eqv]` and every metaop —
`Zeqv`, `Xeqv`, `>>eqv<<`) also answered from the static `apply_reduction_op`
table, so `[eqv] $consumed1, $consumed2` said `False` where raku throws.

The targeted sweep then caught the consequence of (2): making the routine form
consume — as raku does — exposed that mutsu's `unique`/`repeated` never cached a
`Seq`-valued `:as` needle, which is what `roast/S32-list/unique.t`'s last test
("Seq as the result of an :as caches the Seq") pins. `unique` and `repeated` now
`.cache` it; `squish` deliberately does not, because raku's `squish` genuinely
throws `X::Seq::Consumed` on the same input (measured all three side by side).

### Per-file before/after (release build, `scripts/run-roast-test.sh`)

| file | real Test before | real Test after | native before | native after |
| --- | --- | --- | --- | --- |
| `roast/S32-list/tail.t` | 1 failure (#57) | **PASS** | PASS | PASS |
| `roast/S32-list/seq.t` | 1 failure (#34) | **PASS** | PASS | PASS |
| `roast/S16-io/words.t` | 4 failures (#1, #2, #5, #6) | **PASS** | PASS | PASS |

Per the counting note above: this closes **three** named files against the
2026-08-29 re-measured 40 baseline. It composes with #7084 (2 files), #7085
(2), #7086 (1) and #7087 (1) on a disjoint file set — measure the next sweep
rather than trusting the arithmetic.

Pin: `t/seq-cache-list-view-and-eqv-routine.t` (38 assertions, green under real
`raku` as well as mutsu). Verification: `make test` green; the three files green
under BOTH providers; a 444-file native sweep across `S32-list`, `S32-array`,
`S16-io`, `S32-io`, `S02-types`, `S04-statements`, `S07-*`, `S03-operators` and
`integration` green; `scripts/battery-testsuite.sh` on a release build unchanged.
Full write-up:
`news/2026-08/seq-list-view-is-a-list-everywhere-and-the-eqv-routine-is-the-eqv-operator.md`.
One residual was split off rather than forced through:
`todo/tickets/seq-list-view-handle-is-not-itemized-by-scalar-assignment.md` (a
`.raku`-only itemization gap on the deferred List-view handle).

### Note for the rest of the residue

ADR-0038's "one oracle" rule was written about the type *name*. Both bugs here
say the same thing one level down: a value carrying a type tag needs that
discipline everywhere its type is *observable* (`eqv` and `.raku` are type
oracles too), and where an operation has an operator form and a routine form,
the real `Test.rakumod` will find whichever of the two mutsu did not think of as
"the" implementation. `cmp-ok` reaches EVERY operator by name through
`&CALLER::LEXICAL::`, so any remaining operator whose routine form diverges from
its opcode is a live candidate for the rest of the list.
## 2026-08-29 — runtime-name writes: three files, three root causes

(Started 2026-08-28, landed against the 40-regression baseline the
session-opening sweep above reproduced; it takes that count to **37**.)

Slice for the largest remaining cluster on the residue list: a write whose
target NAME is resolved at run time (`$::($n) = v`, `::('$x') = v`, an
assignment inside an `EVAL`'d snippet) was silently lost as soon as it happened
inside an *invoked* closure or a routine. Mainline and a bare block worked, which
is why only the real `Test` provider surfaced it — the real `lives-ok` /
`throws-like` are Raku subs that **call** the Callable they are given, so the
write runs one closure-invocation deep.

The prompt's framing was "three files, one root cause". Measured, it was three
*different* root causes, all needed:

1. The frame-exit writeback (`call_compiled_closure_with_topic` /
   `call_compiled_function_named_inner`) filters purely on compile-time
   knowledge, so a runtime-resolved target passes none of its tests.
   `OpCode::SymbolicDerefStore` is additionally missing from
   `CompiledCode::has_env_writes`, so a closure whose body was only a symbolic
   store skipped the writeback scan entirely (left as-is — the new escape hatch
   runs outside that gate, so widening the flag would only add scan cost).
2. An EVAL'd unit never recorded its own compile-time `free_var_writes` (only
   `where` clauses used `eval_block_value_recording_writes`).
3. `parse_and_eval_with_operators`'s `eval_pre_lexicals` snapshot used
   `Env::keys` (innermost overlay only). Inside a closure/routine the caller's
   lexicals live in a parent tier, so they all looked new and the EVAL's write
   was deleted as an "EVAL-local `my`" — leaving a **tombstone** that hid the
   caller's binding, so a second `EVAL 'say $a'` in the same block died with
   "Variable '$a' is not declared".

Full write-up: `news/2026-08/runtime-name-write-to-outer-lexical.md`. Pin:
`t/runtime-name-write-to-outer-lexical.t` (28 assertions, green under real
`raku` too).

### Measured, file by file (debug build, `scripts/run-roast-test.sh`, both providers)

| file | before (real) | after (real) | native |
| --- | --- | --- | --- |
| `S02-lexical-conventions/comments.t` | 1 failure (#41 "sanity check") | **PASS** | still PASS |
| `S06-signature/sigilless.t` | 1 failure (#5 "swapping worked") | **PASS** | still PASS |
| `S02-names/symbolic-deref.t` | 2 failures (#3, #14 "and the assignment worked") | **PASS** | still PASS |

So: roast correctness regressions under the real provider **-3** (three files
fully cleared, not merely improved). `comments.t`'s remaining "no tab allowed"
`throws-like` — the row this ledger had recorded as its residual — turned out to
pass once the tombstone in root cause 3 was gone, so that file is now clean
under both providers as well.

### Two traps this slice hit, both worth carrying forward

**A system name must never be replayed across a frame boundary.** The first
working version propagated every name on `pending_caller_var_writeback`, which
included `&?BLOCK` (recorded from an EVAL'd unit's `free_var_writes`). Carried
upward it made the real `Test::throws-like`'s `subtest` block run against the
*previous* subtest's `&?BLOCK`, so its `CATCH` saw `Any` and the second of two
consecutive `throws-like { EVAL … }` calls reported "right exception type" as
failed. That regression was invisible in isolation and only reproduced with a
prior `throws-like` in the same file.

**`scripts/battery-testsuite.sh` earned its keep again.** Even after filtering
to plain user lexicals, propagating all of `pending_caller_var_writeback` was
too blunt — that list is also fed by `is rw` writeback misses, Proxy STOREs,
`$CALLER::x` writes and the shared-var lane. Replaying those into every
intervening caller env broke a `given $in { when IO::Handle {…} }` dispatch in
the bundled Text::CSV (`Type check failed in assignment to $io-in`, 90_csv.t),
which a green `make test` (3523 files) and a green 226-file targeted roast sweep
both missed. The fix was a dedicated `pending_runtime_name_writes` list carrying
only the runtime-name writes.

### Two END-phaser bugs found in the same area, filed separately

A fourth file, `roast/S04-phasers/end.t` (tests 6/7, real-provider-only), was
suspected to share this root cause. It does not — it still reproduces with the
fix landed, and its trigger needs neither `EVAL` nor a runtime-resolved name:

```raku
sub callit(&c) { c() }
{ my $a = 42; END { say "END1 (want 42): ", $a.raku }; }
my $a = 0;          # a DIFFERENT binding that merely shares the name
callit { $a };      # mutsu: 0    raku: 42
```

The post-return END-phaser env refresh in `vm_closure_dispatch.rs` is
**name-keyed**, so any called closure capturing a same-named lexical rewrites an
unrelated phaser's capture. Filed as
`todo/tickets/end-phaser-captured-lexical-clobbered-by-a-later-same-named-capture.md`.

An independent END *ordering* divergence surfaced alongside it (mutsu
`END2 END1 END3` vs raku `END3 END2 END1`, i.e. raku is plain reverse
installation and mutsu defers the mainline's own ENDs). It is deliberately NOT
fixed here — `news/2026-08/end-phasers-run-in-install-order.md` made mutsu
install-ordered on purpose — and is filed as
`todo/tickets/end-phaser-run-order-is-not-reverse-installation.md`.


## 2026-08-29: an EVAL'd unit does not inherit the caller's `fatal` (`S02-names/is_default.t`)

`roast/S02-names/is_default.t` regressed on the assertion
`eval-lives-ok 'my $a is default(Failure.new); 1'`. Under `MUTSU_REAL_TEST=1`
the real module's `eval-lives-ok` really EVALs its string from inside a `try`,
and `try` turns `fatal` on implicitly — which mutsu let through into the EVAL'd
unit, where assigning an unhandled `Failure` to a variable throws. The native
provider does not take that path.

`fatal` is lexical to a compilation unit and EVAL compiles a fresh one, so
neither an explicit `use fatal` nor `try`'s implicit one reaches the snippet;
only a snippet that says `use fatal` itself is fatal. `eval_eval_string` saved
and restored `fatal_mode` (so a snippet's own pragma correctly stopped at the
boundary) but never cleared it on the way in.

**The in-code comment that argued for inheriting it was wrong, and worth
recording as a method lesson.** It cited `use fatal; try { EVAL q["bar"[5]] }`
reporting `X::OutOfRange` as proof the caller's `fatal` is live inside. Measured
both ways, that snippet reports `X::OutOfRange` **with or without** `fatal` — a
test whose outcome does not change with the variable under test is not evidence
about it. Re-deriving the whole matrix against rakudo gave the real rule in one
pass.

| file | real Test before | after | native before | after |
| --- | --- | --- | --- | --- |
| `S02-names/is_default.t` | 1 failure (#140) | **PASS** | PASS | PASS |

Fix and pin: `news/2026-08/eval-unit-does-not-inherit-fatal.md`,
`t/eval-unit-does-not-inherit-fatal.t` (14 assertions, green under real `raku`).
Verification: `make test` green (3527 files, 35246 tests); a 505-file targeted
roast sweep across `S02-names`, `S04-exception*`, `S32-exceptions`, `S06-*`,
`S12-*`, `S24-testing`, `S32-num`, `S29-context`, `S05-*` and `integration`
green on the native provider.

## 2026-08-29: a placeholder at an EVAL'd unit's mainline (`S32-exceptions/misc2.t`)

`roast/S32-exceptions/misc2.t` regressed on three `throws-like` assertions
wanting `X::Placeholder::Mainline` (`'$^x'`, `'@_'`, `'"foo".{ say $^a }'`).
mutsu raised nothing at all for `$^a` and `X::Undeclared` for `@_`.

**The check was already implemented — in only one of two parallel chains.**
`check_eval_mainline_placeholders` (`runtime/system_eval_vars.rs`) exists with
the `placeholder` attribute and rakudo's message text, and its only caller was
`runtime/test_functions/throws_like.rs` — the **native** provider's
`throws-like`, which parses its code string itself and runs its own chain of
`check_eval_*` calls. The ordinary EVAL path (`parse_and_eval_with_operators`)
runs the same chain and was missing this member, so real `EVAL` never ran it.
One line, placed ahead of `check_eval_undeclared_vars` (or `@_` is reported as
an undeclared variable instead).

**The obvious follow-up was measured and reverted.** "The native chain's copy is
now redundant, delete it" is false: the native `throws-like` never goes through
`parse_and_eval_with_operators` at all, and removing the line fails this same
file's test 14 under the *native* provider. Both chains need it; the call site
now says so and names the test that proves it. Worth carrying forward as a
shape — **a check reachable from only one of two parallel chains looks exactly
like a native-provider leniency crutch and is not one.** Measure the removal
before believing the label.

| file | real Test before | after | native before | after |
| --- | --- | --- | --- | --- |
| `S32-exceptions/misc2.t` | 3 failures (#13, #14, #15) | **PASS** | PASS | PASS |

Fix and pin: `news/2026-08/eval-mainline-placeholder-check.md`,
`t/eval-mainline-placeholder.t` (15 assertions, green under real `raku`).
Verification: `make test` green (3529 files, 35302 tests); a 483-file targeted
roast sweep across `S32-exceptions`, `S02-names`, `S02-lexical-conventions`,
`S04-exception*`, `S06-*`, `S12-*`, `S24-testing`, `S29-context`, `S05-*` and
`integration` green on the native provider.

Per the counting note above, this closes **one** named file; re-measure rather
than trusting a running total.

## 2026-08-29 (end of session): every remaining file, re-run and classified

The eight PRs that landed today (#7084, #7085, #7086, #7087, #7088, #7089, #7090, #7091) were
measured against a **session-opening sweep of 40 correctness regressions** (42 raw minus two
`exit 124` performance artifacts, and byte-identical in file set to the previous evening's, so the
baseline is reproducible rather than noisy).

Rather than quote a running total — the counting note above warns against exactly that — **all 42
files from that sweep were re-run individually on `main` @ `33cb4434e`** (release,
`MUTSU_ROAST_TIMEOUT_SCALE=2`, one at a time, both providers checked for the survivors). 13 are now
clean. What follows is every file that still fails, with its **first unmarked failing assertion** —
`# TODO`-marked lines are excluded, since the sweep predicate treats them as the expected failures
they are.

| file | first unmarked failure | note |
| --- | --- | --- |
| `6.c/S14-roles/mixin-6c.t` | method Bool in mixin is used | |
| `S02-types/WHICH.t` | ObjAt.raku gives distinct results for different objects | |
| `S03-metaops/hyper.t` | can use hypers with local scoped user-defined operators | `Unsupported reduction operator: +++` |
| `S04-phasers/end.t` | lexical lookup from END block to surrounding BEGIN block works | root-caused, see `todo/tickets/end-phaser-captured-lexical-clobbered-by-a-later-same-named-capture.md` |
| `S05-metachars/closure.t` | One matched | |
| `S05-modifier/pos.t` | Insensitive repeated continued match pos | |
| `S05-modifier/repetition-exhaustive.t` | Second entry of prev. generated `$/` | |
| `S06-multi/redispatch.t` | It's ok to call nextsame in the last/only candidate | **CLOSED** — see the section below |
| `S06-multi/subsignature.t` | It's ok to call nextsame in the last/only candidate (test 66) | **CLOSED** — same cause as the row above |
| `S06-operator-overloading/sub.t` | ... basic infix operator overloading worked | |
| `S06-other/main.t` | MAIN in a module did not get executed | |
| `S12-class/attributes.t` | HOW on attributes lives, custom class | `No such method 'x' for invocant of type 'A'` |
| `S12-coercion/coercion-methods.t` | Roles | |
| `S12-construction/autopairs.t` | class instantiation with autopair, spaces | `Unknown method ... new on Tb` — in flight |
| `S12-enums/misc.t` | did we throws-like X::Enum::NoValue? | |
| `S14-traits/routines.t` | unknown trait mentions `trait_mod:<is>` in dispatch error | |
| `S17-promise/basic.t` | subclasses create subclassed Promises | |
| `S24-testing/10-is-approx.t` | tree-arg version + optional description | real module's own spec |
| `S24-testing/2-force_todo.t` | `# You planned 12 tests, but ran 0` | native-provider-only; needs `#?rakudo eval` fudge or un-whitelisting |
| `S24-testing/3-output.t` | eval error via diag | mutsu's parse-error text is verbose and internally duplicated |
| `S24-testing/6-done_testing.t` | (no unmarked failure; exit 1) | native-provider-only, as above |
| `S32-io/io-cathandle.t` | handles method | `todo/tickets/cathandle-handles-wrongly-lazy-array.md` |
| `S32-list/skip.t` | (no unmarked failure; exit 1) | `todo/tickets/routine-value-self-recursion-after-import-scope-pop.md` |
| `S32-num/int.t` | Int.new | |
| `S32-num/rat.t` | ±Inf/NaN ⇿ Rat | |
| `S02-types/array.t` | (no unmarked failure; exit 255) | needs a second look — may be a mid-file abort |
| `S03-buf/write-int.t` | (exit 124) | **NOT correctness** — the performance class, see below |

`S32-exceptions/misc2.t` is absent because #7091 closed it after this table's build; `roast/S04-statements/return.t`
appeared in the loop with `exit 255` but re-runs clean under both providers, so it is listed nowhere —
treat a lone `exit 255` with no unmarked failure as needing a re-run before it is believed.

**Two rows share one cause** (`redispatch.t` and `subsignature.t`), and three are not interpreter
gaps at all (`S24-testing/{2-force_todo,6-done_testing}.t` are native-provider-only;
`S03-buf/write-int.t` is the timeout class). Everything else is a singleton — this residue genuinely
has no dominant cause left, which is a different situation from the earlier "one-at-a-time"
conclusions this file records having drawn and retracted three times: those were drawn from a
*classification of first-failure text*, this one from re-running every file.

### The performance blocker moved, and it is now one measurement

`todo/perf/interpreter-call-path-in-hot-loops.md` gained a dated section today that supersedes this
file's "step 3 needs the call path as well" note. The short version: the `sprintf-*` family and
`S04-declarations/state.t` **now fit** the per-file budget (state.t 61.8 s -> 15.1 s; sprintf-d.t
22.2 s -> 11.4 s, where mutsu is faster than rakudo), leaving only
`S03-buf/{write-int,read-write-bits}.t`. And the cause is sharper than "the call path": it is the
**`&`-sigil parameter**. `sub f(&c) { 1 }` called `f(&c)` costs 4.32 µs/iter and re-resolves the
callee by name on every call; `sub f($c) { 1 }` with an identical callsite, body and arity costs
0.64 µs and resolves once. Every real-`Test` assertion is the former shape.

## 2026-08-29: a `multi` is a dispatcher even with nowhere to defer to (`S06-multi/redispatch.t`, `S06-multi/subsignature.t`)

`roast/S06-multi/redispatch.t` test 9 ("It's ok to call `nextsame` in the
last/only candidate") failed under `MUTSU_REAL_TEST=1` with "nextsame is not in
the dynamic scope of a dispatcher".

Two independent gaps, both needed:

1. `push_multi_dispatch_frame` (`src/runtime/accessors_state.rs`) pushed no
   frame at all when a multi had a single candidate (`all_candidates.len() <= 1`)
   or when the winner filter emptied `remaining`. Rakudo makes being a `multi`
   the thing that establishes a dispatcher; "no next candidate" is answered
   afterwards with `Nil`, not with `X::NoDispatcher`. All four verbs
   (`nextsame`/`callsame`/`nextwith`/`callwith`) plus `lastcall`/`nextcallee`
   were re-derived against rakudo.
2. `call_function_fallback` (`src/runtime/builtins_operators_fallback.rs`)
   carries an inlined *copy* of that guard, and it is the path a routine invoked
   through a **Callable value** takes (`call_sub_value` -> `call_function` ->
   here). That copy dropped the dispatcher for EVERY multi, one candidate or
   many — so a two-candidate multi called through an `&`-parameter lost its
   frame too. Fixing only (1) left the assertion red.

Both sites now push whenever the name has multi candidates at all (empty
`remaining` when there is nothing to defer to) and push nothing when it has
none, so a plain `sub` still throws.

### Per-file before/after (release build, `scripts/run-roast-test.sh`, both providers)

| file | real Test before | real Test after | native before | native after |
| --- | --- | --- | --- | --- |
| `roast/S06-multi/redispatch.t` | 1 failure (#9) | **PASS** | PASS | PASS |
| `roast/S06-multi/subsignature.t` | 1 failure (#66) | **PASS** | PASS | PASS |

`roast/S06-multi/subsignature.t` carries the identical assertion at test 66 and
was the second whitelisted file the switch regressed. Its other two `not ok`
lines (4 "variable was modified", 43 "[+] overloaded by proto definition") are
`# TODO`-marked expected failures before and after, under both providers, and
are unrelated to this change.

Per the counting note above, this closes **two** named files
(`S06-multi/redispatch.t` and `S06-multi/subsignature.t`); re-measure the sweep
rather than trusting a running total.

Pin: `t/nextsame-in-the-only-candidate.t` (37 assertions, green under real
`raku` as well as mutsu). Full write-up:
`news/2026-08/nextsame-in-the-only-candidate.md`.

### The assertion's wording described neither the trigger nor the fix

"It's ok to call `nextsame` in the last/only candidate" is a true statement
about the spec, but "last" was never the trigger — `nextsame` in the *last of
two* candidates already worked, because two candidates meant a frame got pushed.
What actually broke it was "only" (one candidate, hence no frame) plus a second
thing the wording does not mention at all: the code was reached through a
Callable value, because the real `Test.rakumod`'s `lives-ok` invokes what it is
handed as `try { $code(); 1 }`. That misdirection is the recurring shape of this
campaign's residue: the assertion names a language feature, and the bug is in
the plumbing the real module happens to use to reach it.

### One neighbour split off rather than folded in

An anonymous `Any` parameter never matches, so `multi f(Any)` is dead code and
an `Any` fallback candidate silently disappears (`multi w(Int) { callsame }` /
`multi w(Any) { "any" }` yields `Nil` instead of `"any"`). Different root cause
— argument matching, not the dispatcher stacks: `args_match_param_types` treats
the parser's `__type_only__` placeholder as a bare *term* to resolve from the
env, and `Any` is the one type name that has an env entry (a `Value::NIL`
sentinel installed by `runtime_init.rs`). No roast file in the current residue
gates it. Filed as
`todo/tickets/anonymous-any-parameter-never-matches-in-multi-dispatch.md`.

## 2026-08-29 — `S12-construction/autopairs.t`: the file name lied twice

Slice for one whitelisted file that regressed under `MUTSU_REAL_TEST=1`.

**Read this if you are triaging another file on the residue list: the roast
file's NAME had nothing to do with the failure.** `autopairs.t` failed on
test 2, "class instantiation with autopair, spaces", and neither the autopair
(`:$a`) nor the space that distinguishes that subtest from the passing one
was involved. Nor was `Test` itself. The only thing the real module changes is
*where the snippet runs*: `eval-lives-ok` goes through `eval_exception`, a sub of
a separate compilation unit, so the `EVAL`'d `class Tb { … }` is registered under
that module's package — which rakudo does too. The bug was that every later
reference to the class's SHORT name then broke.

Root cause: `env` was the only bridge from a short type name to its
package-qualified registration, and `env` stores `$C` under the sigil-stripped
key `C` — the same key. So `my C $C` overwrote the alias that made its own type
name resolvable, and `my C $C .= new(...)` (which calls `.new` on the bareword)
died on a bare, never-registered `C` with no methods. Three sites needed the same
registry fallback: the declaration seed
(`nominal_type_object_name_for_constraint`), the block-entry hoist's stale seed
(`exec_set_var_type` now re-seeds a `Package` naming a type that exists nowhere),
and `GetBareWord`'s three "this is a type" branches plus its `Package(Any)`
placeholder guard (one env-blind `resolve_bareword_type_name` probe now).

Full write-up: `news/2026-08/package-type-short-name-vs-same-named-lexical.md`.
Pin: `t/package-type-short-name-vs-same-named-lexical.t` (35 assertions, green
under real `raku` too).

### Measured, file by file (release build, `scripts/run-roast-test.sh`, both providers)

| file | before (real) | after (real) | native |
| --- | --- | --- | --- |
| `S12-construction/autopairs.t` | 1 failure (#2 "class instantiation with autopair, spaces") | **PASS** (4/4) | still PASS (4/4) |

So: roast correctness regressions under the real provider **-1**.

### Two things worth carrying forward

**The hoist's seeding is load-bearing, even though its own doc comment says it
only registers the constraint.** The obvious fix — make `hoist_typed_var_decls`
emit a type-only op — was implemented, measured, and reverted: without the
hoisted seed, `my Int $Int` keeps the `Package(Any)` placeholder that
`SetVarDynamic` writes for every `my`, and `$Int.^name` regresses to `Any` at
plain mainline. The two mechanisms are fighting over the same env key; correcting
the stale seed at the real declaration is the version that satisfies both.

**A same-named lexical also defeats resolution for a LEXICAL class in a routine.**
`module M { sub f { class C {…}; my C $C .= new(:a(7)) } }` did not merely fail to
find the class — after the first fix round it silently built an `Any` instance
whose `.a` worked but whose `.^name`/`.WHAT`/`.raku` all said `Any`. The tell was
`.raku` reading `Any.new` while `.defined` was `True`; the cause was
`GetBareWord`'s `Package(Any)` placeholder guard testing `has_type_direct`, which
cannot see an ADR-0047-mangled qualified key. Any future work in this area should
assume "the short name resolves" and "a type object named exactly this exists"
are different questions.

### Deferred from this file

`EVAL 'my $a; role Tc { has $.a }; my Tc $c .= new(:$a)'` returns an object whose
`.raku` is `Tc.new` in mutsu and `Tc.new(a => Any)` in rakudo — a punned role
loses its attributes in `.raku` (the class form is already correct, and it
reproduces without `EVAL`). The roast assertion is only `eval-lives-ok`, so it
gates nothing here. Filed as
`todo/tickets/punned-role-raku-drops-undefined-attributes.md`.

## 2026-08-28: the routine form of a numeric-comparison operator was not the operator

Three files from the residue table above shared one root cause: the real
`Test.rakumod`'s `cmp-ok` reaches an operator only through the ROUTINE form
(`&CALLER::LEXICAL::("infix:<$op>")`), and `call_infix_routine`'s numeric
comparison handling (`==`, `!=`, `<`, `>`, `<=`, `>=`, `<=>`) folded through
the pure static `apply_reduction_op` table plus its own separate
`coerce_infix_operand_numeric` bridge — a reimplementation of the real
operator's coercion rules, missing Inf-valued Rat/FatRat, exact BigInt
equality, SetHash/Set structural comparison, and a user subclass of Int. Same
fix shape as `eqv` got in `8360b3120`: the routine form and the
`[==]`/`Z==`/`>>==<<` reduction/metaop forms now share the real operator body
(`num_eq_values` / `num_ne_values` / `num_lt_values` / `num_le_values` /
`num_gt_values` / `num_ge_values` / `spaceship_values`) with the `$a == $b`
operator opcode, instead of a separately-maintained fold. Full write-up:
`news/2026-08/infix-routine-form-numeric-comparison.md`. Pin:
`t/infix-routine-form-numeric-comparison.t` (27 assertions, green under real
`raku` too).

### Measured, file by file (release build, `scripts/run-roast-test.sh`, both providers)

| file | before (real) | after (real) | before (native) | after (native) |
| --- | --- | --- | --- | --- |
| `S32-num/int.t` | FAIL (test 118, `.new of subclass of Int`) | **PASS** (exit 0) | PASS | PASS (unchanged) |
| `S02-types/WHICH.t` | FAIL (test 1655, `ObjAt.raku gives distinct results for different objects`) | **PASS** (exit 0) | PASS | PASS (unchanged) |
| `S32-num/rat.t` | FAIL (test 749, `±Inf/NaN ⇿ Rat`, plus a later, unrelated abort) | test 749 now passes; file still aborts later (see below) | PASS | PASS (unchanged) |

`S32-num/int.t` and `S02-types/WHICH.t` are fully closed under both
providers. `S32-num/rat.t`'s named failure (test 749) is fixed, but the file
still aborts later at an unrelated, pre-existing bug: `eqv with
zero-denominator Rationals` crashes `Test.rakumod`'s `proclaim` with `Cannot
modify an immutable Str`, reproducing identically before this change (traced
to `proclaim`'s `$desc is copy` parameter not decoupling from a value that
arrives through a chain of sigilless `\`-capture aliases). Filed as
`todo/tickets/is-copy-param-not-decoupled-through-sigilless-capture-chain.md`
rather than folded into this fix — it is unrelated to numeric-comparison
dispatch.

### Two things that cost the most time here

**The redirect regressed a real behavior before it was noticed: user-defined
`multi sub infix:<==>` stopped winning over the built-in path in the
reduction/metaop form (`eval_reduction_operator_values`).** The `eqv`
precedent's redirect runs unconditionally, but `==`/`!=`/etc. already had an
existing `try_user_infix` check (gated on `value_needs_numeric_bridge`) that
a naive port of the `eqv` pattern silently dropped. Fixed by preserving that
check ahead of the new redirect. Caught by testing the scenario directly
(`class Foo { has $.v }; multi sub infix:<==>(Foo $a, Foo $b) {...}; [==]
($a, $b)`) — not in the original repro list, so always test the
"pre-existing feature this touches" axis, not only the target divergence.

**Unifying the routine and operator forms surfaced a THIRD, pre-existing bug
in the operator itself: `"1" == "1 "` (two `Str` operands) was already
`False`, not `True`.** `exec_num_eq_op`'s "same variant → compare raw values"
shortcut compared two `Str` operands' literal bytes, not their numeric value
— unnoticed because the old routine-form fold's `to_num` always numified
(and trimmed) both operands, silently masking it. `t/reduce-numeric-string-whitespace.t`
(`.unique(with => &[==])` on `("1", 1, "1 ", 2)`) caught this the moment the
routine form stopped taking that separate, accidentally-more-correct code
path. Fixed by widening the shortcut to also numify when BOTH `Str` operands
actually parse as numbers, while leaving genuinely non-numeric `Str` pairs
(mutsu's bare-string enum modeling) on the existing raw-equality path. This
is the general shape the prompt's "warnings that have repeatedly cost time"
section describes: an isolated `-e` probe of the *target* divergence looked
clean, but the *unification itself* had two more edges the target list never
mentioned.

## 2026-08-29: operator scope is lexical, not dynamic (`S06-operator-overloading/sub.t`, `S03-metaops/hyper.t`)

Two whitelisted files, one root cause. mutsu decided whether a user-declared
`sub infix:<op>` was in scope from `Interpreter::module_call_depth` — a *dynamic*
count of how many module frames the VM was inside — where Raku's rule is
*lexical*: the operator belongs to the compilation unit that declared it.

The gate itself was load-bearing (a test file's `sub infix:<+>` must not
intercept `Test.rakumod`'s own `$num_of_tests_run + 1`), but the dynamic
approximation is invisible to the native provider and wrong under the real one:
every real-`Test` assertion calls the caller's block back from inside the module
(`lives-ok` is `try { $code(); 1 }`), and that block was written in the test
file, so the test file's operators must still apply inside it. `hyper.t` is the
same rule reached through `eval-lives-ok`, where the EVAL'd unit declares the
operator *and* uses it.

`Interpreter::current_unit` now names the compilation unit currently executing,
saved/restored around every compiled-routine call, every compiled *closure* call
(a block carries the unit it was written in) and every `EVAL`;
`user_declared_infix_ops` maps each operator name to the units that declared it,
with an empty set (module exports) meaning "visible everywhere"; and
`note_eval_unit_parent` records each EVAL unit's parent so an operator from the
enclosing unit stays in scope inside the EVAL.

**Worth carrying forward: `$?FILE` does NOT answer this question.** The env
entry tracks the unit being *loaded*, so inside a module routine invoked at
runtime it still names the main script. A first attempt that read it passed both
roast files while silently deleting the original protection — the module's own
arithmetic resolved to the caller's candidate again. The pin asserts both
directions.

| file | real Test before | after | native before | after |
| --- | --- | --- | --- | --- |
| `S06-operator-overloading/sub.t` | 1 failure (#13) | **PASS** | PASS | PASS |
| `S03-metaops/hyper.t` | 1 failure (#347) | **PASS** | PASS | PASS |

Fix and pin: `news/2026-08/operator-scope-is-lexical-not-dynamic.md`,
`t/operator-scope-is-lexical-not-dynamic.t` (13 assertions, green under real
`raku`) plus `t/lib/OperatorScopeRunner.rakumod`.

Per the counting note above, this closes **two** named files; re-measure the
sweep rather than trusting a running total.

### Session-opening sweep for the record

`scripts/roast-test-module-sweep.sh` on `main` @ `1d698c171` (release, 1436
whitelisted files, both providers): **28 raw regressions, minus 3 `exit 124`
performance artifacts (`6.d/S32-str/sprintf-d.t`, `S32-str/sprintf-d.t`,
`S03-buf/write-int.t`) = 25 correctness regressions.** `pass under both` 1408,
`fail under both` 0. Detail preserved at
`tmp/real-test-regressions-2026-08-28-r152.txt`.

Two files on that list carry `exit 255` with no unmarked failing assertion and
were re-run individually rather than believed: `S04-statements/return.t` aborts
because `X::ControlFlow::Return` is raised as a bare error string instead of a
typed exception carrying `out-of-dynamic-scope`, and `S02-types/array.t` aborts
because `lives-ok { my $s = (gather die)[] }` reifies the `gather` that a zen
slice must not touch. `S32-exceptions/misc2.t` is back on the list for a new
reason (`X::Syntax::Pod::BeginWithoutIdentifier` has no `.filename`, which the
real `throws-like` calls).

## 2026-08-29 — three exception-object/incidental-VM-bug files: misc2.t, S12-enums/misc.t, S14-traits/routines.t

Cluster of three files whose regressions all *looked* like the same shape
(the real `Test.rakumod`'s `throws-like` interrogating the caught exception
through real Raku method calls, which mutsu's native provider never
exercises) — verified independently per file rather than assumed. Two of
the three turned out to be genuinely unrelated VM bugs, not exception-object
gaps at all; only `misc2.t`'s was the exception-attribute shape the cluster
was named for.

**`roast/S32-exceptions/misc2.t`** — `X::Syntax::Pod::BeginWithoutIdentifier`
had no `.filename` method at all (only the looser `.file`), and its `.line`
was never populated because its builder (`PError::fatal_with_exception`)
never recorded a source position for `parser::parse_program`'s fatal branch
to compute one from. Fixed generally (a new `.filename` accessor alongside
`.line`/`.file`; `parse_program`'s fatal branch now copies computed
`line`/`column` onto any pre-built exception; `builtin_eval` backfills
`filename`/`file` for the whole `X::Comp` family raised while parsing an
EVAL'd string, not just this one class). Write-up:
`news/2026-08/eval-compile-error-filename-and-line.md`. Pin:
`t/eval-compile-error-line-and-filename.t` (8 assertions, green under `raku`).

Fixing that abort exposed a SEPARATE, unrelated bug further into the same
file (`throws-like 'my sub f() { gather { return } }; ~f()'`,
X::ControlFlow::Return`): a `return`-outside-scope raised while forcing a
lazy `gather` is swallowed (or misreported) once the force happens inside a
nested block or a Callable invoked through a plain user sub — a general VM
control-flow bug, not Test-specific (reproduces with a two-line user sub, no
`use Test` at all). Too deep to fix in this slice; filed as
`todo/deep/lazy-gather-return-outside-scope-swallowed-in-nested-block.md`.
misc2.t is therefore not fully green under `MUTSU_REAL_TEST=1` yet, but the
abort point moved from test ~94 to this new, unrelated bug around test ~220.

**`roast/S12-enums/misc.t`** — NOT an exception-object gap. Two unrelated,
general VM correctness bugs, both surfaced by
`throws-like { Direction( 2 <=> 3 ) }, X::Enum::NoValue, type => Direction,
value => Less`:
1. A for-loop's second `.kv` param rebind (`OpCode::SetGlobal`) could
   spuriously raise `X::Assignment::RO` when a PRIOR iteration's value
   happened to be an enum member — the "is this reassigning an enum
   constant" guard checked only the current value's *type*, never that the
   write target's *name* was the constant's own name.
2. An enum's type object did not smartmatch itself (`Color ~~ Color` was
   `False`) — the enum-specific smartmatch arm only considered an enum
   *value* on the LHS, never the type object compared to itself, breaking
   any `$x ~~ Color` where `$x` held the type object (exactly what
   `X::Enum::NoValue.type` is).

Both fixed; write-up `news/2026-08/enum-value-rebind-and-self-smartmatch.md`.
Pins: `t/enum-value-does-not-block-unrelated-rebind.t`,
`t/enum-type-object-smartmatches-itself.t` (both green under `raku`).

**`roast/S14-traits/routines.t`** — NOT an exception-object gap either. A
routine-trait application (`sub f() is TRAIT { }`) silently swallowed the
"no `trait_mod:<is>` candidate actually claims this trait" verdict
unconditionally, instead of raising "Can't use unknown trait" the way the
sibling *variable*-trait path already does (a gap that 2026-08-01's
`f58c424c6` explicitly called out as unfixed for routines). Merely
`use Test;` — which exports `multi sub trait_mod:<is>(Routine:D,
:$test-assertion!)` — was enough to make ANY unrelated unknown routine
trait silently succeed instead of dying, so `try { EVAL 'sub yulia is
krassivaya { }' }` never set `$!`. Fixed to raise the same message the
sibling `!has_trait_mod` branch already uses. Write-up:
`news/2026-08/unrelated-trait-mod-does-not-swallow-unknown-routine-trait.md`.
Pin: `t/unrelated-trait-mod-candidate-does-not-swallow-unknown-trait.t`
(green under `raku`).

### Measured, file by file (release build, `scripts/run-roast-test.sh`, both providers)

| file | before (real) | after (real) | native |
| --- | --- | --- | --- |
| `S32-exceptions/misc2.t` | aborts mid-file (test ~94, "No such method 'filename'") | improved, still aborts (unrelated `lazy-gather-return` bug, test ~220) | still PASS (266/266) |
| `S12-enums/misc.t` | aborts mid-file ("Cannot modify an immutable Order (Less)", desyncs the rest of the file) | **PASS** (28/28) | still PASS (28/28) |
| `S14-traits/routines.t` | 1 failure (test 12, "declaration of a sub with an unknown trait...") | **PASS** (17/17, test 10 stays `#?rakudo todo`) | still PASS (17/17) |

So: `S12-enums/misc.t` and `S14-traits/routines.t` are fully closed under
`MUTSU_REAL_TEST=1`; `S32-exceptions/misc2.t`'s original blocker (the
subject of this slice) is fixed, but the file is blocked by the newly
surfaced `lazy-gather-return-outside-scope-swallowed-in-nested-block`
ticket instead. All three remain green under the native (whitelisted)
provider throughout.

### One thing worth carrying forward

**Don't trust the cluster framing over independent verification.** All
three files were handed over as "one cluster of the same shape (incomplete
exception objects)" — and two of the three were not that at all. The
diagnostic symptom (`throws-like` failing a `.foo matches ...` check, or
"Use of uninitialized value ... in string context") looks identical whether
the exception object is genuinely incomplete or whether an unrelated VM bug
(a stale for-loop rebind, a broken self-smartmatch, a swallowed dispatch
verdict) merely prevented the RIGHT value from ever being computed. Re-derive
the root cause from a from-scratch, `use Test`-free repro every time, even
when the roast test's own `throws-like` line makes the exception-object
explanation look obvious.

## 2026-08-28 — three type-identity divergences, all reproduce without `Test`

Three files, all closed. Unusually for this campaign, each repro reduced to a
plain one-liner with no `Test` involved — real language bugs the real
module's `isa-ok`/`is` happened to gate. All three touch "what type is this
value really" machinery (a `Mixin`-wrapped `Package`/`Instance` not being
recognized where a bare one was, and `Promise`'s hardcoded type name), so
they turned out to share more plumbing than expected, though each has a
distinct root cause.

**1. A punned role's instance did not `isa`/smartmatch its pun.** `R.^pun`
(`Mixin(Package(role), overrides)`, ADR-0060) was invisible to three
argument-side type-name extractors that enumerated `Package`/`Str`/`Instance`
but not `Mixin`: `Value::isa` (`methods_mixin_dispatch.rs`), smartmatch
(`seq_helpers/smart_match.rs`), and `nqp::istype` (`nqp_ops.rs`). The last one
mattered most for the roast regression: the real `Test.rakumod`'s `isa-ok`
calls `nqp::istype($var, $type.WHAT)` for a non-`Str` expected type, and a
pun's own `.WHAT` is *also* a `Mixin`. Fixed all three to unwrap a `Mixin`
argument to its inner `Package`/`Instance` name — for `isa`, only when the
argument is NOT a bare `Package` (the literal role stays excluded from
nominal isa checks, unchanged); for smartmatch, only when `left` is not
itself the bare role Package the pun was generated from (`R ~~ R.^pun` is
correctly `False` even though `R.^pun ~~ R` and `$o ~~ R.^pun` are both
`True` — asymmetric, like ordinary isa). Full write-up:
`news/2026-08/punned-role-isa-and-smartmatch.md`. Pin:
`t/role-pun-isa-smartmatch.t` (green under `raku`).

**2. `Promise` factory methods ignored the invocant subclass.**
`.start`/`.in`/`.at`/`.anyof`/`.allof`/`.then` on `class Meows is Promise {}`
built a plain `Promise`, and even after that was fixed, the subclass name
reaching `.^name` carried a raw ADR-0047 lexical-mangling suffix (visible as
a stray embedded NUL). Fixed by threading the (still internally mangled, for
consistency with `Instance`) class name through `promise_class_name`
(`methods_collection_ops/socket_inet_proc.rs`) and stripping it only at
display time in `dispatch_caret_name`'s `Promise` arm
(`methods_introspect.rs`), mirroring the `Instance`/`Package` arms beside it.
A third, independent bug surfaced once the first two were fixed:
`Interpreter::dispatch_mro`'s `Promise` fallback (`receiver_class.rs`) used
`value_type_name`, hardcoded to the literal `"Promise"` for every Promise
value — so `nqp::istype($meows_promise, Meows)` (again, what the real
`isa-ok` actually calls) stayed `False` even after `.isa(Meows)` (a separate
code path) agreed with `.^name`. Added a dedicated `Promise` arm routing
through the existing `class_chain` registry-MRO mechanism. Full write-up:
`news/2026-08/promise-subclass-factory-methods.md`. Pin:
`t/promise-subclass-factory-methods.t` (green under `raku`).

**3. A mixin on a type object was lost by `.gist`/`.raku`, and by `:U`
parameter binding.** `.^name` on `Any but role Meows {...}` already composed
correctly ("Any+{Meows}"), but `.gist`/`.raku` delegated straight to the bare
inner `Package`, dropping the mixin ("(Any)") — the fast-path mixin method
dispatch (`builtins/methods_0arg/mod.rs`) had a Set/Bag/Mix-inner special
case for this but none for a plain type-object `Package` inner. Fixing that
alone did not close the roast file under `MUTSU_REAL_TEST=1`: the real
`Test.rakumod`'s `is(Mu $got, Mu:U $expected, ...)` multi (selected because
the operand is undefined) could not even *bind* a mixin type object to its
`Mu:U` parameter, because `value_is_defined` (`runtime/types/mod.rs`) had no
`Mixin` arm and treated every mixin — instance or type object alike — as
defined. Added `ValueView::Mixin(inner, _) => value_is_defined(inner)`,
mirroring the existing `ContainerRef` arm. This second bug is general (a
plain `sub f(Mu:U $x) {...}; f(Any but role {...})` failed to bind, nothing
Test-specific), so it is a distinct root cause from the `.gist` fix, not a
symptom of it. Full write-up:
`news/2026-08/mixin-type-object-gist-and-definedness.md`. Pins:
`t/mixin-type-object-gist.t`, `t/mixin-type-object-definedness.t` (both
green under `raku`).

**A regression caught by an existing pin, not roast**: the first
`smart_match.rs` fix above initially recursed unconditionally on any `Mixin`
RHS carrying a `__mutsu_role__*` marker, which flipped
`t/role-pun-metamethod-identity.t` test 8 (`nok R ~~ R.^pun`, "the role
itself does not smartmatch its own pun") to a false `True` — confirmed with
`raku t/role-pun-metamethod-identity.t` (13/13 pass on `raku`, so the
assertion was correct and the interpreter regressed). The asymmetry guard
described above fixes it; all 13 assertions in that file pass again.

### Measured, file by file (release build, `scripts/run-roast-test.sh`, both providers)

| file | before (native) | after (native) | before (real) | after (real) |
| --- | --- | --- | --- | --- |
| `S12-coercion/coercion-methods.t` | PASS | PASS | 2 failures (Roles subtest, tests 2 &amp; 5) | **PASS** |
| `S17-promise/basic.t` | PASS | PASS | 7 failures (subclasses subtest, all 7) | **PASS** |
| `6.c/S14-roles/mixin-6c.t` | PASS | PASS | 2 failures (tests 48-49) | **PASS** |

So: roast correctness regressions under the real provider closed for these
three files; native-provider behavior unaffected (still PASS, and the
`role-pun-metamethod-identity.t`/`promise-subclass-factory-methods.t`/
`mixin-type-object-*.t` local pins cover the underlying language bugs
directly, independent of either `Test` provider).

## 2026-08-29 — lazy-gather-forced `return`/`die` signal delivery: misc2.t closed, array.t closed

Closes out the `lazy-gather-return-outside-scope-swallowed-in-nested-block`
ticket the previous slice filed (`news/2026-08/lazy-gather-return-outside-
scope-resolution.md` has the full writeup; `news/2026-08/gather-lazy-force-
signal-delivery.md` and `news/2026-08/itemized-scalar-sink-does-not-force-
lazy-gather.md` have the two independent root-cause fixes). Two unrelated
bugs shared one observable symptom: a control-flow signal (a `return`, or a
plain `die`) raised while forcing a lazy `gather`/`Seq` was either delivered
to the wrong place or never delivered at all.

1. **False-positive forcing**: mutsu's sink-context forcing ran an
   as-yet-untouched lazy value for side effects even when it had already
   been assigned to a plain scalar (itemized) — raku never forces an
   itemized value merely because it is later discarded, only a genuinely
   bare one. Fixed with a persistent `itemized` flag on both `SeqBody` and
   `LazyList`, set at scalar-assignment time (`vm_var_assign_set_local.rs`)
   and checked by every sink-forcing site.
2. **Missed delivery**: `try`/CATCH unconditionally let ANY `return` control
   signal propagate past itself (correct for a still-live return, wrong for
   one whose target routine had already exited — that one can never be
   caught by unwinding further and must convert to a catchable
   `X::ControlFlow::Return` right there). Separately, a `return` executed
   while FORCING a `gather` never had its target callable id resolved at
   all, so it fell back to "the first enclosing routine call frame catches
   it unconditionally" and was silently absorbed by an unrelated caller
   (exactly the shape the real `Test.rakumod`'s `subtest(&subtests) {
   subtests(); CATCH {...} }` hits). Fixed with a general liveness check
   (`Interpreter::return_target_is_live`, generalizing ADR-0037 §2.3's EVAL-
   context classification to ordinary closures) plus resolving the gather's
   own captured `__mutsu_callable_id` at both of mutsu's gather-forcing
   entry points.

### Measured, file by file (release build, `scripts/run-roast-test.sh`, both providers)

| file | before (real) | after (real) | native |
| --- | --- | --- | --- |
| `S32-exceptions/misc2.t` | aborts mid-file (unrelated `lazy-gather-return` bug, test ~220, "You planned N tests, but ran M") | **PASS** (266/266) | still PASS (266/266) |
| `S02-types/array.t` | `zen and whatever slices` subtest dies where raku lives (`Died` escaping `lives-ok` itself, "You planned 2 tests, but ran 0") | **PASS** (108/108) | still PASS (108/108) |
| `S04-statements/return.t` | not independently blocked by this bug (`X::ControlFlow::Return`'s `.out-of-dynamic-scope` shape was already correct) | still **PASS** (26/26) | still PASS (26/26) |

Pins: `t/itemized-scalar-sink-does-not-force-lazy.t` (8 assertions),
`t/return-target-dead-reaches-nearest-catch.t` (9 assertions) — both green
under `raku` too.

A third, narrower, adjacent gap surfaced while writing the regression
tests — an explicit `.sink()` METHOD call on a gather-based lazy list never
runs the body at all (distinct from the delivery bug above: here forcing
never happens, rather than happening and being misdelivered) — filed
separately as `todo/tickets/lazylist-sink-method-does-not-force-gather-body.md`
rather than folded into this fix, since it is not required by any
currently-tracked roast test.
