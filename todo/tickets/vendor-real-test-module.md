# Vendor rakudo's real `Test` module — measured as reachable, 9 thin `nqp::` ops away

mutsu provides `Test` natively (`src/runtime/test_functions.rs`); `use Test` is
intercepted in `runtime_module.rs`. Of the native providers surveyed on
2026-08-01 (`docs/batteries/pod-to-text.md`), `Test` is the one that is actually
within reach — unlike `NativeCall`
(`todo/deep/nativecall-cannot-be-vendored.md`), which is not.

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
  Recorded in `todo/deep/interpreter-call-path-in-hot-loops.md`; that is the
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
`X::Syntax::Comment::Embedded`, `X::Syntax::Signature::InvocantNotAllowed`,
`X::Comp::Group`, `X::Worry::Precedence::Range`, `X::Syntax::Malformed`,
...) remain open, each needing its own individual parser diagnosis.
