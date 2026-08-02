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
| individual gaps | `bigrat-sort-compare.t` (`cmp-ok` calls `infix:«<»` as a *routine value*; FatRat vs Num answers differently there than the compiled operator), `proxy-list-transparency.t` (`is-deeply` does not FETCH `Proxy` list elements — reports `$(Proxy, Proxy)`), `emit-done-controlflow.t`, `error-reporting-quality.t`, `group-of.t` (`is-deeply` reports "planned 2 tests, but ran 0" inside a subtest), `io-cathandle-lazy.t` (**aborts with a stack overflow** under the real module: `.cache` on a lazy Seq still answers `Seq`, so `is-deeply`'s Seq-narrowing candidate re-dispatches to itself forever — `todo/deep/cache-on-a-lazy-seq-must-not-answer-seq.md`), `subscript-adverbs.t` (**not a `Test` difference and not even about the closure**: `(@a[0]:p).value = 'x'` builds a *snapshot* Pair, so the write has to find the array by scanning `self.env` — and the file's own later `{ my @a = … }` block flips the first block from `PushBlockFrame` to `BlockScope`, which puts `@a` in a local slot where the scan cannot see it. `todo/deep/subscript-p-pair-is-a-snapshot-not-a-container.md`), `throws-like-gather-sink.t` (+ part of `emit-done-controlflow.t`: `todo/deep/eval-context-frame-owns-the-return-target.md`), `whatever-code-fixes.t` | one at a time |

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
