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
   **do not** remove the interception yet; exercise it under a temporary alias
   against a representative sample of `t/` and roast. **IN PROGRESS** — the
   alias exercise has been run by hand (see "Where the alias stands" below) and
   found four general interpreter bugs, all fixed, plus two that are not: an
   unimplemented `&CALLER::LEXICAL::("infix:<…>")`
   (`todo/tickets/caller-lexical-indirect-operator-lookup.md`) and a wrong
   `$?FILE`/`callframe` inside a module
   (`todo/tickets/file-var-and-callframe-inside-a-module.md`). Those two are
   what remains before the file is worth vendoring.
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
counter surviving them. Four general bugs were found and fixed getting there:

| what | fix |
| --- | --- |
| the `nqp::` ops it needs, and bare `nqp::time` in term position | `news/2026-08/nqp-process-ops-for-the-real-test-module.md` |
| mutsu's native Test provider overruling the module's own routines | `news/2026-08/imported-test-routines-beat-the-native-provider.md` |
| `proclaim !($got ~~ $rx), $desc` losing its argument list (forward-declared sub, prefix-`!` argument) | `news/2026-08/listop-argument-may-start-with-a-boolean-prefix.md` |
| `@vars.push: item [...]` dropping the array, so every `subtest` restored garbage | `news/2026-08/item-is-a-listop.md` |

Two are left, each with its own ticket:

- `cmp-ok` needs `&CALLER::LEXICAL::("infix:<$op>")` —
  `todo/tickets/caller-lexical-indirect-operator-lookup.md`. It is the only
  assertion still blocked.
- A **failing** test dies in `proclaim`'s location report, because `$?FILE`
  inside a module is the main script and `callframe` skips the module's frames
  — `todo/tickets/file-var-and-callframe-inside-a-module.md`. Every passing
  assertion is fine; the first failure raises
  `No such method 'file' for invocant of type 'Any'`. This one is
  load-bearing: a test framework that cannot report a failure is not usable.

Neither is Test-specific, so both are worth fixing on their own terms.

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
