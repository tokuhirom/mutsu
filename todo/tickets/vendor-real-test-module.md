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

1. Implement the 9 ops, each with a `t/nqp-*.t` pin. They are independently
   useful — `getstdout`/`getstderr`/`eqaddr`/`can` show up in other real dists.
2. Vendor `Test.rakumod` verbatim to `modules/Rakudo-Core/lib/Test.rakumod` but
   **do not** remove the interception yet; exercise it under a temporary alias
   against a representative sample of `t/` and roast.
3. Only then flip `runtime_module.rs`, and expect the first full `make roast` to
   be the real review.
4. `Test::Util` (roast's helper, `roast/packages/Test-Helpers/`) is a separate
   thing and already loaded from source — check it still composes.

Do not start this in the same PR as anything else.

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
