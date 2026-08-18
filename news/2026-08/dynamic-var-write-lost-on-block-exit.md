# Dynamic-variable writes no longer revert on block exit — and `my $*x` in a `.map` block no longer leaks out

A plain reassignment of an EXISTING dynamic variable (`$*x = ...`, or a
`PROCESS::<$x> = ...` pseudo-stash write, which stores through the identical
`*x`-prefixed env key) made from inside any bare block was lost once that
block exited:

```raku
my $*x = 1;
{ $*x = 99; }
say $*x;   # raku: 99   mutsu (before): 1
```

Real Raku only scopes a *fresh redeclaration* (`my $*x = ...`) to the block;
a plain write-through mutates the existing container and stays visible after
the block exits. Originally filed as a narrower "a `LEAVE`-driven `PROCESS::`
write is lost inside `for`/`while` loop bodies" ticket while investigating
`Log::Timeline`.

## Why the first fix (PR #6647) had to be reverted

THREE independent mechanisms decide whether a `*`-prefixed name propagates
out of a block/closure, and they disagreed:

1. **`exec_block_scope_op`'s env-restore loop** (`src/vm/vm_misc_scope.rs`)
   had an unconditional `if k.starts_with("*") { continue; }` — every
   dynamic key blanket-reverted to its block-entry value, bypassing the
   precise `block_declared` ownership check every other variable uses.
2. **The same function's local-slot restore loops** (both the shadow-slots
   default branch and the `MUTSU_NO_SHADOW_SLOTS` opt-out) had the identical
   blanket `name.starts_with('*')` rule.
3. **The inline `.map`/`.grep` loop** (`eval_map_over_items` /
   `eval_grep_over_items` and their rw twins, `src/runtime/resolution_map_grep.rs`)
   runs the block body via `run_reuse` **in the caller's own env** — no
   closure frame at all — and isolates block-locals only through a
   `touched_keys` save/restore list built from `CompiledCode::my_declared_sym`,
   which the compiler deliberately populates **only for plain `my`** (its
   other consumers, e.g. ADR-0024 free-var slot binding, depend on that).
   A `my $*CWD = $_` inside a `.map` block was therefore never saved or
   restored: the LAST iteration's fresh redeclaration simply stayed in the
   caller's env after the loop.

Mechanism 3 was the pre-existing, hidden bug — `^3 .map: { my $*CWD = $_ }`
leaked `2` into the mainline's `$*CWD` even on unfixed main — but the
blanket rules in mechanisms 1/2 *masked* it: the next bare-block exit
anywhere downstream blanket-reverted the leaked key back to a saved value,
"self-healing" the leak before most tests could observe it. Fixing 1/2 alone
(the reverted PR #6647) removed the mask, and `roast/S32-io/indir.t` test 76
(preceded by exactly this `.map`/`start`/`my $*CWD` shape) began failing
deterministically under a release `-j4` run.

A subtlety confirmed with `rust-gdb` along the way: every dynamic write also
maintains a sigil-prefixed **twin env key** (`set_env_with_main_alias_sym`'s
`twigil_dynamic_alias` mirror stores `$*x` alongside `*x`, and a `*x` read
falls back to `$*x`), so restoring only the bare key still left the leaked
value readable through the twin.

## The fix

All three mechanisms now agree on one ownership test — "was this name
genuinely `my`-redeclared in THIS scope":

- Mechanisms 1 and 2 drop the blanket `*`-prefix rules; dynamic keys fall
  through to the same `block_declared` runtime check as every other name
  (`my $*x` executes `SetLocalDecl`, which records the name there).
- A new `CompiledCode::dynamic_declared_sym` set records `my $*x`
  redeclarations at compile time — **both** env spellings (`*x` and the
  `$*x` twin) — separately from `my_declared_sym` so its documented
  plain-`my`-only invariant is preserved. Consumers:
  - `push_block_declared_keys` (all four inline map/grep loops) adds the
    set to `touched_keys`, so a block-local `my $*x` is saved/restored
    around the loop. No `free_var_syms` exemption: dynamic reads compile to
    by-name `GetGlobal` (no local slot), so a declared-and-read dynamic
    always registers as a free var and the exemption would defeat the
    restore.
  - `call_compiled_closure_with_topic`'s caller-writeback scan
    (`src/vm/vm_closure_dispatch.rs`) and `call_sub_value`'s
    `is_body_private` (`src/runtime/resolution_call_sub.rs`) skip the set,
    so the native-closure and interpreter call paths agree.

A plain `$*x = ...` write-through is not a declaration, never lands in any
of these sets, and now propagates out of blocks, closures, and map/grep
loops alike — matching raku (all shapes raku-verified, including that the
outer `$*CWD` after the indir.t repro is the *untouched process CWD*, not a
leaked iteration value).

## Verification

- `t/dynamic-var-write-through-block-persists.t` (12 assertions, every one
  verified against real raku): PROCESS:: writes from bare blocks and
  for/while `LEAVE` phasers, plain `$*x` block write-through, `my $*x`
  redeclaration visibility + reversion, the three `.map` shapes
  (redeclaration no-leak, write-through propagation, never-declared name
  unresolvable after the map), and the reduced `.map`/`start`/`indir`
  concurrency repro from roast S32-io/indir.t.
- `roast/S32-io/indir.t` 5/5 PASS on a release build under `prove -j4` (the
  exact configuration that caught the PR #6647 regression), plus green runs
  of the dynamic/name-scope and map/grep/start/supply roast clusters and the
  full `t/*dynamic*.t` / `t/*process*.t` / `t/*start*.t` / `t/*thread*.t`
  sweep (64 files, 439 tests).
