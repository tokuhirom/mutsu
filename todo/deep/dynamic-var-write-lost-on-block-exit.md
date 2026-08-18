# A dynamic-variable write from inside a bare block reverts on exit — fixing it correctly needs 3 separate mechanisms, one of which interacts with concurrency

Originally filed as a narrower "a `LEAVE`-driven `PROCESS::` write is lost
specifically inside `for`/`while` loop bodies" ticket while investigating
`Log::Timeline`. A first attempt (PR #6647) found and fixed the real,
general bug in the synchronous case, but CI (`gc-stress`/`jit-stress`)
caught a deterministic regression in `roast/S32-io/indir.t` that traces to
a THIRD, separate restoration mechanism this ticket's simple fix does not
cover — reverted before merge; recorded here as a deep finding instead.

## The bug (synchronous case, still real and still unfixed)

```raku
my $*x = 1;
{ $*x = 99; }
say $*x;   # raku: 99   mutsu: 1
```

A plain reassignment of an EXISTING dynamic variable (`$*x = ...`, or a
`PROCESS::<$x> = ...` pseudo-stash write, which stores through the identical
`*x`-prefixed env key) made from inside ANY bare block — not just a `LEAVE`
phaser, and not just inside a loop — is lost once that block exits. Real
Raku only scopes a *fresh redeclaration* (`my $*x = ...`) to the block; a
plain write-through mutates the existing container and stays visible after
the block exits.

## Where the (incomplete) fix landed and why it broke a roast test

There are (at least) THREE independent places that decide whether a
`*`-prefixed name propagates out of a block/closure, and the synchronous-only
fix touched only the first:

1. **`exec_block_scope_op`'s env-restore loop** (`src/vm/vm_misc_scope.rs`,
   ~line 562): had an unconditional `if k.starts_with("*") { continue; }`
   before the `block_declared`-based ownership check every other variable
   already uses. Fixing this alone (dropping the blanket check, letting
   `*`-prefixed keys fall through to the same `block_declared` check)
   correctly fixes the synchronous bare-block/if/given/for/while-LEAVE cases
   verified in isolation.
2. **`exec_block_scope_op`'s LOCAL-SLOT restore loop** (same file, two more
   sites: the `MUTSU_NO_SHADOW_SLOTS` opt-out branch ~line 632, and the
   default "shadow slots" branch ~line 660) — a SEPARATE restoration pass for
   the fast-path local-variable slots (distinct from the env dict), with the
   IDENTICAL blanket `name.starts_with('*')` bug. Missed in the first PR;
   found and fixed in a follow-up attempt on the same branch, but did NOT
   resolve the CI failure (confirmed empirically — see below).
3. **Closure exit-writeback** (`src/vm/vm_closure_dispatch.rs`) — NOT yet
   located precisely. A closure passed as a first-class value (e.g. a `.map`
   block, a `start { ... }` block body) does not go through
   `OpCode::BlockScope`/`exec_block_scope_op` at all when it has no
   ENTER/LEAVE/KEEP/UNDO phasers (which `.map`/`start` bodies typically
   don't) — it goes through `call_compiled_closure_with_topic`'s own
   `scoped_child`-overlay entry/exit machinery instead. That file's entry
   merge (~line 310-327) already has special-casing for dynamic vars
   (`ContainerRef`-captured `*`-prefixed keys use `entry_or_insert_sym`,
   don't-overwrite, specifically to let a *live* dynamic binding win over a
   stale captured cell — see that code's own comment, which cites an
   `indir.t` bug this exact investigation is adjacent to). The EXIT side of
   that same mechanism was not examined; whatever it does with a
   `*`-prefixed write made during the closure body's execution is where the
   still-unexplained leak below originates.

## The regression that blocked the fix: a `my $*CWD` inside a `.map`/`start` closure leaks to an undeclared outer scope

```raku
my $correct-CWD = "/tmp".IO;
my int $failures;
$failures += [+] await flat ^200 .map: {
    my $*CWD = $_;
    my $prom = start indir :!d, $correct-CWD, {
        my $res = $*CWD !~~ $correct-CWD; $*CWD = 42; $res
    }
    $failures++ unless $*CWD eq $_;
    $prom
}
say "CWD after: ", $*CWD;   # raku/mutsu on main (blanket-rule): dies "not declared" (correct: $*CWD was never
                             # declared in this outer scope) -- mutsu with BOTH fixes above: prints "199"
```

With both the env-loop AND local-slot fixes applied, the LAST iteration's
`my $*CWD = $_` (which should be strictly local to that `.map` closure
invocation, never visible outside it — confirmed via `block_declared`/local
`my`-declaration semantics) leaks into the enclosing mainline scope, which
never declared `$*CWD` at all. This is a **different failure mode** from
the original bug (over-eager reversion) — it's now a MISSING reversion for
a genuinely fresh `my`-declared dynamic inside a closure, specifically when
combined with `.map`/`start` (concurrent task spawning). Reduced from
`roast/S32-io/indir.t`'s test 76 (`indir sets $*CWD to absoluted path`),
which is preceded by exactly this `.map`/`start`/`indir` shape (lines
160-171 of that file) and fails deterministically (6/6 local `-j4` release
runs) once the earlier subtest's leaked/corrupted dynamic-var state poisons
the later one.

## Why this is deep, not a quick follow-up

- Needs the closure exit-writeback mechanism in `vm_closure_dispatch.rs`
  precisely located and understood (not yet done) before a fix can be
  designed, let alone applied.
- The bug only manifests through `start` (task/thread spawning) combined
  with a `.map`-closure-scoped `my $*x` redeclaration — this codebase's own
  conventions flag the `start`/dynamic-var/env interaction as a known
  fragile area (see `shared_vars_active`, per-task registry clones, and the
  `native-array-push-after-a-start` history in
  `todo/tickets/digest-ripemd-start-per-block-overhead.md`'s own notes and
  `news/2026-08/native-array-push-after-a-start.md`).
- A correct fix must reconcile THREE independent restoration mechanisms
  (env-loop, local-slot-loop, closure-exit-writeback) to agree on the exact
  same `block_declared`-style "was this genuinely redeclared here" test,
  without breaking the existing `indir.t` `ContainerRef`-captured-dynamic
  special case in `vm_closure_dispatch.rs`'s entry merge (whose own comment
  already documents fixing a *previous* `indir`/dynamic-var bug — this area
  has been touched by bugfixes before and is evidently still not fully
  sound).
- High verification burden: any change here needs the full `t/`/roast sweep
  plus specific attention to `t/*dynamic*.t`, `t/*process*.t`,
  `t/*start*.t`, `t/*thread*.t`, and `roast/S32-io/indir.t` under `-j`
  parallel release builds (debug-build/single-run testing did NOT catch this
  regression — only a release build under load did).

## Repro (regression, once a full fix is attempted)

```sh
cargo build --release
timeout 15 target/release/mutsu -e '
my $correct-CWD = "/tmp".IO;
my int $failures;
$failures += [+] await flat ^200 .map: {
    my $*CWD = $_;
    my $prom = start indir :!d, $correct-CWD, {
        my $res = $*CWD !~~ $correct-CWD; $*CWD = 42; $res
    }
    $failures++ unless $*CWD eq $_;
    $prom
}
say "CWD after: ", $*CWD;
'
# Expect a "not declared" die (or equivalent -- $*CWD was never declared at
# this scope); "199" (or any leaked value) means the closure-exit leak is
# still present.
```

And the original synchronous bug (still open, still worth a narrow fix on
its own once the above is understood, OR together with it):

```sh
timeout 10 target/debug/mutsu -e 'my $*x = 1; { $*x = 99; }; say $*x;'
# raku: 99   mutsu: 1
```
