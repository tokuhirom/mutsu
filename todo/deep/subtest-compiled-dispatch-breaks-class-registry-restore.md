# Compiled-first `subtest` dispatch breaks class/registry lifecycle for async bodies

## Summary

PR #6499 attempted to speed up `subtest { ... }` by dispatching the block
through the VM's compiled closure path (`vm_call_on_value` ->
`call_compiled_closure`) instead of the AST carrier
(`call_sub_value` -> `eval_block_value` -> a fresh `Compiler::compile()`
call on every invocation). The AST carrier's per-call recompile is real
waste (see the now-deleted `news/2026-08/subtest-compiled-first-dispatch.md`
for the original measurement), but the compiled-first dispatch broke a real
Cro::HTTP behavior and was reverted (`src/runtime/test_functions/tap_subtest.rs`,
`subtest_call_block` now unconditionally calls `call_sub_value` again).

## The CI-blocking regression

`scripts/battery-testsuite.sh`'s bundled-library gate caught a genuine
regression in the vendored `Cro::HTTP` test suite:
`t/http-middleware.rakutest` — `before-parse`/`after-serialize` byte-level
transform middleware silently became no-ops (expected `'HELLO WORLD'`, got
`'hello WORLD'` — the `subst('hello','HELLO')` never ran) and several other
middleware-interaction subtests in the same file also failed. Confirmed via
a clean A/B (`cargo build --release` on `origin/main` in a separate
worktree, and reverting only this file's dispatch decision locally): the
failure reproduces identically on `main` at PR #6499's own commit
(`944736d11`) — it is not caused by any other change in that PR or by later
fix-forward work in the same PR (two unrelated bugs, `andthen.t`'s
Slip-arg-dropping tail call and `is_default.t`'s object-hash metadata loss,
were also surfaced by the same PR but are independent and were fixed
separately, see the PR's fix-forward commit).

Forcing `subtest_call_block` to always use `call_sub_value` (disabling the
compiled-first path entirely) makes the truncated repro
(`roast`/battery test file up through the "Byte-level middleware" subtest)
pass cleanly again.

## A second, independent symptom: escaped class registrations

While investigating, a related-but-distinct minimal repro was found that
also depends on the dispatch path — but in the OPPOSITE direction (this
repro reproduces on **both** dispatch paths, so it is a pre-existing bug,
not something #6499 introduced or fixed):

```raku
use Test;
plan 1;

my $captured-type;
subtest {
    my class Upper {
        method go() { "hi" }
    }
    $captured-type = Upper;
    ok 1, 'declared';
};

say $captured-type.new.go();   # dies: X::Method::NotFound: Unknown method
                                # value dispatch (fallback disabled): new on Upper
```

Root cause: `test_fn_subtest` (`src/runtime/test_functions/tap_subtest.rs`)
snapshots several registry tables (`snapshot_subtest_decls`) before running
the subtest body and wholesale-restores them afterward
(`restore_subtest_decls`), including `registry.classes`. This is meant to
scope `my class`/`my sub`/etc. declared *inside* a subtest so they don't
leak into the outer lexical scope by *name* — but the restore is a blanket
`registry.classes = <pre-subtest snapshot>`, which also erases the
registry entry for a class whose **type object escaped via a captured
variable** (`$captured-type = Upper` above). Reflection on the escaped type
object still works right after the subtest (`.^name`, `.^methods` — these
apparently don't need a fresh by-name registry lookup), but constructing a
new instance via `.new` does need one, and finds nothing, because the
snapshot restore already stripped the entry.

This second bug is orthogonal to the compiled-dispatch regression above
(same failure with `call_sub_value` forced) and is NOT what broke
`http-middleware.rakutest` (that test's classes are constructed
*synchronously within their own declaring subtest*, well before any
restore fires — the failure there is about an async
`whenever`/`supply`-pipeline transform silently not taking effect, not a
`.new` crash). It is filed here rather than as a `todo/tickets/` entry
because a correct fix likely needs to change what `restore_subtest_decls`
snapshots (e.g. only remove entries that were *never observed outside the
subtest*, or stop restoring `classes` at all and rely on name-scoping done
some other way) — a design question, not a one-line patch.

## Why compiled dispatch specifically breaks the async-transform case

Not yet root-caused. Working hypothesis, not confirmed: `call_compiled_closure`
(the VM's closure-call entry, used by `vm_call_on_value`) and
`call_sub_value`'s own compiled-code branch both ultimately run the same
`CompiledCode`, but set up the call frame differently — closure env merge
order (`merge_all`/caller-priority handling — see the extensive comments in
`src/runtime/resolution_call_sub.rs` around `is_authoritative`/`self`
force-install), and possibly which frame/thread a class's method bytecode
ends up associated with for later cross-thread/cross-task invocation from a
`whenever` body. `docs/adr/` has prior art on lexical/dynamic-scope and
instance-capture correctness (ADR-0022/0023/0024/0025) that is probably the
right starting point for whoever picks this back up, since the symptom
shape (a closure/method captured in one call context behaving correctly
when invoked synchronously but wrong when invoked later from another
thread/task) matches that family of bugs.

## Suggested path back to the perf win

1. Fix (or redesign) `restore_subtest_decls` so it does not erase a class
   registration whose type object has escaped the subtest via an assignment
   to an outer variable — this alone might be enough to also fix the
   async-transform regression, if (as seems likely) the two symptoms share
   a root cause in how the compiled path's class registration timing
   interacts with the snapshot/restore window.
2. Re-add the compiled-first `subtest_call_block` dispatch behind the same
   `has_bytecode` check PR #6499 used, gated on the fix above.
3. Re-verify with both this file's minimal repro and the full
   `Cro::HTTP` battery suite (`scripts/battery-testsuite.sh`, or
   `MUTSU_FUDGE=1 prove -I modules/Cro-HTTP/lib -I modules/Cro-Core/lib -I
   modules/Cro-TLS/lib <fetched http-middleware.rakutest>`) before
   re-landing.

## Affected files

- `src/runtime/test_functions/tap_subtest.rs` (`subtest_call_block`,
  `snapshot_subtest_decls`/`restore_subtest_decls`)
- `src/runtime/registration_class_body_exit.rs` (`finalize_class_registration`,
  the `ClassRegSnapshot` rollback machinery — related but not identical;
  did not need changing to fix ticket-session Bug A/Bug B)
