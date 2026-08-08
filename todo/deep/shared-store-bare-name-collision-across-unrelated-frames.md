# Two unrelated frames sharing a variable name collide through the global store

Once any thread has been spawned, a plain lexical write goes through
`set_env_plain_lexical` → `set_shared_var_sym` into `shared_vars`, a
**process-global map keyed by bare name**. Two frames that merely happen to use
the same variable name — in unrelated files, at unrelated times — then read each
other's values.

This is the clearest real-code instance found so far, in Cro's
`t/http-session-inmemory.rakutest`:

```raku
given Cro::HTTP::Client.new(:cookie-jar) -> $client {
    for 1..5 -> $i {
        given await $client.get("$url/hits") {
            is await(.body-text), "Visit $i",
                "Session cookie being sent makes state work (request $i)";
        }
    }
}
```

Instrumented (`note` before and after the request):

```
[T] loop top i=1
[T] after get i=4        <-- the request rewrote the loop variable
[T] in given i=4
not ok 3 - Session cookie being sent makes state work (request 4)
[T] loop top i=2
[T] after get i=4
...
```

Every iteration reports `request 4`, and only the iteration where the body
really is `Visit 4` passes.

## Update 2026-08-08: the concrete Cro instance was a multi-param `for` loop

The instrumentation below was re-run on current `main` and identified the
writer exactly: **`Cro.rakumod`'s `for @components-in.kv -> $i, $comp`** (the
pipeline compose), not the HPACK `while` loop originally guessed. A
multi-parameter `for` binds its parameters with a plain `Stmt::Assign`
(`build_for_bind_stmts`), which reaches `set_shared_var_sym` and publishes each
per-iteration value under the bare name — while a *single*-param loop binds
natively and is exempt. Minimal repro (no Cro, no modules):

```raku
sub compose(@components) {
    my $last;
    for @components.kv -> $i, $comp { $last = $i + $comp }
}
await start { 1 };
for 1..5 -> $i {
    compose([10, 20, 30, 40, 50]);
    await start { 1 };
    say "loop i=$i";     # raku: 1..5;  mutsu (before the fix): 1,4,4,4,4
}
```

Fixed by masking a multi-param loop's names out of the bare-name lane for the
loop's duration (the rule `exec_set_var_dynamic_op` already applies to a `my`
re-declaration) — `src/vm/vm_for_loop_body.rs`, pinned by
`t/for-multi-param-shared-lane.t`. `t/http-session-inmemory.rakutest`'s
tests 3-7 still fail after it, but with a *different* wrong value (`-1`, the
HPACK Huffman `while --$i >= 0` residue) reaching the frame through the **`env`
axis, not the store** — no `SharedStore` write or pull for `i` happens any more.
So the remaining leak on this file is a plain env-key collision, and this
ticket's store-keying redesign is no longer what blocks it.

Related, still open and single-threaded (no store involved):
`todo/tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md` —
a multi-param loop variable with no local slot in its frame writes a *global*
of the same name (`sub f() { for <a b>.kv -> $j, $u {} }` clobbers an outer
`my $j`). Same root cause (the bind is an assignment, not a declaration); the
real fix is making it a per-iteration declaration, which is entangled with the
§1.4 shadow-slot campaign.

## Mechanism, measured

`Env::insert`/`insert_sym` were instrumented behind an env var to print a
backtrace on every write to the key `i`. During the request, an unrelated
`while`-loop somewhere in the Cro/dependency stack counts a variable of its own
called `$i` down from 13:

```
[CLOBBER] insert(str) i = 12
   1: runtime_shared_vars::set_shared_var_sym
   2: vm_env_helpers::set_env_with_main_alias_sym
   3: vm_env_helpers::set_env_with_main_alias
   4: vm_misc_coerce::exec_pre_decrement_op_inner
   …
  10: vm_control_ops::exec_while_loop_op_inner
```

Each `--$i` lands in the global map, and the test's own `$i` — an ordinary `for`
loop variable in a different file — reads the last value written there.

## Why this is the architectural issue, not a local bug

`session-shared-store-bare-name-collision` (2026-07-17) root-caused the same map
for zef: `clone_for_thread` migrates *every* lexical into it by bare name, and
the `thread_redeclared_vars` mask does not help because each spawned thread has
its own `Interpreter` (and its own mask) while the map is a single
`Arc<RwLock<HashMap<String, _>>>` for the whole process. The recorded conclusion
was that the fix is the store's **keying** — a per-lineage store where
`clone_for_thread` gives the child a store that inherits from the parent's and
writes back on join — and that it needs an ADR first.

Related open tickets, all downstream of the same map:

- `todo/tickets/supply-block-lexical-leaks-through-thread-lane.md` — a supply
  block's `my` reaching the caller when a thread drives the emit;
- `todo/tickets/cue-loop-lexical-shared-lane-residue.md`.

## Why it matters now

It is what remains between mutsu and Cro's session tests: with the
five client-side fixes of 2026-08-08 landed,
`t/http-session-inmemory.rakutest` runs 13 tests and passes 6, and the failures
that are left are this collision (tests 3-7) plus the concurrent-client pair
(8-9), which is the same map under two clients at once.

## Reproducing

```
bash tmp/run-session-test.sh          # the instrumentable copy of the test
MUTSU_DEBUG_CLOBBER=i bash tmp/run-session-test.sh
```

The second form needs the temporary hook in `Env::insert`/`insert_sym` described
above — note that the String-keyed `insert` does **not** route through
`insert_sym`, so both have to be hooked.
