# A callee's parameter no longer clobbers the caller's after a thread has run

Two routines that merely happen to use the same parameter name aliased each
other once any thread had run. The callee's argument replaced the caller's
value:

```raku
sub inner($desc) { say "inner $desc" }
sub outer($desc) { await start { inner("c") }; say "outer desc=$desc" }
outer("OUTER");
```

```
$ raku                    $ mutsu (before)
inner c                   inner c
outer desc=OUTER          outer desc=c
```

Remove the `await start { … }` and mutsu is correct; give the two routines
different parameter names and mutsu is correct. Only the combination fails.

## Cause

Spawning a thread turns on the cross-thread shared store (`shared_vars`), which
is keyed by **bare name**. `sync_env_from_locals*` mirrors a frame's local slots
into `env`, and `set_shared_var_sym` forwards any name the store already holds
back into it and marks it dirty; the next `await` pulls every dirty name into
the parent's env. `outer`'s `$desc` had seeded the store at spawn time, so
`inner`'s parameter — a completely different binding that happens to share the
key — overwrote it.

`exec_set_var_dynamic_op` already handles the same hazard for `my`: while the
store is active, a re-declaration is a fresh binding shadowing whatever the
store holds, so the name goes into `thread_redeclared_vars` and its writes stay
thread-local. A **parameter is the same thing** — a fresh per-invocation binding
— and was not marked. `call_compiled_function_named_inner` now marks the
routine's scalar parameter names right after `bind_function_args_values`, with
the same exclusions the `my` case uses: `@`/`%` names back the atomic element
stores and must keep propagating, `&` names are routines, and `$_`/`self` are
already excluded from the store's seeding.

## What it fixes

Found while measuring roast under the vendored `Test.rakumod`
(`todo/tickets/vendor-real-test-module.md`). The real module has
`multi sub subtest(&subtests, $desc = '')` and `sub pass($desc = '')`, so a
subtest whose body does `await $p.then: { pass "a"; pass "b"; pass "c" }`
reported `ok 1 - c` where rakudo reports `ok 1 - planless threaded` —
`t/subtest-threaded-pass-count.t`. It is also expected to account for most of
`todo/tickets/retire-native-test-tap.md`'s "the tap callback collected nothing"
group, where the emit runs on a timer or scheduler thread.

Pinned by `t/thread-callee-param-does-not-clobber-caller.t`, which also asserts
the two things the mark must *not* break: a lexical the thread genuinely closes
over still crosses the boundary, and an `is rw` parameter still writes back to
its caller's container. It passes under `raku` too.
