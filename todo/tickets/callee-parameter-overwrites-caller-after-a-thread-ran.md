# A callee's parameter overwrites the caller's same-named one once a thread has run

Two module-level routines that happen to use the same parameter name share a
lexical slot across the call once the inner call has gone through a thread
boundary. The caller's value is replaced by the callee's.

## Minimal repro (no `Test`, no threads visible in the module)

```raku
# lib/M.rakumod
unit module M;
sub inner($desc) is export { say "inner $desc" }
sub outer(&body, $desc) is export { body(); say "outer desc=$desc" }
```

```raku
# repro.raku
use M;
outer { my $p = Promise.new;
        start { $p.keep(True) };
        await $p.then: { inner("c") } }, "OUTER";
```

```
$ raku  -Ilib repro.raku      $ mutsu -I lib repro.raku
inner c                       inner c
outer desc=OUTER              outer desc=c          <-- wrong
```

Drop the `Promise`/`start`/`.then` and call `inner("c")` directly from the block
and mutsu is correct, so the trigger is the callee running on (or after) another
thread, not the nesting.

## Why it matters

- `t/subtest-threaded-pass-count.t` regresses under `MUTSU_REAL_TEST=1` for
  exactly this reason. The real `Test.rakumod` has
  `multi sub subtest(&subtests, $desc = '')` and `sub pass($desc = '')`; a
  `subtest` whose body does `await $p.then: { pass "a"; pass "b"; pass "c" }`
  reports `ok 1 - c` where rakudo reports `ok 1 - planless threaded`.
- It is the most likely cause of five of the six regressions in
  `todo/tickets/retire-native-test-tap.md`, where a tap callback's
  `@res.push($_)` collects nothing (`got: []`) when the emit runs on a timer or
  scheduler thread — the same "a thread ran, the outer frame's lexical is not
  what it was" family.

## Where to look

The same area as `354cd623f` ("an array alias survives a thread having run")
and the `shared_vars` scalar lane described in
`todo/deep/*` around the worker-pool work: `src/runtime/runtime_thread.rs`
clones the interpreter per thread and the env is merged back on join, and a
module-level routine's parameters live in the module's env rather than a frame
slot, so two routines of the same module that share a parameter name share the
key. The `$desc` here is *not* a module lexical in Raku — it is a parameter, so
the merge must not treat it as shared state.
