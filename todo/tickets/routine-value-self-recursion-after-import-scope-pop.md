# A captured `&name` reference to a popped proto/multi import stack-overflows instead of dying

Found while verifying `todo/tickets/use-inside-a-block-leaks-to-the-enclosing-scope.md`'s
env-half fix against `roast/S32-list/skip.t` under `MUTSU_REAL_TEST=1` (the
real vendored `Test.rakumod`). That file's own
`BEGIN` block is exactly the shape that triggers this:

```raku
BEGIN my (&plan, &subtest, &is, &is-deeply, &throws-like) = do {
    use Test;
    (&plan, &subtest, &is, &is-deeply, &throws-like)
}
```

**This is a pre-existing bug, unrelated to the env-leak fix.** Confirmed by
reverting the env-scoping change and re-running the same command against the
unmodified worktree tip — the crash reproduces identically either way:

```
$ MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/S32-list/skip.t
thread '<unknown>' has overflowed its stack
fatal runtime error: stack overflow, aborting
```

A `rust-gdb -batch -ex run -ex 'bt 30'` backtrace shows an unbounded cycle:

```
call_function("plan", ...)              # builtins.rs:1230
  -> call_sub_value(...)                # resolution_call_sub.rs:205
    -> eval_call_on_value(...)          # builtins.rs:315
      -> call_function_fallback("plan") # builtins_operators_fallback.rs:748
        -> call_function("plan", ...)   # repeats
```

## Root cause

`plan` in the real `Test.rakumod` is a `proto`/`multi` sub
(`proto sub plan ($?, Cool :$skip-all) {*}`). When `use Test;` executes
inside the `do { }` block, `import_module` installs it under
`GLOBAL::plan` in the proto tables. Reading the bare `&plan` inside the same
block (to build the tuple the `do` block returns) resolves to a
`ValueView::Routine { package: "GLOBAL", name: "plan" }` — a *name-based*
reference, not a bound closure, because a proto/multi has no single
candidate to point at. `my (&plan, ...) = do { ... }` binds this Routine
value into the outer `&plan` local (and, via the usual locals<->env
mirroring, into `env["&plan"]` there too).

Once the `do` block's own `PushImportScope`/`PopImportScope` pops (existing,
pre-existing mechanism — see `runtime/runtime_module.rs`), `GLOBAL::plan` is
removed from the proto tables, because it is a bare importing-package alias,
not `Test`'s own qualified `Test::plan`.

Now `plan 55;` runs at the mainline. `call_sub_value` (`resolution_call_sub.rs`,
~line 169-205) gets the captured `Routine{package:"GLOBAL", name:"plan"}`
value. Since `package == "GLOBAL"` it skips the qualified-name check (line
184: `package != "GLOBAL"`), and since the registry no longer has a bare
`plan` (correctly popped), the `resolve_function`/`has_proto`/
`has_multi_candidates` checks (line 191-193) all fail. Rather than erroring,
line 205 unconditionally falls back to `self.call_function(&name_str, args)`
— i.e. it re-dispatches "plan" **by name**. `call_function` doesn't find it
in the registry either, falls through to `call_function_fallback`
(`builtins_operators_fallback.rs`, ~line 728-748), which checks
`env.get("&plan")` — finds the *same* `Routine` value bound to the outer
`&plan` local — and calls it again via `eval_call_on_value` /
`call_sub_value`. Infinite recursion, no base case, so it stack-overflows
instead of raising a catchable "no such candidate" error.

## Why it wasn't caught before

`roast/S32-list/skip.t` is only run under the real `Test` module with
`MUTSU_REAL_TEST=1` (not part of `make roast`'s default native-provider
run), and the native TAP provider's `plan` is a plain builtin, not a
proto/multi `Routine` value, so this path never triggers under the default
configuration. The env-leak ticket's own description of this file's expected
failure ("`skip()` was passed a non-integer number of tests") predates
whatever later change exposed this stack overflow instead — the two
symptoms are unrelated; the newer one happens first and prevents the file
from ever reaching the `skip()` call.

## Where it bites

- `roast/S32-list/skip.t` under `MUTSU_REAL_TEST=1` only (not in
  `roast-whitelist.txt`'s default run).
- Any `my (&name, ...) = do { use SomeProtoOrMultiExportingModule; (&name, ...) }`
  pattern that captures a proto/multi export as a bare `&name` reference and
  calls it later from OUTSIDE the scope that imported it, once the import
  scope's registry entries are correctly popped.

## Why it's not a one-liner

The unconditional `call_function(&name_str, args)` fallback at
`resolution_call_sub.rs:205` exists to route method-dispatcher-shaped
`Routine` values and legitimately-resolvable names through the normal call
path; it needs a base case that raises `X::Method::NotFound` /
"Undeclared routine" (matching raku's own behavior — `raku` reports
`Undeclared routine: ourfoo` for the equivalent construct at *compile* time,
since it resolves names lexically) instead of re-entering `call_function`
when nothing resolves and the previous call in the chain already tried the
exact same name. A minimal fix needs a cycle guard (e.g. detect that
`call_function_fallback`'s `env.get("&name")` returned the very `Routine`
value that's already being dispatched) or, closer to raku's real semantics,
resolving the proto/multi captured by `&name` eagerly (at the point
`(&plan, ...)` is read, while the import is still in scope) into something
that doesn't need a later by-name re-lookup at all.
