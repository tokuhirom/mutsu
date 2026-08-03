# A multi-parameter `for` loop clobbers the enclosing lexical it shadows

```raku
my $v = "outer";
for (1, 2) -> $k, $v { say "in loop $k $v" }   # in loop 1 2
say "after: $v";                               # raku: outer
                                               # mutsu: (uninitialized Any)
```

The loop parameter is a fresh binding that shadows the enclosing `my $v` only
for the duration of the block; after the loop the outer value must be back.

## Where it is

`exec_for_loop_body` (`src/vm/vm_for_loop_body.rs`) already saves and restores
each `spec.multi_param_names` entry — but only its **env** entry and its readonly
flags. A compiler-scoped `my $v` lives in a *local slot* and is not necessarily
mirrored into env, so `self.env().get("v")` returns `None` at save time, the
restore takes the `env.remove(name)` branch, and the slot keeps the loop's last
iteration value.

The obvious patch — also save `self.locals[find_local_slot(code, name)]` and put
it back — does **not** work as written: `find_local_slot` uses
`code.locals.iter().position(...)` (first match) while shadowing needs
`rposition`, and the loop parameter and the shadowed outer lexical may or may not
share a slot depending on how `build_for_bind_stmts` compiled the binding
(`Stmt::Assign` vs `Stmt::VarDecl`). It was tried and reverted while fixing
`news/2026-08/for-multi-param-stale-type-constraint.md`; the restore ran (slot 0,
correct saved value) yet the post-loop read still saw the loop value, so the
post-loop `GetLocal` resolves a *different* slot. Working out which slot each
side owns is the actual work here.

## It also leaks across frames through the cross-thread lane

Because the binding is a `Stmt::Assign`, it compiles to a by-name store, which
publishes into the bare-name cross-thread lane exactly like an ordinary
assignment — a real `my` would have been masked by `thread_redeclared_vars`. So
the loop parameter of a multi-param `for` in one routine overwrites a *different*
routine's live same-named lexical:

```
for ^3 -> $i { ...anything that awaits a Cro request... ; say "round $i" }
```

printed `round 2` in every iteration, because `Cro.compose`'s
`for @components-in.kv -> $i, $comp` (`Cro.rakumod:560`, six components) had
published its own `$i` under the bare name `i`, and the caller's `await` pulled
it back. Confirmed with `rust-gdb` breaking on the shared-store write: the
backtrace is `set_env_with_main_alias("i") <- exec_for_loop_body <-
call_compiled_method("Cro", "compose")`. Renaming the caller's loop variable
makes it go away, which is how it was isolated.

This is the strongest argument for fixing the binding form itself (making the
multi-param bind a real per-iteration declaration) rather than patching the
save/restore: one change would settle the value clobber, this lane leak, and the
type-constraint save/restore added in
`news/2026-08/for-multi-param-stale-type-constraint.md`.

## Why it matters

Same family as the shared-lane container escape
(`news/2026-08/threaded-array-mutation-escapes-to-the-caller.md`): a fresh
binding silently becoming the enclosing one. It is easy to hit in ordinary code
(`for %h.kv -> $k, $v` inside a routine that already has a `$v`), and it is
silent — no error, just a wrong value later.

Pin when fixed: extend `t/for-multi-param-type-constraint.t` with the value-
restore cases (typed and untyped, `$`/`@`/`%` sigils, nested loops reusing a
name).
