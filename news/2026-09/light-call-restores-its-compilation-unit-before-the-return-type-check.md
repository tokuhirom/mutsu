# The positional-light call path restores `current_unit` before its return-type check

[#7558](https://github.com/tokuhirom/mutsu/issues/7558) parks a full
implementation of module-private top-level sub scoping (closed PR #7436) behind
a blocker of its own, and names two independent pre-existing bugs that branch
fixed on the way, "both worth salvaging separately if this stays parked". This
is the second of them:

> `call_compiled_function_positional_light_at`'s return-type-check failure path
> returned **without restoring `current_unit`** — a leak that also mis-scoped
> user-declared operators.

## The defect

`call_compiled_function_positional_light_at` saves the caller's compilation unit
on entry (`enter_compilation_unit`) and restores it on the way out. Every other
piece of per-call state — locals, loop-local scopes, block-declared vars, the
env overlay, the routine package, pragma state — is restored in one block just
before the function's tail. `current_unit` was not: it sat *after* the
return-type check, which has its own `return Err(...)`.

So a routine whose declared return type its body violates left the caller
running with `current_unit` still naming the callee's unit. That matters because
`current_unit` is what user-declared operator scoping resolves against —
`user_infix_override` → `declaring_unit_is_in_scope` walks it, and the `EVAL`
parent chain, to decide whether an `infix:<...>` declared in some compilation
unit is visible here. A *caught* return-type failure would therefore silently
re-scope the caller's operators for the rest of the program.

The fix is to restore it with everything else, immediately after
`leave_routine_package`. Nothing between there and the old restore point reads
`current_unit`, so the move is behaviour-preserving.

## The leak is latent on `main`, and that is worth recording

The closed branch found this because it made cross-unit light calls reachable.
On `main` they are not: a call into another compilation unit does not take this
path at all. Verified under `rust-gdb`, breaking on the return-type-failure
`return` for a `use`d module whose exported sub is light-eligible
(`our sub bad-return(Int $n --> Int) is export { "not an Int" }`) — none of
`bad-return(1)`, `RetTypeUnitLeak::bad-return(1)` or `&bad-return(1)` reaches
`call_compiled_function_positional_light_at`, while the identical sub declared
in the *same* unit does. So there is no observable repro to pin, and none is
claimed here.

That is the useful half of the finding for anyone picking #7558 back up: of its
two "salvage separately" items, this one is a two-line reorder that becomes
load-bearing the moment cross-unit light calls exist, and the other (the
name-keyed call caches `pos_light_call_cache` / `light_call_cache` /
`otf_call_cache` / `func_multi_resolve_cache` / `fn_resolve_cache` being keyed
by `(name, package)`, with no room for a winner that depends on which unit is
asking) is a design observation with nothing to fix until the same point.
