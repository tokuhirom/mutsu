# An anonymous state variable resets when its enclosing routine is re-entered

A bare `$` (`$++` / `++$`) is an anonymous state variable belonging to its
enclosing block's **clone**. A named routine's own body is cloned once, at
registration, so a `$` written directly in it keeps counting across calls; the
blocks *inside* that routine are re-cloned on every call, so a `$` in one of
them restarts. mutsu keyed the counter by its compile-time name alone, so it
kept counting everywhere:

    sub f() { (map { ++$ }, 1, 2, 3).join(',') }
    say f();   # raku: 1,2,3   mutsu: 1,2,3
    say f();   # raku: 1,2,3   mutsu: 4,5,6

That was the last wrong-digest cause in grondilu's `Digest::RIPEMD`, whose
output stage rotates the five hash words with `map { $_[[^5].rotate(++$)] }`:
the second and later `rmd160(...)` calls in one process rotated by the wrong
amount and returned a correct-but-*rotated* digest. Each call was correct in a
fresh process, which is what made it look like a hashing bug rather than a
scoping one. `todo/tickets/digest-dist-blockers.md` blocker 2 is closed by this,
and with it the whole `Digest` distribution.

## The rule, and where each half of it lives

> the counter resets iff the `$` is **lexically inside a nested block** that is
> **lexically inside a routine**, and then it resets once per *call of that
> routine* — not per block iteration.

The classification is static and only the bucket is dynamic, so the two halves
split cleanly.

**Compile time.** `CompiledCode::anon_state_nested_depth` is a compile-only
cursor that is non-zero while the compiler emits inside a nested block within a
routine, and `add_constant` records every `__ANON_STATE_<id>__` it sees while
the cursor is set into that chunk's `per_call_anon_states`. `add_constant` is
the single choke point every variable-name constant passes through, and the
`__ANON_STATE_<id>__` spelling is used for nothing else, so no emit site had to
change.

The set lives **on the chunk**, not in a registry keyed by id. A global registry
looks sound — the parser mints one id per source occurrence — but the same
source is compiled several times (the routine-hoist pass,
`record_type_body_captures`' capture analysis, then the real body) and those
passes do not all reproduce the true lexical context; one pass classifying an id
wrongly would poison it for the whole process. Keeping the classification with
the chunk means only the chunk that actually runs decides.

The cursor is set wherever a block body is compiled: at child-compiler
construction when `!is_routine && lexically_in_routine` (`map`/`grep`/pointy
blocks), in `push_dynamic_scope_lexical`/`pop_dynamic_scope_lexical` for an
inline body, and — because several runtime paths *re-compile* a block body from
its AST and so lose the lexical position — restated at those sites
(`eval_map_over_items` and its grep/rw siblings, `exec_make_gather_op`).

The inline case is **opt-in**: `Stmt::For`'s arm arms
`Compiler::anon_state_enable_next` for the one scope its body is about to push,
and only when `!is_statement_modifier`. It has to be opt-in because `Stmt::If`
and `Stmt::While` carry no `is_statement_modifier` flag, so `$++ if C` — which
must keep counting, and which `roast/S32-list/rotor.t`'s hand-written `Iterator`
depends on — cannot be told from `if C { $++ }`. Leaving the shapes that cannot
be classified unmarked keeps them exactly as they were. The flag is cleared at
the top of every `compile_stmt` so it can never leak to an unrelated block.

**Run time.** `RoutineFrame` gained a monotonic `invocation_id`, and
`anon_state_key` folds in the id of the innermost enclosing **non-block** frame
for a marked name. At the mainline there is no such frame, so the id is a
constant and a top-level `$` keeps counting — `[ $++ xx 3 ] xx 3` is still
`0..8`.

## The env shadow

A marked name has to be read from the state store *outright*, never from `env`.
These names compile to `GetGlobal`/`SetGlobal`, so their env entry outlives the
block clone: on the first use in a new call the store legitimately misses while
env still holds the previous call's value. The five read chains therefore
consult `per_call_anon_state_read` first, and that helper answers the site's
default on a store miss rather than falling through — falling through is exactly
what resurrected the stale count during development.

## Known residue

A named sub *nested inside another routine* still persists where rakudo resets:

    sub outer() { sub inner() { ++$ }; (inner(), inner()).join(',') }
    say outer();   # 1,2 in both
    say outer();   # raku: 1,2   mutsu: 3,4

`inner` is re-cloned per call of `outer` in rakudo, but the runtime rule picks
the innermost non-block frame — which is `inner` itself, whose own invocation
changes per call of `inner`, not per call of `outer`. Getting this right needs
the compile-time routine-nesting depth carried to the frame lookup. The
behaviour is unchanged from before this fix, and no `roast` or `t/` test
exercises it.

Pinned by `t/anon-state-per-routine-call.t` (14 tests, all also passing under
`raku`), which covers both directions of the rule row by row.
