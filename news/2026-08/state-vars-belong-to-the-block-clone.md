# A `state` variable belongs to its block's clone

Raku clones a block every time its *enclosing* block runs, and a `state` cell —
named, or the implicit one behind a bare `$` — belongs to the clone. So a
`state` in a nested block restarts on every execution of the construct that
contains it, while the iterations of one loop execution share it (the loop body
is the block the loop statement cloned once):

```raku
for ^2 { say (map { ++$ }, ^3).join(",") }   # 1,2,3 / 1,2,3
for ^3 { print ++$ }                         # 1 2 3   (one clone)
sub f { if 1 { state $n; ++$n } }; f(); f()  # 1 / 1
```

mutsu had this only for real closures (per-clone `state_scope_id`, minted by
`MakeClosure`) and for loop bodies (`reset_state_locals_in_range` at
loop-statement entry). Three gaps remained, and this change closes all of them.

## The bare `$` is now a real `state` declaration

`#5892` gave an anonymous `$` a *parallel* mechanism: the parser classified each
occurrence as per-call or not and baked the answer into the minted name
(`__ANON_STATE_PC_<id>__`), which the VM keyed by the innermost enclosing
routine frame's `invocation_id`. That reset per routine *call*, which is not the
unit Raku uses — it could not restart a counter inside a `reduce` callback that
runs several times within one call, and at the mainline it never reset at all
(`for ^2 { say (map { ++$ }, ^3).join(",") }` printed `1,2,3` then `4,5,6`).

The parser now emits an implicit `state $__ANON_STATE_<id>__;` declaration at the
top of the block the `$` appears in, and the classification, the `PC` name
spelling, `anon_state_key`, `per_call_anon_state_read` and the
`enclosing_routine_invocation_id` fold are all gone. A bare `$` is what it is in
Raku — a `state` variable of its block — so it inherits the real machinery
rather than approximating it.

One wrinkle made this non-obvious: statement parsing is memoized, and
`block_stmt` speculatively parses `{ … }` as a hash literal before re-parsing it
as a block, so the surviving parse is a pure memo hit. The names minted during
the *discarded* parse were recorded in that parse's scope, so the surviving
block emitted no declaration at all. The memo now carries the anonymous-state
names each statement minted into its enclosing scope and replays them on a hit.

## An `if` branch and a bare nested block are blocks too

Both compile inline, so they had neither a closure's `state_scope_id` nor a
loop's entry reset, and a `state` in one persisted for the life of the enclosing
routine. The new `OpCode::ResetStateLocals` drops the `state` variables
initialized in a given instruction range, and is emitted at the entry of an
inline nested block that declares `state` at its own level — from all four
`if`-compilation paths (statement, value position, `do`-expression, and the
constant-folded branch) and from a genuine source `{ … }` block. A *synthetic*
body is deliberately excluded: a loop body is the block the loop statement
cloned once, and its iterations must share the cell.

A postfix `if`/`unless` **statement modifier** lowers to the same `Stmt::If` but
introduces no block, so the statement it gates belongs to the enclosing block and
must not be reset (`sub f { state $n = 0 if 1; ++$n }` counts across calls).
`Stmt::If` therefore gains an `is_statement_modifier` flag, mirroring the one
`Stmt::For` and `Stmt::Given` already carry for the same reason, set by the
modifier lowering in `parser/stmt/modifier.rs` and threaded to the branch
compilers.

## The interpreter carrier path kept the caller's scope

`call_sub_value` runs a code object's body through `eval_block_value`, which is
how every builtin that takes a callback but is not one of the specially-handled
`map`/`grep` forms invokes it — `classify` and `categorize` among them. It never
installed the callee's `state` scope, so `state_scope_id` stayed at the
*caller's* and the callback's `state` was shared by every clone
(`sub cl { <a b c>.classify({ ~($ ~= $_) }) }` accumulated across calls). It now
hands the callee's scope across `run_nested`'s register reset via
`pending_nested_state_scope`, the same way the operator-fallback path already
did. The named-vs-anonymous scope-id rule is factored into
`Interpreter::sub_state_scope_id` and shared with the compiled closure dispatch.

## Result

`Digest::RIPEMD`'s last wrongness is gone: the output stage's
`map { $_[[^5].rotate(++$)] }` runs once per compression block inside the
`reduce` callback, and its counter now restarts with each — so a multi-block
message no longer digests as the correct five words rotated by (blocks - 1). The
dist's `t/ripemd.t` passes in full — 8/8 RFC vectors — on a release build.
Pinned by `t/state-var-per-block-clone.t`.
