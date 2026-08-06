# A lexical `&emit` now shadows the `supply { ... }` control-flow sugar everywhere in the block

`news/2026-08/lexical-shadows-builtin-call.md` fixed a lexical `&emit`/`&done`
shadowing the control-flow builtin for an ordinary bare call, including from a
nested closure and a non-final statement call — but not a bare `emit(...)`
written *directly inside* a `supply { ... }` block's own body:

```raku
my &emit = { say "lexical emit called with $_[0]"; "e" };
my $s = supply {
    my $r = emit(42);
    say "result: $r";
}
$s.tap({ say "tapped: $_" });
```

used to print `result:` (an uninitialized value) followed by `tapped: 42` —
mutsu emitted `42` into the supply instead of calling the lexical `&emit`,
which raku's own output shows should run instead (`lexical emit called with
42` / `result: e`, nothing tapped).

## Two separate gaps

`src/parser/primary/ident/supply.rs`'s `rewrite_supply_stmt` and its
expression-position twin `supply_emit_expr.rs::rewrite_expr` recognise a bare
statement-form `emit ARGS;` and the `.emit` topic-method sugar and rewrite
them to `$emitter.emit(...)` *syntactically*, at parse time, with no check
for whether `emit` is a declared lexical in scope. Fixed by gating each
rewrite site on `!is_user_declared_sub("emit")`, mirroring the parent
ticket's `done` fix.

That covers every shape the parser rewrite ever sees — but `emit(...)` used
as a sub-expression (assigned to a variable, inside a ternary, ...) was
*never* rewritten by the parser at all; it stays a plain `Expr::Call{name:
"emit", ...}` and reaches the interpreter's real `emit` builtin
(`runtime/builtins.rs`) at runtime. Outside a `supply` block this is caught
by the compiler's `amp_binding_in_active_scope` check, which routes a
shadowed bare call straight to the lexical binding before the builtin is ever
reached — but a `supply { ... }` on-demand body is *re-compiled from its AST
on every invocation* (`call_sub_value` -> `eval_block_value`, used because
the on-demand callback is a first-class `Value` invoked later from
`Supply.tap`/`.act`, not a compile-time-visible call site). That recompile
starts a fresh `Compiler` with no enclosing lexical-scope metadata, so
`amp_binding_in_active_scope` is blind on every call after the first and the
real `emit` builtin runs instead.

Fixed by having the `emit` builtin fall back to a runtime env lookup before
running its control-flow behaviour: `&emit` lives in `env` under its
`&`-prefixed key regardless of how the call compiled, so
`self.env.get("&emit")` plus the existing `env_callable_is_lexical_override`
helper (widened from `pub(super)` to `pub(crate)` to reach `runtime::
builtins` from `vm::vm_call_func_ops`) detects a genuine lexical override and
dispatches to it instead. This runs *before* the dynamic
`active_supply_emitters`/`supply_emit_buffer` fallback, so it does not
disturb `emit` reached from a genuinely unrelated nested sub (`sub e($x) {
emit $x }; supply { e(1); emit 2 }` still emits `1, 2` into the dynamically
enclosing supply, unaffected — no lexical `&emit` is in scope there).

`done` needed no equivalent runtime fix: unlike `emit`, a bare `done` has no
real builtin fallback implementation at all (it only exists as the
`Stmt::ReactDone` the statement parser produces, already gated on
`!is_user_declared_sub("done")`), so an unrewritten `done()` reaching this
same recompiled-body dispatch resolves through ordinary function-call
fallback to the lexical with no interference.

## Verification

`t/emit-inside-supply-block-lexical-shadow.t` pins the fix across all three
call shapes (assigned to a variable, bare statement, inside a ternary),
confirms the lexical actually receives the call argument, and pins the two
regression shapes that must keep working: an unshadowed `emit` in the same
three shapes, and `emit` reached from a genuinely unrelated nested sub. The
full `t/` suite (2903 files) and the 60 whitelisted `S17-supply`/`react`/
`emit` roast files (910 tests) pass unchanged.
