# A run-time-exported value term parses, and a custom infix can take an `is rw` operand

The ecosystem `French` distribution (locked on the roulette board, #7884) exports its whole surface
through a run-time `sub EXPORT` hook, in a shape distinct from the `UNIT::`-grep idiom
[ADR-0087](../../docs/adr/0087-runtime-export-hook-parse-time-approximation.md) already
approximates: every name is a LOCAL declaration inside the hook's own body, hand-assembled into the
returned `Map`, rather than drawn from the compunit's unit scope.

```raku
sub EXPORT(|) {
    my &infix:<et> = sub ($a, $b) { $a && $b };
    my \vrai = True;
    Map.new('&infix:<et>' => &infix:<et>, 'vrai' => vrai);
}
```

`1 et 2` already worked regardless of this idiom — the parser's custom-infix-word matcher accepts
any non-reserved word speculatively and resolves it at run time, with nothing for the scan to
consult either way. `vrai et 2` did not: `Unknown function: et`. `vrai` is a plain VALUE term with
no such fallback, so an unknown bareword defaulted to a listop-call head, and `vrai et 2` misparsed
as `vrai(et, 2)` — the error names the wrong culprit (`et`, evaluated as if it were a zero-arg call)
because the term guess ran first and swallowed the infix guess's operand as an argument before it
ever got a chance. This was `French`'s whole operator suite: 12 of 30 assertions, dying at the first
`vrai et vrai`.

`collect_export_hook_value_terms` (`src/parser/stmt/simple/module_exports/export_hook.rs`) walks a
unit-scope `sub EXPORT`'s own body for a sigilless value declaration (`my \x = ...`, the `VarDecl` +
sibling `MarkSigillessReadonly` pair the parser already marks it with) and registers each one the
same way an exported `constant` already is. `declared_term_symbol`
(`src/parser/primary/ident/term_literals.rs`) then falls back to that registry when the local
`term_symbols` lookup misses, so an imported term gets the same "this is a complete term, not a
listop head" treatment a locally-declared one already had.

## The second bug the same suite surfaced: a custom infix couldn't take an `is rw` operand

Past that, `French`'s `plus_égal`/`moins_égal`/`fois_égal` (`$x plus_égal 5`, Raku's `+=` spelled
out) died too: "Parameter '$a' expects a writable container (variable) as an argument, but got '10'
(Int) as a value without a container." Their underlying subs declare `sub ($a is rw, $b) { $a += $b
}`, and `Compiler::compile_expr_infix_func` compiled both operands with a plain `compile_expr` —
which always leaves a bare value on the stack — instead of `compile_call_arg`, the function every
ordinary call argument already goes through to attach the `WrapVarRef` metadata an `is rw` parameter
needs to bind the caller's own container. Swapping in `compile_call_arg` for both operands closes it
generally, not just for French: any user-defined `infix:<op>` sub with an `is rw` parameter, on
either side, now binds correctly whether called as `$x op $y` or as `&infix:<op>($x, $y)`.

`French` goes from 12/30 to 30/30, matching `raku`.

## A second-order regression the fix itself introduced, caught before publishing

`compile_call_arg`'s `WrapVarRef` tag reaches `exec_infix_func_op` at run time, and that opcode
also serves every OTHER `Expr::InfixFunc` shape — including two that are never a custom sub at all:
a native word operator's own compound-assign desugaring (`$m mod= 5` lowers to
`$m = $m mod 5`, whose `InfixFunc` operand is the SAME variable the outer assignment targets), and
the Unicode arithmetic aliases (`×`/`÷`). Passing the still-wrapped `WrapVarRef` value into their
native dispatch (`apply_reduction_op`'s `arith_mod`, `core_unicode_arith_alias_infix`) reads it as an
unmatched type and silently defaults, so `$m mod= 5` computed `0 mod 5` instead of `17 mod 5` —
caught by the local suite's `t/lang/operators/word-compound-assign-loose-operand.t`,
`t/lang/operators/user-infix-unicode-times-slash.t` and
`t/routines/dispatch/user-infix-unicode-multiply.t` before this ever reached a PR.

The fix stays targeted rather than reverted: `exec_infix_func_op` first tries `try_user_infix` with
the operands still wrapped (so a user candidate CAN bind an `is rw` parameter), and only unwraps for
`core_unicode_arith_alias_infix`'s own native dispatch. `call_infix_fallback`'s native reduction path
(`apply_reduction_op`, reached for `mod`/`div`/junctions/... when no user candidate exists) unwraps
its own operands too, but its *other* fallback — resolving a purely custom `infix:<op>` sub by name,
the path `plus_égal` actually takes, since `try_user_infix` only ever competes against a
CORE-recognized operator name — still receives the operands wrapped, exactly as it must to bind an
`is rw` parameter.

Pinned by `t/modules/import-export/export-hook-value-term.t` (4 assertions, using the new
`t/lib/ExportHookValueTerm.rakumod` fixture) and `t/routines/signature/custom-infix-rw-first-param.t`
(9 assertions, including the `mod=`/`div=`/`×`/`÷` controls for the regression above), both
dual-oracled against `raku`.
