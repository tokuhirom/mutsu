# An inline `my $x = expr` used as an expression never itemized its own value

`(my $x = [1, 2]).raku` rendered `[1, 2]`, dropping the `$` — even though a
plain `my $x = [1, 2]; $x.raku` on two lines correctly rendered `$[1, 2]`.
The same gap affected every itemizable container kind: Array, Hash-variable
source, `Seq`, `List`, and `Slip`.

## Root cause

An inline declaration like `my $x = [1, 2]` used where an expression is
expected (`(...)`, a call argument, the RHS of another assignment) parses as
`Grouped(DoStmt(VarDecl {...}))` and compiles through
`compile_expr_do_stmt` (`src/compiler/expr_block.rs`). Its plain-scalar arm
computed the itemized value only for what gets *stored* — via
`emit_set_named_var`, which runs the same `itemize_scalar_store` the
statement-form `SetLocal`/`SetGlobal` paths already use — but the value the
expression itself *yields* was a `Dup` taken **before** that store, whenever
the declaration had no local slot of its own. That is the common case: a
fresh, non-shadowing `my $x` at the top of a routine or file gets no
`decl_slot` (only a shadowing declaration, or one a later container-slot
read resolves to, does), so the vast majority of inline declarations hit
the un-itemized `Dup` path.

Fixed by always reading the value back after the store
(`emit_get_named_var`) instead of conditionally `Dup`-ing before it. This
also happens to handle `Slip` correctly for free: reading back the already-
stored value reflects whatever itemization the store applied (including the
per-value `Slip` itemization flag from #8478), without needing a second,
separately-implemented itemizer for this path at all.

## Repro

```raku
say (my $a = [1, 2]).raku;          # raku: $[1, 2]           mutsu (before): [1, 2]
my @src = 1, 2;
say (my $b = @src).raku;            # raku: $[1, 2]           mutsu (before): [1, 2]
my %hsrc = a => 1;
say (my $c = %hsrc).raku;           # raku: ${:a(1)}          mutsu (before): {:a(1)}
say (my $d = (1, 2).Seq).raku;      # raku: $((1, 2).Seq)     mutsu (before): (1, 2).Seq
say (my $e = (1, 2)).raku;          # raku: $(1, 2)           mutsu (before): (1, 2)
say (my $f = slip(5, 6)).raku;      # raku: $(slip(5, 6))     mutsu (before): slip(5, 6)
```

The fix also covers chained assignment (`$a = $b = [1, 2]`, each `=` is
itself an expression), an inline declaration used as a call argument, a
shadowing declaration (which already had a local slot, so was already on
the correct path), and a typed inline declaration (`my Any $m = [...]`).

Pinned in `t/vm/vardecl-expr-value-itemized.t`.
