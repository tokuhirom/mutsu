# ADR-0019 D3-4: `CompiledMethodDecl` for the augment walker

Completing the D3-2/D3-3 conversion, `augment_class`'s `MethodDecl` arm now
also builds one `decl = CompiledMethodDecl::from_stmt(stmt)` and reads every
field off it instead of the original partial `Stmt::MethodDecl { .. }`
destructure — the last of the three `method`/`submethod`-registration
walkers ADR-0019's D3 scoping pass identified.

`name_expr` is still evaluated from the raw AST here
(`self.eval_block_value(&[Stmt::Expr(expr.clone())])`), unlike the class and
role walkers' D3-1 chunk-cursor lookup: `augment class` has no compiled
declaration plan at all (`Stmt::AugmentClass` still indexes `stmt_pool` via
the legacy `AugmentClass(u32)` opcode), so there is no `method_name_chunks`
vector to read from. Giving it one is separate, larger scope.

This slice deliberately preserves, rather than fixes, every drift point the
D3 scoping pass found in `augment_class`: `MethodDef.is_my` is still set
from the raw `is_my` flag (the class/role walkers use `is_submethod`
instead, since `my`/`our method` are filtered out of `class_def.methods`
before insertion at those two sites but not here); duplicate-method
detection is still not privacy-aware (`all_from_role` only, no `is_private`
comparison); and `is_lexical_only`/`is_our_only` gating, `handles`
forwarders, custom-trait/`is_export` handling, and BUILD/TWEAK `:$!attr`
validation remain entirely absent from this walker.

The point of landing this conversion without also fixing the drift: with
all three walkers now sharing one `CompiledMethodDecl::from_stmt`
construction site, the remaining differences are visible as unused struct
fields at this call site rather than as absent bindings in an independently
hand-written destructure pattern — which is what makes a future D3-5 able to
compare and reconcile them directly, the way D2b's `CompiledAttrDecl`
unification fixed its own four-way `Stmt::HasDecl` drift by construction.

Verified the same way as D3-2/D3-3: the full `t/` suite (27810 tests) plus
95 whitelisted roast files covering `S12-methods`, `S14-roles`,
`S12-attributes`, `S12-class`, `S12-construction`, and every file this
repository's own grep found referencing `augment class`/`augment role`
(`S10-packages/use-with-class.t`, `S32-exceptions/misc2.t`,
`integration/rule-in-class-Str.t`, `integration/advent2009-day22.t`,
`S12-enums/thorough.t`), all green.
