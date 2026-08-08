# ADR-0019 D7-3: typed role parent ops replace the hides/hidden string markers

`CompiledRoleDeclPlan` gained `parent_ops: Vec<RoleParentOp>`, a typed record of every
`does`/`hides`/`is hidden` clause in a role's own body. Each op mirrors one `DoesDecl` statement
in the (`SyntheticBlock`-flattened) body, computed once at compile time by a new
`Compiler::compile_role_parent_ops` — the same flatten `compile_role_attr_decls` already uses, so
the compiler's op list and the runtime's body walk stay aligned by position.

Previously `role_body_does_decl` classified each `DoesDecl` statement by string-matching its name
against `__mutsu_role_hidden__` and stripping a `__mutsu_role_hides__` prefix, on every role
registration. That classification now happens once, at compile time; the runtime reads a typed
`RoleParentOp { name, hides, hidden, args }` by position instead, via a cursor in the same style
D3-1's `method_name_chunk_idx` established.

`args` also carries ADR-0019 D4-1's parsed bracket-argument expressions, compiled to
`DeclTraitArg` chunks the way D4-2 compiled the class-header site. The role-body `does` site's own
parametric-candidate resolution (`resolve_role_candidate_with_args`) now evaluates these
precompiled chunks instead of re-parsing the concatenated parent string — the piece D4-2
deliberately left open for D7 — reusing D4-3's `should_treat_role_arg_as_type_expr` bail-out for
coercion-type arguments (`does R[Str:D(Numeric)]`) that parse as an `Expr` but must not be
evaluated as one.

Left untouched on purpose: the same function's earlier `concretized_parent` lookup, a second
independent `resolve_role_candidate` call over the same bracket text. Wiring precompiled args
into that call too would collapse today's double-evaluation of a side-effecting bracket argument
into a single evaluation — a real behavior change, out of this slice's narrow scope.

Verified via the full `t/` suite (27,992 tests), every whitelisted `S14-roles`/`S12-coercion`
roast file, and `t/mro-role-hides.t` (hides/hidden-specific coverage), all green.
