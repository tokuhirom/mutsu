# ADR-0019 D4-2: parent/role bracket arguments lowered to declaration-trait-arg chunks

D4-1 taught the parser to also capture `is Parent[Args]`/`does Role[Args]`/`hides Parent[Args]`
bracket content as a real, parsed `Vec<Expr>` (`Stmt::ClassDecl::parent_args`), riding alongside
the unchanged concatenated-string parent name. D4-2 lowers those expressions at compile time
instead of leaving them as raw AST for a future registration-time walk.

`CompiledClassDeclPlan` gains `parent_arg_chunks: Vec<(String, Vec<DeclTraitArg>)>`, keyed by the
same full concatenated parent string `parents`/`does_parents`/`hidden_parents` already use as a
registry lookup key. Each argument is compiled with the existing `compile_decl_trait_arg` helper
(the C5 mechanism also used for custom-trait arguments): a literal argument (`R[42]`) stays a
`DeclTraitArg::Literal` with no chunk at all, and everything else becomes a `DeclTraitArg::Compiled`
re-entrant bytecode chunk. `compiler/stmt.rs`'s `qualify_decl_name` package-qualification pass
already re-keys `parent_args` before `add_class_decl_plan` runs, so `parent_arg_chunks`'s keys are
consistent with the (possibly package-qualified) parent strings for free.

The role-body `does` site (`Stmt::DoesDecl::args`) has no plan carriage yet by design — its typed
encoding belongs to D7's role-structure plan ops, so D4-2 covers only the class header, per the
design doc's scoping.

No consumer reads `parent_arg_chunks` outside a new compiler unit test yet — that is D4-3
(registration cutover: `resolve_role_candidate` gains a `pre_args: Option<&[Value]>` fast path
that evaluates these chunks instead of re-parsing the concatenated string through
`eval_role_arg_values`).
