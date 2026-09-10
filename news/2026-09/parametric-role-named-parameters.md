# A parametric role with named parameters can be instantiated

`role R[Str:D :$v] { method x { $v } }` composed as `C.new but R[:v<hi>]`
died with `No matching candidate found for the parametric role` — a role
parameterised by a **named** parameter never matched, so neither `but
R[:v(...)]` nor `does R[:v(...)]` worked, while a positional parameter
(`role R2[::T]`, `role R3[$v]`) bound fine.

Two separate bugs, both rooted in the same place: a `[...]` bracket used to
parameterise a role is structurally a call's argument list, and Rakudo
decides an argument's named-ness from call-site *syntax* (colon-pair vs.
`=>`), not from the flavour of `Pair` the expression happens to evaluate to
(ADR-0021) — a function call already normalizes this at the call boundary,
but a subscript's bracket content never did.

1. **Candidate matching never saw the named argument.** `:v<hi>` inside
   `R[...]` parses to the same bare `Binary{FatArrow}` AST node a colon-pair
   always does (verified via `--dump-ast`; an explicit `R["v" => "hi"]`
   parses to a distinguishing `PositionalPair(...)` wrapper instead). But
   `Expr::Index`'s bracket content was compiled through the fully generic
   `compile_expr`, which emits the data-default `MakePair` for any bare
   `Binary{FatArrow}` — producing a `ValuePair`, invisible to
   `role_candidate_arity_ok`'s named-argument scan (`is_string_pair_value`).
   The role's only candidate then looked like it took one *positional*
   argument it never got, and matching failed outright.

   Fixed in `Compiler::compile_subscript_index`
   (`src/compiler/expr_data.rs`): a bare `Binary{FatArrow}` index (or a
   comma-list containing one) now mints the named `Pair` flavour via
   `mint_named_pair`/`OpCode::MakeNamedArg`, the same mechanism a function
   call's argument-list compile already uses for `f(:v<hi>)`. Verified this
   doesn't change Rakudo's own distinction: `R["v" => "hi"]` (an explicit
   arrow, wrapped in `PositionalPair` by the parser) still fails to match,
   exactly like Rakudo.

2. **Even once matching succeeded, the bound value was wrong.**
   `compose_role_on_value` (`src/runtime/types/roles.rs`) built the role's
   per-parameter env bindings with `param_names.iter().zip(role_args.iter())`
   — a raw positional zip over the *original*, unresolved call-site
   arguments. For `R[:v<hi>]` that is call-site order (one `Pair`), not the
   value `resolve_role_candidate_with_args` had already correctly bound by
   name via `bind_function_args_values`. `$v` inside the role body ended up
   holding the whole `v => "hi"` Pair instead of `"hi"`.

   Fixed by reusing `resolve_role_candidate_with_args`'s own resolved
   per-parameter values (the third element of its returned tuple) for this
   zip, falling back to the raw `role_args` only when no candidate list
   exists (a role with one unconditional definition, where the order is
   already positional by construction).

Verified against `raku`: the repro, the `does` spelling (no sink-context
warning), a `:v(...)` colon-pair-with-parens form, a mixed
positional+named role signature, both positional-parameter controls,
Rakudo's own explicit-arrow-does-not-bind-named behavior, a plain
positional comma-list role (unaffected by the named-detection), and plain
array/hash subscripts (unaffected by the shared compiler change). Pinned by
`t/issue-7757-parametric-role-named-params.t`. All 73 existing
`t/*role*`/`t/*parametric*`/`t/*does*` files (667 assertions) and all 131
existing `t/*index*`/`t/*subscript*`/`t/*colonpair*`/`t/*pair*` files (1476
assertions) continue to pass unchanged.

Closes #7757.
