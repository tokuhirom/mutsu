# ADR-0019 D4 design: parent/role bracket arguments as parsed expressions

Design pass (2026-08-08, no code landed) detailing the D4-1/D4-2/D4-3 sub-boxes the 2026-08-08
scoping pass named for the one live D4 piece: `is Parent[Args]`/`does Role[Args]`/`hides
Parent[Args]` bracket content is captured as raw balanced-bracket source text, string-concatenated
onto the parent name, and re-parsed + tree-walked **per argument, per registration** by
`eval_role_arg_values`.

## Current state (2026-08-08 survey)

- `parse_optional_bracket_suffix` (`parser/stmt/class/class_decl.rs:60-79`) scans by `[`/`]`
  depth only (no quote awareness) and returns the raw text including brackets. Concat sites:
  `class_decl.rs:416-417` (`is`), `:459-460` (lowercase `is` fallback), `:474-477` (`does`,
  into both `parents` and `does_parents`), `:486-489` (`hides`), plus three the original ADR
  note missed: `class_decl.rs:237-238`/`:247` (augment), `role_decl.rs:311-313` (role `does`,
  folded into a synthetic `Stmt::DoesDecl { name: Symbol }` in the body at `:422-429`), and
  `package_decl.rs:215-216` (unit-class `is`).
- AST carriers are plain strings: `Stmt::ClassDecl { parents, hidden_parents, does_parents:
  Vec<String> }` (`ast.rs:1012-1034`); `Stmt::RoleDecl` has **no** parent field at all (parents
  travel as body `DoesDecl`s). `CompiledClassDeclPlan` mirrors the strings
  (`opcode.rs:2507-2512`); `CompiledRoleDeclPlan` has no parent fields.
- `resolve_role_candidate` (`registration_role.rs:134-262`) splits the bracket text back out
  (`split_balanced_comma_list`, `runtime/mod.rs:84-107` — tracks `(`/`[` depth but **not quotes
  or braces**, so `R["a,b"]` mis-splits) and `eval_role_arg_values` (`registration_role.rs:
  18-74`) re-parses each argument string with `parse_source` + `eval_block_value`, with four
  text-based heuristics: the `::`-prefix rejection, `should_treat_role_arg_as_type_expr` (char
  whitelist + `:`/`(`/`::` presence → treat as type name, with an enum-value probe and a
  never-registry-checked `Value::package(text)` fallback), and the `{...}` → `({...})`
  block-literal paren wrap.
- Call sites: three declaration-origin (`registration_class_compose.rs:88`,
  `registration_role_body.rs:212` and `:268`) and two genuinely dynamic that must keep the
  string path forever (`registration_class_augment.rs:985` — pun-name round-trip, which already
  demonstrates the lossiness by having to guard `resolved_args != type_args`;
  `methods_qualified.rs:291` — `$obj.R[Int]::meth` concretization strings).

## Design

**D4-1 — parser: capture `Vec<Expr>` alongside the string (additive, no behavior change).**
`parse_optional_bracket_suffix` additionally attempts to parse the bracket content as a
comma-separated expression list with the real expression parser. On success, the parsed
`Vec<Expr>` rides in new AST fields — `Stmt::ClassDecl::parent_args: Vec<(String,
Vec<Expr>)>` keyed by the full concatenated parent string (covering `is`/`does`/`hides`
uniformly), and an `args: Option<Vec<Expr>>` payload on `Stmt::DoesDecl` for the role-side
synthetic parents. On parse failure the field is simply absent — the string path remains
authoritative, so nothing the depth-scan accepted is ever rejected. The concatenated string is
kept **unchanged** as the registry key and display name. This also sidesteps
`split_balanced_comma_list`'s quote-blindness for every declaration that gets `Expr`s: the real
parser splits `R["a,b"]` correctly (a latent bug class the string path cannot fix).

**D4-2 — compiler: lower argument chunks onto the class plan.** For each `parent_args` entry,
compile each `Expr` with `compile_decl_trait_arg` (literal short-circuit + `Compiled` chunks,
the C5 mechanism) into `CompiledClassDeclPlan::parent_arg_chunks: Vec<(String,
Vec<DeclTraitArg>)>`, keyed by the same full parent string. The role-body `DoesDecl` site has
no plan carriage yet — its typed encoding belongs to D7's role-structure plan ops (see
`todo/deep/adr0019-d7-d8-role-plan-encoding.md`), so D4-2 deliberately covers only the class
header; the role-body cutover joins the D7 slices to avoid inventing a throwaway plan field.

**D4-3 — registration cutover for the class-header site.** Least-invasive signature change per
the survey: `resolve_role_candidate` gains `pre_args: Option<&[Value]>` (or a
`_with_args` wrapper keeping the 1-arg form). Only the `eval_role_arg_values` call swaps:
`Some(v) => v.to_vec()`; everything downstream (arity filter, trial `bind_function_args_values`,
specificity sort, winner re-bind) already operates on `Vec<Value>`. `compose_class_parent_roles`
evaluates the plan chunks via `eval_decl_trait_arg` and passes the values; the four existing
string-path callers pass `None` unchanged. The text-based heuristics are naturally bypassed on
the value path (a parsed `Expr` argument cannot be the `::T`-in-application error case — that
shape fails the D4-1 parse and stays on the string path, where the check still fires).

## Verification and risks

The behavioral-parity risk the scoping pass flagged concentrates in `eval_role_arg_values`'s
heuristics: the enum-value probe and the `Value::package(text)` type-name fallback mean the
string path sometimes produces a *type object* where a naive `Expr` evaluation would produce a
*value* (or an error). D4-3 must therefore make the chunk evaluation reproduce the observable
outcomes, not the mechanism: build a case table against `raku` covering type names
(`R[Int]`), nested parameterizations (`R[Array[Int]]`), enum values, literals
(`R[42]`, `R["a,b"]`), blocks (`R[{ $_ * 2 }]`), `where` types, and named args (`R[:x(1)]`),
run under both paths, and gate the cutover on table equality (plus `t/`, roast S14-roles, and
the battery gate — parametric roles are load-bearing for Cro). Sequencing: D4-1 and D4-2 are
independently landable with zero behavior change; D4-3 is the only risky slice and can soak
behind the case table.
