# ADR-0019 D4-1: bracketed `is`/`does`/`hides` parents also captured as parsed expressions

`is Parent[Args]`/`does Role[Args]`/`hides Parent[Args]` bracket content has always been captured
as raw balanced-bracket source text, concatenated onto the parent name into `parents`/
`does_parents`/`hidden_parents`. That concatenated string is re-parsed and tree-walked per
argument, per registration, by `eval_role_arg_values` — the target of a future cutover
(D4-3/D7-3).

This slice is purely additive groundwork: the parser also parses the bracket content as a real,
comma-separated expression list with the expression parser, riding alongside the unchanged
string in two new AST fields:

- `Stmt::ClassDecl::parent_args: Vec<(String, Vec<Expr>)>`, keyed by the same full concatenated
  parent string that already appears in `parents`/`does_parents`/`hidden_parents`.
- `Stmt::DoesDecl::args: Option<Vec<Expr>>`, for the role-body synthetic `does` parents (a
  `Stmt::RoleDecl` body has no parent field of its own — parents travel as body `DoesDecl`s).

An entry is present only when the bracket content parses cleanly as a complete expression list
(`parse_bracket_arg_exprs`, a new free function in `class_decl.rs`); on any parse failure the
string remains the sole source of truth, so nothing the existing balanced-bracket scan accepted
is ever rejected. Nine call sites needed the new capture: the class-body `is`/`does`/`hides`
loop, `unit class`'s `is` clause, `unit role`'s `does` clause, and grammar's `is`/`does` clauses
(two more than the seven the original design doc enumerated — `grammar`'s own `is`/`does` loop
desugars to a `ClassDecl` just like a plain class). `augment class`'s bracket sites and the
body-level `does R;`/`also does R;` forms stay string-only, since neither ever parses bracket
content today.

One correctness wrinkle: `compiler/stmt.rs`'s `qualify_decl_name` (the `unit class`/`unit
module` package-qualification pass, which rewrites bare parent names to `Pkg::Name`) had to
re-key `parent_args` through the same `qualify_parent` closure it already applies to `parents`/
`does_parents`/`hidden_parents` — otherwise a future lookup by the (now-qualified) parent string
would silently miss.

No consumer reads either new field yet — that is D4-2 (compiler: lower the exprs into compiled
chunks on the class plan) and D4-3/D7-3 (registration cutover), tracked separately. Verified
with four new parser unit tests and a full `make test` run (27962 tests, PASS).
