# ADR-0137: AST analyses walk the AST through one typed visitor, never its serialized form

- **Status**: Accepted (implemented for the four serde-based analyses; hand-rolled walkers
  port opportunistically)
- **Date**: 2026-10-01
- **Related**: [#10441](https://github.com/tokuhirom/mutsu/issues/10441),
  [ADR-0113](0113-frame-lexical-inner-subs.md) (frame-lexical proof)

## Context

Four compile- and registration-time analyses answered yes/no questions about a subtree by
serializing it with `serde_json` and walking or grepping the JSON: the frame-lexical proof
(`compiler/frame_lexical_ast_scan.rs`, `frame_lexical_routines.rs`), the parameterized-role
"does this statement mention a role parameter?" check
(`runtime/registration_role_body_lexical.rs`) and TRIR inlining's "does this body mention
`return`?" (`trir/compile/inline.rs`).

That allocated a full JSON copy of the subtree per question, and it could not tell an identifier
from a string literal with the same text: `say "EVAL"` disqualified a routine from the
frame-lexical treatment, `my constant X is export = "T"` deferred a declaration in `role R[::T]`
to composition, and `say "return"` stopped an inline. The JSON shape was also an untyped,
implicit contract with the AST definition: a renamed variant silently changed every answer.
Besides those, about seven hand-rolled walkers each recurse over the AST in their own way.

## Decision

`src/ast_visit/` holds one read-only visitor, `trait Visit`, with hooks `visit_stmt`,
`visit_expr`, `visit_param`, `visit_regex_node` and `visit_name`, and the matching `walk_*`
functions that supply the default recursion.

1. **Exhaustive walks.** Every `walk_*` function destructures every variant with every field
   named — no `_ =>` arm, no `..` rest pattern — so adding an AST variant or field is a compile
   error in the walker rather than a silent miss in every analysis.
2. **Typed names.** Every string field that holds an identifier is reported through
   `visit_name(name, NameKind)`, where `NameKind` says what position it is in (call, sub
   declaration, `&name`, `$name`, type, method, trait, module, label, operator, regex name,
   source text compiled later, symbolic lookup, ...). Literal data — string literals, hash and
   named-argument keys, `tr///` tables, messages, version strings — is never reported.
3. **New analyses use it.** An analysis that needs to inspect a subtree implements `Visit`
   instead of serializing the AST or writing another recursive walker. `serde_json` stays for real
   JSON I/O only (META6.json, the profiler report, the wasm bridge, the precomp format).

The visitor is not mutable (`VisitMut`): no analysis that motivated it rewrites the tree, and a
rewriting pass has different needs (ownership, re-validation of `ParamCode`). A mutable variant is
a separate decision when the first rewriting pass wants one.

## Consequences

- The four serde-based analyses run without allocating a copy of the tree, and string literals no
  longer flip their answers (pinned by unit tests next to each analysis and by
  `t/modules/import-export/param-role-body-exported-class-and-constant.t`).
- Positions that name code by text compiled later (`s///` replacements, regex code-block text)
  are reported as `NameKind::Source`; analyses treat a whole-word occurrence in them as a mention,
  which is stricter than the old exact-leaf comparison.
- The remaining hand-rolled walkers (`parser/outer_redecl`, `parser/sink_warn.rs`,
  `parser/whenever_scope.rs`, `runtime/begin_prologue/nested.rs`,
  `runtime/eval_routine_magicals.rs`, `runtime/undeclared_routines.rs`, ...) keep working and are
  ported when next touched.
