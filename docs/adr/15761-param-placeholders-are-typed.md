# ADR-15761: Parameter placeholders are read from `ParamDef.target`, never recognised by name

- **Status**: Proposed
- **Date**: 2026-11-08
- **Deciders**: Claude (lane:rakuast worker 19), for the maintainer's review
- **Issue**: [#15761](https://github.com/tokuhirom/mutsu/issues/15761) (reading (A), family 1: the `ParamDef` placeholders)
- **Related**: [ADR-10723](10723-rakuast-is-the-frontend-ir.md) Stage 2, [ADR-15334](15334-stage-2-parser-builds-typed-rakuast-nodes.md) Amendments 4 and 5

## 1. Context

A parameter whose source form has no variable name is executed under a **placeholder name**:
`__subsig__` for `(...)`, `@` for `[...]`, `__type_only__` for `sub f(Int)`, `__type_capture__T`
for a bare `::T`, `__literal__` for a literal pattern, `__ANON_*` for anonymous sigil parameters.
[`ParamTarget`](../../src/ast.rs) already records which form the placeholder stands for, and the
RakuAST view reads `target`, so `convert` never recognises a name (ADR-10723 Stage 2).

The execution layer still does. Measured 2026-11-08 on `main`, the placeholder spellings are
compared by name in `compiler/stmt.rs` (pointy-block parameter selection), `light_closure_shape.rs`,
`parser/stmt/sub/param_list.rs` (twice), `rakuast/pointy_bind.rs`, `value/signature.rs`
(`is_anonymous_param_name`), `runtime/main_usage.rs` and the parser's two `__literal__` makers.
Every consumer carries its own subset of the list, so a new placeholder form has to be added to
all of them and one missed site silently turns an anonymous parameter into a variable named
`__subsig__` (a wrong `.gist`, a bogus "declared twice", or a lexical that leaks into a
closure's captured set).

## 2. Decision

1. **One definition.** `ast` owns the placeholder vocabulary: the constants (`PARENS_PARAM`,
   `BRACKETS_PARAM`, `TYPE_ONLY_PARAM`, `TYPE_CAPTURE_PREFIX`, plus `LITERAL_PARAM`) and exactly
   two predicates:
   - `ParamDef::is_placeholder(&self)` (a later slice; not yet defined) — for every consumer that holds a `ParamDef`; it is
     defined over the placeholder name in the first slice and moves to `target` once the binding
     keys no longer depend on the name;
   - `ast::is_param_placeholder_name(&str)` — the single name-based test, for the consumers that only
     ever receive a name (a `VarDecl` name in the compiler, `SigParam` rendering in `value`).
2. **No consumer spells a placeholder.** `src/compiler`, `src/vm`, `src/runtime`, `src/value`,
   `src/light_closure_shape.rs` and `src/rakuast` call the predicates; only `src/ast.rs` and the
   parser makers contain the spellings. A shrinking ratchet (like
   `check-frontend-debt`) counting the quoted spellings outside those files follows once the remaining
   name-only consumers are gone.
3. **`target` must be total before it is read.** Today `ParamTarget::of_name` derives a form only for the
   anonymous sigils; `__literal__` has no `ParamTarget` at all, and `rakuast::lower` rewrites a
   parameter's name after construction. A consumer reads `target` only once every constructor and every
   such rewrite sets it (a `ParamTarget::Literal` is added with that slice), or its verdict would differ
   from the name's.
4. **Name-only consumers shrink over time.** Where a consumer can be handed the `ParamDef` (or a
   `ParamTarget`) instead of a name, it is; the compiler's pointy-parameter selection is the first
   (its `VarDecl` already originates from a `ParamDef`). `is_param_placeholder_name` is the residue that
   a later slice removes by carrying the target on `SigParam`.
5. **No behaviour change, no cache bump in slice 1.** Placeholder *names* stay as they are in this
   family's first slices (they are the binding keys for destructuring); only who recognises them
   changes. A slice that changes a name or the serialized `ParamDef` shape bumps the precompilation
   cache version in the same PR.

## 3. Consequences

- Adding a placeholder form touches `ast` only; the ratchet fails a consumer that spells one.
- The remaining three families of #15761 (`__mutsu_assign_*_lvalue`, `SyntheticBlock` variants
  carrying a real scope, the `"v6"` pseudo-module) keep their own ADRs; this one does not decide them.

## 4. Alternatives rejected

- **Make the placeholder an empty name plus `target`.** The binding machinery keys destructuring
  temporaries by the placeholder, and `name.is_empty()` already means "no name written" for other
  forms (`sp.name.is_empty()` in `main_usage`); merging the two would change `.gist` and
  `Signature.params` output. Possible later, once every consumer reads `target`.
- **Leave the spellings and rely on review.** This is the status quo whose cost §1 measures.
