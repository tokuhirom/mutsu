//! Native `.map` over a concrete array with a simple block.
//!
//! `@a.map({ ... })` previously always fell back to the interpreter's
//! `dispatch_map_method` orchestration (see docs/vm-decoupling.md, lever A). The
//! block *body* already runs compiled on the VM (`vm_call_on_value` ->
//! `call_compiled_closure`); only the surrounding iteration loop lived in the
//! interpreter. This runs that loop in the VM for the common, simple case
//! (including multi-arity blocks like `-> $a, $b { ... }`, which consume the
//! source in `arity`-sized chunks) and falls back for anything that needs the
//! interpreter's richer orchestration (Slip/phaser/lazy-`return` handling,
//! `next`/`last`/`take` control flow, `.assuming`/composed blocks, non-array
//! targets, pair-shaped elements, a short final chunk, …).
//!
//! Eligibility is intentionally conservative: when the block body contains any
//! construct that could escape the map loop (a `return`, loop control,
//! `take`/`emit`, a phaser, …) — or any expression form this scanner does not
//! explicitly recognize as safe — we fall back. A false "safe" verdict would
//! silently produce a wrong result, so unknown forms default to "not simple".

use super::*;
use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;

impl VM {
    /// Try to run `target.map(block)` natively. Returns `Some(result)` when
    /// handled in the VM, `None` to fall back to the interpreter unchanged.
    ///
    /// Only `.map` is handled here, not `.grep`: `.grep` returns a subset of the
    /// *original* elements that must stay rw-view-bound to the source array
    /// (`@a.grep(...)>>++` updates `@a`), which the interpreter's aggregate
    /// binding preserves and a freshly-built result array cannot.
    pub(super) fn try_native_array_map(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "map" {
            return None;
        }
        // Exactly one positional Sub argument.
        if args.len() != 1 {
            return None;
        }
        let Value::Sub(data) = &args[0] else {
            return None;
        };
        // Only a plain concrete array (`my @a = ...`, `ArrayKind::Array`). On
        // such an array mutsu's `.map` returns a `List`. Every other kind needs
        // the interpreter's specialized dispatch and must NOT come here:
        // `Shaped` maps over *leaves* (not the raw items), `Lazy` must stay lazy
        // (eager iteration could hang on an infinite source), `ItemArray`/`List`
        // and ranges/seqs have their own one-arg-rule / Seq-returning semantics.
        // (`ArrayKind::is_real_array()` is too broad — it also matches Shaped,
        // Lazy and ItemArray — so match the kind explicitly.)
        let items = match target {
            Value::Array(items, ArrayKind::Array) => items.clone(),
            _ => return None,
        };
        // A `Pair`/`ValuePair` element passed positionally to the block is bound
        // as a *named* argument by the closure-call machinery (and skipped when
        // setting the implicit `$_`), so the block would see no topic. The
        // interpreter's map sets `$_` explicitly; until the native path does the
        // same, fall back when the source contains pair-shaped elements.
        if items
            .iter()
            .any(|v| matches!(v, Value::Pair(..) | Value::ValuePair(..)))
        {
            return None;
        }
        // The block must be a plain single-arity closure with no signature
        // complexity and no `.assuming`/compose wrapping.
        if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
            return None;
        }
        if data.env.contains_key("__mutsu_routine_name")
            || data.env.contains_key("__mutsu_compose_left")
            || data.env.contains_key("__mutsu_compose_right")
        {
            return None;
        }
        let requires_full_binding = data.param_defs.iter().any(|pd| {
            pd.named
                || pd.slurpy
                || pd.sigilless
                || pd.optional_marker
                || pd.default.is_some()
                || pd.type_constraint.is_some()
                || pd.where_constraint.is_some()
                || pd.sub_signature.is_some()
                || pd.outer_sub_signature.is_some()
                || pd.code_signature.is_some()
                || pd.shape_constraints.is_some()
        });
        if requires_full_binding {
            return None;
        }
        // The body must not contain anything that escapes the loop (return / loop
        // control / take / emit / phaser), nor any expression form we cannot
        // prove safe.
        if !body_is_simple(&data.body) {
            return None;
        }

        // Each call consumes `arity` consecutive items. A 0-param block uses the
        // implicit `$_` (arity 1); a multi-arity block (`-> $a, $b { }`) chunks
        // the source. When the source length isn't a multiple of the arity the
        // last chunk is short, which the interpreter (and raku) treat as an
        // error ("Not enough elements" / "Too few positionals"); defer those so
        // the error path stays in one place.
        let arity = data.params.len().max(1);
        if arity > 1 && !items.len().is_multiple_of(arity) {
            return None;
        }

        let block = args[0].clone();
        let mut result: Vec<Value> = Vec::with_capacity(items.len() / arity + 1);
        let mut i = 0usize;
        while i < items.len() {
            let chunk: Vec<Value> = items[i..i + arity].to_vec();
            let value = match self.vm_call_on_value(block.clone(), chunk, None) {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            match value {
                Value::Slip(elems) => result.extend(elems.iter().cloned()),
                v => result.push(v),
            }
            i += arity;
        }

        // On a real array, mutsu's `.map` yields a List-kind array (matching the
        // interpreter's return shape).
        Some(Ok(Value::Array(
            std::sync::Arc::new(result),
            ArrayKind::List,
        )))
    }
}

/// True when every statement in `stmts` is safe to execute inside a native
/// map loop: nothing can escape the loop (`return`/`last`/`take`/…) and nothing
/// mutates the topic `$_`. The latter matters because map's `$_` is rw-aliased
/// to the source element, so a `$_`-mutating block writes back to the array —
/// which the clone-based native loop cannot reproduce.
fn body_is_simple(stmts: &[Stmt]) -> bool {
    stmts.iter().all(stmt_is_simple)
}

fn stmt_is_simple(stmt: &Stmt) -> bool {
    match stmt {
        // Plain value-producing statements: recurse into their expression.
        Stmt::Expr(e) => expr_is_simple(e),
        Stmt::VarDecl { expr, .. } => expr_is_simple(expr),
        // Assigning to the topic `$_` (`$_ = …`, `$_ ~= …`, `$_ .= …`) mutates
        // the source element; only assignments to other (captured/outer) names
        // are safe.
        Stmt::Assign { name, expr, .. } => name != "_" && expr_is_simple(expr),
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            es.iter().all(expr_is_simple)
        }
        // A line-number marker for diagnostics; it neither escapes the loop nor
        // mutates the topic. Pointy/`Lambda` block bodies carry these (placeholder
        // blocks don't), so accepting them lets `-> $a { ... }` map natively too.
        Stmt::SetLine(_) => true,
        // Self-contained nested control structures: safe as long as their bodies
        // are simple (a `last`/`next` inside them is over-conservatively rejected
        // by the leaf rules below, which is fine).
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => expr_is_simple(cond) && body_is_simple(then_branch) && body_is_simple(else_branch),
        Stmt::Block(body) | Stmt::SyntheticBlock(body) => body_is_simple(body),
        // Everything else — control flow (`return`/`last`/`next`/`redo`/`take`/
        // `goto`/`proceed`/`succeed`), phasers, declarations, loops, given/when,
        // etc. — is treated as an escape: fall back to the interpreter.
        _ => false,
    }
}

fn expr_is_simple(expr: &Expr) -> bool {
    match expr {
        // Leaves with no embedded statements or control flow.
        Expr::Literal(_)
        | Expr::Var(_)
        | Expr::ArrayVar(_)
        | Expr::HashVar(_)
        | Expr::CodeVar(_)
        | Expr::CaptureVar(_)
        | Expr::BareWord(_)
        | Expr::Whatever
        | Expr::HyperWhatever
        | Expr::EnvIndex(_)
        | Expr::NonDestructiveSubst { .. }
        | Expr::MatchRegex(_) => true,
        // `s///` / `tr///` mutate the topic `$_` in place; the native loop cannot
        // write that back to the source element.
        Expr::Subst { .. } | Expr::Transliterate { .. } => false,
        // Control-flow expressions escape the loop.
        Expr::ControlFlow { .. } => false,
        // `$_++` / `$_--` (and any pre/postfix increment) mutate in place.
        Expr::Unary { op, expr } | Expr::PostfixOp { op, expr } => {
            !matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus) && expr_is_simple(expr)
        }
        Expr::Reduction { expr, .. }
        | Expr::PositionalPair(expr)
        | Expr::ZenSlice(expr)
        | Expr::IndirectTypeLookup(expr) => expr_is_simple(expr),
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => expr_is_simple(left) && expr_is_simple(right),
        Expr::InfixFunc { left, right, .. } => {
            expr_is_simple(left) && right.iter().all(expr_is_simple)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => expr_is_simple(cond) && expr_is_simple(then_expr) && expr_is_simple(else_expr),
        Expr::Index { target, index, .. } => expr_is_simple(target) && expr_is_simple(index),
        Expr::MethodCall { target, args, .. }
        | Expr::DynamicMethodCall { target, args, .. }
        | Expr::HyperMethodCall { target, args, .. }
        | Expr::HyperMethodCallDynamic { target, args, .. } => {
            expr_is_simple(target) && args.iter().all(expr_is_simple)
        }
        Expr::CallOn { target, args } => expr_is_simple(target) && args.iter().all(expr_is_simple),
        Expr::Call { args, .. } => args.iter().all(expr_is_simple),
        Expr::StringInterpolation(items)
        | Expr::ArrayLiteral(items)
        | Expr::BracketArray(items, _)
        | Expr::CaptureLiteral(items) => items.iter().all(expr_is_simple),
        Expr::Hash(items) => items
            .iter()
            .all(|(_, val)| val.as_ref().is_none_or(expr_is_simple)),
        // Anything else (do-blocks, gather, lambdas, try, symbolic deref, …) may
        // embed statements or reflective behavior we cannot vet here: fall back.
        _ => false,
    }
}
