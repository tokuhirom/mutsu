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
        target_name: Option<&str>,
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
        // setting the implicit `$_`), so the block would see no topic. For the
        // common single-element-per-call case we set `$_` explicitly via
        // `call_compiled_closure_with_topic` (matching the interpreter); a
        // multi-arity block (`-> $a, $b { }`) consumes a *chunk* per call, where
        // a single override topic is ambiguous, so still fall back there.
        let has_pairs = items
            .iter()
            .any(|v| matches!(v, Value::Pair(..) | Value::ValuePair(..)));
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
        // Classify the body: `None` => escapes/unprovable (fall back);
        // `Some(false)` => simple read-only; `Some(true)` => simple but mutates
        // the topic `$_` (`$_++`, `$_ = …`, `$_ .= …`, bare `s///`/`tr///`).
        let mutates_topic = classify_body(&data.body)?;

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
        // A multi-arity block binds a chunk per call, where the single override
        // topic of `call_compiled_closure_with_topic` is ambiguous: keep deferring
        // pair-containing sources to the interpreter for arity > 1.
        if has_pairs && arity > 1 {
            return None;
        }
        // When the source has pair-shaped elements we set `$_`/the positional
        // param explicitly via the topic override. That override only handles a
        // pointy/bare block with no params (implicit `$_`) or a single *plain*
        // positional param. Anything else — a placeholder param (`$^a`, which
        // makes arg binding raise "Missing required placeholder"), an aggregate
        // param, or a full signature (`param_defs` populated) — would mis-bind or
        // error, so defer those to the interpreter.
        if has_pairs
            && !(data.param_defs.is_empty()
                && match data.params.as_slice() {
                    [] => true,
                    [p] => super::vm_closure_dispatch::is_plain_positional_param(p),
                    _ => false,
                })
        {
            return None;
        }

        // rw binding: a `$_`-mutating block writes back to the source element
        // (`@a.map({ $_++ })` mutates `@a`). That needs a concrete `@`-array
        // variable to write to (the mut method opcode gives us `target_name`),
        // single element per call, and non-pair elements (writing a mutated pair
        // back is out of scope). Without a writeback target a topic mutation
        // cannot be reproduced by the clone-based loop, so defer to the
        // interpreter.
        let writeback_name = if mutates_topic {
            match target_name {
                Some(name) if name.starts_with('@') && arity == 1 && !has_pairs => {
                    Some(name.to_string())
                }
                _ => return None,
            }
        } else {
            None
        };

        let block = args[0].clone();
        let mut result: Vec<Value> = Vec::with_capacity(items.len() / arity + 1);
        // Mutable copy of the source, updated from each captured `$_` for the rw
        // writeback (only allocated when a writeback target exists).
        let mut source_after: Vec<Value> = if writeback_name.is_some() {
            items.to_vec()
        } else {
            Vec::new()
        };
        let mut i = 0usize;
        while i < items.len() {
            let chunk: Vec<Value> = items[i..i + arity].to_vec();
            // For a Pair element (arity == 1) the general call machinery would
            // bind it as a named arg and skip `$_`; force it as the topic.
            let explicit_topic =
                if has_pairs && matches!(chunk[0], Value::Pair(..) | Value::ValuePair(..)) {
                    Some(chunk[0].clone())
                } else {
                    None
                };
            let call = if explicit_topic.is_some() || writeback_name.is_some() {
                if writeback_name.is_some() {
                    self.rw_map_topic_capture = None;
                }
                self.vm_call_map_block(&block, chunk, explicit_topic, writeback_name.is_some())
            } else {
                self.vm_call_on_value(block.clone(), chunk, None)
            };
            let value = match call {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            // Capture the block's final `$_` back into the source element.
            if writeback_name.is_some()
                && let Some(mutated) = self.rw_map_topic_capture.take()
            {
                source_after[i] = mutated;
            }
            match value {
                Value::Slip(elems) => result.extend(elems.iter().cloned()),
                v => result.push(v),
            }
            i += arity;
        }

        // Write the mutated source array back to its variable (Raku rw binding).
        // The mut method opcode sets `env_dirty` after we return, which re-syncs
        // the variable's local slot from this env update. A fresh array Arc loses
        // the source's per-Arc element-type metadata (`my Int @a` → `Array[Int]`),
        // so re-register it on the new container.
        if let Some(name) = writeback_name {
            let meta = self.interpreter.container_type_metadata(target);
            let new_array = Value::real_array(source_after);
            if let Some(info) = meta {
                self.interpreter
                    .register_container_type_metadata(&new_array, info);
            }
            self.set_env_with_main_alias(&name, new_array);
        }

        // On a real array, mutsu's `.map` yields a List-kind array (matching the
        // interpreter's return shape).
        Some(Ok(Value::Array(
            std::sync::Arc::new(result),
            ArrayKind::List,
        )))
    }
}

/// Classify a map block body for the native fast path.
///
/// Returns:
/// - `None` — the body can escape the loop (`return`/`last`/`take`/phaser/…) or
///   contains a form this scanner cannot prove safe; the caller must fall back.
/// - `Some(false)` — simple and does NOT mutate the topic `$_` (the common
///   read-only map block; runnable with the clone-based loop unconditionally).
/// - `Some(true)` — simple but mutates `$_` (`$_++`/`$_--`, `$_ = …`, `$_ .= …`,
///   bare `s///`/`tr///`). map rw-aliases `$_` to the source element, so the
///   caller only runs such a block natively when it can write the mutation back.
fn classify_body(stmts: &[Stmt]) -> Option<bool> {
    let mut mutates = false;
    for s in stmts {
        mutates |= classify_stmt(s)?;
    }
    Some(mutates)
}

fn classify_exprs(exprs: &[Expr]) -> Option<bool> {
    let mut mutates = false;
    for e in exprs {
        mutates |= classify_expr(e)?;
    }
    Some(mutates)
}

fn classify_stmt(stmt: &Stmt) -> Option<bool> {
    match stmt {
        // A bare `s///` / `tr///` statement mutates the topic `$_` in place.
        Stmt::Expr(Expr::Subst { .. }) | Stmt::Expr(Expr::Transliterate { .. }) => Some(true),
        Stmt::Expr(e) => classify_expr(e),
        Stmt::VarDecl { expr, .. } => classify_expr(expr),
        // Assigning to the topic `$_` (`$_ = …`, `$_ ~= …`, `$_ .= …`) mutates
        // the source element; assignments to other (captured/outer) names are
        // handled by the closure's free-var writeback and are not a topic mutation.
        Stmt::Assign { name, expr, .. } => Some(classify_expr(expr)? || name == "_"),
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => classify_exprs(es),
        // A line-number marker for diagnostics; neither escapes nor mutates.
        // Pointy/`Lambda` block bodies carry these (placeholder blocks don't),
        // so accepting them lets `-> $a { ... }` map natively too.
        Stmt::SetLine(_) => Some(false),
        // Self-contained nested control structures: safe as long as their bodies
        // are simple (a `last`/`next` inside them is over-conservatively rejected
        // by the leaf rules below, which is fine).
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => Some(classify_expr(cond)? | classify_body(then_branch)? | classify_body(else_branch)?),
        Stmt::Block(body) | Stmt::SyntheticBlock(body) => classify_body(body),
        // Everything else — control flow (`return`/`last`/`next`/`redo`/`take`/
        // `goto`/`proceed`/`succeed`), phasers, declarations, loops, given/when,
        // etc. — is treated as an escape: fall back to the interpreter.
        _ => None,
    }
}

fn classify_expr(expr: &Expr) -> Option<bool> {
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
        | Expr::MatchRegex(_) => Some(false),
        // A nested `s///` / `tr///` (e.g. under `~~`) targets whichever value the
        // surrounding construct aliases to `$_`, which we cannot determine here;
        // be conservative. Only a *statement-level* `s///`/`tr///` is recognized
        // as a topic mutation (see `classify_stmt`).
        Expr::Subst { .. } | Expr::Transliterate { .. } => None,
        // Control-flow expressions escape the loop.
        Expr::ControlFlow { .. } => None,
        Expr::Unary { op, expr } | Expr::PostfixOp { op, expr } => {
            if matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus) {
                // `$_++` / `$_--` mutate the topic; an increment of any other
                // variable cannot be reproduced by the clone-based loop.
                match expr.as_ref() {
                    Expr::Var(n) if n == "_" => Some(true),
                    _ => None,
                }
            } else {
                classify_expr(expr)
            }
        }
        Expr::Reduction { expr, .. }
        | Expr::PositionalPair(expr)
        | Expr::ZenSlice(expr)
        | Expr::IndirectTypeLookup(expr) => classify_expr(expr),
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => Some(classify_expr(left)? | classify_expr(right)?),
        Expr::InfixFunc { left, right, .. } => Some(classify_expr(left)? | classify_exprs(right)?),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Some(classify_expr(cond)? | classify_expr(then_expr)? | classify_expr(else_expr)?),
        Expr::Index { target, index, .. } => Some(classify_expr(target)? | classify_expr(index)?),
        Expr::MethodCall { target, args, .. }
        | Expr::DynamicMethodCall { target, args, .. }
        | Expr::HyperMethodCall { target, args, .. }
        | Expr::HyperMethodCallDynamic { target, args, .. } => {
            Some(classify_expr(target)? | classify_exprs(args)?)
        }
        Expr::CallOn { target, args } => Some(classify_expr(target)? | classify_exprs(args)?),
        Expr::Call { args, .. } => classify_exprs(args),
        Expr::StringInterpolation(items)
        | Expr::ArrayLiteral(items)
        | Expr::BracketArray(items, _)
        | Expr::CaptureLiteral(items) => classify_exprs(items),
        Expr::Hash(items) => {
            let mut mutates = false;
            for (_, val) in items {
                if let Some(v) = val {
                    mutates |= classify_expr(v)?;
                }
            }
            Some(mutates)
        }
        // Anything else (do-blocks, gather, lambdas, try, symbolic deref, …) may
        // embed statements or reflective behavior we cannot vet here: fall back.
        _ => None,
    }
}
