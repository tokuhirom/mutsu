//! Native `.map` over a concrete array for a `$_`-mutating / rw-param block.
//!
//! This loop exists for exactly one thing the shared map loop
//! (`runtime/resolution_map_grep_rw.rs::eval_map_over_items_rw`) cannot do:
//! Raku's rw binding of `$_` to the source element, i.e. `@a.map({ $_++ })` and
//! `@a.map(-> $x is rw { $x++ })` mutating `@a`. It captures the block's final
//! `$_` directly (`rw_map_topic_capture`) instead of relying on the shared
//! loop's `__mutsu_rw_map_topic__` assignment mirror, which is why it — and only
//! it — also covers prefix `++$_`/`--$_` and `tr///`.
//!
//! ADR-0058 §9.4: this loop used to run AT the `.map` call, which made
//! `@a.map({ $_++ })` eager where rakudo is lazy. It now runs from the
//! deferred Seq's pull (`Interpreter::pull_rw_map`) and hands its writeback
//! back to the caller to publish, instead of re-binding the receiver's name
//! itself — at pull time that frame is gone.
//!
//! **A read-only map block must NOT come here.** It runs 4-7.6x slower than the
//! shared loop, because this one calls the general closure-call machinery once
//! per element while the shared loop compiles the body once and reuses the
//! frame. See the measured table at the `writeback_name` bail-out below. The
//! loop originally handled every simple block (docs/vm-decoupling.md Step 6, an
//! avowedly "metric-only" decoupling with no timing taken); it was narrowed to
//! the writeback cases once that cost was measured.
//!
//! Eligibility is otherwise intentionally conservative: when the block body
//! contains any construct that could escape the map loop (a `return`, loop
//! control, `take`/`emit`, a phaser, …) — or any expression form this scanner
//! does not explicitly recognize as safe — we fall back. A false "safe" verdict
//! would silently produce a wrong result, so unknown forms default to "not
//! simple".

use super::*;
use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;

/// What the native rw map loop produces: the `.map` result elements, and the
/// source elements after the block's rw write-backs (which the caller
/// publishes into the source container).
pub(crate) type NativeRwMapOutcome = Result<(Vec<Value>, Vec<Value>), RuntimeError>;

impl Interpreter {
    /// Try to run `target.map(block)` natively for a block that WRITES BACK
    /// into the source elements. Returns `Some((result elements, the source
    /// elements after the block's rw writes))` when handled here, `None` to
    /// fall back to the shared loop (`eval_map_over_items_rw`) unchanged.
    ///
    /// Called from the deferred `.map` PULL (`Interpreter::pull_rw_map`), not
    /// from the `.map` call: ADR-0058 makes every `.map` hand back a Seq whose
    /// callback runs at first consumption, so this loop runs there too. It
    /// therefore does not publish the writeback itself — the caller does, in
    /// place, because at pull time there is no frame holding the source's name
    /// to write through.
    ///
    /// Only `.map` is handled here, not `.grep`: `.grep` returns a subset of the
    /// *original* elements that must stay rw-view-bound to the source array
    /// (`@a.grep(...)>>++` updates `@a`), which the interpreter's aggregate
    /// binding preserves and a freshly-built result array cannot.
    pub(crate) fn try_native_rw_map_over(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<NativeRwMapOutcome> {
        // Exactly one positional Sub argument.
        if args.len() != 1 {
            return None;
        }
        let ValueView::Sub(data) = args[0].view() else {
            return None;
        };
        // Only a plain concrete array (`my @a = ...`, `ArrayKind::Array`).
        // Every other kind needs the interpreter's specialized dispatch and must
        // NOT come here:
        // `Shaped` maps over *leaves* (not the raw items), `Lazy` must stay lazy
        // (eager iteration could hang on an infinite source), `ItemArray`/`List`
        // and ranges/seqs have their own one-arg-rule / Seq-returning semantics.
        // (`ArrayKind::is_real_array()` is too broad — it also matches Shaped,
        // Lazy and ItemArray — so match the kind explicitly.)
        let items = match target.view() {
            ValueView::Array(items, ArrayKind::Array) => items.clone(),
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
            .any(|v| matches!(v.view(), ValueView::Pair(..) | ValueView::ValuePair(..)));
        // The block must be a plain single-arity closure with no signature
        // complexity and no `.assuming`/compose wrapping.
        if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
            return None;
        }
        if crate::runtime::resolution_map_grep::sub_is_call_carrier(&data) {
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

        // An explicit `is rw`/`is raw` scalar block param (`-> $x is rw { $x++
        // }`) also needs a writeback: Raku passes each array element's
        // container to the block, so mutating the param mutates the source
        // element, same as a `$_`-mutating block below. `requires_full_binding`
        // above already proved this is the only param (not named/slurpy/
        // sigilless/optional/defaulted/typed/…), so no further shape check is
        // needed here.
        let rw_param = (arity == 1)
            .then(|| data.param_defs.first())
            .flatten()
            .filter(|pd| pd.traits.iter().any(|t| t == "rw" || t == "raw"));

        // rw binding: a `$_`-mutating block, or an explicit rw/raw param,
        // writes back to the source element (`@a.map({ $_++ })` /
        // `@a.map(-> $x is rw { $x++ })` mutate `@a`). That needs a single
        // element per call and non-pair elements (writing a mutated pair back
        // is out of scope); the caller supplies the concrete `@`-array
        // receiver the writeback is published into.
        if mutates_topic || rw_param.is_some() {
            if arity != 1 || has_pairs {
                return None;
            }
        } else {
            // No writeback needed, so this loop has nothing the shared
            // compile-once/`run_reuse` loop (`eval_map_over_items_rw`) cannot
            // do — and it is MUCH slower at it. This loop calls the general
            // closure-call machinery once per element
            // (`call_compiled_closure_with_topic`: a scoped env child, the full
            // captured-env merge, per-instance state lookups and an exit
            // writeback diff), all of which is loop-invariant; the shared loop
            // compiles the body once and rebinds only the param/topic per
            // iteration. Measured over a 131072-element array (release build,
            // us/elem, this loop vs the shared one):
            //
            //     map({ $_ })        2.15  ->  0.28   (7.6x)
            //     map({ $_ + 1 })    2.18  ->  0.31   (7.0x)
            //     map({ $_.Int })    6.87  ->  1.23   (5.6x)
            //     map({ $_.succ })   7.20  ->  1.22   (5.9x)
            //     map({ abs($_) })   5.46  ->  1.36   (4.0x)
            //
            // Step 6 of docs/vm-decoupling.md introduced this loop as an
            // explicitly "metric-only" decoupling (to zero the `map` method
            // *fallback counter*) and took no timing at all; the shared loop is
            // not a tree-walker either — it runs the same compiled bytecode
            // through `run_reuse` — so routing read-only maps back to it costs
            // nothing but that counter.
            return None;
        }

        let block = args[0].clone();
        let mut result: Vec<Value> = Vec::with_capacity(items.len() / arity + 1);
        // Mutable copy of the source, updated from each captured `$_` for the
        // rw writeback the caller publishes.
        let mut source_after: Vec<Value> = items.to_vec();
        let mut i = 0usize;
        while i < items.len() {
            let chunk: Vec<Value> = items[i..i + arity].to_vec();
            let value = if rw_param.is_some() {
                // Same transient-`ContainerRef`-cell pattern as
                // `deepmap_leaf_call`: Raku passes the block a *container* for
                // the element, so an `is rw`/`is raw` param can write through
                // it. The existing binder already treats a bare `ContainerRef`
                // argument as a writable lvalue (see
                // `bind_function_args_values`), so no other plumbing is needed.
                let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(chunk[0].clone()));
                let res = match self.call_sub_value(
                    block.clone(),
                    vec![Value::container_ref(cell.clone())],
                    false,
                ) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                source_after[i] = cell.lock().unwrap().clone();
                res.deref_container()
            } else {
                // For a Pair element (arity == 1) the general call machinery
                // would bind it as a named arg and skip `$_`; force it as the
                // topic.
                let explicit_topic = if has_pairs
                    && matches!(
                        chunk[0].view(),
                        ValueView::Pair(..) | ValueView::ValuePair(..)
                    ) {
                    Some(chunk[0].clone())
                } else {
                    None
                };
                self.rw_map_topic_capture = None;
                let v = match self.vm_call_map_block(&block, chunk, explicit_topic, true) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                // Capture the block's final `$_` back into the source element.
                if let Some(mutated) = self.rw_map_topic_capture.take() {
                    source_after[i] = mutated;
                }
                v
            };
            if let ValueView::Slip(elems) = value.view() {
                result.extend(elems.iter().cloned());
            } else {
                result.push(value);
            }
            i += arity;
        }

        // The caller publishes `source_after` into the source container in
        // place (Raku rw binding), which keeps the per-container element-type
        // metadata (`my Int @a` -> `Array[Int]`) that the old rebuild-and-
        // re-bind route had to re-register by hand.
        Some(Ok((result, source_after)))
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
        | Expr::MatchRegex(_)
        | Expr::MatchRegexTree { .. } => Some(false),
        // A nested `s///` / `tr///` (e.g. under `~~`) targets whichever value the
        // surrounding construct aliases to `$_`, which we cannot determine here;
        // be conservative. Only a *statement-level* `s///`/`tr///` is recognized
        // as a topic mutation (see `classify_stmt`).
        Expr::Subst { .. } | Expr::Transliterate { .. } => None,
        // Control-flow expressions escape the loop.
        Expr::ControlFlow { .. } => None,
        Expr::Unary { op, expr } | Expr::PostfixOp { op, expr } => {
            if matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus) {
                // `$_++` / `$_--` mutate the topic. A `++`/`--` of a plain
                // *named* scalar (`$c++`, a captured-outer or block-local var)
                // does NOT touch the topic and is reproduced by the native loop
                // exactly like `$c = $c + 1` (its free-var write is recorded and
                // drained at the call site, #3307) — so classify it as a simple,
                // non-topic-mutating body. Any other `++`/`--` target (an indexed
                // element `@a[$i]++`, an attribute `$o.x++`, a deref) is not a
                // plain name write and stays an escape (fall back).
                match expr.as_ref() {
                    Expr::Var(n) if n == "_" => Some(true),
                    Expr::Var(_) => Some(false),
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
        // The `.=`-on-topic marker (`$_ .= meth` / `.=meth`) mutates `$_`, exactly
        // like the `Stmt::Assign { name: "_" }` case in `classify_stmt`.
        Expr::Call { name, .. } if name.resolve() == "__mutsu_topic_dotassign" => Some(true),
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => classify_exprs(args),
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
