//! The `s///` / `S///` replacement side.
//!
//! Raku's substitution replacement is a **`qq` quote**, not a special little
//! template language: it interpolates variables with their postcircumfixes
//! (`$x`, `$0`, `$<name>`, `@a[1]`, `%h{$/}`), evaluates embedded `{ ... }`
//! code blocks (including one glued straight onto literal text, `d{lc $0}`),
//! and honours the whole backslash-escape set (`\n`, `\x[41]`,
//! `\c[LATIN SMALL LETTER Z]`, and `\<punct>` for a literal punctuation
//! character). mutsu used to re-implement a *subset* of that grammar by hand in
//! the VM, which is why `$<name>`, `%h{...}`, `@a[idx]` and `\:` all came out
//! wrong. This module instead hands the replacement source to the one real
//! interpolation parser (`parse_dispatch::parse_qq_interpolation`) and evaluates
//! the resulting expression.
//!
//! The parse is cached per replacement source, and the resulting body carries a
//! stable `cache_id` so the carrier compile cache can reuse its bytecode across
//! matches — previously every `{...}` block was re-parsed *and* re-compiled once
//! per match.

use super::*;
use crate::ast::{Expr, Stmt};
use std::sync::Arc;

/// The captures one substitution match produced, in the shape the replacement
/// (and the `$/` the substitution publishes) need them: `$0`, `$1`, ... plus
/// `$<name>`.
#[derive(Clone, Default)]
pub(crate) struct SubstMatchCaps {
    pub(crate) positional: Vec<String>,
    pub(crate) named: std::collections::HashMap<String, Vec<String>>,
}

/// One piece of a replacement whose interpolations are nothing but capture
/// references — see [`SubstReplPlan::Dynamic::capture_parts`].
#[derive(Clone)]
pub(crate) enum ReplPart {
    Lit(String),
    /// `$/` — the whole match.
    Whole,
    /// `$0`, `$1`, ...
    Pos(usize),
    /// `$<name>`
    Named(String),
}

/// How one substitution replacement produces its text.
#[derive(Clone)]
pub(crate) enum SubstReplPlan {
    /// The replacement interpolates nothing, so its text is fixed up front.
    Static(Arc<str>),
    /// The replacement interpolates: Raku evaluates it once per match, with
    /// `$/` (and through it `$0`, `$<name>`, ...) bound to that match.
    Dynamic {
        body: Arc<Vec<Stmt>>,
        cache_id: u64,
        /// A shortcut for the overwhelmingly common interpolating replacement
        /// (`$0`, `$<name>`, `$/` and literal text, no user code): its parts,
        /// read straight off the parsed expression above. Splicing capture
        /// texts together is far cheaper than invoking the evaluation carrier
        /// once per match, and it is not a second *grammar* — the parts come
        /// from the same parse. Any part it cannot supply (a capture the match
        /// did not produce, a quantified `$<name>` with several values) makes it
        /// decline, and that match falls back to evaluating `body`, so the
        /// observable semantics — including the `Use of Nil in string context`
        /// warning — stay those of the one evaluator.
        capture_parts: Option<Arc<Vec<ReplPart>>>,
    },
}

/// Read a replacement expression as a list of [`ReplPart`]s, or `None` when it
/// contains anything that needs real evaluation.
fn capture_parts_of(expr: &Expr) -> Option<Vec<ReplPart>> {
    fn part_of(e: &Expr) -> Option<ReplPart> {
        match e {
            Expr::Literal(v) => match v.view() {
                ValueView::Str(s) => Some(ReplPart::Lit(s.to_string())),
                _ => None,
            },
            // `$/` interpolated on its own.
            Expr::Var(name) if name == "/" => Some(ReplPart::Whole),
            // `$0` / `$<name>`: the interpolation grammar lowers both to a
            // subscript on `$/`.
            Expr::Index {
                target,
                index,
                is_positional,
            } => {
                let Expr::Var(t) = target.as_ref() else {
                    return None;
                };
                if t != "/" {
                    return None;
                }
                let Expr::Literal(v) = index.as_ref() else {
                    return None;
                };
                match (v.view(), is_positional) {
                    (ValueView::Int(n), true) if n >= 0 => Some(ReplPart::Pos(n as usize)),
                    (ValueView::Str(name), false) => Some(ReplPart::Named(name.to_string())),
                    _ => None,
                }
            }
            _ => None,
        }
    }
    match expr {
        Expr::StringInterpolation(parts) => parts.iter().map(part_of).collect(),
        other => part_of(other).map(|p| vec![p]),
    }
}

/// Splice `parts` together for one match, or `None` when a referenced capture is
/// missing (or is a multi-value quantified named capture) and the general
/// evaluator has to take over.
pub(crate) fn expand_capture_parts(
    parts: &[ReplPart],
    matched: &str,
    caps: &SubstMatchCaps,
) -> Option<String> {
    let mut out = String::new();
    for part in parts {
        match part {
            ReplPart::Lit(s) => out.push_str(s),
            ReplPart::Whole => out.push_str(matched),
            ReplPart::Pos(n) => out.push_str(caps.positional.get(*n)?),
            ReplPart::Named(name) => match caps.named.get(name)?.as_slice() {
                [one] => out.push_str(one),
                _ => return None,
            },
        }
    }
    Some(out)
}

impl Interpreter {
    /// The [`SubstReplPlan`] for `src`, parsing it under `qq` rules on first use
    /// and caching the result (a `:g` substitution asks for the same plan once
    /// per op execution, and the plan is reused across every match).
    pub(super) fn subst_replacement_plan(&mut self, src: &str) -> SubstReplPlan {
        if let Some(plan) = self.subst_repl_plans.get(src) {
            return plan.clone();
        }
        let expr = crate::parse_dispatch::parse_qq_interpolation(src);
        let plan = match &expr {
            Expr::Literal(v) => match v.view() {
                ValueView::Str(s) => Some(SubstReplPlan::Static(Arc::from(s.as_str()))),
                _ => None,
            },
            _ => None,
        }
        .unwrap_or_else(|| SubstReplPlan::Dynamic {
            capture_parts: capture_parts_of(&expr).map(Arc::new),
            body: Arc::new(vec![Stmt::Expr(expr)]),
            // Drawn from the same global counter as `SubData::id`, so it can
            // never collide with a closure's carrier-compile-cache entry.
            cache_id: crate::value::next_instance_id(),
        });
        self.subst_repl_plans.insert(src.to_string(), plan.clone());
        plan
    }

    /// Evaluate a dynamic replacement body for the current match. `$/` and the
    /// numbered capture env entries must already be bound by the caller.
    pub(super) fn eval_subst_replacement(
        &mut self,
        body: &[Stmt],
        cache_id: u64,
    ) -> Result<String, RuntimeError> {
        // The replacement runs while `$_` still holds the substitution target;
        // a `{...}` block that re-topicalizes (any nested `for`/`given`, or a
        // method call carrier) must not leave the topic changed for the next
        // match.
        let saved_topic = self.env().get("_").cloned();
        let result = loan_env!(self, eval_block_value_cached(body, cache_id));
        if let Some(topic) = saved_topic {
            self.env_mut().insert("_".to_string(), topic);
        }
        Ok(result?.to_string_value())
    }
}
