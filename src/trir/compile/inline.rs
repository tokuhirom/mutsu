//! Call-only inner `my sub`s, inlined into the enclosing TRIR body
//! (ADR-0112 Step 2).
//!
//! JSON::Fast's `unjsonify-string` declares `my sub fetch-codepoint`, which
//! reads the enclosing routine's `codes` and `$pos`. ADR-0113 proved such a
//! sub is never observed as anything but a bare call (no `&name`, no
//! `EVAL`, no pseudo-package, no dispatcher redispatch, no `state`), and made
//! it a frame lexical on the untyped path. Under that proof the sub has no
//! identity a program can see, so a call to it is equivalent to its body
//! evaluated in its own lexical scope — which is what this pass emits.
//!
//! The scope is the one the DECLARATION sees: the enclosing routine's
//! bindings as they stand at the declaration statement. A variable the
//! routine declares later is invisible to the sub, and a name the sub
//! declares itself shadows the routine's for the sub's body only. Only the
//! shapes whose inlining is plainly equivalent are admitted: no parameters,
//! no return type, no traits, no `return` (which would leave the ENCLOSING
//! chunk), no recursion, and no reads of the sub's own `$_`/`$/`/`$!`.
//! Anything else declines the enclosing routine, which then takes the
//! untyped path unchanged.

use std::collections::HashMap;

use super::{Binding, TrirCompiler};
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::trir::{TrKind, TrOp};
use crate::value::Value;

/// An inner sub ready to be inlined.
#[derive(Debug, Clone)]
pub(crate) struct InlineSub {
    /// The body, compiled afresh at each call site.
    body: Vec<Stmt>,
    /// The enclosing routine's bindings as the declaration saw them.
    scope: HashMap<String, Binding>,
}

impl TrirCompiler<'_> {
    /// Note the frame-lexical inner subs `body` declares at its top level.
    pub(super) fn collect_inline_subs(&mut self, body: &[Stmt], frame_lexicals: &[Symbol]) {
        for stmt in body {
            if let Stmt::SubDecl { name, .. } = stmt
                && frame_lexicals.contains(name)
            {
                self.inline_subs.insert(name.as_str().to_string(), None);
            }
        }
    }

    /// Record an inner sub's declaration, or decline when it is a shape
    /// whose inlining is not plainly equivalent to the call.
    pub(super) fn declare_inline_sub(&mut self, stmt: &Stmt) -> Option<()> {
        let Stmt::SubDecl {
            name,
            params,
            param_defs,
            return_type,
            body,
            custom_traits,
            ..
        } = stmt
        else {
            return None;
        };
        let n = name.as_str().to_string();
        if !params.is_empty() || !param_defs.is_empty() {
            self.note_decline(|| format!("inner sub {n} with parameters"));
            return None;
        }
        if return_type.is_some() || !custom_traits.is_empty() {
            self.note_decline(|| format!("inner sub {n} with a return type or trait"));
            return None;
        }
        if body_mentions_return(body) {
            self.note_decline(|| format!("inner sub {n} with a `return`"));
            return None;
        }
        self.inline_subs.insert(
            n,
            Some(InlineSub {
                body: body.clone(),
                scope: self.locals.clone(),
            }),
        );
        Some(())
    }

    /// Inline a call to inner sub `name`.
    pub(super) fn compile_inline_call(&mut self, name: &str, args: &[Expr]) -> Option<TrKind> {
        // A no-paren call carries the parser's Test call-site marker, which
        // the untyped dispatch strips; nothing else may be passed.
        if args.iter().any(|a| !is_callsite_marker(a)) {
            self.note_decline(|| format!("a call to inner sub {name} with arguments"));
            return None;
        }
        let Some(Some(sub)) = self.inline_subs.get(name).cloned() else {
            self.note_decline(|| format!("a call to inner sub {name} before its declaration"));
            return None;
        };
        if self.inline_stack.iter().any(|n| n == name) {
            self.note_decline(|| format!("recursion into inner sub {name}"));
            return None;
        }
        let saved = std::mem::replace(&mut self.locals, sub.scope);
        self.inline_stack.push(name.to_string());
        let last = self.compile_body(&sub.body, false);
        self.inline_stack.pop();
        self.locals = saved;
        match last? {
            Some(kind) => Some(kind),
            None => {
                let idx = self.add_const(Value::NIL);
                self.ops.push(TrOp::ConstObj(idx));
                Some(TrKind::Obj)
            }
        }
    }
}

/// The `__mutsu_test_callsite_line => N` pair the parser adds to a no-paren
/// call.
pub(super) fn is_callsite_marker(e: &Expr) -> bool {
    matches!(
        e,
        Expr::Binary { left, op: crate::token_kind::TokenKind::FatArrow, .. }
            if matches!(left.as_ref(), Expr::Literal(v)
                if v.as_str() == Some("__mutsu_test_callsite_line"))
    )
}

/// Whether `body` contains a `return` anywhere: in an inlined body it would
/// leave the enclosing chunk instead of the sub.
fn body_mentions_return(body: &[Stmt]) -> bool {
    let Ok(json) = serde_json::to_string(body) else {
        return true;
    };
    json.contains("\"Return\"") || json.contains("\"return\"")
}
