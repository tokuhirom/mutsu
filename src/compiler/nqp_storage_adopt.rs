//! `nqp::bindattr($wrapper, Map, '$!storage', $storage)` on a hash wrapper the
//! routine just made with `nqp::create(Hash)` (#16888, EEL 0.3's `EEL-hash`):
//!
//! ```raku
//! my $hash := nqp::create(Hash);
//! nqp::bindattr(nqp::decont($hash), Map, '$!storage', $storage);
//! $hash
//! ```
//!
//! Rakudo's Hash is a wrapper around a separate storage object, so the new
//! wrapper writes into the storage every other holder of it sees (EEL's event
//! captures and its frame's capture hash share one). mutsu has no wrapper: a
//! Hash is its node. The generic bind copies the storage into the target's
//! node and re-points the storage *variable* at it (ADR-12666 §3.6.1), which
//! is right when the target is a container someone else holds (`fill(%h)`
//! installing a fresh `nqp::hash`) and wrong here, where the storage is the
//! shared one and the target a throwaway: the wrapper variable is rebound to a
//! `Hash` view of the storage's node instead, so both name one store.
//!
//! The rebind applies only to a scalar this routine declared as
//! `my $x := nqp::create(Hash)` that is still empty when the bind runs; any
//! other shape keeps the generic bind.

use super::*;

impl Compiler {
    /// Record or forget `name` as a fresh hash wrapper, by the declaration's
    /// initializer.
    // Cost: O(1).
    pub(super) fn note_nqp_fresh_hash_decl(&mut self, name: &str, bind: bool, init: &Expr) {
        let fresh = bind
            && !name.starts_with(['@', '%', '&'])
            && matches!(init, Expr::Call { name: callee, args, .. }
                if callee.as_str() == "nqp::create"
                    && matches!(args.as_slice(), [Expr::Literal(v)]
                        if matches!(v.view(), crate::value::ValueView::Package(p) if p == "Hash")));
        if fresh {
            self.nqp_fresh_hash_vars.insert(name.to_string());
        } else if !self.nqp_fresh_hash_vars.is_empty() {
            self.nqp_fresh_hash_vars.remove(name);
        }
    }

    /// Compile `nqp::bindattr(WRAPPER, Map, '$!storage', $storage)` on a fresh
    /// hash wrapper as
    /// `nqp::elems(WRAPPER) ?? <the generic bind> !! (WRAPPER := nqp::hllize($storage))`.
    /// `false` when the call is not that shape.
    // Cost: O(1) more than the generic bind (an `elems` probe and a rebind).
    pub(super) fn try_compile_nqp_storage_adopt(&mut self, name: &str, args: &[Expr]) -> bool {
        if self.nqp_fresh_hash_vars.is_empty() || name != "nqp::bindattr" || args.len() != 4 {
            return false;
        }
        let wrapper = match &args[0] {
            Expr::Var(v) => v,
            Expr::Call {
                name: callee,
                args: inner,
                ..
            } if callee.as_str() == "nqp::decont" => match inner.as_slice() {
                [Expr::Var(v)] => v,
                _ => return false,
            },
            _ => return false,
        };
        if !self.nqp_fresh_hash_vars.contains(wrapper.as_str()) {
            return false;
        }
        let is_lit = |e: &Expr, want: &str| match e {
            Expr::Literal(v) => match v.view() {
                crate::value::ValueView::Str(s) => s.as_str() == want,
                crate::value::ValueView::Package(p) => p == want,
                _ => false,
            },
            _ => false,
        };
        if !is_lit(&args[1], "Map") || !is_lit(&args[2], "$!storage") {
            return false;
        }
        let Expr::Var(storage) = &args[3] else {
            return false;
        };
        let call = |name: &str, args: Vec<Expr>| Expr::Call {
            name: crate::symbol::Symbol::intern(name),
            args,
            listop: false,
        };
        let adopt = Expr::AssignExpr {
            name: wrapper.clone(),
            expr: Box::new(call("nqp::hllize", vec![Expr::Var(storage.clone())])),
            is_bind: true,
        };
        let generic = call("nqp::bindattr", args.to_vec());
        // The fallback must not re-enter this rule.
        let saved = std::mem::take(&mut self.nqp_fresh_hash_vars);
        self.compile_expr(&Expr::Ternary {
            cond: Box::new(call(
                "nqp::elems",
                vec![call("nqp::decont", vec![Expr::Var(wrapper.clone())])],
            )),
            then_expr: Box::new(generic),
            else_expr: Box::new(adopt),
        });
        self.nqp_fresh_hash_vars = saved;
        true
    }
}
