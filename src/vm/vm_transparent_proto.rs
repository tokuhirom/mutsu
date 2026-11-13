//! A `{*}`-only `proto f(|)` is transparent to dispatch.
//!
//! An explicit `proto sub f(|) {*}` declares exactly the dispatcher a `multi`
//! family gets implicitly: it binds any argument list, runs no code of its own
//! and carries no routine trait, so the winner is whatever the candidates
//! decide. Array::Sorted::Util declares one for each of its families (`my
//! proto sub insert(|) {*}`). Treated as a proto, such a call took the
//! trivial-proto bypass, which runs the winner through the general binder;
//! treated as the multi family it is, the call takes the multi path, whose
//! winner gets the positional light bind and the type-keyed light cache.
//!
//! Every other proto stays opaque: a body beyond `{*}` must run, a real
//! signature gates the call, and a trait (`is cached`, `is rw`, `is raw`, a
//! return type, `is DEPRECATED`, a test assertion) or an operator's
//! precedence changes what the call means.

use super::*;

impl Interpreter {
    /// Whether `name` has a `proto` that is not transparent in the sense of
    /// this module. `false` when it has no proto at all.
    // Cost: O(1) on a memo hit; a miss is one `has_proto` and one proto
    // lookup, O(p), p = packages on the bare-name search path.
    pub(crate) fn has_opaque_proto_cached_sym(&mut self, name: &str, name_sym: Symbol) -> bool {
        if !self.has_proto_cached_sym(name, name_sym) {
            return false;
        }
        // A scoped family's answer depends on the executing compunit, which
        // the memo key does not carry (as `has_proto_cached_sym`).
        if self.operator_has_import_scope_sym(name_sym) {
            return true;
        }
        let pgen = self.registry().proto_generation();
        if self.caches.opaque_proto_cache_gen != pgen {
            self.caches.opaque_proto_cache.clear();
            self.caches.opaque_proto_cache_gen = pgen;
        }
        let key = self.bare_name_ctx_key(name_sym);
        if let Some(&cached) = self.caches.opaque_proto_cache.get(&key) {
            return cached;
        }
        let opaque = !self
            .resolve_proto_function_shared(name)
            .is_some_and(|proto| Self::proto_is_transparent(&proto));
        self.caches.opaque_proto_cache.insert(key, opaque);
        opaque
    }

    /// A `{*}`-only (or bodyless) proto with a bare `(|)` signature and no
    /// trait.
    // Cost: O(b), b = statements in the proto body (markers only, normally).
    fn proto_is_transparent(proto: &crate::ast::FunctionDef) -> bool {
        use crate::ast::{Expr, Stmt};
        let mut significant = proto.body.iter().filter(|s| !s.is_marker());
        let trivial_body = match (significant.next(), significant.next()) {
            (None, _) => true,
            (Some(Stmt::Expr(Expr::Whatever)), None) => true,
            _ => false,
        };
        trivial_body
            && Self::is_bare_capture_signature(&proto.param_defs)
            && !proto.is_cached
            && !proto.is_rw
            && !proto.is_raw
            && !proto.empty_sig
            && !proto.is_test_assertion
            && proto.return_type.is_none()
            && proto.deprecated_message.is_none()
            && proto.op_prec.is_none()
    }
}
