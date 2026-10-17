//! `OpCode::LexImportBuiltinOp`: a built-in operator used lexically inside a
//! block whose `use` imported user candidates for it (ADR-8751 stage 2).
//!
//! `multi infix:<+>(Foo, Foo)` imported by `{ use M; ... }` adds to the
//! built-in `+` for that block only. The import binds the candidates in the
//! block's lexical import set, as a dispatcher value that falls back to the
//! built-in (`with_builtin_operator_fallback`), and puts nothing in the
//! routine registry, so code written outside the block never sees them.
//! The operator's own opcode (`Add`) and every other op that applies it (a
//! compound assignment, a reduction, a hyper) consult user candidates through
//! one funnel, `try_user_infix`. This prefix makes the bound candidates that
//! funnel's first answer for the duration of the op after it: a candidate
//! that accepts the operands answers, and the built-in answers the rest,
//! Junction operands threaded as the op threads them.
//!
//! The scope is the routine frame the op runs in. A candidate's own body, or
//! a user method the built-in calls to coerce an operand, runs in a frame of
//! its own and sees only what its own code imported.

use super::*;
use crate::runtime::dispatch_state::LexicalOperatorScope;

impl Interpreter {
    /// Run the op at `ip + 1` with the user candidates of the built-in
    /// operator `name_idx` that one of the import sets of chain `chain_idx`
    /// binds, or as it is when none binds them. Returns the ip after that op.
    // Cost: O(c) probe, c = enclosing `use`-blocks, plus the op at ip + 1.
    pub(super) fn exec_lex_import_builtin_op_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        chain_idx: u32,
        name_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<usize, RuntimeError> {
        let name = code.const_sym(name_idx);
        let dispatcher = code
            .lexical_import_chains
            .get(chain_idx as usize)
            .and_then(|chain| self.lexical_import_binding(chain, name))
            .and_then(|binding| match binding {
                crate::runtime::lexical_imports::LexicalImport::Code(value)
                    if Self::is_builtin_operator_dispatcher(&value) =>
                {
                    Some(value)
                }
                _ => None,
            });
        let mut next_ip = ip + 1;
        let Some(dispatcher) = dispatcher else {
            return Ok(next_ip);
        };
        self.dispatch
            .lexical_operator_scopes
            .push(LexicalOperatorScope {
                name,
                dispatcher,
                depth: self.routine_stack().len(),
            });
        let result = self.exec_one(code, &mut next_ip, compiled_fns);
        self.dispatch.lexical_operator_scopes.pop();
        result.map(|()| next_ip)
    }

    /// Whether `value` is a dispatcher over user candidates of a built-in
    /// operator (`Interpreter::with_builtin_operator_fallback`).
    // Cost: O(1).
    fn is_builtin_operator_dispatcher(value: &Value) -> bool {
        matches!(value.view(), ValueView::Sub(data)
            if data.env.get_sym(crate::symbol::wk::multi_dispatch_builtin_fallback()).is_some())
    }

    /// The user candidates of the built-in operator `name` in scope for the
    /// op running in this routine frame ([`Self::exec_lex_import_builtin_op_site`]),
    /// as the dispatcher's captured candidate list.
    // Cost: O(s), s = nested operator scopes (usually 0 or 1).
    pub(crate) fn lexical_operator_candidates(&self, name: &str) -> Option<Vec<Value>> {
        let scopes = &self.dispatch.lexical_operator_scopes;
        if scopes.is_empty() {
            return None;
        }
        let depth = self.routine_stack().len();
        let scope = scopes
            .iter()
            .rev()
            .find(|scope| scope.depth == depth && scope.name.as_str() == name)?;
        let ValueView::Sub(data) = scope.dispatcher.view() else {
            return None;
        };
        data.env
            .get_sym(crate::symbol::wk::multi_dispatch_candidates())
            .and_then(|value| value.as_list_items().map(ToOwned::to_owned))
    }

    /// Whether [`Self::lexical_operator_candidates`] has an answer for `name`.
    // Cost: O(s), as there; O(1) with no operator scope open.
    #[inline]
    pub(crate) fn lexical_operator_in_scope(&self, name: &str) -> bool {
        !self.dispatch.lexical_operator_scopes.is_empty()
            && self.lexical_operator_candidates(name).is_some()
    }
}
