//! The type-object lane of `SmartMatchExpr`: `$x ~~ Foo` whose right side is
//! a bare type name that its site has already resolved to a type object.
//!
//! The general op topicalizes the left side, syncs the locals a regex on the
//! right might read, runs the right side as a nested range and then works out
//! whether that range modified the topic. None of it is observable when the
//! right side is a bare word that resolves to a type without running any
//! code: no routine is called, so nothing can read the topic or write a
//! caller variable while it is evaluated. Such a match is a type test, and
//! it was ~13K instructions for `$obj ~~ SomeRole` (#17331: Monad's
//! `self ~~ Monad::Maybe::Some` on every `is-some`).
//!
//! The lane answers only when the bare word's site memo
//! (`CompiledCode::bareword_sites`) holds a type for it under the same
//! conditions `GetBareWord` itself serves that memo, and no block-scoped `use`
//! binds the name as a term or routine. The match itself, and everything
//! after it, is the general op's own code.

use super::*;

impl Interpreter {
    /// The type object the right side of the `SmartMatchExpr` whose range is
    /// `[rhs_start, rhs_end)` evaluates to, when it is a bare word that
    /// resolves to that type without running code; `None` otherwise.
    // Cost: O(c) on a candidate site, c = the import sets of its chain (each
    // one hash probe), plus the bare-word memo's two `env` probes; O(1) for
    // any other right side.
    fn smartmatch_rhs_type(
        &self,
        code: &CompiledCode,
        rhs_start: usize,
        rhs_end: usize,
    ) -> Option<Value> {
        let (name_idx, chain_idx) = match code.ops.get(rhs_start..rhs_end)? {
            [OpCode::GetBareWord(name_idx)] => (*name_idx, None),
            [
                OpCode::LexImportCall { chain_idx },
                OpCode::GetBareWord(name_idx),
            ] => (*name_idx, Some(*chain_idx)),
            _ => return None,
        };
        let name = code.const_sym(name_idx);
        // `LexImportCall` runs a routine (a bare word with no arguments) or
        // pushes a hook-bound term when one of the chain's import sets binds
        // the name; either may observe the topic. Only a chain that binds
        // neither falls through to the `GetBareWord`.
        if let Some(chain_idx) = chain_idx {
            let chain = code.lexical_import_chains.get(chain_idx as usize)?;
            if self.lexical_import_hook_term(chain, name).is_some()
                || self.lexical_import_binding(chain, name).is_some()
            {
                return None;
            }
        }
        // The conditions `exec_get_bare_word_op` serves its memo under.
        let generation = self.registry_write_generation();
        let value =
            code.bareword_sites
                .cached(code.constants.len(), name_idx as usize, generation)?;
        // Only a type object: a memoized constant term (a `Regex`, say)
        // takes the general op, whose match may bind `$/`.
        let env_ok = match value.view() {
            ValueView::Package(sym) => self.env_leaves_type_name_as(name, sym),
            _ => false,
        };
        (env_ok && self.bare_name_visible_here(name)).then_some(value)
    }

    /// Run the `SmartMatchExpr` at `*ip` through the type lane (see the module
    /// docs). `Ok(false)`, having done nothing, when its right side does not
    /// qualify.
    // Cost: the bare-word probe above plus the type test itself (see
    // `eval_smartmatch_with_junctions_ex`).
    pub(super) fn try_smart_match_type_lane(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        rhs_end: u32,
        lhs: Option<&crate::opcode::SmartMatchLhs>,
        (negate, rhs_is_match_regex): (bool, bool),
    ) -> Result<bool, RuntimeError> {
        let rhs_start = *ip + 1;
        let rhs_end = rhs_end as usize;
        let Some(right) = self.smartmatch_rhs_type(code, rhs_start, rhs_end) else {
            return Ok(false);
        };
        let (lhs_var, boxed_native, native_declared) = match lhs {
            Some(crate::opcode::SmartMatchLhs::Var {
                name,
                boxed_native,
                native_declared,
                ..
            }) => (Some(name), *boxed_native, native_declared.as_deref()),
            _ => (None, false, None),
        };
        // As the general op: the left side is read, so a `Proxy` FETCHes.
        let left = self.stack.pop().unwrap_or(Value::NIL);
        let left = self.auto_fetch_proxy(&left)?;
        self.pending_local_updates.clear();
        let out = match self.direct_native_lhs_match(
            lhs_var,
            boxed_native,
            native_declared,
            &left,
            &right,
        ) {
            Some(matched) => Value::truth(matched != negate),
            None => {
                self.eval_smartmatch_with_junctions_ex(left, right, negate, rhs_is_match_regex)?
            }
        };
        self.stack.push(out);
        // The general op's epilogue for a match that is not a pure regex: a
        // custom `ACCEPTS` / HOW `type_check` may have written caller state.
        if !code.match_name_local_slots().is_empty() || !self.pending_local_updates.is_empty() {
            let written: std::collections::HashSet<String> = self
                .pending_local_updates
                .iter()
                .map(|(n, _)| n.clone())
                .collect();
            self.writeback_match_locals(code, &written);
            for name in &written {
                let is_match_name =
                    name == "/" || (!name.is_empty() && name.bytes().all(|b| b.is_ascii_digit()));
                if is_match_name || self.find_local_slot(code, name).is_some() {
                    continue;
                }
                self.record_caller_var_writeback(name);
            }
        }
        self.pending_local_updates.clear();
        self.apply_pending_caller_var_writeback(code);
        *ip = rhs_end;
        Ok(true)
    }
}
