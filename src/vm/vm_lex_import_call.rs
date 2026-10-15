//! `OpCode::LexImportCall`: a bareword call lexically inside a block that
//! `use`s a module calls the routine the import bound (ADR-8751 stage 1).

use super::vm_call_func_ops::CodeVarBinding;
use super::*;

impl Interpreter {
    /// Run the `CallFunc` / `CallOnCodeVar` / bare `GetBareWord` at `ip + 1`
    /// through the routine
    /// one of the import sets of chain `chain_idx` binds its callee to, or
    /// push that routine for a `GetCodeVar` there. Returns `false`, having
    /// done nothing, when no set binds the name; the caller then runs the
    /// op itself.
    pub(super) fn exec_lex_import_call_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        chain_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<bool, RuntimeError> {
        let (name_idx, call) = match code.ops.get(ip + 1) {
            Some(OpCode::CallFunc {
                name_idx,
                arity,
                arg_sources_idx,
                ..
            }) => (*name_idx, Some((*arity, *arg_sources_idx, false))),
            Some(OpCode::CallOnCodeVar {
                name_idx,
                arity,
                arg_sources_idx,
                bare_args,
                ..
            }) => (*name_idx, Some((*arity, *arg_sources_idx, *bare_args))),
            // `&g`: the binding as a value.
            Some(OpCode::GetCodeVar(name_idx)) => (*name_idx, None),
            // A bare `g`: a call with no arguments.
            Some(OpCode::GetBareWord(name_idx)) => (*name_idx, Some((0, None, false))),
            _ => return Ok(false),
        };
        let Some(chain) = code.lexical_import_chains.get(chain_idx as usize) else {
            return Ok(false);
        };
        let Some(target) = self.lexical_import_binding(chain, code.const_sym(name_idx)) else {
            return Ok(false);
        };
        let Some((arity, arg_sources_idx, bare_args)) = call else {
            let value = match target {
                crate::runtime::lexical_imports::LexicalImport::Code(value) => value,
                crate::runtime::lexical_imports::LexicalImport::Routine(def) => {
                    self.sub_value_from_function_def((*def).clone())
                }
            };
            self.stack.push(value);
            return Ok(true);
        };
        let call_ip = ip + 1;
        self.sync_source_line(code, call_ip);
        // `use fatal`: as the `CallFunc` itself checks its arguments.
        self.explode_if_fatal_failure_in_call_args(
            Self::const_str(code, name_idx),
            arity as usize,
        )?;
        let binding = match target {
            crate::runtime::lexical_imports::LexicalImport::Code(value) => {
                CodeVarBinding::Bound(value)
            }
            crate::runtime::lexical_imports::LexicalImport::Routine(key) => {
                CodeVarBinding::Routine(key)
            }
        };
        self.exec_call_on_code_var_op(
            code,
            name_idx,
            arity,
            arg_sources_idx,
            bare_args,
            binding,
            compiled_fns,
        )
        .inspect_err(|e| self.record_call_resume_point(code, call_ip, e))?;
        Ok(true)
    }

    /// Run the binary `InfixFunc` at `ip + 1` through the routine one of the
    /// import sets of chain `chain_idx` binds `infix:<op>` (`name_idx`) to.
    /// Returns `false`, having done nothing, when no set binds it.
    pub(super) fn exec_lex_import_infix_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        chain_idx: u32,
        name_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<bool, RuntimeError> {
        let Some(chain) = code.lexical_import_chains.get(chain_idx as usize) else {
            return Ok(false);
        };
        let Some(target) = self.lexical_import_binding(chain, code.const_sym(name_idx)) else {
            return Ok(false);
        };
        let call_ip = ip + 1;
        self.sync_source_line(code, call_ip);
        let binding = match target {
            crate::runtime::lexical_imports::LexicalImport::Code(value) => {
                CodeVarBinding::Bound(value)
            }
            crate::runtime::lexical_imports::LexicalImport::Routine(def) => {
                CodeVarBinding::Routine(def)
            }
        };
        self.exec_call_on_code_var_op(code, name_idx, 2, None, false, binding, compiled_fns)
            .inspect_err(|e| self.record_call_resume_point(code, call_ip, e))?;
        Ok(true)
    }
}
