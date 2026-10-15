//! `OpCode::LexImportCall`: a bareword call lexically inside a block that
//! `use`s a module calls the routine the import bound (ADR-8751 stage 1).

use super::vm_call_func_ops::CodeVarBinding;
use super::*;

impl Interpreter {
    /// Run the `CallFunc` at `ip + 1` through the routine one of the import
    /// sets of chain `chain_idx` binds its callee to. Returns `false`, having
    /// done nothing, when no set binds the name; the caller then runs the
    /// `CallFunc` itself.
    pub(super) fn exec_lex_import_call_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        chain_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<bool, RuntimeError> {
        let Some(OpCode::CallFunc {
            name_idx,
            arity,
            arg_sources_idx,
            ..
        }) = code.ops.get(ip + 1)
        else {
            return Ok(false);
        };
        let Some(chain) = code.lexical_import_chains.get(chain_idx as usize) else {
            return Ok(false);
        };
        let Some(target) = self.lexical_import_binding(chain, code.const_sym(*name_idx)) else {
            return Ok(false);
        };
        let call_ip = ip + 1;
        self.sync_source_line(code, call_ip);
        // `use fatal`: as the `CallFunc` itself checks its arguments.
        self.explode_if_fatal_failure_in_call_args(
            Self::const_str(code, *name_idx),
            *arity as usize,
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
            *name_idx,
            *arity,
            *arg_sources_idx,
            false,
            binding,
            compiled_fns,
        )
        .inspect_err(|e| self.record_call_resume_point(code, call_ip, e))?;
        Ok(true)
    }
}
