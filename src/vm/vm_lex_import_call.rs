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
        let name = code.const_sym(name_idx);
        // A fallback chain (a call written above the unit's `use`) answers
        // only a name that means nothing else here.
        let chain = match chain.split_first() {
            Some((mark, late))
                if mark.with_str(|m| m == crate::runtime::lexical_imports::LATE_IMPORT_MARK) =>
            {
                if self.has_function(name.as_str())
                    || self
                        .env()
                        .get_sym(crate::runtime::dispatch_key::amp_sym(name))
                        .is_some()
                {
                    return Ok(false);
                }
                late
            }
            _ => chain.as_slice(),
        };
        let Some(target) = self.lexical_import_operator_binding(chain, name) else {
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

    /// Push the term a `sub EXPORT` hook bound in one of the import sets of
    /// chain `chain_idx` for the bare word read at `ip + 1` (a `GetBareWord`,
    /// or the term half of a `GetExportTermOrJump`), and return the ip to go
    /// on at. `None`, having done nothing, when no set binds the term.
    pub(super) fn exec_lex_import_term_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        chain_idx: u32,
    ) -> Option<usize> {
        let (name_idx, next) = match code.ops.get(ip + 1)? {
            OpCode::GetBareWord(name_idx) => (*name_idx, ip + 2),
            OpCode::GetExportTermOrJump { name_idx, end } => (*name_idx, *end as usize),
            _ => return None,
        };
        let chain = code.lexical_import_chains.get(chain_idx as usize)?;
        let value = self.lexical_import_hook_term(chain, code.const_sym(name_idx))?;
        self.stack.push(value);
        Some(next)
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

    /// Run the `Reduction` or `HyperOp` at `ip + 1` with the routine one of
    /// the import sets of chain `chain_idx` binds `infix:<op>` (`name_idx`)
    /// to. Returns `false`, having done nothing, when no set binds it.
    pub(super) fn exec_lex_import_meta_site(
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
        let callable = match target {
            crate::runtime::lexical_imports::LexicalImport::Code(value) => value,
            crate::runtime::lexical_imports::LexicalImport::Routine(def) => {
                self.sub_value_from_function_def((*def).clone())
            }
        };
        self.sync_source_line(code, ip + 1);
        match code.ops.get(ip + 1) {
            Some(OpCode::Reduction(spec_idx)) => {
                let spec = code.reduction_spec(*spec_idx);
                self.exec_reduction_op(code, spec, Some(callable))?;
            }
            Some(OpCode::HyperOp {
                dwim_left,
                dwim_right,
                ..
            }) => {
                self.exec_hyper_func_op(
                    code,
                    name_idx,
                    *dwim_left,
                    *dwim_right,
                    false,
                    Some(callable),
                    compiled_fns,
                )?;
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    /// Call `def`, the routine a lexical import set binds, as a by-name call
    /// of it would: a Junction argument whose parameter does not accept one
    /// is autothreaded (`maybe_autothread_func_call`), one call per
    /// eigenstate, before the routine runs.
    // Cost: O(j) calls of `def` (j = eigenstates of the threaded Junctions; 1
    // without a Junction argument) plus O(a) per call, a = arguments.
    pub(super) fn call_lexical_import_routine(
        &mut self,
        def: &crate::ast::FunctionDef,
        args: Vec<Value>,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        // A routine a trait wrapped (`is native` binds the routine's `$!do`)
        // runs its wrapper, as a by-name call does.
        if self.any_routine_wrapped()
            && let Some(wrapped) = self.wrapped_sub_for_call(&def.name.resolve())
            && let ValueView::Sub(data) = Self::unwrap_callable_mixin(wrapped.clone()).view()
            && data.package == def.package
        {
            return self.vm_call_sub_value(wrapped, args);
        }
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, v)| Self::extract_junction_from_arg(v).is_some())
            .map(|(i, _)| i)
            .collect();
        if !junction_indices.is_empty() {
            let indices =
                self.autothread_indices_for_params(&args, &junction_indices, &def.param_defs);
            if !indices.is_empty() {
                let idx = self.pick_autothread_junction_index(&args, &indices);
                if let Some((kind, values, pair_key)) = Self::extract_junction_from_arg(&args[idx])
                {
                    let mut results = Vec::with_capacity(values.len());
                    for eigenstate in values.iter() {
                        let mut threaded = args.clone();
                        threaded[idx] = match &pair_key {
                            Some(key) => Value::pair(key.clone(), eigenstate.clone()),
                            None => eigenstate.clone(),
                        };
                        results.push(self.call_lexical_import_routine(
                            def,
                            threaded,
                            compiled_fns,
                        )?);
                    }
                    return Ok(Value::junction(kind, results));
                }
            }
        }
        self.compile_and_call_unmatched_qualified_def(def, args, compiled_fns)
    }
}
