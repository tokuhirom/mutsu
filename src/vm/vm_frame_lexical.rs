//! Frame-lexical routines at run time (ADR-0112).
//!
//! The compiler marks a `my sub` that its enclosing routine body only ever
//! calls by bare name (`compiler/frame_lexical_routines.rs`). Such a routine
//! never enters the program-global registry:
//!
//! - its declaration derives the routine's definition the first time it runs
//!   in an interpreter (through the ordinary registration, which is then
//!   rolled back) and is a single table probe after that;
//! - a call site lists it in its chunk's `lexical_routines`, and the call
//!   handlers hand it here before any name-keyed resolution runs.

use super::*;
use crate::opcode::FrameLexicalRef;

/// What a frame-lexical call dispatches to: the routine's compiled body as
/// registration would have adapted it, and the package it runs in.
#[derive(Debug, Clone)]
pub(crate) struct FrameLexicalTarget {
    pub(crate) cf: Arc<CompiledFunction>,
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
}

/// A call site's argument-shape facts, as its opcode states them.
#[derive(Debug, Clone, Copy)]
pub(super) struct FrameLexicalCallSite {
    pub(super) arity: u32,
    pub(super) arg_sources_idx: Option<u32>,
    /// `ExecCallPairs` never tracked rw-argument sources.
    pub(super) track_sources: bool,
    pub(super) call_has_named: bool,
}

impl Interpreter {
    /// `RegisterDecl` of a frame-lexical sub plan: derive the definition once
    /// per interpreter, register nothing.
    pub(super) fn exec_declare_frame_lexical_routine(
        &mut self,
        code: &CompiledCode,
        plan_idx: u32,
        r: FrameLexicalRef,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        if self.frame_lexical_routines.contains_key(&r) {
            return Ok(());
        }
        // The ordinary registration performs every check and derivation a
        // declaration owes (signature normalization, type-constraint
        // resolution against the declaring package, redeclaration); keep
        // its definition and roll the registry back to what it was.
        let snapshot = self.snapshot_routine_registry();
        let registered = self.exec_register_sub_op_in_registry(code, plan_idx, compiled_fns);
        let def = registered.map(|()| {
            let key = crate::qualified::qualified(self.current_package_sym(), r.name);
            self.registry().functions.get(&key).cloned()
        });
        self.restore_routine_registry(snapshot);
        let def = def?;
        let key = code
            .sub_decl_plans
            .get(plan_idx as usize)
            .and_then(|plan| plan.compiled_routine_keys.first().copied());
        let cf = def
            .as_ref()
            .and_then(|def| def.compiled.clone())
            .or_else(|| key.and_then(|k| compiled_fns.get(&k).cloned()));
        let Some(cf) = cf else {
            return Err(RuntimeError::new(format!(
                "internal error: frame-lexical routine '{}' has no compiled body",
                r.name.as_str()
            )));
        };
        let package = def.map_or_else(|| self.current_package_sym(), |def| def.package);
        crate::runtime::cow_table_mut(&mut self.frame_lexical_routines).insert(
            r,
            FrameLexicalTarget {
                cf,
                package,
                name: r.name,
            },
        );
        Ok(())
    }

    /// A bare call site whose callee this chunk lists as a frame-lexical
    /// routine. Pops the `arity` arguments and returns the call's value, or
    /// `None` (arguments untouched) when this interpreter has not derived the
    /// routine, so the caller's ordinary dispatch takes over.
    pub(super) fn exec_frame_lexical_call(
        &mut self,
        code: &CompiledCode,
        r: FrameLexicalRef,
        site: FrameLexicalCallSite,
        compiled_fns: &CompiledFns,
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(target) = self.frame_lexical_routines.get(&r).cloned() else {
            return Ok(None);
        };
        let FrameLexicalCallSite {
            arity,
            arg_sources_idx,
            track_sources,
            call_has_named,
        } = site;
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("Interpreter stack underflow in CallFunc"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let decoded_sources = if track_sources {
            self.decode_arg_sources(code, arg_sources_idx)
        } else {
            None
        };
        let (args, arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, decoded_sources);
        let (args, callsite_line) = self.sanitize_call_args_owned(args);
        let arg_sources = arg_sources.filter(|s| s.len() == args.len());
        // `CallFuncNamed` counts the parser's callsite-line marker among its
        // named arguments; once that is stripped a call may have none left.
        let call_has_named = call_has_named
            && args
                .iter()
                .any(|a| matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)));
        let args = if self.in_lvalue_assignment {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        self.set_pending_callsite_line(callsite_line);
        let result = self.call_frame_lexical_target(
            code,
            &target,
            args,
            arg_sources,
            call_has_named,
            compiled_fns,
        );
        // Captured-outer and `is rw` writes the callee recorded reach the
        // caller's slots on either exit, as for any compiled call.
        self.apply_pending_rw_writeback(code);
        result.map(Some)
    }

    fn call_frame_lexical_target(
        &mut self,
        code: &CompiledCode,
        target: &FrameLexicalTarget,
        args: Vec<Value>,
        arg_sources: Option<Vec<Option<String>>>,
        call_has_named: bool,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        let cf = &target.cf;
        let name_sym = target.name;
        let name = name_sym.as_str();
        if let Some(threaded) = self.autothread_frame_lexical_call(
            code,
            target,
            &args,
            &arg_sources,
            call_has_named,
            compiled_fns,
        )? {
            return Ok(threaded);
        }
        let result = if args.is_empty() && !cf.is_raw && Self::is_fast_call_eligible(cf, name) {
            self.call_compiled_function_fast(cf, name, name_sym, compiled_fns)?
        } else if !call_has_named
            && Self::is_positional_light_call_eligible(
                cf,
                name,
                Self::positional_light_argc(&args),
                &args,
                Some(code),
            )
            && !Self::call_shares_container_into_scalar_param(cf, &args)
        {
            self.call_compiled_function_positional_light(
                cf,
                &args,
                compiled_fns,
                name,
                name_sym,
                Some(code),
            )?
        } else if Self::is_light_call_eligible(cf, name)
            && !Self::call_shares_container_into_scalar_param(cf, &args)
            && !Self::call_shares_container_into_named_scalar_param(
                cf,
                &args,
                arg_sources.as_deref(),
            )
        {
            self.call_compiled_function_light(cf, &args, compiled_fns, name, name_sym)?
        } else {
            self.set_pending_call_arg_sources(arg_sources);
            let result =
                self.call_compiled_function_named(cf, args, compiled_fns, target.package, name_sym);
            self.set_pending_call_arg_sources(None);
            result?
        };
        self.maybe_fetch_rw_proxy(result, !cf.returns_container())
    }

    /// Junction autothreading for a frame-lexical call: the same rule as
    /// `maybe_autothread_func_call`, against the routine's own signature
    /// rather than one resolved by name.
    fn autothread_frame_lexical_call(
        &mut self,
        code: &CompiledCode,
        target: &FrameLexicalTarget,
        args: &[Value],
        arg_sources: &Option<Vec<Option<String>>>,
        call_has_named: bool,
        compiled_fns: &CompiledFns,
    ) -> Result<Option<Value>, RuntimeError> {
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, v)| Self::extract_junction_from_arg(v).is_some())
            .map(|(i, _)| i)
            .collect();
        if junction_indices.is_empty() {
            return Ok(None);
        }
        let indices =
            self.autothread_indices_for_params(args, &junction_indices, &target.cf.param_defs);
        if indices.is_empty() {
            return Ok(None);
        }
        let thread_idx = self.pick_autothread_junction_index(args, &indices);
        let Some((kind, values, pair_key)) = Self::extract_junction_from_arg(&args[thread_idx])
        else {
            return Ok(None);
        };
        let mut results = Vec::with_capacity(values.len());
        let mut writeback: std::collections::HashSet<String> = std::collections::HashSet::new();
        for eigenstate in values.iter() {
            let mut threaded = args.to_vec();
            threaded[thread_idx] = match &pair_key {
                Some(key) => Value::pair(key.clone(), eigenstate.clone()),
                None => eigenstate.clone(),
            };
            results.push(self.call_frame_lexical_target(
                code,
                target,
                threaded,
                arg_sources.clone(),
                call_has_named,
                compiled_fns,
            )?);
            writeback.extend(std::mem::take(&mut self.pending_rw_writeback_sources));
        }
        self.pending_rw_writeback_sources.extend(writeback);
        Ok(Some(Value::junction(kind, results)))
    }
}
