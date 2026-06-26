use super::*;
use crate::symbol::Symbol;

/// Extract `(positional_arg_index, sigil-less_param_name)` for each scalar
/// `is rw`/`is raw` positional parameter of a multi candidate's signature.
/// Used by the nextsame/callsame+rw redispatch chain (§D capstone) to locate the
/// FIRST (winning, compiled) candidate's rw params: their CURRENT value is
/// forwarded to the next candidate, and the chain's final value is written back
/// into the first candidate's VM local slot so its exit flush propagates it
/// rather than clobbering it with its own pre-nextsame value.
pub(super) fn rw_scalar_positional_params(
    param_defs: &[crate::ast::ParamDef],
) -> Vec<(usize, String)> {
    let mut out = Vec::new();
    let mut pos = 0usize;
    for pd in param_defs {
        if pd.is_invocant || pd.named {
            continue;
        }
        let is_scalar = pd.name != "_"
            && pd
                .name
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_');
        let is_rw = pd.traits.iter().any(|t| t == "rw" || t == "raw");
        if is_scalar && is_rw && !pd.slurpy && !pd.double_slurpy && !pd.onearg {
            out.push((pos, pd.name.clone()));
        }
        // Slurpy params consume the remaining positionals; positional indexing
        // past one is ambiguous, but rw scalars always precede a slurpy, so the
        // indices collected above stay correct.
        if !pd.slurpy && !pd.double_slurpy {
            pos += 1;
        }
    }
    out
}

impl Interpreter {
    pub(super) fn no_dispatcher_error(func_name: &str) -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "{func_name} is not in the dynamic scope of a dispatcher"
            )),
        );
        let msg = format!("{func_name} is not in the dynamic scope of a dispatcher");
        let ex = Value::make_instance(Symbol::intern("X::NoDispatcher"), attrs);
        RuntimeError {
            exception: Some(Box::new(ex)),
            ..RuntimeError::new(msg)
        }
    }

    /// Trim the candidate list so that the current call is the final candidate.
    /// After lastcall, callsame/nextsame from the same dispatch context return Nil.
    pub(super) fn builtin_lastcall(&mut self) -> Result<Value, RuntimeError> {
        // Clear remaining candidates of the topmost dispatch frame.
        // Try wrap dispatch stack first.
        if let Some(frame) = self.wrap_dispatch_stack.last_mut() {
            frame.remaining.clear();
            return Ok(Value::Bool(true));
        }
        // Try method dispatch stack.
        if let Some(frame) = self.method_dispatch_stack.last_mut() {
            frame.remaining.clear();
            return Ok(Value::Bool(true));
        }
        // Try multi dispatch stack.
        if let Some(top) = self.multi_dispatch_stack.last_mut() {
            top.1.clear();
            return Ok(Value::Bool(true));
        }
        // Outside a dispatch context: no-op (return False).
        Ok(Value::Bool(false))
    }

    /// Call next method/multi candidate with the original args; returns the result.
    pub(super) fn builtin_callsame(&mut self) -> Result<Value, RuntimeError> {
        self.dispatch_next_candidate("callsame", None, false)
    }

    /// Call next method/multi candidate with the original args; never returns (tail-call).
    pub(super) fn builtin_nextsame(&mut self) -> Result<Value, RuntimeError> {
        self.dispatch_next_candidate("nextsame", None, true)
    }

    /// Call next method/multi candidate with new args; returns the result.
    pub(super) fn builtin_callwith(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.dispatch_next_candidate("callwith", Some(args.to_vec()), false)
    }

    /// Call next method/multi candidate with new args; never returns (tail-call).
    pub(super) fn builtin_nextwith(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.dispatch_next_candidate("nextwith", Some(args.to_vec()), true)
    }

    /// Re-dispatch to the same multi/method from the top with new arguments.
    pub(super) fn builtin_samewith(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Use the samewith context stack to find the enclosing multi sub/method.
        if let Some((name, invocant)) = self.samewith_context_stack.last().cloned() {
            if let Some(inv) = invocant {
                // Method dispatch: re-call the method on the same invocant
                return self.call_method_with_values(inv, &name, args.to_vec());
            } else {
                // Sub dispatch: re-call the function by name
                return self.call_function(&name, args.to_vec());
            }
        }
        Err(RuntimeError::new(
            "samewith called outside of a dispatch context",
        ))
    }

    /// Shared implementation for callsame/nextsame/callwith/nextwith.
    /// `override_args`: if Some, use these args instead of the original.
    /// `tail_call`: if true, raise a return-control exception with the result.
    fn dispatch_next_candidate(
        &mut self,
        func_name: &str,
        override_args: Option<Vec<Value>>,
        tail_call: bool,
    ) -> Result<Value, RuntimeError> {
        // Try wrap dispatch stack first (wrapper chains).
        if let Some(frame) = self.wrap_dispatch_stack.last_mut() {
            if let Some(next) = frame.remaining.first().cloned() {
                frame.remaining.remove(0);
                let call_args = override_args.unwrap_or_else(|| frame.args.clone());
                // If this is a method wrap original, separate the invocant
                // from the args and dispatch as a method call.
                let result = if let Value::Sub(ref data) = next
                    && data.env.get("__mutsu_method_wrap_original").is_some()
                    && !call_args.is_empty()
                {
                    let invocant = call_args[0].clone();
                    let method_args = call_args[1..].to_vec();
                    let method_name = data.name.resolve();
                    self.call_method_with_values(invocant, &method_name, method_args)?
                } else {
                    self.call_sub_value(next, call_args, false)?
                };
                if tail_call {
                    return Err(RuntimeError {
                        return_value: Some(result),
                        ..RuntimeError::new("")
                    });
                }
                return Ok(result);
            }
            // Remaining is empty.
            if frame.sub_id != 0 {
                // Non-method wrap: exhausted — return Nil
                if tail_call {
                    return Err(RuntimeError {
                        return_value: Some(Value::Nil),
                        ..RuntimeError::new("")
                    });
                }
                return Ok(Value::Nil);
            }
            // Method wraps (sub_id == 0): fall through to method dispatch stack
            // so callsame inside the original method can continue the MRO chain.
        }
        // Try method dispatch stack
        if !self.method_dispatch_stack.is_empty() {
            let frame_idx = self.method_dispatch_stack.len() - 1;
            let is_override = override_args.is_some();
            let (receiver_class, invocant, mut call_args, owner_class, mut method_def, rw_params) = {
                let frame = &mut self.method_dispatch_stack[frame_idx];
                let Some((owner_class, method_def)) = frame.remaining.first().cloned() else {
                    if tail_call {
                        return Err(RuntimeError {
                            return_value: Some(Value::Nil),
                            ..RuntimeError::new("")
                        });
                    }
                    return Ok(Value::Nil);
                };
                frame.remaining.remove(0);
                let rw_params = frame.rw_params.clone();
                let call_args = if let Some(new_args) = override_args {
                    // Update the frame's args so subsequent callsame uses the new args
                    frame.args = new_args.clone();
                    new_args
                } else {
                    frame.args.clone()
                };
                // Use the current `self` from the environment instead of the stale
                // frame invocant.  The method body may have mutated attributes
                // (e.g. `$.tracker ~= "bar,"`) before calling callsame/callwith,
                // so the frame's snapshot is outdated.
                let current_invocant = self
                    .env
                    .get("self")
                    .cloned()
                    .unwrap_or_else(|| frame.invocant.clone());
                (
                    frame.receiver_class.clone(),
                    current_invocant,
                    call_args,
                    owner_class,
                    method_def,
                    rw_params,
                )
            };
            // §B: compile the next MRO candidate on-demand if it has no compiled code
            // (a runtime-added / not-yet-compiled body), so the dispatch below runs it
            // compiled rather than tree-walked — the non-delegation tree-walk arm of
            // `forward_resolved_delegation` was deleted (#3680). Delegation forwarders
            // keep their synthesized empty body uncompiled.
            if method_def.compiled_code.is_none() && method_def.delegation.is_none() {
                Self::compile_method_def_in_place(&mut method_def, &owner_class);
            }
            // nextsame/callsame+rw chaining for methods (§D capstone): the stored
            // method args are plain values (no varref), so without an arg source the
            // next candidate's `is rw` param dies with X::Parameter::RW. Forward the
            // first candidate's CURRENT rw value and name the FIRST candidate's param
            // as the source, so the next candidate writes back into it (env-only, all
            // method candidates run via the interpreter) and the first candidate's own
            // exit writeback then propagates the chained result to the caller.
            let mut rw_sources: Vec<Option<String>> = Vec::new();
            let mut have_rw_source = false;
            if !rw_params.is_empty() && !is_override {
                rw_sources = vec![None; call_args.len()];
                for (pos, first_param) in &rw_params {
                    if *pos >= call_args.len() {
                        continue;
                    }
                    if let Some(cur) = self.env.get(first_param).cloned() {
                        call_args[*pos] = crate::runtime::types::unwrap_varref_value(cur);
                    }
                    rw_sources[*pos] = Some(first_param.clone());
                    have_rw_source = true;
                }
            }
            if have_rw_source {
                self.set_pending_call_arg_sources(Some(rw_sources));
            }
            // The first method candidate runs as compiled bytecode (VM slots), so
            // its exit flush reads its rw-param slot. Capture its frame code now to
            // write the chain's final value into that slot after the redispatch.
            let caller_code = self.current_code;
            // §B: run the next MRO candidate as compiled bytecode (`call_compiled_method`)
            // when it has compiled code (always, after the on-demand compile above,
            // except a delegation forwarder). Both leave the active
            // `method_dispatch_stack` frame in place (neither pushes a new one), so a
            // further `nextsame`/`callsame` inside the candidate continues this same MRO
            // chain. Methods without compiled code (a delegation forwarder)
            // keep the interpreter path.
            let method_name_for_dispatch = self
                .samewith_context_stack
                .last()
                .map(|(n, _)| n.clone())
                .unwrap_or_default();
            let empty_fns: HashMap<String, crate::opcode::CompiledFunction> = HashMap::new();
            let dispatch_result = match &invocant {
                Value::Instance {
                    class_name,
                    attributes,
                    id: target_id,
                } => {
                    if let Some(cc) = method_def.compiled_code.clone() {
                        self.call_compiled_method(
                            &receiver_class,
                            &owner_class,
                            &method_name_for_dispatch,
                            &method_def,
                            &cc,
                            attributes.to_map(),
                            call_args,
                            Some(invocant.clone()),
                            &empty_fns,
                        )
                        .map(|(result, updated, adjusted)| {
                            // Commit only an `adjusted` (`:=`-recovered) snapshot, exactly
                            // like `dispatch_compiled_method`: an unadjusted run already
                            // mutated the shared attribute cell in place, so writing the
                            // baseline snapshot back would clobber it (e.g. a parent
                            // `callsame` candidate's `self.x ~= ...`).
                            let new_inv = if adjusted {
                                Value::write_back_sharing(
                                    attributes,
                                    *class_name,
                                    updated,
                                    *target_id,
                                )
                            } else {
                                invocant.clone()
                            };
                            (result, Some(new_inv))
                        })
                    } else {
                        self.forward_resolved_delegation(
                            &receiver_class,
                            &owner_class,
                            method_def,
                            attributes.to_map(),
                            call_args,
                            Some(invocant.clone()),
                        )
                        .map(|(result, updated)| {
                            (
                                result,
                                Some(Value::write_back_sharing(
                                    attributes,
                                    *class_name,
                                    updated,
                                    *target_id,
                                )),
                            )
                        })
                    }
                }
                _ => {
                    if let Some(cc) = method_def.compiled_code.clone() {
                        self.call_compiled_method(
                            &receiver_class,
                            &owner_class,
                            &method_name_for_dispatch,
                            &method_def,
                            &cc,
                            HashMap::new(),
                            call_args,
                            Some(invocant.clone()),
                            &empty_fns,
                        )
                        .map(|(result, _, _)| (result, None))
                    } else {
                        self.forward_resolved_delegation(
                            &receiver_class,
                            &owner_class,
                            method_def,
                            HashMap::new(),
                            call_args,
                            Some(invocant.clone()),
                        )
                        .map(|(result, _)| (result, None))
                    }
                }
            };
            if have_rw_source {
                self.set_pending_call_arg_sources(None);
            }
            let (result, updated_invocant) = dispatch_result?;
            // Write the chain's final value (now in env under each first-candidate
            // param name) back into the first (compiled) candidate's VM local slot
            // so its exit flush propagates it instead of its own pre-nextsame value.
            if caller_code != 0 && have_rw_source {
                // SAFETY: caller_code is the address of the CompiledCode of the
                // first (compiled) method candidate, the live ancestor frame
                // currently executing this nextsame/callsame.
                let code = unsafe { &*(caller_code as *const crate::opcode::CompiledCode) };
                for (_pos, first_param) in &rw_params {
                    if let Some(slot) = code.locals.iter().position(|n| n == first_param)
                        && let Some(val) = self.env.get(first_param).cloned()
                    {
                        self.locals[slot] = val;
                    }
                }
            }
            if let Some(new_invocant) = updated_invocant
                && let Some(frame) = self.method_dispatch_stack.get_mut(frame_idx)
            {
                frame.invocant = new_invocant;
            }
            if tail_call {
                return Err(RuntimeError {
                    return_value: Some(result),
                    ..RuntimeError::new("")
                });
            }
            return Ok(result);
        }
        // Try multi dispatch stack
        if let Some((_name, candidates, orig_args, rw_params)) =
            self.multi_dispatch_stack.last().cloned()
        {
            let is_override = override_args.is_some();
            let mut call_args = override_args.unwrap_or(orig_args);
            // nextsame/callsame+rw chaining (§D capstone): forward each scalar rw
            // param's CURRENT value (it was mutated by the first candidate's body
            // before it called nextsame) to the next candidate, and record the
            // caller source + first candidate slot so the chain's final value is
            // written back into the first (compiled) candidate's VM local slot —
            // its exit flush reads that slot, so without this the first
            // candidate's own pre-nextsame value clobbers the chained result.
            let caller_code = self.current_code;
            let mut rw_writebacks: Vec<(String, String)> = Vec::new();
            let mut rw_sources: Vec<Option<String>> = Vec::new();
            let mut have_rw_source = false;
            if !rw_params.is_empty() && !is_override {
                rw_sources = vec![None; call_args.len()];
                for (pos, first_param) in &rw_params {
                    let Some(arg) = call_args.get(*pos).cloned() else {
                        continue;
                    };
                    let caller_source =
                        crate::runtime::types::indexed_varref_from_value(&arg).map(|(n, _, _)| n);
                    // The first candidate's live (body-mutated) param value.
                    if let Some(cur) = self.env.get(first_param).cloned() {
                        call_args[*pos] =
                            match crate::runtime::types::indexed_varref_from_value(&arg) {
                                Some((name, _, index)) => {
                                    crate::runtime::types::make_varref_value(name, cur, index)
                                }
                                None => cur,
                            };
                    }
                    if let Some(src) = caller_source {
                        if *pos < rw_sources.len() {
                            rw_sources[*pos] = Some(src.clone());
                        }
                        rw_writebacks.push((src, first_param.clone()));
                        have_rw_source = true;
                    }
                }
            }
            // Find the first candidate whose signature matches the (possibly new) args.
            let mut matched_idx = None;
            for (i, cand) in candidates.iter().enumerate() {
                if self.args_match_param_types(&call_args, &cand.param_defs) {
                    matched_idx = Some(i);
                    break;
                }
            }
            let Some(idx) = matched_idx else {
                // No candidate matches — return Nil (nowhere to defer to)
                if tail_call {
                    return Err(RuntimeError {
                        return_value: Some(Value::Nil),
                        ..RuntimeError::new("")
                    });
                }
                return Ok(Value::Nil);
            };
            let next_def = candidates[idx].clone();
            let remaining = candidates[idx + 1..].to_vec();
            let stack_len = self.multi_dispatch_stack.len();
            // Keep rw_params fixed: it always identifies the FIRST candidate's
            // slots, even as the chain advances through later candidates.
            self.multi_dispatch_stack[stack_len - 1] =
                (_name, remaining, call_args.clone(), rw_params.clone());
            if have_rw_source {
                self.set_pending_call_arg_sources(Some(rw_sources));
            }
            let result = self.call_function_def(&next_def, &call_args);
            if have_rw_source {
                self.set_pending_call_arg_sources(None);
            }
            let result = result?;
            // Write the chain's final value (now in env under each caller source)
            // back into the first candidate's VM local slot so its exit flush
            // propagates it instead of its own pre-nextsame value.
            if !rw_writebacks.is_empty() {
                // SAFETY: caller_code is the address of the CompiledCode of the
                // first (compiled) candidate, which is the live ancestor frame
                // currently executing this nextsame/callsame.
                let code = (caller_code != 0)
                    .then(|| unsafe { &*(caller_code as *const crate::opcode::CompiledCode) });
                for (caller_source, first_param) in &rw_writebacks {
                    let Some(val) = self.env.get(caller_source).cloned() else {
                        continue;
                    };
                    // Update the first candidate's param both as a VM local slot
                    // (its exit flush reads the slot) and in env (a callsame body
                    // that resumes after the redispatch may read the param by
                    // name), so the chain's final value survives both routes.
                    if let Some(code) = code
                        && let Some(slot) = code.locals.iter().position(|n| n == first_param)
                    {
                        self.locals[slot] = val.clone();
                    }
                    self.env.insert(first_param.clone(), val);
                }
            }
            if tail_call {
                return Err(RuntimeError {
                    return_value: Some(result),
                    ..RuntimeError::new("")
                });
            }
            return Ok(result);
        }
        // Fallback: if we are inside a `new` method and nextwith/callwith is called,
        // dispatch to the built-in Mu.new (i.e., bless) on the current invocant.
        // In Raku, Mu.new(*%attrinit) is always the base candidate in the MRO for `new`.
        // Check both routine_stack (VM path) and samewith_context_stack (interpreter path).
        if matches!(func_name, "nextwith" | "callwith") {
            let in_new = self
                .routine_stack
                .last()
                .is_some_and(|frame| frame.name == "new")
                || self
                    .samewith_context_stack
                    .last()
                    .is_some_and(|(name, _)| name == "new");
            if in_new && let Some(invocant) = self.env.get("self").cloned() {
                let call_args = override_args.unwrap_or_default();
                let result = self.call_method_with_values(invocant, "bless", call_args)?;
                if tail_call {
                    return Err(RuntimeError {
                        return_value: Some(result),
                        ..RuntimeError::new("")
                    });
                }
                return Ok(result);
            }
        }
        // If we're inside a method but there's simply no next candidate in the MRO,
        // return Nil (this is the Raku behavior for callsame/callwith at the end of
        // the MRO).  Plain subs without multi dispatch should still throw.
        if !self.method_class_stack.is_empty() {
            if tail_call {
                return Err(RuntimeError {
                    return_value: Some(Value::Nil),
                    ..RuntimeError::new("")
                });
            }
            return Ok(Value::Nil);
        }
        // Not in any dispatch context
        Err(Self::no_dispatcher_error(func_name))
    }

    pub(super) fn builtin_nextcallee(&mut self) -> Result<Value, RuntimeError> {
        // Check wrap dispatch stack first (wrapper chains)
        if let Some(frame) = self.wrap_dispatch_stack.last_mut() {
            if let Some(next) = frame.remaining.first().cloned() {
                frame.remaining.remove(0);
                return Ok(next);
            }
            return Ok(Value::Nil);
        }
        // Check method dispatch stack
        // (not yet implemented for methods — return Nil)
        // Check multi dispatch stack
        let Some((_name, candidates, orig_args, rw_params)) =
            self.multi_dispatch_stack.last().cloned()
        else {
            return Ok(Value::Nil);
        };
        // Find the first candidate that matches the original arguments,
        // mirroring the behavior of dispatch_next_candidate/callsame.
        // This ensures nextcallee returns the candidate that callsame would
        // have dispatched to, skipping non-matching candidates.
        let mut matched_idx = None;
        for (i, cand) in candidates.iter().enumerate() {
            if self.args_match_param_types(&orig_args, &cand.param_defs) {
                matched_idx = Some(i);
                break;
            }
        }
        let Some(idx) = matched_idx else {
            return Ok(Value::Nil);
        };
        let next_def = candidates[idx].clone();
        // Remove this candidate and all before it from the remaining list
        let remaining = candidates[idx + 1..].to_vec();
        let stack_len = self.multi_dispatch_stack.len();
        self.multi_dispatch_stack[stack_len - 1] = (_name, remaining, orig_args, rw_params);
        // Return as a callable Sub value
        Ok(Value::make_sub(
            next_def.package,
            next_def.name,
            next_def.params.clone(),
            next_def.param_defs.clone(),
            next_def.body.clone(),
            next_def.is_rw,
            self.env.clone(),
        ))
    }
}
