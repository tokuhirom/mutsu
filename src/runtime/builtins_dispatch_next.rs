use super::*;
use crate::symbol::Symbol;

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
            let (receiver_class, invocant, call_args, owner_class, method_def) = {
                let frame = &mut self.method_dispatch_stack[frame_idx];
                let Some((owner_class, method_def)) = frame.remaining.first().cloned() else {
                    return Ok(Value::Nil);
                };
                frame.remaining.remove(0);
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
                )
            };
            let (result, updated_invocant) = match &invocant {
                Value::Instance {
                    class_name,
                    attributes,
                    id: target_id,
                } => {
                    let (result, updated) = self.run_instance_method_resolved(
                        &receiver_class,
                        &owner_class,
                        method_def,
                        (**attributes).clone(),
                        call_args,
                        Some(invocant.clone()),
                    )?;
                    self.overwrite_instance_bindings_by_identity(
                        &class_name.resolve(),
                        *target_id,
                        updated.clone(),
                    );
                    (
                        result,
                        Some(Value::make_instance_with_id(
                            *class_name,
                            updated,
                            *target_id,
                        )),
                    )
                }
                _ => {
                    let (result, _) = self.run_instance_method_resolved(
                        &receiver_class,
                        &owner_class,
                        method_def,
                        HashMap::new(),
                        call_args,
                        Some(invocant.clone()),
                    )?;
                    (result, None)
                }
            };
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
        if let Some((_name, candidates, orig_args)) = self.multi_dispatch_stack.last().cloned() {
            let call_args = override_args.unwrap_or(orig_args);
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
                return Ok(Value::Nil);
            };
            let next_def = candidates[idx].clone();
            let remaining = candidates[idx + 1..].to_vec();
            let stack_len = self.multi_dispatch_stack.len();
            self.multi_dispatch_stack[stack_len - 1] = (_name, remaining, call_args.clone());
            let result = self.call_function_def(&next_def, &call_args)?;
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
                .is_some_and(|(_, name)| name == "new")
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
        let Some((_name, candidates, _orig_args)) = self.multi_dispatch_stack.last().cloned()
        else {
            return Ok(Value::Nil);
        };
        let Some(next_def) = candidates.first().cloned() else {
            return Ok(Value::Nil);
        };
        // Remove this candidate from the remaining list
        let remaining = candidates[1..].to_vec();
        let stack_len = self.multi_dispatch_stack.len();
        self.multi_dispatch_stack[stack_len - 1] = (_name, remaining, Vec::new());
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
