//! Junction autothreading at the wrapped original of a method `.wrap` chain.
//!
//! A wrapped method binds its call to the outermost wrapper's signature first
//! (`Interpreter::method_call_outermost_wrapper`): a `sub (|capture)` wrapper
//! receives a Junction argument whole. Its `callsame` then reaches the original
//! method, whose own signature decides threading exactly as a direct call
//! would: a parameter that does not accept a Junction runs the method once per
//! eigenstate and the results form a Junction of the same kind (rakudo's
//! `Routine.wrap` + `callsame`; LibTmux's `is single-operation` trait).

use super::*;

impl Interpreter {
    /// When the innermost method dispatch frame is about to leave a wrapper
    /// for its Candidate and a Junction argument does not bind to that
    /// candidate's parameters, run the candidate once per eigenstate through
    /// [`Self::dispatch_next_candidate`] and answer the Junction of results.
    /// `None` when nothing threads (the caller continues normally).
    // Cost: O(p + a) to decide, p = candidate parameters, a = arguments; the
    // threaded case adds one candidate call per eigenstate.
    pub(super) fn autothread_wrapped_candidate(
        &mut self,
        frame_idx: usize,
        func_name: &str,
        override_args: &Option<Vec<Value>>,
    ) -> Result<Option<Value>, RuntimeError> {
        let frame = &self.dispatch.method_dispatch_stack[frame_idx];
        if !frame.in_wrapper {
            return Ok(None);
        }
        let Some(DeferralEntry::Candidate { def, .. }) = frame.remaining.first() else {
            return Ok(None);
        };
        // A wrapper's `callwith` args carry the invocant as element 0.
        let (invocant, args) = match override_args {
            Some(new_args) => match new_args.split_first() {
                Some((inv, rest)) => (inv.clone(), rest.to_vec()),
                None => (frame.invocant.clone(), Vec::new()),
            },
            None => (frame.invocant.clone(), frame.args.clone()),
        };
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, a)| Self::extract_junction_from_arg(a).is_some())
            .map(|(i, _)| i)
            .collect();
        if junction_indices.is_empty() {
            return Ok(None);
        }
        let param_defs = def.param_defs.clone();
        let indices = self.autothread_indices_for_params(&args, &junction_indices, &param_defs);
        if indices.is_empty() {
            return Ok(None);
        }
        let thread_idx = self.pick_autothread_junction_index(&args, &indices);
        let Some((kind, values, pair_key)) = Self::extract_junction_from_arg(&args[thread_idx])
        else {
            return Ok(None);
        };
        let snapshot = self.dispatch.method_dispatch_stack[frame_idx].clone();
        let mut results = Vec::with_capacity(values.len());
        for eigenstate in values.iter() {
            self.dispatch.method_dispatch_stack[frame_idx] = snapshot.clone();
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(invocant.clone());
            for (i, arg) in args.iter().enumerate() {
                call_args.push(if i != thread_idx {
                    arg.clone()
                } else if let Some(key) = &pair_key {
                    Value::pair(key.clone(), eigenstate.clone())
                } else {
                    eigenstate.clone()
                });
            }
            results.push(self.dispatch_next_candidate(func_name, Some(call_args), false)?);
        }
        Ok(Some(Value::junction(kind, results)))
    }
}
