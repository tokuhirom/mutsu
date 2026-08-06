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
            return Ok(Value::TRUE);
        }
        // Try method dispatch stack.
        if let Some(frame) = self.method_dispatch_stack.last_mut() {
            frame.remaining.clear();
            return Ok(Value::TRUE);
        }
        // Try multi dispatch stack.
        if let Some(top) = self.multi_dispatch_stack.last_mut() {
            top.1.clear();
            return Ok(Value::TRUE);
        }
        // Outside a dispatch context: no-op (return False).
        Ok(Value::FALSE)
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
        // A lazy `gather` body re-pushes the context it captured at creation for
        // the duration of its force (see `push_captured_samewith_context`), so
        // this stack is correct there too.
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

    /// Env key holding the routine name a lazy `gather` was created inside, so
    /// `samewith` still resolves once the gather body is forced. By then the
    /// declaring routine's dynamic `samewith_context_stack` frame is gone — and
    /// worse, the frame on top belongs to whichever routine happened to force
    /// the gather, so consulting the dynamic stack silently redispatched the
    /// WRONG routine rather than failing.
    ///
    /// `samewith` is *lexical* in Rakudo — it re-dispatches `&?ROUTINE` — so
    /// capturing it with the gather's env snapshot is the right shape, not a
    /// workaround: the body keeps referring to the routine it was written in.
    pub(crate) const SAMEWITH_LEXICAL_NAME_KEY: &str = "__mutsu_samewith_lexical_name";
    /// Companion of [`Self::SAMEWITH_LEXICAL_NAME_KEY`] holding the invocant for
    /// a method (absent for a plain sub).
    pub(crate) const SAMEWITH_LEXICAL_INVOCANT_KEY: &str = "__mutsu_samewith_lexical_invocant";

    /// Record the innermost dynamic samewith context into `env` so a closure
    /// captured from it (today: a lazy `gather` body) can still redispatch.
    pub(crate) fn capture_samewith_context_into(&self, env: &mut crate::env::Env) {
        let Some((name, invocant)) = self.samewith_context_stack.last() else {
            return;
        };
        env.insert(
            Self::SAMEWITH_LEXICAL_NAME_KEY.to_string(),
            Value::str(name.clone()),
        );
        if let Some(inv) = invocant {
            env.insert(Self::SAMEWITH_LEXICAL_INVOCANT_KEY.to_string(), inv.clone());
        }
    }

    /// Re-push the samewith context `env` captured (see
    /// [`Self::capture_samewith_context_into`]) for the duration of a lazy
    /// `gather` body's execution, so `samewith` written in that body resolves
    /// to the routine the body was written in. Returns whether a frame was
    /// pushed; the caller must then `pop_captured_samewith_context`.
    ///
    /// Pushing (rather than consulting the env at the `samewith` call) keeps
    /// ordinary stack semantics: a routine *called from* the body pushes its
    /// own frame on top and its `samewith` still means itself.
    pub(crate) fn push_captured_samewith_context(&mut self, env: &crate::env::Env) -> bool {
        let Some(name) = env.get(Self::SAMEWITH_LEXICAL_NAME_KEY) else {
            return false;
        };
        let name = name.to_string_value();
        let invocant = env.get(Self::SAMEWITH_LEXICAL_INVOCANT_KEY).cloned();
        self.samewith_context_stack.push((name, invocant));
        true
    }

    /// Undo a [`Self::push_captured_samewith_context`] that returned `true`.
    pub(crate) fn pop_captured_samewith_context(&mut self, pushed: bool) {
        if pushed {
            self.samewith_context_stack.pop();
        }
    }

    /// When a user method on a subclass of a builtin metamodel class
    /// (Metamodel::ClassHOW / Metamodel::GrammarHOW) exhausts the user-defined
    /// MRO, provide the NATIVE metamodel implementation as the final
    /// `callsame` candidate. Native metamodel methods are not `MethodDef`
    /// candidates, so the regular MRO chain cannot reach them.
    fn native_metamodel_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let (_depth, _receiver, method_name, orig_args) =
            self.metamodel_dispatch_stack.last().cloned()?;
        // Only fire when the innermost method dispatch is the metamodel method
        // itself (not some helper method it called).
        if self
            .samewith_context_stack
            .last()
            .is_none_or(|(n, _)| n != &method_name)
        {
            return None;
        }
        let args: Vec<Value> = override_args.map(<[Value]>::to_vec).unwrap_or(orig_args);
        match method_name.as_str() {
            "find_method" => {
                let obj = args.first()?.clone();
                let name = args.get(1)?.to_string_value();
                Some(Ok(self
                    .classhow_find_method(&obj, &name)
                    .unwrap_or(Value::NIL)))
            }
            // mutsu has no method cache to publish; the default is a no-op.
            "publish_method_cache" => Some(Ok(Value::NIL)),
            // `callsame` from a user `new_type` override (OO::Monitors'
            // MonitorHOW) while a DECLARE'd class is being registered: the
            // native part of `new_type` — creating and registering the type —
            // has already run, so the base candidate simply returns the type
            // object under registration.
            "new_type" => self.pending_declare_new_type.clone().map(Ok),
            // Any other native ClassHOW metamethod (`add_method`, `compose`,
            // `add_attribute`, `attributes`, ...) is the final base candidate
            // when the user MRO is exhausted — same routing as the direct
            // native fallback for methods a user HOW does not override.
            _ if Self::is_classhow_method(&method_name) => {
                Some(self.dispatch_classhow_method(&method_name, args))
            }
            _ => None,
        }
    }

    /// When a user BUILDALL/POPULATE/clone method (typically installed by a
    /// custom HOW's `add_method` — OO::Monitors) exhausts the user MRO via
    /// `callsame`/`nextsame`, provide the NATIVE Mu base behavior as the final
    /// candidate: the already-built instance for BUILDALL/POPULATE (mutsu's
    /// native build ran before the user hook — see `run_user_buildall_hook`),
    /// and the native attribute-copying clone for `clone`.
    fn native_mu_base_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let method_name = self.samewith_context_stack.last().map(|(n, _)| n.clone())?;
        if !matches!(method_name.as_str(), "BUILDALL" | "POPULATE" | "clone") {
            return None;
        }
        let frame = self.method_dispatch_stack.last()?;
        let frame_args = frame.args.clone();
        let invocant = self
            .env
            .get("self")
            .cloned()
            .unwrap_or_else(|| frame.invocant.clone());
        match method_name.as_str() {
            "BUILDALL" | "POPULATE" => Some(Ok(invocant)),
            "clone" => {
                let args: Vec<Value> = override_args.map(<[Value]>::to_vec).unwrap_or(frame_args);
                self.native_instance_clone_value(&invocant, &args)
            }
            _ => None,
        }
    }

    /// When a user `is Array` subclass overrides a Positional protocol method
    /// (`AT-POS`/`ASSIGN-POS`/`BIND-POS`/`DELETE-POS`/`elems`/`push`/...) and
    /// calls `nextsame`/`nextwith` (or `callsame`/`callwith`), the NATIVE array
    /// behavior on the instance's backing `__mutsu_array_storage` is the final
    /// base candidate — `Array` is not a user-registered class with real
    /// `MethodDef`s, so the regular MRO chain never reaches it (mirrors
    /// `native_mu_base_next_candidate`). Without this, e.g. `Array::Rounded`'s
    /// `method AT-POS($index) { nextwith $index.round }` silently returned Nil.
    fn native_array_storage_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let method_name = self.samewith_context_stack.last().map(|(n, _)| n.clone())?;
        // A single (non-multi, non-wrapped) compiled method pushes no
        // `method_dispatch_stack` frame, so the invocant/args must come from
        // the samewith context and `self` rather than a dispatch frame (mirrors
        // `native_mu_base_next_candidate`'s `self.env.get("self")` fallback).
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| {
                self.samewith_context_stack
                    .last()
                    .and_then(|(_, i)| i.clone())
            })
            .or_else(|| self.env.get("self").cloned())?;
        let args: Vec<Value> = match override_args {
            Some(a) => a.to_vec(),
            None => self
                .method_dispatch_stack
                .last()
                .map(|f| f.args.clone())
                .unwrap_or_default(),
        };
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = invocant.view()
        else {
            return None;
        };
        if !attributes.contains_key("__mutsu_array_storage")
            || !self
                .mro_readonly(&class_name.resolve())
                .iter()
                .any(|n| n == "Array")
        {
            return None;
        }
        let method_sym = Symbol::intern(&method_name);
        attributes.with_attr_mut("__mutsu_array_storage", |storage| {
            self.try_native_method(storage, method_sym, &args)
        })?
    }

    /// When a user-overridden grammar `parse`/`subparse`/`parsefile` calls
    /// `nextsame`/`nextwith` (or `callsame`/`callwith`) and the user MRO is
    /// exhausted, the NATIVE grammar parse is the final base candidate. It is not
    /// a `MethodDef`, so the regular MRO chain never reaches it (mirrors
    /// `native_metamodel_next_candidate`). YAMLish relies on this: its
    /// `method parse` wraps the native parse to inject `:actions(Actions)`.
    fn native_grammar_parse_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let method_name = self.samewith_context_stack.last().map(|(n, _)| n.clone())?;
        if !matches!(method_name.as_str(), "parse" | "subparse" | "parsefile") {
            return None;
        }
        let frame = self.method_dispatch_stack.last()?;
        let receiver_class = frame.receiver_class.clone();
        let orig_args = frame.args.clone();
        if !self.class_is_grammar(&receiver_class) {
            return None;
        }
        let args: Vec<Value> = override_args.map(<[Value]>::to_vec).unwrap_or(orig_args);
        Some(self.dispatch_package_parse(&receiver_class, &method_name, &args))
    }

    /// Shared implementation for callsame/nextsame/callwith/nextwith.
    /// `override_args`: if Some, use these args instead of the original.
    /// `tail_call`: if true, raise a return-control exception with the result.
    /// Read the FIRST candidate's live rw-param value during a nextsame/callsame
    /// redispatch. The first candidate's body ran `$x = ...` before deferring, and
    /// under the (B) env-write policy that mutation lands only in its VM local slot
    /// (the env write is skipped) — so read the slot of the currently-executing
    /// frame (`self.current_code`/`self.locals`) first and fall back to env.
    fn first_candidate_rw_value(&self, first_param: &str) -> Option<Value> {
        if self.current_code != 0 {
            // SAFETY: `self.current_code` is the CompiledCode of the frame that is
            // synchronously executing this nextsame/callsame — the first candidate.
            let code = unsafe { &*(self.current_code as *const crate::opcode::CompiledCode) };
            if let Some(slot) = code.locals.iter().position(|n| n == first_param)
                && let Some(val) = self.locals.get(slot)
            {
                return Some(val.clone());
            }
        }
        self.env.get(first_param).cloned()
    }

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
                let is_override = override_args.is_some();
                let call_args = override_args.unwrap_or_else(|| frame.args.clone());
                let is_method_wrap = frame.sub_id == 0;
                // Restore the original call site's arg-source names: the
                // outermost wrapper's own binding consumed the pending ones, so
                // an `is rw` parameter of the next callee (a wrappee or the
                // original routine) would otherwise have no writable source to
                // name and die with X::Parameter::RW. `callwith`-style override
                // args are a different call, so they keep no sources.
                let frame_arg_sources = frame.arg_sources.clone();
                let restore_sources = !is_override && frame_arg_sources.is_some();
                if restore_sources {
                    self.set_pending_call_arg_sources(frame_arg_sources);
                }
                // If this is a method wrap original, separate the invocant
                // from the args and dispatch as a method call.
                let result = if let ValueView::Sub(data) = next.view()
                    && data.env.get("__mutsu_method_wrap_original").is_some()
                    && !call_args.is_empty()
                {
                    let invocant = call_args[0].clone();
                    let method_args = call_args[1..].to_vec();
                    let method_name = data.name.resolve();
                    self.call_method_with_values(invocant, &method_name, method_args)?
                } else {
                    // This exact call continues the active wrap chain — it must
                    // run `next` directly, not re-enter the chain from the top.
                    // An inner *wrapper* sees the same invocant-prepended
                    // argument list the outermost one did, so its sources need
                    // the same shift.
                    if restore_sources && is_method_wrap {
                        self.shift_arg_sources_for_wrap_invocant();
                    }
                    if let ValueView::Sub(data) = next.view() {
                        self.wrap_skip_once = Some(data.id);
                    }
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
                        return_value: Some(Value::NIL),
                        ..RuntimeError::new("")
                    });
                }
                return Ok(Value::NIL);
            }
            // Method wraps (sub_id == 0): fall through to method dispatch stack
            // so callsame inside the original method can continue the MRO chain.
        }
        // Try method dispatch stack
        if !self.method_dispatch_stack.is_empty() {
            let frame_idx = self.method_dispatch_stack.len() - 1;
            let is_override = override_args.is_some();
            // If the next MRO candidate carries a method-level wrap chain, route the
            // call through its wrappers so a `nextsame` reaching a wrapped parent
            // method runs ...->wrapper->original->... in order (S06-advanced/wrap.t
            // GH#2178). The initial dispatch (class_dispatch.rs) applies wraps only
            // to the first method called; without this, a wrapper added to a parent
            // method via `^find_method(...).wrap(...)` is skipped on `nextsame`.
            //
            // The candidate is NOT removed here: only the wrappers are pushed onto
            // the wrap-dispatch stack. When the wrappers' `nextsame` chain is
            // exhausted (sub_id == 0) it falls through to this same method frame,
            // and the `!is_inside_wrap_dispatch()` guard is then true — so the
            // original candidate body runs once via the normal path below and its
            // own `nextsame` continues the rest of the MRO.
            if !is_override && !self.is_inside_wrap_dispatch() {
                let peeked = self.method_dispatch_stack[frame_idx]
                    .remaining
                    .first()
                    .cloned();
                if let Some((owner_class, method_def)) = peeked {
                    let method_name_now = self
                        .samewith_context_stack
                        .last()
                        .map(|(n, _)| n.clone())
                        .unwrap_or_default();
                    if !method_name_now.is_empty()
                        && self.has_any_wrap_chains()
                        && let Some(cand_idx) = self.find_method_candidate_index(
                            &owner_class,
                            &method_name_now,
                            &method_def,
                        )
                        && let Some(chain) = self
                            .get_method_wrap_chain(&owner_class, &method_name_now, cand_idx)
                            .cloned()
                    {
                        let invocant = self.env.get("self").cloned().unwrap_or_else(|| {
                            self.method_dispatch_stack[frame_idx].invocant.clone()
                        });
                        // The wrap dispatch expects the invocant at position 0; the
                        // method frame's stored args do not include it.
                        let mut wrap_call_args = vec![invocant];
                        wrap_call_args.extend(self.method_dispatch_stack[frame_idx].args.clone());
                        let outermost = chain.last().unwrap().1.clone();
                        let mut wrap_remaining: Vec<Value> = Vec::new();
                        for i in (0..chain.len() - 1).rev() {
                            wrap_remaining.push(chain[i].1.clone());
                        }
                        let frame = WrapDispatchFrame {
                            sub_id: 0,
                            remaining: wrap_remaining,
                            args: wrap_call_args.clone(),
                            arg_sources: self.pending_call_arg_sources().cloned(),
                        };
                        self.wrap_dispatch_stack.push(frame);
                        self.shift_arg_sources_for_wrap_invocant();
                        let result = self.call_sub_value(outermost, wrap_call_args, false);
                        self.wrap_dispatch_stack.pop();
                        let result = result?;
                        if tail_call {
                            return Err(RuntimeError {
                                return_value: Some(result),
                                ..RuntimeError::new("")
                            });
                        }
                        return Ok(result);
                    }
                }
            }
            let (receiver_class, invocant, mut call_args, owner_class, mut method_def, rw_params) = {
                let frame = &mut self.method_dispatch_stack[frame_idx];
                let Some((owner_class, method_def)) = frame.remaining.first().cloned() else {
                    // User MRO exhausted: a grammar `parse`/`subparse` override, an
                    // `is Array` subclass's Positional override, or a metamodel-HOW
                    // dispatch falls through to the native implementation as the
                    // last candidate before giving up.
                    let result = if let Some(res) =
                        self.native_grammar_parse_next_candidate(override_args.as_deref())
                    {
                        res?
                    } else if let Some(res) =
                        self.native_mu_base_next_candidate(override_args.as_deref())
                    {
                        res?
                    } else if let Some(res) =
                        self.native_array_storage_next_candidate(override_args.as_deref())
                    {
                        res?
                    } else {
                        match self.native_metamodel_next_candidate(override_args.as_deref()) {
                            Some(res) => res?,
                            None => Value::NIL,
                        }
                    };
                    if tail_call {
                        return Err(RuntimeError {
                            return_value: Some(result),
                            ..RuntimeError::new("")
                        });
                    }
                    return Ok(result);
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
                let dist = self.resolve_package_distribution(&owner_class);
                Self::compile_method_def_in_place_with_dist(&mut method_def, &owner_class, dist);
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
                    if let Some(cur) = self.first_candidate_rw_value(first_param) {
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
            let empty_fns = crate::opcode::CompiledFns::default();
            let fns_ref = method_def.compiled_fns.as_deref().unwrap_or(&empty_fns);
            let dispatch_result = match invocant.view() {
                ValueView::Instance {
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
                            &attributes.to_map(),
                            call_args,
                            Some(invocant.clone()),
                            fns_ref,
                        )
                        .map(|(result, reconciled)| {
                            // Commit only an adjusted (`:=`-recovered) snapshot, exactly
                            // like `dispatch_compiled_method`: an unadjusted run already
                            // mutated the shared attribute cell in place, so writing the
                            // baseline snapshot back would clobber it (e.g. a parent
                            // `callsame` candidate's `self.x ~= ...`).
                            let new_inv = if let Some(updated) = reconciled {
                                Value::write_back_sharing(
                                    &attributes,
                                    class_name,
                                    updated,
                                    target_id,
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
                                    &attributes,
                                    class_name,
                                    updated,
                                    target_id,
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
                            &AttrMap::new(),
                            call_args,
                            Some(invocant.clone()),
                            fns_ref,
                        )
                        .map(|(result, _)| (result, None))
                    } else {
                        self.forward_resolved_delegation(
                            &receiver_class,
                            &owner_class,
                            method_def,
                            AttrMap::new(),
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
                    if let Some(cur) = self.first_candidate_rw_value(first_param) {
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
                        return_value: Some(Value::NIL),
                        ..RuntimeError::new("")
                    });
                }
                return Ok(Value::NIL);
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
            // Run the next candidate as bytecode — the body the declaration plan
            // compiled, or one on-the-fly compile memoized per candidate — instead
            // of recompiling `next_def.body` on every deferral (ADR-0019 C6d-1).
            //
            // Deliberately NOT `compile_and_call_function_def`: that entry pushes a
            // fresh multi-dispatch frame for the name, and this deferral chain
            // *owns* the frame it just advanced above. Re-pushing it restarts the
            // chain at the first candidate, so the next `nextsame` defers to the
            // same candidate forever (stack overflow in
            // `t/multi-where-otf-dispatch.t`). It also must not push a samewith
            // context, matching the interpreter entry this replaces.
            let cf = match &next_def.compiled {
                Some(compiled) => std::sync::Arc::clone(compiled),
                None => self.otf_compile_function_def(&next_def),
            };
            // Prefer the candidate's own nested-sub table over an empty one
            // (ADR-0019 C6e-3c) — this deferral chain owns no `CompiledFns` of
            // its own to offer.
            let empty_fns = crate::opcode::CompiledFns::default();
            let fns = cf.compiled_fns.as_deref().unwrap_or(&empty_fns);
            let next_pkg = next_def.package.resolve();
            let next_name = next_def.name.resolve();
            let result = self.call_compiled_function_named(
                &cf,
                call_args.clone(),
                fns,
                &next_pkg,
                &next_name,
            );
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
        // A metamodel-HOW method (user subclass of Metamodel::ClassHOW /
        // Metamodel::GrammarHOW) with no user MRO frame at all: the native
        // metamodel implementation is the next (and last) candidate.
        if let Some(res) = self.native_metamodel_next_candidate(override_args.as_deref()) {
            let result = res?;
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
        // A single (non-multi, non-wrapped) compiled method pushes no
        // `method_dispatch_stack` frame at all, so an `is Array` subclass's
        // Positional override (`method AT-POS($i) { nextwith $i.round }`)
        // reaches here directly rather than the exhausted-MRO branch above.
        // The native array behavior on the backing `__mutsu_array_storage` is
        // still the correct base candidate.
        if let Some(res) = self.native_array_storage_next_candidate(override_args.as_deref()) {
            let result = res?;
            if tail_call {
                return Err(RuntimeError {
                    return_value: Some(result),
                    ..RuntimeError::new("")
                });
            }
            return Ok(result);
        }
        // If we're inside a method but there's simply no next candidate in the MRO,
        // return Nil (this is the Raku behavior for callsame/callwith at the end of
        // the MRO).  Plain subs without multi dispatch should still throw.
        if !self.method_class_stack.is_empty() {
            if tail_call {
                return Err(RuntimeError {
                    return_value: Some(Value::NIL),
                    ..RuntimeError::new("")
                });
            }
            return Ok(Value::NIL);
        }
        // Not in any dispatch context
        Err(Self::no_dispatcher_error(func_name))
    }

    pub(super) fn builtin_nextcallee(&mut self) -> Result<Value, RuntimeError> {
        // Check wrap dispatch stack first (wrapper chains)
        if let Some(frame) = self.wrap_dispatch_stack.last_mut() {
            if let Some(next) = frame.remaining.first().cloned() {
                frame.remaining.remove(0);
                // The wrappee returned by nextcallee is the inner code object
                // itself: calling it must run it directly, never re-enter the
                // wrap chain (which would recurse into the wrapper forever).
                if let crate::value::ValueView::Sub(data) = next.view() {
                    let mut direct = crate::value::SubData::clone(&data);
                    direct
                        .env
                        .insert("__mutsu_wrap_direct".to_string(), Value::TRUE);
                    return Ok(Value::sub_value(crate::gc::Gc::new(direct)));
                }
                return Ok(next);
            }
            return Ok(Value::NIL);
        }
        // Check method dispatch stack
        // (not yet implemented for methods — return Nil)
        // Check multi dispatch stack
        let Some((_name, candidates, orig_args, rw_params)) =
            self.multi_dispatch_stack.last().cloned()
        else {
            return Ok(Value::NIL);
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
            return Ok(Value::NIL);
        };
        let next_def = candidates[idx].clone();
        // Remove this candidate and all before it from the remaining list
        let remaining = candidates[idx + 1..].to_vec();
        let stack_len = self.multi_dispatch_stack.len();
        self.multi_dispatch_stack[stack_len - 1] = (_name, remaining, orig_args, rw_params);
        // Return as a callable Sub value
        Ok(Value::make_sub_for_routine(
            next_def.package,
            next_def.name,
            next_def.params.clone(),
            next_def.param_defs.clone(),
            next_def.body.clone(),
            next_def.is_rw,
            self.env.clone(),
            next_def.compiled.clone(),
        ))
    }
}
