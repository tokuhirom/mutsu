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

/// ADR-0019 E9b-0: which of the three deferral stacks currently holds the
/// innermost live dispatch context, per `Interpreter::innermost_dispatch_stack`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DispatchFrameKind {
    Wrap,
    Method,
    Multi,
}

impl Interpreter {
    /// ADR-0019 E9b-0: `wrap_dispatch_stack`, `method_dispatch_stack`, and
    /// `multi_dispatch_stack` are independent stacks, each stamped with a
    /// shared monotonic `dispatch_token` at push time. `callsame`/`nextsame`/
    /// `lastcall`/`nextcallee` must resolve to the INNERMOST live dynamic
    /// dispatch context — the frame with the highest token among the three
    /// stacks' top frames — rather than a fixed wrap-then-method-then-multi
    /// search order, which lets an outer frame on one stack shadow a more
    /// recently pushed frame on a different stack (e.g. a method deferral
    /// nested inside a sub wrapper, or vice versa).
    fn innermost_dispatch_stack(&self) -> Option<DispatchFrameKind> {
        let mut best: Option<(u64, DispatchFrameKind)> = None;
        if let Some(frame) = self.wrap_dispatch_stack.last() {
            best = Some((frame.dispatch_token, DispatchFrameKind::Wrap));
        }
        if let Some(frame) = self.method_dispatch_stack.last()
            && best.is_none_or(|(t, _)| frame.dispatch_token > t)
        {
            best = Some((frame.dispatch_token, DispatchFrameKind::Method));
        }
        if let Some(entry) = self.multi_dispatch_stack.last()
            && best.is_none_or(|(t, _)| entry.4 > t)
        {
            best = Some((entry.4, DispatchFrameKind::Multi));
        }
        best.map(|(_, kind)| kind)
    }

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
        // Clear remaining candidates of the innermost live dispatch frame
        // (ADR-0019 E9b-0: chosen by dispatch_token, not a fixed stack order).
        match self.innermost_dispatch_stack() {
            Some(DispatchFrameKind::Wrap) => {
                if let Some(frame) = self.wrap_dispatch_stack.last_mut() {
                    frame.remaining.clear();
                }
                Ok(Value::TRUE)
            }
            Some(DispatchFrameKind::Method) => {
                if let Some(frame) = self.method_dispatch_stack.last_mut() {
                    frame.remaining.clear();
                }
                Ok(Value::TRUE)
            }
            Some(DispatchFrameKind::Multi) => {
                if let Some(top) = self.multi_dispatch_stack.last_mut() {
                    top.1.clear();
                }
                Ok(Value::TRUE)
            }
            // Outside a dispatch context: no-op (return False).
            None => Ok(Value::FALSE),
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
        // A lazy `gather` body re-pushes the context it captured at creation for
        // the duration of its force (see `push_captured_samewith_context`), so
        // this stack is correct there too.
        if let Some(ctx) = self.samewith_context_stack.last().cloned() {
            if let Some(inv) = ctx.invocant {
                // Method dispatch: re-call the method on the same invocant
                return self.call_method_with_values(inv, &ctx.name, args.to_vec());
            } else {
                // Sub dispatch: re-call the function by name
                return self.call_function(&ctx.name, args.to_vec());
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
        let Some(ctx) = self.samewith_context_stack.last() else {
            return;
        };
        env.insert(
            Self::SAMEWITH_LEXICAL_NAME_KEY.to_string(),
            Value::str(ctx.name.clone()),
        );
        if let Some(inv) = &ctx.invocant {
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
        self.push_samewith_context(&name, invocant, None);
        true
    }

    /// Undo a [`Self::push_captured_samewith_context`] that returned `true`.
    pub(crate) fn pop_captured_samewith_context(&mut self, pushed: bool) {
        if pushed {
            self.pop_samewith_context();
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
            .is_none_or(|ctx| ctx.name != method_name)
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
        let method_name = self
            .samewith_context_stack
            .last()
            .map(|ctx| ctx.name.clone())?;
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
        // ADR-0019 E9c-1: read name/invocant/args off the SAME
        // `samewith_context_stack` entry (a single clone) rather than
        // name/invocant from one stack and args from a separately
        // pushed/popped stack — the former dual-stack shape could pair the
        // top-of-args-stack entry with a DIFFERENT (deeper, stale)
        // context if a raw push sat above the pairing `push_method_
        // samewith_context` push; see `SamewithContext`'s doc comment.
        let ctx = self.samewith_context_stack.last().cloned();
        let method_name = ctx.as_ref().map(|c| c.name.clone())?;
        // A single (non-multi, non-wrapped) compiled method pushes no
        // `method_dispatch_stack` frame, so the invocant/args must come from
        // the samewith context and `self` rather than a dispatch frame (mirrors
        // `native_mu_base_next_candidate`'s `self.env.get("self")` fallback).
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| ctx.as_ref().and_then(|c| c.invocant.clone()))
            .or_else(|| self.env.get("self").cloned())?;
        let args: Vec<Value> = match override_args {
            Some(a) => a.to_vec(),
            None => self
                .method_dispatch_stack
                .last()
                .map(|f| f.args.clone())
                // A single (non-multi, non-wrapped) compiled method — the
                // common case for a `method push(...) { nextsame }` override on
                // an `is Array` subclass — pushes no `method_dispatch_stack`
                // frame at all, so the original call args live only in the
                // samewith context's own `args` field (set by
                // `push_method_samewith_context`). Without this, `args`
                // silently defaulted to empty and the deferred push appended
                // nothing.
                .or_else(|| ctx.as_ref().and_then(|c| c.args.clone()))
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
        // Mutating array methods (push/append/prepend/unshift/pop/shift) must
        // route through the NATIVE mutable-array path, not `try_native_method`
        // (a pure `&Value` dispatch that has no entry for these methods at all
        // and silently returns `None` — the "no error, no effect" symptom this
        // ticket describes). `attributes.with_attr_mut` hands out a `&mut
        // Value` into the instance's SHARED attribute cell, so the mutation is
        // visible to every other holder of the same instance (not a detached
        // copy) — mirrors the direct `$a.push(...)` fast path in
        // `vm_call_method_mut_ops.rs`. Raku's base `Array.push`/`.append`/
        // `.prepend`/`.unshift` return the invocant itself (not the raw backing
        // array), so those four map the mutation's Ok value to `invocant.clone()`
        // — the SAME instance (same id, same attribute cell), so `===` and
        // `.^name` come out right; `pop`/`shift` return the removed element as-is.
        if matches!(
            method_name.as_str(),
            "push" | "append" | "prepend" | "unshift" | "pop" | "shift"
        ) {
            let outcome = attributes.with_attr_mut("__mutsu_array_storage", |storage| {
                Self::native_array_storage_mut(storage, &method_name, &args)
            })??;
            return Some(outcome.map(|value| match method_name.as_str() {
                "push" | "append" | "prepend" | "unshift" => invocant.clone(),
                _ => value,
            }));
        }
        let method_sym = Symbol::intern(&method_name);
        attributes.with_attr_mut("__mutsu_array_storage", |storage| {
            self.try_native_method(storage, method_sym, &args)
        })?
    }

    /// The Associative twin of [`Self::native_array_storage_next_candidate`]:
    /// when a user `is Hash`/`is Map` subclass overrides an Associative
    /// protocol method (`AT-KEY`/`ASSIGN-KEY`/`DELETE-KEY`/...) and calls
    /// `nextsame`/`nextwith` (or `callsame`/`callwith`), the NATIVE hash
    /// behavior on the instance's backing `__mutsu_hash_storage` is the final
    /// base candidate.
    fn native_hash_storage_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ctx = self.samewith_context_stack.last().cloned();
        let method_name = ctx.as_ref().map(|c| c.name.clone())?;
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| ctx.as_ref().and_then(|c| c.invocant.clone()))
            .or_else(|| self.env.get("self").cloned())?;
        let args: Vec<Value> = match override_args {
            Some(a) => a.to_vec(),
            None => self
                .method_dispatch_stack
                .last()
                .map(|f| f.args.clone())
                .or_else(|| ctx.as_ref().and_then(|c| c.args.clone()))
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
        if !attributes.contains_key("__mutsu_hash_storage")
            || !self
                .mro_readonly(&class_name.resolve())
                .iter()
                .any(|n| Self::is_associative_base(n))
        {
            return None;
        }
        // Mutating key methods write into the SHARED backing storage in
        // place (interior mutability via `with_attr_mut`), mirroring the
        // Array analog's simple-mutator fast path.
        if matches!(method_name.as_str(), "ASSIGN-KEY" | "DELETE-KEY") && !args.is_empty() {
            let key = args[0].to_string_value();
            let is_assign = method_name == "ASSIGN-KEY";
            let value = if is_assign {
                args.get(1).cloned().unwrap_or(Value::NIL)
            } else {
                Value::NIL
            };
            let outcome = attributes.with_attr_mut("__mutsu_hash_storage", |storage| {
                storage.with_hash_mut(|gc| {
                    let data = crate::value::gc_data_mut(gc);
                    if is_assign {
                        data.insert(key.clone(), value.clone());
                    } else {
                        data.remove(&key);
                    }
                })
            });
            outcome?;
            return Some(Ok(value));
        }
        if matches!(method_name.as_str(), "push" | "append") {
            let outcome = attributes.with_attr_mut("__mutsu_hash_storage", |storage| {
                storage.with_hash_mut(|gc| {
                    let data = crate::value::gc_data_mut(gc);
                    for arg in &args {
                        if let ValueView::Pair(k, v) = arg.view() {
                            data.insert(k.to_string(), v.clone());
                        }
                    }
                })
            });
            outcome?;
            return Some(Ok(invocant.clone()));
        }
        let method_sym = Symbol::intern(&method_name);
        attributes.with_attr_mut("__mutsu_hash_storage", |storage| {
            self.try_native_method(storage, method_sym, &args)
        })?
    }

    /// When a role mixed directly into a native builtin value (`%h does R`,
    /// `@a does R`, `"x" does R`, ...) overrides a method and calls
    /// `nextsame`/`nextwith` (or `callsame`/`callwith`), the NATIVE method on
    /// the mixin's inner value is the final base candidate. This mirrors
    /// [`native_array_storage_next_candidate`], but for a plain `Mixin` over
    /// a builtin `Value` rather than an `is Array` subclass's synthesized
    /// `__mutsu_array_storage` attribute.
    ///
    /// `dispatch_mixin_method_call` (`runtime::methods_mixin_dispatch`) only
    /// pushes a `method_dispatch_stack` frame with real "next candidate"
    /// entries when the mixin's inner value is a user-declared `Instance`
    /// (its `base_class`/`resolve_all_methods_with_owner` lookup needs a
    /// registered class name) — a native `Hash`/`Array`/`Str`/... inner value
    /// has no `MethodDef`s to find, so that frame is empty and `nextsame`
    /// previously fell through to the generic "exhausted MRO" `Nil` at the
    /// end of [`dispatch_next_candidate`] instead of reaching the real
    /// native implementation (`Hash::AT-KEY`, ...). Verified against
    /// `Hash::Restricted`'s `restrict-current`/`restrict-given` roles, whose
    /// `AT-KEY`/`ASSIGN-KEY`/`BIND-KEY`/`STORE` overrides all `nextsame`/
    /// `callsame` to the real `Hash` behavior once the allowed-keys check
    /// passes.
    fn native_mixin_base_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ctx = self.samewith_context_stack.last().cloned();
        let method_name = ctx.as_ref().map(|c| c.name.clone())?;
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| ctx.as_ref().and_then(|c| c.invocant.clone()))
            .or_else(|| self.env.get("self").cloned())?;
        let ValueView::Mixin(inner, _) = invocant.view() else {
            return None;
        };
        // A user-`Instance` inner is handled by the regular MRO chain that
        // `dispatch_mixin_method_call` already builds; only a native inner
        // (no `MethodDef`s at all) needs this bridge.
        if matches!(inner.view(), ValueView::Instance { .. }) {
            return None;
        }
        let args: Vec<Value> = match override_args {
            Some(a) => a.to_vec(),
            None => self
                .method_dispatch_stack
                .last()
                .map(|f| f.args.clone())
                .or_else(|| ctx.as_ref().and_then(|c| c.args.clone()))
                .unwrap_or_default(),
        };
        self.try_native_method(inner, Symbol::intern(&method_name), &args)
    }

    /// When a user `gist`/`Str`/`raku` override calls `nextsame`/`callsame`
    /// and the user MRO is exhausted, Mu's native default implementation is
    /// the final base candidate — it is not a `MethodDef`, so the regular MRO
    /// chain never reaches it (mirrors `native_array_storage_next_candidate`).
    /// Reads invocant/args off `method_dispatch_stack` when a frame exists
    /// (a multi/wrapped override), falling back to the samewith context and
    /// `self` for a plain single-candidate compiled method, which pushes no
    /// `method_dispatch_stack` frame at all.
    ///
    /// `gist`/`raku` route through `default_instance_repr` (the
    /// `ClassName.new(...)` rendering); a plain instance has no comparable
    /// default `Str` method to defer to (Rakudo's own default is an
    /// identity-ish `ClassName<objectid>`, not reproducible here), so `Str`
    /// uses the same generic `ClassName()` fallback `Value`'s `Display`
    /// impl already renders for an instance with no better answer.
    fn native_any_base_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ctx = self.samewith_context_stack.last().cloned()?;
        if !matches!(ctx.name.as_str(), "gist" | "Str" | "raku") {
            return None;
        }
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| ctx.invocant.clone())
            .or_else(|| self.env.get("self").cloned())?;
        if ctx.name == "Str" {
            return Some(Ok(Value::str(invocant.to_string_value())));
        }
        let args: Vec<Value> = match override_args {
            Some(a) => a.to_vec(),
            None => self
                .method_dispatch_stack
                .last()
                .map(|f| f.args.clone())
                .or_else(|| ctx.args.clone())
                .unwrap_or_default(),
        };
        self.default_instance_repr(&invocant, &ctx.name, &args)
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
        let method_name = self
            .samewith_context_stack
            .last()
            .map(|ctx| ctx.name.clone())?;
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
        // ADR-0019 E9b-0: resolve to the innermost live dispatch context (by
        // dispatch_token) instead of a fixed wrap-then-method-then-multi order,
        // so an outer sub/method wrap does not shadow a more recently pushed
        // frame on a different stack (and vice versa).
        let innermost = self.innermost_dispatch_stack();
        // Try wrap dispatch stack first (SUB wraps only — ADR-0019 E9b-2 moved
        // method wraps into `method_dispatch_stack` as `DeferralEntry::Wrapper`
        // prefix entries, so this stack no longer carries a `sub_id == 0`
        // entry) — only when it is genuinely the innermost context.
        if innermost == Some(DispatchFrameKind::Wrap)
            && let Some(frame) = self.wrap_dispatch_stack.last_mut()
        {
            if let Some(next) = frame.remaining.first().cloned() {
                frame.remaining.remove(0);
                let is_override = override_args.is_some();
                let call_args = override_args.unwrap_or_else(|| frame.args.clone());
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
                if let ValueView::Sub(data) = next.view() {
                    self.wrap_skip_once = Some(data.id);
                }
                let result = self.call_sub_value(next, call_args, false)?;
                if tail_call {
                    return Err(RuntimeError::return_signal(result));
                }
                return Ok(result);
            }
            // Remaining is empty: exhausted — return Nil.
            if tail_call {
                return Err(RuntimeError::return_signal(Value::NIL));
            }
            return Ok(Value::NIL);
        }
        // Try method dispatch stack — only when it is genuinely the innermost context.
        if innermost == Some(DispatchFrameKind::Method) && !self.method_dispatch_stack.is_empty() {
            let frame_idx = self.method_dispatch_stack.len() - 1;
            let is_override = override_args.is_some();
            // ADR-0019 E9b-2 decision 3: lazy mid-MRO wrap splice. When the
            // front entry is an un-spliced Candidate that itself carries a
            // method-level wrap chain (e.g. `^find_method(...).wrap(...)` on
            // a PARENT method, reached via nextsame/callsame), splice
            // `[Wrapper(chain, outermost included)..., Candidate{
            // wraps_spliced: true}]` in its place so the match below advances
            // into the first Wrapper. Replaces the old mid-MRO
            // peek-and-intercept block entirely (no separate
            // WrapDispatchFrame, no re-entry dance) — S06-advanced/wrap.t
            // GH#2178.
            loop {
                let is_unspliced_candidate = matches!(
                    self.method_dispatch_stack[frame_idx].remaining.first(),
                    Some(DeferralEntry::Candidate {
                        wraps_spliced: false,
                        ..
                    })
                );
                if !is_unspliced_candidate || is_override || !self.has_any_wrap_chains() {
                    break;
                }
                let method_name_now = self
                    .samewith_context_stack
                    .last()
                    .map(|ctx| ctx.name.clone())
                    .unwrap_or_default();
                if method_name_now.is_empty() {
                    break;
                }
                let Some(DeferralEntry::Candidate { owner, def, .. }) = self.method_dispatch_stack
                    [frame_idx]
                    .remaining
                    .first()
                    .cloned()
                else {
                    break;
                };
                let owner_class = owner.resolve();
                let Some(cand_idx) =
                    self.find_method_candidate_index(&owner_class, &method_name_now, &def)
                else {
                    break;
                };
                let Some(chain) =
                    self.get_method_wrap_chain(&owner_class, &method_name_now, cand_idx)
                else {
                    break;
                };
                // Unlike the winner's own prefix (built once at frame
                // construction, outermost invoked directly), nobody invokes
                // the outermost directly here — the WHOLE chain becomes
                // Wrapper entries, followed by the spliced candidate.
                let mut splice: Vec<DeferralEntry> = Vec::with_capacity(chain.len() + 1);
                for i in (0..chain.len()).rev() {
                    splice.push(DeferralEntry::Wrapper(chain[i].1.clone()));
                }
                splice.push(DeferralEntry::Candidate {
                    owner,
                    def,
                    wraps_spliced: true,
                });
                self.method_dispatch_stack[frame_idx]
                    .remaining
                    .splice(0..1, splice);
                // Loop back: the front entry is now Wrapper(outermost).
            }
            match self.method_dispatch_stack[frame_idx]
                .remaining
                .first()
                .cloned()
            {
                None => {
                    // User MRO exhausted: a grammar `parse`/`subparse` override, an
                    // `is Array` subclass's Positional override, or a metamodel-HOW
                    // dispatch falls through to the native implementation as the
                    // last candidate before giving up. ADR-0019 E9b-2: a wrapped
                    // method now always has a frame, so "frame exists, remaining
                    // empty" is the single exhaustion signal (the #6349
                    // `wrap_chain_exhausted` bool is retired).
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
                    } else if let Some(res) =
                        self.native_hash_storage_next_candidate(override_args.as_deref())
                    {
                        res?
                    } else if let Some(res) =
                        self.native_any_base_next_candidate(override_args.as_deref())
                    {
                        res?
                    } else if let Some(res) =
                        self.native_mixin_base_next_candidate(override_args.as_deref())
                    {
                        res?
                    } else {
                        match self.native_metamodel_next_candidate(override_args.as_deref()) {
                            Some(res) => res?,
                            None => Value::NIL,
                        }
                    };
                    if tail_call {
                        return Err(RuntimeError::return_signal(result));
                    }
                    return Ok(result);
                }
                Some(DeferralEntry::Wrapper(code)) => {
                    // ADR-0019 E9b-2: advance leg for a wrap-prefix entry —
                    // invoke it with [invocant, ...args] and the (shifted)
                    // wrap-captured call-site arg sources, mirroring today's
                    // (now sub-only) WrapDispatchFrame wrapper leg.
                    self.method_dispatch_stack[frame_idx].remaining.remove(0);
                    let caller_in_wrapper = self.method_dispatch_stack[frame_idx].in_wrapper;
                    if let Some(new_args) = override_args {
                        // `callwith`'s args are invocant-INCLUSIVE (element 0
                        // is the new SELF) exactly when the CALLER is itself
                        // a wrapper block — a wrapper's own positional
                        // signature is `(invocant, ...args)`. A candidate
                        // body landing here via a freshly mid-MRO-spliced
                        // wrap chain (decision 3) is NOT a wrapper, so its
                        // override args stay invocant-exclusive
                        // (S06-advanced/dispatching.t "Args to callwith in
                        // wrapper/multi are used by enclosing ...").
                        let frame = &mut self.method_dispatch_stack[frame_idx];
                        if caller_in_wrapper {
                            let mut it = new_args.into_iter();
                            if let Some(inv) = it.next() {
                                frame.invocant = inv;
                            }
                            frame.args = it.collect();
                        } else {
                            frame.args = new_args;
                        }
                    }
                    let frame = &mut self.method_dispatch_stack[frame_idx];
                    // ADR-0019 E9b-2: unlike the plain-Candidate advance leg
                    // below, this call runs from INSIDE a wrapper block's own
                    // execution, not from a method body — `self.env`'s "self"
                    // binding at this point (if any) is leftover from the
                    // wrapper closure's LEXICAL capture (e.g. OO::Monitors'
                    // wrapper closes over `add_method`'s own `self`, the HOW
                    // instance), not the true invocant. `frame.invocant` is
                    // the correct, stable value captured at frame-push time
                    // (and kept live via the shared attribute cell — no
                    // snapshot to go stale), mirroring how the pre-E9b-2
                    // `WrapDispatchFrame.args[0]` was used unconditionally,
                    // with no env lookup, for every wrap-stack advance.
                    frame.in_wrapper = true;
                    let current_invocant = frame.invocant.clone();
                    let mut call_args = vec![current_invocant];
                    call_args.extend(frame.args.clone());
                    let frame_arg_sources = frame.arg_sources.clone();
                    let restore_sources = !is_override && frame_arg_sources.is_some();
                    if restore_sources {
                        self.set_pending_call_arg_sources(frame_arg_sources);
                        self.shift_arg_sources_for_wrap_invocant();
                    }
                    if let ValueView::Sub(data) = code.view() {
                        self.wrap_skip_once = Some(data.id);
                    }
                    let result = self.call_sub_value(code, call_args, false)?;
                    if tail_call {
                        return Err(RuntimeError::return_signal(result));
                    }
                    return Ok(result);
                }
                Some(DeferralEntry::Candidate { .. }) => {}
            }
            let (
                receiver_class,
                invocant,
                mut call_args,
                owner_class,
                mut method_def,
                rw_params,
                came_from_wrapper,
                frame_wrap_arg_sources,
            ) = {
                let frame = &mut self.method_dispatch_stack[frame_idx];
                let entry = frame.remaining.first().cloned().expect(
                    "ADR-0019 E9b-2: the match above only falls through here for a Candidate",
                );
                let DeferralEntry::Candidate {
                    owner: owner_sym,
                    def: method_def,
                    ..
                } = entry
                else {
                    unreachable!(
                        "ADR-0019 E9b-2: the match above only falls through here for a Candidate"
                    )
                };
                let owner_class = owner_sym.resolve();
                let method_def = *method_def;
                frame.remaining.remove(0);
                let rw_params = frame.rw_params.clone();
                let frame_wrap_arg_sources = frame.arg_sources.clone();
                // ADR-0019 E9b-2: whether the code CURRENTLY executing (the
                // thing whose callsame/callwith call reached this Candidate)
                // is a wrapper block rather than a real method body — reached
                // either directly from the caller's single outermost wrapper
                // (chain.len() == 1, so no `Wrapper` entry ever ran), from
                // the `Wrapper` advance leg above, or NOT at all when a plain
                // candidate's own nextsame/nextwith lands here in the
                // ordinary (non-wrap) MRO tail.
                let came_from_wrapper = frame.in_wrapper;
                // Use the current `self` from the environment instead of the stale
                // frame invocant.  The method body may have mutated attributes
                // (e.g. `$.tracker ~= "bar,"`) before calling callsame/callwith,
                // so the frame's snapshot is outdated.
                //
                // ADR-0019 E9b-2 exception: when `came_from_wrapper`, the
                // CURRENT execution context is a wrapper BLOCK, not this
                // candidate's own method body, so `self.env`'s "self"
                // binding (if any) is leftover from the wrapper closure's
                // lexical capture (e.g. OO::Monitors' wrapper closes over
                // `add_method`'s own `self`, the HOW instance), not the true
                // invocant — using it here dispatched `bump()` against the
                // HOW and died "no such attribute '$!n'". `frame.invocant`
                // is the correct, stable value (kept live via the shared
                // attribute cell, so there is no staleness to guard against
                // on this leg).
                let base_invocant = if came_from_wrapper {
                    frame.invocant.clone()
                } else {
                    self.env
                        .get("self")
                        .cloned()
                        .unwrap_or_else(|| frame.invocant.clone())
                };
                // `callwith`'s args are invocant-INCLUSIVE (element 0 is the
                // new SELF) exactly when the caller is a wrapper block —
                // mirrors the `Wrapper` advance leg's own split above
                // (S06-advanced/dispatching.t "Args to callwith in
                // wrapper/multi are used by enclosing multi and method
                // dispatch").
                let (current_invocant, call_args) = match override_args {
                    Some(new_args) if came_from_wrapper => {
                        let mut it = new_args.into_iter();
                        let inv = it.next().unwrap_or_else(|| base_invocant.clone());
                        (inv, it.collect::<Vec<_>>())
                    }
                    Some(new_args) => (base_invocant, new_args),
                    None => (base_invocant, frame.args.clone()),
                };
                frame.args = call_args.clone();
                frame.invocant = current_invocant.clone();
                frame.in_wrapper = false;
                (
                    frame.receiver_class.clone(),
                    current_invocant,
                    call_args,
                    owner_class,
                    method_def,
                    rw_params,
                    came_from_wrapper,
                    frame_wrap_arg_sources,
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
            // ADR-0019 E9b-2: a candidate freshly reached right after a
            // Wrapper group (the former by-name "original" re-entry, or a
            // lazily mid-MRO-spliced wrapped parent) restores the wrap's TRUE
            // outer call-site arg sources directly, instead of the rw_params
            // synthetic self-referential handoff name below — an `is rw`
            // parameter must still write back to the CALLER's variable, not a
            // synthetic name (`t/wrap-invocant-arg-source.t` test E). When the
            // frame carries no wrap arg sources (an ordinary non-wrap
            // nextsame/callsame chain, or a mid-MRO wrap whose winner wasn't
            // itself wrapped), fall back to the pre-existing rw_params
            // chain-forwarding dance unchanged.
            let use_wrap_sources =
                came_from_wrapper && frame_wrap_arg_sources.is_some() && !is_override;
            let mut have_rw_source = false;
            if use_wrap_sources {
                self.set_pending_call_arg_sources(frame_wrap_arg_sources);
            } else {
                // nextsame/callsame+rw chaining for methods (§D capstone): the stored
                // method args are plain values (no varref), so without an arg source the
                // next candidate's `is rw` param dies with X::Parameter::RW. Forward the
                // first candidate's CURRENT rw value and name the FIRST candidate's param
                // as the source, so the next candidate writes back into it (env-only, all
                // method candidates run via the interpreter) and the first candidate's own
                // exit writeback then propagates the chained result to the caller.
                let mut rw_sources: Vec<Option<String>> = Vec::new();
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
                .map(|ctx| ctx.name.clone())
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
            if use_wrap_sources || have_rw_source {
                self.set_pending_call_arg_sources(None);
            }
            let (result, updated_invocant) = dispatch_result?;
            // Write the chain's final value (now in env under each first-candidate
            // param name) back into the first (compiled) candidate's VM local slot
            // so its exit flush propagates it instead of its own pre-nextsame value.
            // ADR-0019 E9b-2: skipped for `use_wrap_sources` — that path
            // mirrors the deleted by-name "original" re-entry, which never
            // ran this rw_params-specific chain-forwarding dance either (the
            // wrap's own arg_sources already routed the writeback through the
            // TRUE call-site variable name, not a synthetic per-candidate one).
            if !use_wrap_sources && caller_code != 0 && have_rw_source {
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
                return Err(RuntimeError::return_signal(result));
            }
            return Ok(result);
        }
        // Try multi dispatch stack — only when it is genuinely the innermost
        // context. Exhaustion semantics stay per-family: an exhausted wrap or
        // method frame does not fall through to an unrelated outer multi.
        if innermost == Some(DispatchFrameKind::Multi)
            && let Some((_name, candidates, orig_args, rw_params, dispatch_token)) =
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
                    return Err(RuntimeError::return_signal(Value::NIL));
                }
                return Ok(Value::NIL);
            };
            let next_def = candidates[idx].clone();
            let remaining = candidates[idx + 1..].to_vec();
            let stack_len = self.multi_dispatch_stack.len();
            // Keep rw_params fixed: it always identifies the FIRST candidate's
            // slots, even as the chain advances through later candidates.
            self.multi_dispatch_stack[stack_len - 1] = (
                _name,
                remaining,
                call_args.clone(),
                rw_params.clone(),
                dispatch_token,
            );
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
                return Err(RuntimeError::return_signal(result));
            }
            return Ok(result);
        }
        // A metamodel-HOW method (user subclass of Metamodel::ClassHOW /
        // Metamodel::GrammarHOW) with no user MRO frame at all: the native
        // metamodel implementation is the next (and last) candidate.
        if let Some(res) = self.native_metamodel_next_candidate(override_args.as_deref()) {
            let result = res?;
            if tail_call {
                return Err(RuntimeError::return_signal(result));
            }
            return Ok(result);
        }
        // Fallback: if we are inside a `new` method and nextsame/callsame/
        // nextwith/callwith is called, dispatch to the built-in Mu.new (i.e.,
        // bless) on the current invocant. In Raku, Mu.new(*%attrinit) is
        // always the base candidate in the MRO for `new`. Check both
        // routine_stack (VM path) and samewith_context_stack (interpreter
        // path). `nextsame`/`callsame` (override_args is None) implicitly
        // forward the ORIGINAL call's args rather than an empty list — read
        // them off the method dispatch frame / samewith context, mirroring
        // `native_array_storage_next_candidate`'s same fallback for a
        // no-frame single compiled method.
        if matches!(func_name, "nextsame" | "callsame" | "nextwith" | "callwith") {
            let in_new = self
                .routine_stack
                .last()
                .is_some_and(|frame| frame.name == "new")
                || self
                    .samewith_context_stack
                    .last()
                    .is_some_and(|ctx| ctx.name == "new");
            if in_new && let Some(invocant) = self.env.get("self").cloned() {
                let call_args = match override_args {
                    Some(args) => args,
                    None => self
                        .method_dispatch_stack
                        .last()
                        .map(|f| f.args.clone())
                        .or_else(|| {
                            self.samewith_context_stack
                                .last()
                                .and_then(|c| c.args.clone())
                        })
                        .unwrap_or_default(),
                };
                let result = self.call_method_with_values(invocant, "bless", call_args)?;
                if tail_call {
                    return Err(RuntimeError::return_signal(result));
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
                return Err(RuntimeError::return_signal(result));
            }
            return Ok(result);
        }
        // Same no-frame shape as above, for an `is Hash`/`is Map` subclass's
        // Associative override (`method AT-KEY($k) { nextwith $k.lc }`).
        if let Some(res) = self.native_hash_storage_next_candidate(override_args.as_deref()) {
            let result = res?;
            if tail_call {
                return Err(RuntimeError::return_signal(result));
            }
            return Ok(result);
        }
        // Same no-frame shape as above, for a `gist`/`Str`/`raku` override
        // (`method gist() { "custom+" ~ callsame }`) with no wrap/multi/role
        // complications.
        if let Some(res) = self.native_any_base_next_candidate(override_args.as_deref()) {
            let result = res?;
            if tail_call {
                return Err(RuntimeError::return_signal(result));
            }
            return Ok(result);
        }
        // A role mixed directly into a native builtin value (`%h does R`)
        // pushes no `method_dispatch_stack` frame either — the native method
        // on the mixin's inner value is the correct base candidate.
        if let Some(res) = self.native_mixin_base_next_candidate(override_args.as_deref()) {
            let result = res?;
            if tail_call {
                return Err(RuntimeError::return_signal(result));
            }
            return Ok(result);
        }
        // If we're inside a method but there's simply no next candidate in the MRO,
        // return Nil (this is the Raku behavior for callsame/callwith at the end of
        // the MRO).  Plain subs without multi dispatch should still throw.
        // ADR-0019 E9b-2: the `wrap_chain_exhausted` bool (#6349) is retired — a
        // wrapped method now always has a `method_dispatch_stack` frame, so its
        // exhaustion is already handled by the Method branch above.
        if !self.method_class_stack.is_empty() {
            if tail_call {
                return Err(RuntimeError::return_signal(Value::NIL));
            }
            return Ok(Value::NIL);
        }
        // Not in any dispatch context
        Err(Self::no_dispatcher_error(func_name))
    }

    pub(super) fn builtin_nextcallee(&mut self) -> Result<Value, RuntimeError> {
        // ADR-0019 E9b-0: resolve to the innermost live dispatch context, same
        // as dispatch_next_candidate/builtin_lastcall.
        let innermost = self.innermost_dispatch_stack();
        // Check wrap dispatch stack first (wrapper chains) — only when it is
        // genuinely the innermost context.
        if innermost == Some(DispatchFrameKind::Wrap)
            && let Some(frame) = self.wrap_dispatch_stack.last_mut()
        {
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
        // Method dispatch is not yet implemented for nextcallee — Nil, same as
        // when there is no live dispatch context at all.
        if innermost != Some(DispatchFrameKind::Multi) {
            return Ok(Value::NIL);
        }
        // Check multi dispatch stack
        let Some((_name, candidates, orig_args, rw_params, dispatch_token)) =
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
        self.multi_dispatch_stack[stack_len - 1] =
            (_name, remaining, orig_args, rw_params, dispatch_token);
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
