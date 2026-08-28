use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// ADR-0024: `mainline_lexical_frame_active` (the read/write resolver for
    /// a mainline named sub's captured free variables) keys off the LAST
    /// `routine_stack` frame. This predicate predates ADR-0037 Slice 1, which
    /// made every "light"/"fast" compiled-call path (`call_compiled_function_fast`,
    /// `call_compiled_function_light[_spec]`, `call_compiled_function_positional_light`)
    /// push a `RoutineFrame` unconditionally, so the frame is no longer
    /// missing on these paths. This exclusion has not been re-verified
    /// against that; keep forcing a mainline-lexical-capturing sub onto the
    /// frame-pushing path (`call_compiled_function_named[_inner]`) by
    /// excluding it from every light/fast eligibility check at the call site
    /// until it is. `mainline_lexical_subs` is empty for the overwhelmingly
    /// common program, so this is one `is_empty` test beyond what those
    /// checks already do.
    #[inline]
    fn light_call_blocked_by_mainline_capture(&self, name: &str) -> bool {
        !self.mainline_lexical_subs.is_empty() && self.mainline_lexical_subs.contains(name)
    }

    /// Names of builtin listops/functions that a same-named user-defined
    /// subroutine may shadow. When both exist, the user sub wins.
    ///
    /// This is intentionally narrow: most user subs don't conflict with a
    /// builtin, and unconditionally routing every `has_function` name through
    /// `call_function_fallback` changes dispatch in ways that affect things
    /// like MAIN/GENERATE-USAGE handling.
    /// True if the `&name` value in env comes from a lexical override
    /// (e.g. `sub callit(&foo) { ... }`) rather than the normal package
    /// binding for the named sub. A lexical override has a different
    /// identity (its stored `SubData.name` does not match `name`) — either
    /// because it's an anonymous block passed as `&foo`, or because it's a
    /// different sub with the same parameter name.
    pub(crate) fn env_callable_is_lexical_override(val: &Value, name: &str) -> bool {
        if let ValueView::Sub(sub) = val.view() {
            let stored = sub.name.resolve();
            // Anonymous block or mismatched name => lexical override.
            stored.is_empty() || stored != name
        } else {
            false
        }
    }

    /// Core type names whose bareword call form `Name(...)` is the type's
    /// COERCION rather than a call to a routine — the set `call_function`
    /// implements a coercion arm for. In Raku a user `sub Int(Str $s) {...}`
    /// does not occlude `Int('42')`; the declaration is reachable only through
    /// the explicitly `&`-sigiled `&Int('42')`.
    ///
    /// Restricted to the core coercers on purpose: a bareword call to a *user*
    /// class's name is also a coercion in rakudo (and dies with
    /// `X::Coerce::Impossible` when the class has no coercion method), but
    /// mutsu has no such coercion protocol yet, so those names keep resolving
    /// to whatever routine is declared.
    pub(super) fn name_is_core_type_coercer(name: &str) -> bool {
        matches!(
            name,
            "Int"
                | "Num"
                | "Str"
                | "Bool"
                | "Uni"
                | "Rat"
                | "FatRat"
                | "Complex"
                | "Real"
                | "Numeric"
                | "Array"
                | "List"
                | "Hash"
                | "Set"
                | "SetHash"
                | "Bag"
                | "BagHash"
                | "Mix"
                | "MixHash"
        )
    }

    /// Control-flow / dispatch-control names that must never be taken over by
    /// the lexical `&`-var Interpreter dispatch: the interpreter's call_function match
    /// implements their non-local semantics (loop control, gather/take,
    /// multi-dispatch redirection), and a lexical `&return`-style binding
    /// dispatched as a plain closure would lose them (or recurse infinitely).
    fn is_control_flow_function_name(name: &str) -> bool {
        matches!(
            name,
            "return"
                | "return-rw"
                | "take"
                | "take-rw"
                | "emit"
                | "done"
                | "last"
                | "next"
                | "redo"
                | "proceed"
                | "succeed"
                | "leave"
                | "die"
                | "fail"
                | "warn"
                | "exit"
                | "callsame"
                | "nextsame"
                | "callwith"
                | "nextwith"
                | "samewith"
                | "nextcallee"
                | "lastcall"
                | "make"
                | "start"
        )
    }

    /// Resolve a *pure* lexical `&name` callable for Interpreter-native dispatch
    /// (Track A): a `&code` parameter binding (local slot) or a `my &f = ...`
    /// env binding, for a name with NO same-named package sub / proto / multi
    /// (the shadow case is handled separately via `lexical_override` in
    /// `exec_call_func_op`). Restricted to plain `Sub`/`WeakSub` values —
    /// `Routine` (builtin references like `&r = &return`), `Mixin` (CALL-ME)
    /// and anything else keep the interpreter terminal, whose dispatch handles
    /// their special semantics. Returns `None` for builtin / interpreter-handled
    /// / carrier / control-flow names so precedence is unchanged.
    pub(super) fn lexical_amp_var_callable(
        &mut self,
        code: Option<&CompiledCode>,
        name: &str,
    ) -> Option<Value> {
        if Self::is_control_flow_function_name(name)
            || crate::runtime::Interpreter::is_builtin_function(name)
            || Self::is_interpreter_carrier_function(name)
            || self.is_interpreter_handled_function(name)
            || self.has_function(name)
            || self.has_proto_cached(name)
            || self.has_multi_candidates_cached(name)
        {
            return None;
        }
        let ampname = format!("&{}", name);
        let candidate = code
            .and_then(|c| self.locals_get_by_name(c, &ampname))
            .or_else(|| self.env().get(&ampname).cloned());
        candidate.filter(|v| {
            matches!(v.view(), ValueView::Sub(_) | ValueView::WeakSub(_))
                || matches!(v.view(), ValueView::Routine { .. })
                || matches!(
                    v.view(),
                    ValueView::Instance { class_name, .. }
                        if matches!(class_name.as_str(), "Method" | "Submethod" | "Regex")
                )
        })
    }

    /// Resolve a lexical `&infix:<op>` override (a `&infix:<op>` parameter or a
    /// `my &infix:<op>` binding) that shadows the package-level operator. Returns
    /// the bound callable when present, else `None`.
    pub(super) fn lexical_infix_override(
        &mut self,
        code: &CompiledCode,
        infix_name: &str,
    ) -> Option<Value> {
        let ampname = format!("&{}", infix_name);
        let candidate = self
            .locals_get_by_name(code, &ampname)
            .or_else(|| self.env().get(&ampname).cloned());
        candidate.filter(|v| {
            matches!(
                v.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Mixin(..)
            )
        })
    }

    /// `CallFuncNamed`: a call site whose literal named args travel out-of-band
    /// (bare values on the stack + a `NamedArgsSpec`). The light-call cache hit
    /// binds them by `Symbol` with zero Pair boxing; every other route
    /// materializes the Pairs in place on the stack and delegates to the
    /// ordinary `exec_call_func_op`, so behavior off the fast path is
    /// byte-identical to the old MakePair form.
    pub(super) fn exec_call_func_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        spec_idx: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        let arity_usize = arity as usize;
        let spec = &code.named_arg_specs[spec_idx as usize];
        // Fast path: light-call cache hit, spec-aware binding. Mirrors the
        // CallFunc light-cache block, with the container/junction guards
        // folded into one conservative scan (an Array/Hash value anywhere
        // may need container sharing; a Junction may need autothreading —
        // both fall back to the materializing slow path).
        if self.light_call_cache_gen == self.fn_resolve_gen
            && self.stack.len() >= arity_usize
            && (self.amp_param_shadowed_names.is_empty()
                || !self
                    .amp_param_shadowed_names
                    .contains(&code.const_sym(name_idx)))
        {
            let name_sym = code.const_sym(name_idx);
            if let Some((cached_key, cached_fp)) = self.light_call_cache.get(&name_sym)
                && let Some(cf) = compiled_fns.get(cached_key)
                && cf.fingerprint == *cached_fp
            {
                let base = self.stack.len() - arity_usize;
                let needs_slow = self.stack[base..].iter().any(|v| {
                    fn is_guarded(v: &Value) -> bool {
                        matches!(
                            v.view(),
                            ValueView::Array(..)
                                | ValueView::Hash(..)
                                | ValueView::Junction { .. }
                                // A Slip spreads into the argument list; only the
                                // slow path flattens it.
                                | ValueView::Slip(..)
                        )
                    }
                    match v.view() {
                        // A positional `$var` arg arrives VarRef-wrapped; the
                        // guard is about the inner value's kind.
                        ValueView::VarRef { value, .. } => is_guarded(value),
                        _ => is_guarded(v),
                    }
                });
                if !needs_slow {
                    let mut args = self.take_locals_from_pool(0);
                    args.extend(self.stack.drain(base..));
                    let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                    if cl.is_some() {
                        loan_env!(self, set_pending_callsite_line(cl));
                    }
                    let name_str = Self::const_str(code, name_idx);
                    let result = self.call_compiled_function_light_spec(
                        cf,
                        &args,
                        compiled_fns,
                        name_str,
                        Some(spec),
                    );
                    self.recycle_locals(args);
                    self.stack.push(result?);
                    self.drain_and_reconcile_after_cached_call(code);
                    return Ok(());
                }
            }
        }
        // Fallback: materialize the named Pairs in place, then run the
        // ordinary CallFunc dispatch (which re-records the dispatch stat;
        // subtract nothing — the double count is a stats-only artifact of
        // the fallback and keeps this wrapper branch-free).
        if self.stack.len() >= arity_usize {
            let base = self.stack.len() - arity_usize;
            for e in &spec.entries {
                let slot = &mut self.stack[base + e.pos as usize];
                let val = std::mem::replace(slot, Value::NIL);
                *slot = Value::pair(e.key.clone(), val);
            }
        }
        self.exec_call_func_op(code, name_idx, arity, arg_sources_idx, compiled_fns)
    }

    pub(super) fn exec_call_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        // NativeCall: a sub declared `is native(...)` is dispatched through C
        // FFI rather than running its (`{ * }`) Raku body. The registry is
        // empty in the overwhelmingly common case, so this guard is free.
        if !self.native_call_specs.is_empty() {
            let name_str = Self::const_str(code, name_idx);
            // A native descriptor is keyed by the sub's short name (and, when the
            // declaring package is known at registration, its qualified name). A
            // package-qualified callsite (`OpenSSL::EVP::EVP_aes_128_cbc`) may
            // therefore need the short-name fallback — `unit module` subs are
            // registered while `current_package` is still GLOBAL, so only the
            // short name is present. `resolve_native_call_spec` also honors a
            // same-scope plain-sub shadow of a bare-name native descriptor
            // (Raku: a local declaration shadows a same-named
            // imported/needed symbol).
            let spec = self.resolve_native_call_spec(name_str);
            if let Some(mut spec) = spec {
                self.resolve_native_ret_struct(&mut spec);
                let arity_usize = arity as usize;
                if self.stack.len() < arity_usize {
                    return Err(RuntimeError::new(format!(
                        "NativeCall: '{}' called with too few arguments on the stack",
                        spec.symbol
                    )));
                }
                let start = self.stack.len() - arity_usize;
                let mut args: Vec<Value> = self.stack.drain(start..).collect();
                // Drop the synthetic callsite-line marker the compiler may append.
                args.retain(|a| !Self::is_callsite_line_marker(a));
                let (result, out_args) =
                    crate::runtime::nativecall::call_native_with_out_args(self, &spec, &args)?;
                // An `is rw` numeric out-parameter whose argument is a plain
                // variable arrives as a `VarRef`; the marshalling layer cannot
                // reach the caller's slot, so write it back here by name —
                // `PQunescapeBytea($v, my size_t $elems)` must leave the
                // written length in `$elems`.
                if !out_args.is_empty() {
                    let mut wrote = false;
                    for (idx, val) in out_args {
                        if let crate::value::ValueView::VarRef { name, .. } = args[idx].view() {
                            let n = name.resolve().to_string();
                            self.env_mut().insert(n.clone(), val);
                            self.pending_rw_writeback_sources.push(n);
                            wrote = true;
                        }
                    }
                    if wrote {
                        self.apply_pending_rw_writeback(code);
                    }
                }
                self.stack.push(result);
                return Ok(());
            }
        }
        // An empty-signature proto (`proto bar {*}`) gates the whole dispatch:
        // a call with positional arguments can never reach a candidate. Reject
        // it here, before the light-call caches would dispatch directly to a
        // multi candidate. Guarded by `is_empty()` so the common case is free.
        if !self.empty_sig_proto_names.is_empty()
            && self
                .empty_sig_proto_names
                .contains(&code.const_sym(name_idx))
            // Re-verify against the registry: the name-only set can go stale
            // (an EVAL-scoped `proto bar {*}` must not veto an unrelated
            // mainline `bar`), so only gate while an empty-sig proto is still
            // the visible one.
            && self
                .resolve_proto_function(Self::const_str(code, name_idx))
                .is_some_and(|p| p.empty_sig)
        {
            let arity_usize = arity as usize;
            if self.stack.len() >= arity_usize {
                let stack_args = &self.stack[self.stack.len() - arity_usize..];
                let positional_types: Vec<String> = stack_args
                    .iter()
                    .filter(|a| {
                        !Self::is_callsite_line_marker(a)
                            && !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
                    })
                    .map(crate::value::types::what_type_name)
                    .collect();
                if !positional_types.is_empty() {
                    let name_str = Self::const_str(code, name_idx);
                    let msg = format!(
                        "Calling {}({}) will never work with signature of the proto ()",
                        name_str,
                        positional_types.join(", ")
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("objname".to_string(), Value::str(name_str.to_string()));
                    attrs.insert("signature".to_string(), Value::str("()".to_string()));
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::TypeCheck::Argument"),
                        attrs,
                    )));
                    return Err(err);
                }
            }
        }
        // If this name is used as a `&`-sigil parameter anywhere, a lexical
        // `&name` binding in the current frame may shadow a same-named package
        // sub. The name-keyed light-call caches cannot represent that, so bypass
        // them and let the slow path's `lexical_override` resolve correctly.
        // Guarded by `is_empty()` so the common (no `&`-param) case is free.
        let skip_name_caches = if self.amp_param_shadowed_names.is_empty() {
            false
        } else {
            self.amp_param_shadowed_names
                .contains(&code.const_sym(name_idx))
        };
        // ADR-0054 Slice 4: whether this call site wrote a `|EXPR` argument,
        // decided once from the compile-time descriptor rather than by
        // probing the stack for Slip-shaped values on every cache check
        // below. A plain argument that merely evaluates to a Slip
        // (`f(@a.Slip)`) does not set this, so such a call stays eligible
        // for the light-call / OTF caches instead of forfeiting them.
        let call_has_slip = Self::stack_args_have_slip(code, arg_sources_idx);
        // Ultra-fast path: positional light-call cache for positional-only functions.
        if !skip_name_caches {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = code.const_sym(name_idx);
            if self.pos_light_call_cache_gen == self.fn_resolve_gen {
                // An OTF-compiled body is owned by the cache rather than by
                // `compiled_fns`, so take an `Arc` handle to it and let the
                // borrow on `self` end before the call below.
                let mut otf_hold: Option<Arc<CompiledFunction>> = None;
                let cur_pkg_sym = self.current_package_sym();
                let cached: Option<&CompiledFunction> =
                    match self.pos_light_call_cache.get(&name_sym) {
                        Some(crate::runtime::PosLightTarget::Compiled { key, fingerprint }) => {
                            let (key, fingerprint) = (*key, *fingerprint);
                            compiled_fns
                                .get(&key)
                                .filter(|cf| cf.fingerprint == fingerprint)
                        }
                        Some(crate::runtime::PosLightTarget::Otf {
                            callsite_package,
                            cf,
                        }) => {
                            if *callsite_package == cur_pkg_sym {
                                otf_hold = Some(Arc::clone(cf));
                            }
                            None
                        }
                        None => None,
                    };
                if let Some(cf) = cached.or(otf_hold.as_deref()) {
                    let arity_usize = arity as usize;
                    if self.stack.len() >= arity_usize {
                        // One fused pass over the args (J4d): junction detection
                        // (skip the fast path to allow auto-threading) and the
                        // callsite-line marker peek used to walk the args
                        // separately; both are per-value view checks, so fold
                        // them into a single scan.
                        let stack_args = &self.stack[self.stack.len() - arity_usize..];
                        let mut has_junction = false;
                        let mut cl: Option<i64> = None;
                        for v in stack_args {
                            let view = v.view();
                            if matches!(view, ValueView::Junction { .. }) {
                                has_junction = true;
                                break;
                            }
                            if cl.is_none() {
                                cl = Self::callsite_line_of_view(&view);
                            }
                        }
                        // Slice 2d: array/hash into a plain `$` param must share the
                        // caller's container -> fall through to the slow path.
                        let share_into_scalar =
                            Self::call_shares_container_into_scalar_param(cf, stack_args);
                        if !has_junction && !call_has_slip && !share_into_scalar {
                            let start = self.stack.len() - arity_usize;
                            // Pooled args buffer (J4d): `drain(..).collect()`
                            // was one malloc/free per call on the hottest call
                            // path; the locals pool already recycles
                            // `Vec<Value>`s, so borrow it for the args too.
                            let mut args = self.take_locals_from_pool(0);
                            args.extend(self.stack.drain(start..));
                            if cl.is_some() {
                                loan_env!(self, set_pending_callsite_line(cl));
                            }
                            let result = self.call_compiled_function_positional_light(
                                cf,
                                &args,
                                compiled_fns,
                                name_str,
                            );
                            self.recycle_locals(args);
                            self.stack.push(result?);
                            // Slice F: drain any captured-outer writes the body
                            // recorded through to this caller frame's local slots
                            // (the slow dispatch path drains too; the cached fast
                            // path must as well or a second call's write is lost).
                            // The env_dirty-gated reconcile also catches a
                            // *nested* callee's captured-outer write (multi-frame
                            // accumulation), which the single-frame drain misses.
                            // The reconcile is free for a pure call (env not
                            // dirtied — e.g. fib).
                            self.drain_and_reconcile_after_cached_call(code);
                            return Ok(());
                        }
                    }
                }
            } else {
                self.pos_light_call_cache.clear();
                self.pos_light_call_cache_gen = self.fn_resolve_gen;
            }
        }

        // Light-call cache check for named-param functions.
        if !skip_name_caches {
            let name_sym = code.const_sym(name_idx);
            if self.light_call_cache_gen == self.fn_resolve_gen {
                if let Some((cached_key, cached_fp)) = self.light_call_cache.get(&name_sym)
                    && let Some(cf) = compiled_fns.get(cached_key)
                    && cf.fingerprint == *cached_fp
                {
                    let arity_usize = arity as usize;
                    // Slice 2d (named follow-up): an `@`/`%` variable passed by
                    // name to a plain `$` named param must share the caller's
                    // container; the named light path binds a copy. Decode the
                    // arg sources lazily (only on a named-cache hit) so the
                    // common path pays nothing.
                    let named_share = self.stack.len() >= arity_usize && {
                        let decoded = self.decode_arg_sources(code, arg_sources_idx);
                        Self::call_shares_container_into_named_scalar_param(
                            cf,
                            &self.stack[self.stack.len() - arity_usize..],
                            decoded.as_deref(),
                        )
                    };
                    // Junction autothreading happens in the slow dispatch
                    // (maybe_autothread_func_call), which this cache hit
                    // bypasses. A Junction in a positional slot of a mixed
                    // signature must still thread, so skip the fast path
                    // (named-only signatures never autothread, but the scan
                    // is one view check per arg either way).
                    let has_junction = self.stack.len() >= arity_usize
                        && self.stack[self.stack.len() - arity_usize..]
                            .iter()
                            .any(|v| matches!(v.view(), ValueView::Junction { .. }));
                    if self.stack.len() >= arity_usize
                        && !named_share
                        && !has_junction
                        && !call_has_slip
                        && !Self::call_shares_container_into_scalar_param(
                            cf,
                            &self.stack[self.stack.len() - arity_usize..],
                        )
                    {
                        let start = self.stack.len() - arity_usize;
                        // Pooled args buffer (J4d): `drain(..).collect()` was one
                        // malloc/free per call; borrow the locals pool's recycled
                        // `Vec<Value>` instead (mirrors the positional cached path).
                        let mut args = self.take_locals_from_pool(0);
                        args.extend(self.stack.drain(start..));
                        // Extract callsite line for deprecation tracking
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                        if cl.is_some() {
                            loan_env!(self, set_pending_callsite_line(cl));
                        }
                        let name_str = Self::const_str(code, name_idx);
                        let result =
                            self.call_compiled_function_light(cf, &args, compiled_fns, name_str);
                        self.recycle_locals(args);
                        self.stack.push(result?);
                        // Slice F: drain captured-outer writes through to this
                        // caller frame's local slots (see the positional-light
                        // cached path above). The env_dirty-gated reconcile also
                        // covers a nested callee's captured-outer write
                        // (multi-frame accumulation).
                        self.drain_and_reconcile_after_cached_call(code);
                        return Ok(());
                    }
                }
            } else {
                self.light_call_cache.clear();
                self.light_call_cache_gen = self.fn_resolve_gen;
            }
        }

        // OTF-compiled function cache check: for user-defined functions that
        // were compiled on-the-fly (not in compiled_fns), use the cached
        // compiled form to avoid the expensive interpreter fallback.
        // The body is `Arc`-shared, so releasing the borrow on `self` costs one
        // refcount bump; the entry stays in the table (it used to be `remove`d
        // and re-`insert`ed around every call, memcpying a ~1 kB
        // `CompiledFunction` twice per call — see `otf_call_cache`'s doc).
        if !skip_name_caches {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = code.const_sym(name_idx);
            if self.otf_call_cache_gen == self.fn_resolve_gen {
                // Skip this type-blind name-keyed fast cache for multi names: the
                // right candidate depends on argument types, which this cache
                // does not key on (the multi fork resolves per-call via
                // resolve_function_with_types instead). Guard the lookup, not the
                // gen check, so a multi call never clears the whole cache.
                // The cache is also package-blind, so an entry is only reusable
                // from the package it was resolved under (PLAN 8.22): a `unit
                // module Foo`'s non-exported sub must not stay callable by its
                // bare name once control returns to the consumer's GLOBAL scope.
                // A package mismatch leaves the entry in place for the package
                // that does own it.
                let cur_pkg_sym = self.current_package_sym();
                if !self.has_multi_candidates_cached_sym(name_sym)
                    && let Some((def_pkg_sym, cf)) = self
                        .otf_call_cache
                        .get(&name_sym)
                        .filter(|(pkg, _, _)| *pkg == cur_pkg_sym)
                        .map(|(_, def_pkg, cf)| (*def_pkg, Arc::clone(cf)))
                    && !cf.has_inner_subs
                {
                    let arity_usize = arity as usize;
                    if self.stack.len() >= arity_usize && !call_has_slip {
                        let start = self.stack.len() - arity_usize;
                        // Pooled args buffer (mirrors the two light-call cached
                        // paths above): `drain(..).collect()` was one malloc/free
                        // per call on this path, which every block-local sub call
                        // takes. Only the cold `call_compiled_function_named` arm
                        // consumes the `Vec`; the light arms borrow it and it goes
                        // back to the pool below.
                        let mut args = self.take_locals_from_pool(0);
                        args.extend(self.stack.drain(start..));

                        // Extract callsite line for deprecation tracking
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                        if cl.is_some() {
                            loan_env!(self, set_pending_callsite_line(cl));
                        }

                        // Slice 2d: array/hash passed to a plain `$` param must
                        // share the caller's container -> force the slow binding
                        // path (the slot-only fast paths bind a detached copy).
                        // The named variant (`f(n => @a)` into `:$n`) needs the
                        // arg-source table to find the caller variable, so decode
                        // it here (only on an otf-cache hit).
                        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
                        let share_into_scalar =
                            Self::call_shares_container_into_scalar_param(&cf, &args);
                        let named_share = Self::call_shares_container_into_named_scalar_param(
                            &cf,
                            &args,
                            decoded_sources.as_deref(),
                        );
                        // A Junction arg must reach the slow dispatch, which
                        // autothreads it (this cache hit bypasses that check).
                        let has_junction = args
                            .iter()
                            .any(|v| matches!(v.view(), ValueView::Junction { .. }));
                        let mainline_capture_blocked =
                            self.light_call_blocked_by_mainline_capture(name_str);
                        let result = if !share_into_scalar
                            && !named_share
                            && !has_junction
                            && !mainline_capture_blocked
                            && Self::is_light_call_eligible(&cf, name_str)
                        {
                            self.call_compiled_function_light(&cf, &args, compiled_fns, name_str)
                        } else if !share_into_scalar
                            && !named_share
                            && !mainline_capture_blocked
                            && Self::is_positional_light_call_eligible(&cf, name_str)
                        {
                            // Promote to the ultra-fast positional cache at the
                            // top of this function, so the next call skips this
                            // whole preamble (eligibility re-analysis, arg-source
                            // decode, junction/slip/share re-scans) instead of
                            // re-deriving it per call. `has_junction` /
                            // `stack_args_have_slip` / the share checks above are
                            // per-CALL properties, and that path re-checks all of
                            // them in its own fused scan before dispatching.
                            self.pos_light_call_cache.insert(
                                name_sym,
                                crate::runtime::PosLightTarget::Otf {
                                    callsite_package: cur_pkg_sym,
                                    cf: Arc::clone(&cf),
                                },
                            );
                            self.call_compiled_function_positional_light(
                                &cf,
                                &args,
                                compiled_fns,
                                name_str,
                            )
                        } else {
                            // The body must run under its *defining* package, not
                            // the callsite's (see `otf_call_cache`'s doc comment).
                            let pkg = def_pkg_sym.resolve();
                            self.push_samewith_context(name_str, None, None);
                            let pushed_dispatch =
                                loan_env!(self, push_multi_dispatch_frame(name_str, &args));
                            // The named-share writeback reads the arg sources;
                            // make them available to bind_function_args_values.
                            self.set_pending_call_arg_sources(decoded_sources);
                            let r = self.call_compiled_function_named(
                                &cf,
                                std::mem::take(&mut args),
                                compiled_fns,
                                &pkg,
                                name_str,
                            );
                            self.set_pending_call_arg_sources(None);
                            self.pop_samewith_context();
                            if pushed_dispatch {
                                self.pop_multi_dispatch();
                            }
                            r
                        };
                        self.recycle_locals(args);
                        let result = result?;
                        // Slice F: drain this frame's recorded captured-outer
                        // writes, plus (env_dirty-gated) reconcile to catch a
                        // nested callee's captured-outer write that the
                        // single-frame drain misses (multi-frame accumulation).
                        self.drain_and_reconcile_after_cached_call(code);
                        self.stack.push(result);
                        // Slice 6.3 step 2: all three sub-cases now signal env_dirty
                        // precisely — light / positional_light via their scoped-overlay
                        // merge, and the named sub-case via call_compiled_function_named's
                        // return merge. No blanket mark needed.
                        return Ok(());
                    }
                }
            } else {
                self.otf_call_cache.clear();
                self.otf_call_cache_gen = self.fn_resolve_gen;
            }
        }

        // If there's a lexical `&name` override — either as a compiled local
        // slot (e.g. from a `&foo` parameter binding) or in the env — it
        // shadows package-level subs. Skip the fast path and dispatch via
        // the lexical callable below.
        let lexical_override: Option<Value> = {
            let name_str = Self::const_str(code, name_idx);
            // Only look for a lexical override when there is actually a
            // same-named package sub to shadow. When no package sub exists,
            // the normal dispatch path already handles lexical `&name`
            // bindings correctly (via its own env lookup), and avoiding
            // this branch prevents regressions where dispatching through
            // `call_sub_value` behaves differently (e.g. dynamic `$*ERR`
            // handling for `note` inside a caller-provided block).
            // Pure disjunction — every arm yields `None` — so order it
            // cheapest-first: the base-name negative gate (#5574) short-
            // circuits the whole check for a builtin like `make` (no registry
            // key carries the name, so `has_function` is false), and the
            // multi-candidates full-map scan runs last, memoized.
            if !self.fn_base_name_registered(name_str)
                || !self.has_function(name_str)
                || self.has_proto_cached(name_str)
                || self.has_multi_candidates_cached(name_str)
            {
                None
            } else {
                let ampname = format!("&{}", name_str);
                // First check local slots (parameter bindings live here).
                let from_local = self.locals_get_by_name(code, &ampname);
                let candidate = from_local.or_else(|| self.env().get(&ampname).cloned());
                candidate.filter(|v| Self::env_callable_is_lexical_override(v, name_str))
            }
        };
        let has_lexical_override = lexical_override.is_some();
        // Early fast path: for cached zero-arg compiled functions, skip ALL the
        // expensive arg processing, CALL-ME check, wrap chain check, autothread, etc.
        // Only the callsite line pair (if present) needs to be popped from the stack.
        if !has_lexical_override && arity <= 1 {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = code.const_sym(name_idx);
            let cache_key = (name_sym, 0usize, Vec::<String>::new());
            let use_cache = !self.has_multi_candidates_cached(name_str);
            if use_cache
                && self.fn_resolve_cache_gen == self.fn_resolve_gen
                && self.wrap_sub_id_for_name(name_str).is_none()
                && !loan_env!(self, routine_is_test_assertion_by_name(name_str, &[]))
                && let Some((cached_key, cached_fp, _)) = self.fn_resolve_cache.get(&cache_key)
                && let Some(cf) = compiled_fns.get(cached_key)
                && cf.fingerprint == *cached_fp
                && Self::is_fast_call_eligible(cf, name_str)
                && !self.light_call_blocked_by_mainline_capture(name_str)
                && !cf.is_raw
            {
                // Pop the callsite pair arg(s) from the stack and extract callsite line
                let arity = arity as usize;
                if self.stack.len() >= arity && arity > 0 {
                    let start = self.stack.len() - arity;
                    let popped: Vec<Value> = self.stack.drain(start..).collect();
                    let cl = crate::runtime::Interpreter::peek_callsite_line(&popped);
                    if cl.is_some() {
                        loan_env!(self, set_pending_callsite_line(cl));
                    } else {
                        // `is_fast_call_eligible` requires a completely empty
                        // signature (`cf.params.is_empty() && cf.param_defs.is_empty()`),
                        // so any popped value that is NOT the synthetic
                        // callsite-line marker is a genuine over-supplied
                        // positional argument (`todo/tickets/fast-binder-skips-too-many-positionals-check.md`).
                        // Raise the same "Too many positionals passed"
                        // wording the general binder produces
                        // (binding_signature.rs) -- several call sites
                        // pattern-match on this exact message.
                        return Err(RuntimeError::typed(
                            "X::TypeCheck::Argument",
                            Self::type_check_argument_attrs(
                                name_str,
                                &cf.param_defs,
                                &popped,
                                format!(
                                    "Too many positionals passed; expected 0 arguments but got {}",
                                    popped.len()
                                ),
                            ),
                        ));
                    }
                }
                let result =
                    match self.call_compiled_function_fast(cf, name_str, name_sym, compiled_fns) {
                        Ok(v) => v,
                        Err(e) => {
                            // Slice F (exception-escape coherence): an exceptional exit
                            // (`die`/`fail`) still ran the callee's UNDO/LEAVE phasers,
                            // which can mutate a captured-outer variable (e.g. `UNDO {
                            // $ng ~= "U" }`). The body recorded those writes into
                            // `pending_rw_writeback_sources`; drain them to this caller's
                            // local slots *before* propagating the error, exactly as the
                            // Ok path does, so the reverse `sync_locals_from_env` pull is
                            // not required for coherence.
                            self.apply_pending_rw_writeback(code);
                            return Err(e);
                        }
                    };
                self.stack.push(result);
                // Slice F: write any captured-outer variables the callee mutated
                // straight through to this caller's local slots, so they stay
                // coherent without the reverse `sync_locals_from_env` pull. The
                // env_dirty-gated reconcile additionally catches a *nested*
                // callee's captured-outer write (`via()` -> `bump-outer()` ->
                // `$acc`), which the single-frame drain discards one frame too
                // deep, so `via(); via()` accumulates correctly.
                self.drain_and_reconcile_after_cached_call(code);
                // Slice 6.3 step 2: no blanket env_dirty here. call_compiled_function_fast
                // now signals env_dirty precisely: for a function WITH locals via its
                // scoped-overlay / clone merge (captured-outer write only), and for a
                // 0-local function via the compile-time `has_env_writes` gate. A pure
                // 0-arg call (`sub f { 42 }`) no longer forces a per-call
                // O(caller-locals) locals pull.
                return Ok(());
            }
        }
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("Interpreter stack underflow in CallFunc"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // ADR-0054 S2: spread only the positions the caller wrote as
        // `|EXPR` -- decided by call-site syntax, not by a value merely
        // evaluating to a Slip (`f(@a.Slip)` stays one argument).
        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
        let (args, arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, decoded_sources);
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.sanitize_call_args_owned(args);
        // Don't auto-FETCH Proxy args for control flow builtins that must preserve containers,
        // or when in lvalue assignment context (e.g. f() = 42 calls f with in_lvalue_assignment=true).
        let skip_proxy_fetch = matches!(
            name.as_str(),
            "return-rw"
                | "return"
                | "die"
                | "fail"
                | "leave"
                | "__mutsu_assign_method_lvalue"
                | "__mutsu_index_assign_method_lvalue"
                | "__mutsu_index_delete_method_lvalue"
        ) || self.in_lvalue_assignment;
        let args = if skip_proxy_fetch {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        loan_env!(self, set_pending_callsite_line(callsite_line));
        // A lexically-bound `&callsame`/`&callwith`/`&nextsame`/`&nextwith`/
        // `&samewith` (`my &callwith := -> ... { ... }`) shadows the built-in
        // dispatcher routine of the same name (roast advent2013-day21.t pointy
        // block). These names are control-flow builtins, so the normal
        // `lexical_amp_var_callable` path excludes them; check the binding
        // explicitly here and call it as an ordinary sub.
        if matches!(
            name.as_str(),
            "callsame" | "callwith" | "nextsame" | "nextwith" | "samewith"
        ) {
            let ampname = format!("&{}", name);
            if let Some(callable) = self
                .locals_get_by_name(code, &ampname)
                .or_else(|| self.env().get(&ampname).cloned())
                .filter(|v| matches!(v.view(), ValueView::Sub(_) | ValueView::WeakSub(_)))
            {
                let result = self.vm_call_sub_value(callable, args, false)?;
                self.apply_pending_rw_writeback(code);
                self.stack.push(result);
                return Ok(());
            }
        }
        // Check if there's a CALL-ME override from trait_mod mixin
        let call_me_override =
            self.env()
                .get(&format!("&{}", name))
                .cloned()
                .and_then(|callable| {
                    let has_call_me = if let ValueView::Mixin(_, mixins) = callable.view() {
                        mixins.keys().any(|key| {
                            key.strip_prefix("__mutsu_role__")
                                .is_some_and(|rn| self.role_has_method(rn, "CALL-ME"))
                        })
                    } else {
                        false
                    };
                    if has_call_me { Some(callable) } else { None }
                });
        // Junction auto-threading for function call arguments:
        // If any positional arg is a Junction and the function parameter doesn't accept
        // Junction (i.e., not typed as Mu or Junction), auto-thread over the junction.
        if let Some(autothread_result) =
            self.maybe_autothread_func_call(code, &name, &args, &arg_sources, compiled_fns)?
        {
            self.stack.push(autothread_result);
            // Slice F: the threaded eigenstate calls may have mutated captured-outer
            // variables (`sub j($x) { $count++ }`); write their accumulated final
            // env values through to this caller's local slots so they stay coherent
            // without the reverse `sync_locals_from_env` pull.
            self.apply_pending_rw_writeback(code);
            return Ok(());
        }

        // A bareword call to a core type's name is that type's COERCION, and a
        // same-named user sub does not shadow it. Rakudo parses `Int('42')` as
        // the coercer even with `sub Int(Str $s) {...}` in scope; only the
        // `&`-sigiled `&Int('42')` reaches the sub — and that spelling compiles
        // to `CallOnCodeVar`, a different opcode, so gating the bareword funnel
        // here reproduces exactly rakudo's split. This must sit ahead of every
        // user-sub resolution path below; the name-keyed light-call caches
        // above can never hold one of these names because they are only
        // populated further down, past this gate.
        if Self::name_is_core_type_coercer(&name) && !args.is_empty() {
            let result = self.vm_call_function(&name, args)?;
            self.stack.push(result);
            return Ok(());
        }

        // Check wrap chain for named function calls
        if self.wrap_sub_id_for_name(&name).is_some()
            && let Some(sub_val) = self.get_wrapped_sub(&name)
        {
            let result = self.vm_call_sub_value(sub_val, args, false)?;
            // Slice F (multi-frame coherence): a wrapper closure (`&f.wrap(-> {
            // $seen = True; callsame })`) mutates a captured caller lexical by name.
            // The closure dispatch recorded it precisely (`pending_*_writeback`);
            // drain it so the caller's slot refreshes without the blanket env→locals
            // pull (env_dirty-removal substrate). The blanket reconcile stays as the
            // fallback (no-op under the substrate harness).
            self.apply_pending_rw_writeback(code);
            self.stack.push(result);
            return Ok(());
        }

        // Lexical `&name` binding (e.g. from `sub callit(&foo) { foo(1) }`)
        // takes precedence over package-level compiled subs. Dispatch
        // Interpreter-natively via `vm_call_on_value` (same as the pure-lexical case in
        // `dispatch_func_call_inner`, Track A): `call_compiled_closure` roots
        // the closure frame at the live caller env (scoped_child) so dynamic
        // vars (`my $*ERR` in the caller) stay visible, and first-class
        // instance cells make mutating methods on caller-held instances visible
        // across frames. The override value is always a `Sub` value
        // (`env_callable_is_lexical_override`), so this never reaches the
        // interpreter terminal.
        if let Some(callable) = lexical_override {
            let result = self.vm_call_on_value(callable, args, Some(compiled_fns))?;
            self.stack.push(result);
            return Ok(());
        }
        // Slice F (env<->locals coherence, docs/env-locals-coherence.md): the
        // lvalue-method writeback builtins (`$p.value = X` / `.value--`,
        // `@a.head = v`, `%h.AT-KEY(k) = v`, `@a.first(...) = v`, ...) mutate
        // their target variable in `env` *by name* (`self.env.insert(var, ...)`)
        // and rely on the reverse pull to refresh the caller's local slot. The
        // target variable name is the 5th argument. Capture it so we can write
        // the new env value straight through to the local slot after dispatch,
        // keeping locals coherent without depending on the `env_dirty` backstop.
        let lvalue_writeback_target = match name.as_str() {
            "__mutsu_assign_method_lvalue" => args
                .get(4)
                .map(|v| v.to_string_value())
                .filter(|s| !s.is_empty()),
            "__mutsu_index_assign_method_lvalue" => args
                .get(if args.len() >= 6 { 5 } else { 4 })
                .map(|v| v.to_string_value())
                .filter(|s| !s.is_empty()),
            "__mutsu_index_delete_method_lvalue" => args
                .get(3)
                .map(|v| v.to_string_value())
                .filter(|s| !s.is_empty()),
            _ => None,
        };
        let package_index_lvalue = name == "__mutsu_index_assign_method_lvalue"
            && args
                .first()
                .is_some_and(|target| matches!(target.view(), ValueView::Package(_)));
        // Snapshot the target's CURRENT env value so the writeback below can tell
        // whether the lvalue builtin actually changed it. Some lvalue methods do
        // NOT write `env[target]` at all (`Failure.handled = True` only flips a
        // global registry keyed by the instance id; the instance in the slot is
        // untouched). Under the `(B)` per-store env-write gate a preceding
        // `my $f = Failure.new` leaves `env<f>` at its `Any` decl seed, so an
        // unconditional pull would clobber the live instance slot with that stale
        // `Any`. Mirrors the mechanism-#3 `env_changed` guard in
        // `carrier_writeback_changed_aggregates`.
        let lvalue_writeback_pre = lvalue_writeback_target
            .as_ref()
            .map(|t| self.env().get(t).cloned());
        let result = match self.dispatch_func_call_inner(
            code,
            &name,
            args,
            arg_sources,
            call_me_override,
            compiled_fns,
        ) {
            Ok(v) => v,
            Err(e) => {
                // Slice F (exception-escape coherence): an exceptional exit still
                // ran the callee's UNDO/LEAVE phasers, which can mutate a
                // captured-outer variable. Drain those recorded writes to the
                // caller's local slots before propagating the error.
                self.apply_pending_rw_writeback(code);
                return Err(e);
            }
        };
        if let Some(target) = lvalue_writeback_target
            && let Some(slot) = self.find_local_slot(code, &target)
            // Mirror the reverse pull's invariant: never clobber a live
            // `HashEntryRef` binding slot with a plain env copy.
            && !matches!(self.locals[slot].view(), ValueView::HashEntryRef { .. })
            && let Some(val) = self.env().get(&target).cloned()
            // Only apply when the lvalue builtin genuinely changed `env[target]`
            // during the call (see the snapshot above). Gate OFF this is
            // byte-identical: env tracks the slot, so a builtin that writes
            // `env[target]` always leaves `prev != val`, and one that does not
            // leaves `prev == val == the live slot value` (a no-op pull anyway).
            && (package_index_lvalue
                || match lvalue_writeback_pre {
                    Some(Some(ref prev)) => !prev.same_variant(&val) || *prev != val,
                    _ => true,
                })
        {
            self.locals[slot] = val;
        }
        // Slice F: write any `is rw` parameter writeback through to the caller's
        // local slot (see `apply_pending_rw_writeback`).
        self.apply_pending_rw_writeback(code);
        self.stack.push(result);
        // env_dirty is now managed inside dispatch_func_call_inner: the
        // interpreter / native fallback branches set it (they mutate env by
        // name), while the compiled fast paths (positional_light / light /
        // named) rely on their own scoped-overlay merge to signal env_dirty
        // only when a captured-outer write actually happened. This stops a pure
        // compiled call (e.g. `fib`) from forcing a redundant locals pull per
        // call.
        Ok(())
    }

    pub(super) fn exec_call_on_value_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallOnValue",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // ADR-0054 S2: spread only the `|EXPR` positions, decided by
        // call-site syntax rather than a value's runtime Slip-shape.
        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
        let (args, arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, decoded_sources);
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallOnValue target".to_string())
        })?;

        // Resolve slot refs to their underlying values before dispatch
        let target = if matches!(target.view(), ValueView::HashEntryRef { .. }) {
            target.hash_entry_read()
        } else {
            target
        };

        // Upgrade WeakSub (e.g., &?BLOCK) to strong Sub before dispatch
        let target = if let ValueView::WeakSub(weak) = target.view() {
            match weak.upgrade() {
                Some(strong) => Value::sub_value(strong),
                None => Value::NIL,
            }
        } else {
            target
        };

        let sub_is_rw = if let ValueView::Sub(data) = target.view() {
            data.is_rw
        } else {
            false
        };
        self.set_pending_call_arg_sources(arg_sources);
        let result = self.vm_call_on_value(target, args, Some(compiled_fns));
        self.set_pending_call_arg_sources(None);
        let result = result?;
        let result = loan_env!(self, maybe_fetch_rw_proxy(result, sub_is_rw))?;
        self.apply_pending_rw_writeback(code);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_call_on_code_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallOnCodeVar",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // ADR-0054 S2: spread only the `|EXPR` positions, decided by
        // call-site syntax rather than a value's runtime Slip-shape.
        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
        let (args, arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, decoded_sources);
        let (args, callsite_line) = self.sanitize_call_args_owned(args);
        // `sanitize_call_args_owned` may drop a synthetic callsite-line
        // marker pair, shortening `args` by one relative to the just-aligned
        // `arg_sources` -- re-check length here (mirrors the pre-ADR-0054
        // guard) rather than in `spread_call_args_by_syntax`, which aligns
        // against the pre-sanitize argument list.
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        loan_env!(self, set_pending_callsite_line(callsite_line));
        // resolve_code_var handles pseudo-package stripping internally
        let mut target = loan_env!(self, resolve_code_var(&name));
        // A `&`-sigil binding may live only in this frame's LOCAL SLOT, never in
        // env — that is how a `&`-sigil named parameter binds (`sub f(:&cb)`,
        // see `news/2026-08/named-callable-parameter-binds.md`). `&cb()` in such
        // a body therefore answered "Unknown function: cb" while `&cb.defined`
        // worked, because only the env was consulted.
        if target.is_nil()
            && let Some(slot) = self.find_local_slot(code, &format!("&{name}"))
            && let Some(val) = self.locals.get(slot)
            && !val.is_nil()
        {
            target = val.clone();
        }
        // Fallback for fast-path method dispatch (skip_env_setup=true):
        // &!attr is not set in env, so read directly from self's instance
        // attributes when available.
        if target.is_nil()
            && let Some(attr_name) = name.strip_prefix('!').filter(|n| !n.is_empty())
            && let Some(ValueView::Instance { attributes, .. }) = self
                .get_env_with_main_alias("self")
                .as_ref()
                .map(Value::view)
            && let Some(attr_val) = attributes.as_map().get(attr_name)
        {
            target = attr_val.clone();
        }
        let result = if !target.is_nil() {
            let sub_is_rw = if let ValueView::Sub(data) = target.view() {
                data.is_rw
            } else {
                false
            };
            self.set_pending_call_arg_sources(arg_sources.clone());
            let result = self.vm_call_on_value(target, args, Some(compiled_fns));
            self.set_pending_call_arg_sources(None);
            let result = result?;
            loan_env!(self, maybe_fetch_rw_proxy(result, sub_is_rw))?
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?
        } else if !self.has_proto_cached(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            let cf_auto_fetch = !cf.is_raw;
            let pkg = self.current_package().to_string();
            self.set_pending_call_arg_sources(arg_sources.clone());
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.set_pending_call_arg_sources(None);
            let result = result?;
            loan_env!(self, maybe_fetch_rw_proxy(result, cf_auto_fetch))?
        } else {
            // Sync Interpreter locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals_needed(code);
            }
            self.set_pending_call_arg_sources(arg_sources);
            let result = self.call_function_compiled_first(&name, args, compiled_fns);
            self.set_pending_call_arg_sources(None);
            result?
        };
        let result = loan_env!(self, maybe_fetch_rw_proxy(result, true))?;
        self.apply_pending_rw_writeback(code);
        self.stack.push(result);
        Ok(())
    }

    /// Inner dispatch for function calls. Handles CALL-ME override, compiled functions,
    /// native functions, and interpreter fallback. Returns the result value.
    pub(super) fn dispatch_func_call_inner(
        &mut self,
        code: &CompiledCode,
        name: &str,
        args: Vec<Value>,
        arg_sources: Option<Vec<Option<String>>>,
        call_me_override: Option<Value>,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        if name == "__PROTO_DISPATCH__" {
            // `{*}` inside a compiled proto body (ledger §D): the proto-dispatch
            // marker rewritten by `rewrite_proto_dispatch_stmts`. Resolve and run
            // the winning multi candidate VM-natively (compiled bytecode) instead
            // of bouncing through interpreter `call_proto_dispatch` + `run_block`.
            return self.vm_call_proto_dispatch(code, compiled_fns);
        }
        if let Some(callable) = call_me_override {
            let result = self.try_compiled_method_or_interpret(callable, "CALL-ME", args);
            let result = result?;
            loan_env!(self, maybe_fetch_rw_proxy(result, true))
        } else {
            self.set_pending_call_arg_sources(arg_sources.clone());
            let compiled = if !self.has_proto_cached(name) {
                self.find_compiled_function(compiled_fns, name, &args)
            } else {
                None
            };
            self.set_pending_call_arg_sources(None);
            if let Some(cf) = compiled {
                // Try positional light call path first (ultra-fast, no env clone).
                // Skip for multi functions since the cache doesn't differentiate by arg types.
                if Self::is_positional_light_call_eligible(cf, name)
                    && !Self::call_shares_container_into_scalar_param(cf, &args)
                    && !self.has_multi_candidates_cached(name)
                    && !loan_env!(self, routine_is_test_assertion_by_name(name, &args))
                    && self.wrap_sub_id_for_name(name).is_none()
                    && !self.light_call_blocked_by_mainline_capture(name)
                {
                    let name_sym = Symbol::intern(name);
                    if !self.pos_light_call_cache.contains_key(&name_sym) {
                        for (key, func) in compiled_fns {
                            if std::ptr::eq(func, cf) {
                                self.pos_light_call_cache.insert(
                                    name_sym,
                                    crate::runtime::PosLightTarget::Compiled {
                                        key: *key,
                                        fingerprint: cf.fingerprint,
                                    },
                                );
                                break;
                            }
                        }
                    }
                    let result =
                        self.call_compiled_function_positional_light(cf, &args, compiled_fns, name);
                    let result = result?;
                    return loan_env!(self, maybe_fetch_rw_proxy(result, true));
                }
                // Try light call path for simple functions in tight loops.
                // This avoids the expensive env clone/restore cycle.
                //
                // Skipped for a multi, exactly as the positional-light path above
                // is: the cache is keyed by NAME, so a second call with different
                // named arguments would reuse the first call's candidate, and the
                // light path pushes neither the multi-dispatch frame nor the
                // samewith context a candidate's `nextsame` needs. Only the
                // `multi f(:x($))` / `multi f(:y($))` shape reached this — a
                // candidate set that shares one positional signature — and until
                // those candidates got distinct compiled-routine keys, resolution
                // could not hand this path a per-candidate body at all
                // (roast/S06-multi/positional-vs-named.t).
                if Self::is_light_call_eligible(cf, name)
                    && !self.has_multi_candidates_cached(name)
                    && !Self::call_shares_container_into_scalar_param(cf, &args)
                    && !Self::call_shares_container_into_named_scalar_param(
                        cf,
                        &args,
                        arg_sources.as_deref(),
                    )
                    && !loan_env!(self, routine_is_test_assertion_by_name(name, &args))
                    && self.wrap_sub_id_for_name(name).is_none()
                    && !self.light_call_blocked_by_mainline_capture(name)
                {
                    // Populate light-call cache so subsequent calls skip resolution
                    let name_sym = Symbol::intern(name);
                    if !self.light_call_cache.contains_key(&name_sym) {
                        // Find the compiled_fns key for this function
                        for (key, func) in compiled_fns {
                            if std::ptr::eq(func, cf) {
                                self.light_call_cache
                                    .insert(name_sym, (*key, cf.fingerprint));
                                break;
                            }
                        }
                    }
                    let result = self.call_compiled_function_light(cf, &args, compiled_fns, name);
                    let result = result?;
                    return loan_env!(self, maybe_fetch_rw_proxy(result, true));
                }
                self.set_pending_call_arg_sources(arg_sources.clone());
                let pushed_dispatch = loan_env!(self, push_multi_dispatch_frame(name, &args));
                self.push_samewith_context(name, None, None);
                // Use the function's defining package so that lookups inside the
                // function body resolve against the correct namespace.
                let pkg = if let Some(cached_pkg) = self.cached_fn_package(name, args.len()) {
                    cached_pkg
                } else {
                    let resolved_def = loan_env!(self, resolve_function_with_types(name, &args));
                    if let Some(ref def) = resolved_def {
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args)
                            .or_else(|| self.pending_callsite_line());
                        loan_env!(self, check_deprecation_for_def_with_line(def, cl));
                    }
                    resolved_def
                        .map(|def| def.package.resolve())
                        .unwrap_or_else(|| self.current_package().to_string())
                };
                let cf_auto_fetch = !cf.is_raw;
                let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, name);
                self.set_pending_call_arg_sources(None);
                self.pop_samewith_context();
                if pushed_dispatch {
                    self.pop_multi_dispatch();
                }
                // Slice 6.3 step 2: no blanket mark. call_compiled_function_named
                // now signals env_dirty precisely — its return merge sets it when a
                // captured-outer / `is rw` writeback (or an `is raw` return) actually
                // wrote a caller-aliasing value. A pure heavy-signature call (default
                // param, return type, where-constraint) no longer forces a per-call
                // O(caller-locals) pull.
                let result = result?;
                loan_env!(self, maybe_fetch_rw_proxy(result, cf_auto_fetch))
            } else {
                // Interpreter / native fallback paths route through the
                // tree-walking interpreter, which can mutate the shared env by
                // name (globals, dynamic vars, captured-outer writes). Mark env
                // dirty so the caller re-syncs its locals. The compiled fast
                // paths above instead rely on their own scoped-overlay merge to
                // signal env_dirty only when a captured-outer write happened, so
                // a pure compiled call no longer forces a per-call locals pull.
                if self.has_proto_cached(name)
                    && let Some(def) = self.vm_resolve_trivial_proto_candidate(name, &args)
                {
                    // VM-native proto dispatch (ledger §D, multi-dispatch VM-ization):
                    // a trivial-body proto (`proto foo {*}` / bodyless) resolves its
                    // winning multi candidate via the VM-owned registry (phase ②) and
                    // runs it as compiled bytecode, bypassing the tree-walk proto body
                    // + `__PROTO_DISPATCH__` round-trip and the candidate body's own
                    // `run_block`. Non-trivial proto bodies, unresolved/ambiguous
                    // candidates, and non-OTF-compilable candidates return None from
                    // the resolver above and fall through to the interpreter, which
                    // produces the proper dispatch result / X::Multi::NoMatch error.
                    // `nextsame`/`callsame`/`callwith`/`samewith` from the selected
                    // candidate still work because compile_and_call_function_def pushes
                    // the same multi-dispatch + samewith frames the interpreter would.
                    let is_raw = def.is_raw;
                    let result = self.compile_and_call_function_def(&def, args, compiled_fns)?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, !is_raw))
                } else if self.has_proto_cached(name)
                    && let Some(result) =
                        self.vm_try_run_nontrivial_proto_body(name, args.clone(), compiled_fns)
                {
                    // VM-native non-trivial proto body (ledger §D): a proto with a
                    // real body (`proto foo($x) { say "x"; {*} }`) runs that body as
                    // compiled bytecode instead of tree-walking it. The `{*}` inside
                    // still redispatches to the winning multi candidate through the
                    // existing proto-dispatch handler. Non-OTF-eligible protos return
                    // None and fall through to the interpreter unchanged.
                    result
                } else if self.has_multi_candidates_cached(name) && !self.has_proto_cached(name) {
                    // User-defined multi candidates take priority over builtins.
                    // Resolve the winning candidate Interpreter-side via the same resolver
                    // call_function_fallback uses (③ PR-3, ledger §2). When the
                    // winner is unambiguous and OTF-compilable, run it as compiled
                    // bytecode instead of tree-walking through the interpreter.
                    // For functions, ambiguity is signalled by returning None +
                    // a pending_dispatch_error (dispatch.rs choose_best_matching_
                    // candidate), so a Some(def) here is already an unambiguous
                    // winner. Clear any stale pending error first (mirrors
                    // resolve_function_with_alias) so a prior call's ambiguity
                    // can't leak. Non-otf-compilable (where/default/code-param) and
                    // no-match/ambiguous all fall through to call_function_fallback,
                    // which re-resolves and raises X::Multi::Ambiguous / NoMatch.
                    // The selected candidate's own redispatch (`nextsame`/`callsame`/
                    // `callwith`) still works because compile_and_call_function_def
                    // pushes the same multi-dispatch frame the interpreter would.
                    // Skip names the interpreter must handle natively even when a
                    // multi candidate is registered for them: native Test routines
                    // (is-eqv/is-deeply/…) register multi stubs but are implemented
                    // in Rust, so OTF-compiling the stub bypasses the native handler
                    // and corrupts behaviour (regressed S16-io/words.t,
                    // S32-io/slurp.t via is-eqv). Mirrors the non-builtin OTF path's
                    // is_interpreter_handled_function gate below.
                    let _ = self.take_pending_dispatch_error();
                    if !self.is_interpreter_handled_function(name)
                        // Sound multi-function resolution cache: for a type+arity-
                        // deterministic multi this returns the winner without the
                        // per-call registry walk + candidate match/rank/dedup;
                        // value-dependent / un-keyable / ambiguous calls resolve
                        // fresh (byte-identical to `resolve_function_with_types`).
                        && let Some(def) = loan_env!(self, resolve_function_multi_cached(name, &args))
                        // A genuine multi candidate: the name is multi-cached, so
                        // `compile_and_call_function_def` never name-caches this
                        // candidate — a default param is safe here (unlike the
                        // single/builtin-shadow paths). See
                        // `def_is_otf_compilable_multi_candidate`.
                        && Self::def_is_otf_compilable_multi_candidate(&def)
                    {
                        let is_raw = def.is_raw;
                        let result =
                            self.compile_and_call_function_def(&def, args, compiled_fns)?;
                        loan_env!(self, maybe_fetch_rw_proxy(result, !is_raw))
                    } else {
                        crate::vm::vm_stats::record_function_fallback(name);
                        self.set_pending_call_arg_sources(arg_sources);
                        let result = self.vm_call_function_fallback(name, &args);
                        self.set_pending_call_arg_sources(None);
                        let result = result?;
                        loan_env!(self, maybe_fetch_rw_proxy(result, true))
                    }
                } else if loan_env!(self, user_function_matches_call(name, &args)) {
                    // A user-defined sub shadows a same-named builtin (③ PR-2). When
                    // the resolved def is a plain single candidate that is
                    // OTF-compilable, run it as compiled bytecode — but resolve it
                    // explicitly and DO NOT fall through to the native arm below
                    // (which would pick the shadowed builtin). proto / multi cases
                    // (this branch is reached by proto'd multis, since the non-proto
                    // multi fork above did not fire) must keep going through
                    // call_function_fallback so candidate dispatch stays correct;
                    // complex-bodied / complex-signature shadows likewise tree-walk.
                    //
                    // Restrict the OTF takeover to genuine builtin shadows: this
                    // branch is also reached by ordinary module/dynamic user subs
                    // (not in compiled_fns) whose args strictly match, and
                    // def_is_otf_compilable does not catch every construct that needs
                    // the interpreter (e.g. a nested `sub` whose `when` control flow
                    // must not escape the enclosing routine — Test::Util's
                    // is-deeply-junction). Those keep tree-walking unchanged.
                    if !self.has_proto_cached(name)
                        && !self.has_multi_candidates_cached(name)
                        && let Some(def) = loan_env!(self, resolve_function_with_types(name, &args))
                    {
                        let is_builtin = crate::runtime::Interpreter::is_builtin_function(name);
                        // Prefer the cross-thread shared captured body for a
                        // `state`-bearing module sub (compiled_fns expansion): one
                        // shared body across threads keeps its `state` cell shared,
                        // which the per-call OTF recompile below cannot.
                        if !is_builtin && let Some(shared) = self.imported_state_body_for_def(&def)
                        {
                            let pkg = self.current_package().to_string();
                            let result = self.call_shared_state_body(
                                &shared,
                                args,
                                compiled_fns,
                                &pkg,
                                name,
                            )?;
                            return loan_env!(self, maybe_fetch_rw_proxy(result, !shared.is_raw));
                        }
                        let gate_ok = if is_builtin {
                            // Genuine builtin shadow: strict gate (no default —
                            // name-cache pollution hazard, PR #3546).
                            Self::def_is_otf_compilable(&def)
                        } else {
                            // Non-builtin module/dynamic single sub: defaults are
                            // name-cache-safe here (no builtin to mis-bind), but
                            // interpreter-coupled bodies/signatures stay excluded.
                            Self::def_is_otf_compilable_module_single(&def)
                        };
                        if gate_ok {
                            let is_raw = def.is_raw;
                            let result =
                                self.compile_and_call_function_def(&def, args, compiled_fns)?;
                            return loan_env!(self, maybe_fetch_rw_proxy(result, !is_raw));
                        }
                    }
                    crate::vm::vm_stats::record_function_fallback(name);
                    self.set_pending_call_arg_sources(arg_sources);
                    let result = self.vm_call_function_fallback(name, &args);
                    self.set_pending_call_arg_sources(None);
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, true))
                } else if let Some(native_result) =
                    self.try_native_function(Symbol::intern(name), &args)
                {
                    native_result
                } else if !self.is_interpreter_handled_function(name)
                && !self.has_multi_candidates_cached(name)
                && let Some(def) = loan_env!(self, resolve_function_with_types(name, &args))
                // Only OTF-compile simple functions: no default params, no
                // code params (&foo), no where constraints, no closures.
                && Self::def_is_otf_compilable(&def)
                {
                    let is_raw = def.is_raw;
                    let result = self.compile_and_call_function_def(&def, args, compiled_fns)?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, !is_raw))
                } else if let Some(result) = self.try_native_test_function(name, &args) {
                    // Dispatch Test functions straight to their typed handler (lever A).
                    result
                } else if let Some(result) = self.try_nativecast(name, &args) {
                    // NativeCall's `nativecast($target-type, $source)` helper.
                    result
                } else if let Some(result) = self.try_nativesizeof(name, &args) {
                    // NativeCall's `nativesizeof($obj-or-type)` helper.
                    result
                } else if let Some(result) = self.try_cglobal_fetch(name, &args) {
                    // One fetch behind the `Proxy` NativeCall's `cglobal`
                    // returns (see runtime::nativecall_global).
                    result
                } else if let Some(result) = self.try_explicitly_manage(name, &args) {
                    // The leak behind NativeCall's `explicitly-manage`
                    // (see runtime::nativecall_manage).
                    result
                } else if let Some(result) = self.try_trait_mod_does_apply(name, &args) {
                    // The mixin + writeback behind the `trait_mod:<does>`
                    // prelude candidates (see vm::vm_trait_mod_does_ops).
                    result
                } else if let Some(result) = self.try_native_json_function(name, &args) {
                    // Dispatch JSON::Fast / JSON::Tiny `to-json` / `from-json`
                    // to the native implementation (runtime/json.rs).
                    result
                } else if let Some(callable) = self.lexical_amp_var_callable(Some(code), name) {
                    // Pure lexical `&name` callable (a `&code` parameter or
                    // `my &f = ...` with no same-named package sub): dispatch
                    // Interpreter-natively via vm_call_on_value instead of the interpreter
                    // terminal (Track A, ledger §2). Builtin priority is preserved
                    // because try_native_function already ran above. Dynamic vars
                    // (`my $*ERR` in the caller) stay visible because
                    // call_compiled_closure roots the closure frame at the live
                    // caller env (scoped_child) and the captured-env merge is
                    // or_insert (parent-chain aware), so it never shadows them.
                    self.vm_call_on_value(callable, args, Some(compiled_fns))
                } else if let Some(result) = self.try_native_io_function(name, &args) {
                    // File/FS builtin function (`slurp`/`open`/`unlink`/…). Every
                    // user-sub resolution path (compiled_fns / multi / user_function_
                    // matches / OTF) was tried above, so a user `sub slurp` still wins;
                    // reaching here means the builtin operating on the VM-owned
                    // io_handles store + filesystem. Dispatch it natively instead of
                    // recording a tree-walk fallback (§D state ownership ③, function
                    // forms). The `builtin_*` impls are exactly what call_function
                    // routes to (no arg-sources: FS routines have no rw params) =>
                    // byte-identical.
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, true))
                } else if let Some(result) = self.try_native_collection_function(name, &args) {
                    // Pure list/coercion builtin function (`val`/`list`/`slip`/`hash`).
                    // User subs resolved above, so this is the builtin — dispatch
                    // natively instead of the tree-walk fallback (§D(b) dispatch
                    // chain). Same builtin_* impls as call_function => byte-identical.
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, true))
                } else if let Some(op) = name
                    .strip_prefix("infix:<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    // Builtin operator-as-function `infix:<op>(...)` (what `&infix:<+>`,
                    // `[+]`, hyper and `reduce` lower to). Every user-defined operator
                    // path (compiled_fns / multi / user_function_matches / OTF) was
                    // tried above, so reaching here means the builtin operator —
                    // dispatch it straight to the native `call_infix_routine` handler
                    // instead of recording a tree-walk fallback. This mirrors
                    // `call_function_fallback`'s infix arm exactly (the big
                    // `call_function` match has no infix arm, so both reach the same
                    // `call_infix_routine` on the same `self`), with the same
                    // arg-sources + rw-proxy handling => byte-identical. §D state
                    // ownership: the operator handlers are native Rust on VM state.
                    self.set_pending_call_arg_sources(arg_sources);
                    let result = self.call_infix_routine(Self::normalize_unicode_infix(op), &args);
                    self.set_pending_call_arg_sources(None);
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, true))
                } else {
                    // Sync Interpreter locals to env before spawning threads so closures capture them
                    if name == "start" {
                        self.sync_env_from_locals_needed(code);
                    }
                    // EVAL/EVALFILE compile to bytecode and run on a sub-Interpreter, and
                    // pseudo-package reads are reflective env lookups: the
                    // interpreter is a carrier here, not a tree-walk fallback.
                    // CARRIER (is_interpreter_carrier_function) vs TODO: compile to
                    // bytecode (else branch = true tree-walk function fallback). The
                    // record_* split already tracks this at runtime. See ledger §2/§C.
                    if Self::is_interpreter_carrier_function(name) {
                        crate::vm::vm_stats::record_function_carrier(name);
                    } else {
                        crate::vm::vm_stats::record_function_fallback(name);
                    }
                    self.set_pending_call_arg_sources(arg_sources);
                    // Carrier writeback (mirrors `exec_exec_call_op`): an
                    // interpreter carrier like `EVAL` writes caller lexicals
                    // into env BY NAME; drain those writes into this frame's
                    // slots so a subsequent slot read sees them. Previously
                    // masked by the Nil-slot env fallback: an uninitialized
                    // caller scalar's slot stayed Nil, so reads fell back to
                    // env. With the Any seed (PLAN 8.5 step 3) the slot holds
                    // a real value, so the writeback must be explicit
                    // (t/require-expression.t `BEGIN try EVAL`).
                    let reg_gen_before = self.registry_write_generation();
                    let carrier_saved = self.begin_carrier();
                    let result = self.vm_call_function(name, args);
                    let written = self.end_carrier(carrier_saved);
                    self.writeback_carrier_writes(code, &written);
                    self.set_pending_call_arg_sources(None);
                    // Interpreter function calls (e.g. `require`) may register
                    // new subs — invalidate function resolution caches. Only
                    // when the call actually acquired a registry write guard,
                    // though: a blanket bump here cleared the name-keyed caches
                    // after EVERY interpreter-native builtin call (`make`, in a
                    // grammar-action walk, is one per action), forcing a full
                    // registry rescan per call.
                    if self.registry_write_generation() != reg_gen_before {
                        self.fn_resolve_gen += 1;
                    }
                    // substr-rw returns a Proxy that must be preserved (not auto-FETCHed)
                    let auto_fetch = name != "substr-rw";
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, auto_fetch))
                }
            }
        }
    }

    /// Whether a resolved `FunctionDef` is simple enough to compile on-the-fly to
    /// bytecode and run via the Interpreter (instead of tree-walking it through the
    /// interpreter): a plain body and no default/where/code-signature/`&`-code
    /// params. Shared by the non-shadow OTF branch and the builtin-shadow forks
    /// (③ PR-2) so the same compilability gate is applied consistently.
    /// Resolve the winning multi candidate for a *trivial-body* proto so the VM
    /// can run it as compiled bytecode instead of falling back to the tree-walk
    /// proto body + `__PROTO_DISPATCH__` round-trip (ledger §D, multi-dispatch
    /// VM-ization). Returns `None` — leaving the existing interpreter fallback in
    /// place — whenever the bypass would not be byte-identical:
    ///
    /// - the name is interpreter-handled (e.g. a proto'd native Test routine);
    /// - the proto has a *non-trivial* body (statements around `{*}`), which must
    ///   still run;
    /// - no candidate resolves, or resolution is ambiguous (the interpreter then
    ///   raises the proper `X::Multi::NoMatch` / `X::Multi::Ambiguous`);
    /// - the winning candidate is not OTF-compilable, or declares `state` (whose
    ///   shared-cell identity the OTF body-fingerprint cache cannot preserve).
    pub(super) fn vm_resolve_trivial_proto_candidate(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<crate::ast::FunctionDef> {
        use crate::ast::{Expr, Stmt};
        if self.is_interpreter_handled_function(name) {
            return None;
        }
        let proto = self.resolve_proto_function(name)?;
        // A trivial proto body is empty (bodyless proto) or exactly `{*}` — only
        // then is bypassing the body safe. `{*}` parses to `Stmt::Expr(Whatever)`.
        // The compiler prepends line-tracking `SetLine` markers (no runtime
        // effect on dispatch), so ignore those when judging triviality.
        let significant: Vec<&Stmt> = proto
            .body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        let trivial = significant.is_empty()
            || (significant.len() == 1 && matches!(significant[0], Stmt::Expr(Expr::Whatever)));
        if !trivial {
            return None;
        }
        // The proto's OWN signature is a gate: `proto f(Int $x) {*}` rejects a
        // `Str` arg even when a candidate (`multi f($)`) would accept it
        // (S06-multi/proto.t: "proto signature is checked"). Bypassing the body
        // would skip that check, so only proceed when the args satisfy the proto
        // signature; otherwise fall back so the interpreter raises the proper
        // X::TypeCheck::Argument. An empty proto signature accepts anything.
        if !proto.param_defs.is_empty() && !self.method_args_match(args, &proto.param_defs) {
            return None;
        }
        // Ambiguity is signalled by `None` + a pending dispatch error; clear any
        // stale one first (mirrors the non-proto multi fork) so a prior call's
        // ambiguity can't leak into this resolution.
        let _ = self.take_pending_dispatch_error();
        let def = self.resolve_proto_candidate_with_types(name, args)?;
        if def.empty_sig && !args.is_empty() {
            return None;
        }
        // A trivial-proto candidate is a genuine multi candidate (same caching
        // profile in `compile_and_call_function_def` regardless of defaults), so a
        // default param is safe to OTF here — see
        // `def_is_otf_compilable_multi_candidate`.
        if !Self::def_is_otf_compilable_multi_candidate(&def) {
            return None;
        }
        Some(def)
    }

    /// Run a *non-trivial-body* proto (`proto foo($x) { say "x"; {*} }`) as
    /// compiled bytecode instead of tree-walking its body through the interpreter
    /// (ledger §D, multi-dispatch VM-ization). The proto body is rewritten so each
    /// `{*}` becomes a `__PROTO_DISPATCH__()` call, then compiled and run like any
    /// routine: its `{*}` redispatch still resolves and runs the winning multi
    /// candidate through the existing proto-dispatch handler (reached from the
    /// compiled body's `__PROTO_DISPATCH__` call), so behaviour — including the
    /// candidate's `nextsame`/`callsame` — is byte-identical to the interpreter.
    ///
    /// Returns `None` (leaving the interpreter fallback in place) whenever the
    /// bypass would not be safe / byte-identical:
    /// - the name is interpreter-handled, or has no proto / a *trivial* body
    ///   (handled by `vm_resolve_trivial_proto_candidate` instead);
    /// - the args do not satisfy the proto's own signature (the interpreter then
    ///   raises the proper `X::TypeCheck::Argument`);
    /// - the proto's signature or (rewritten) body is not OTF-compilable, or the
    ///   body declares `state` (whose shared-cell identity the fingerprint cache
    ///   cannot preserve).
    pub(super) fn vm_try_run_nontrivial_proto_body(
        &mut self,
        name: &str,
        args: Vec<Value>,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        use crate::ast::{Expr, Stmt};
        if self.is_interpreter_handled_function(name) {
            return None;
        }
        let proto = self.resolve_proto_function(name)?;
        // Only handle *non-trivial* bodies here; the trivial (bodyless / `{*}`-only)
        // case is the trivial resolver's job. A bodyless proto (`def.body.empty`)
        // dispatches implicitly and must not be compiled here.
        let significant: Vec<&Stmt> = proto
            .body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        let trivial = significant.is_empty()
            || (significant.len() == 1 && matches!(significant[0], Stmt::Expr(Expr::Whatever)));
        if trivial {
            return None;
        }
        // The proto's OWN signature is a gate (same as the trivial path): bypassing
        // to compiled code must still reject args the proto signature forbids.
        if !proto.param_defs.is_empty() && !self.method_args_match(&args, &proto.param_defs) {
            return None;
        }
        // ADR-0019 C8: a plan-derived proto already carries the bytecode for
        // its `{*}`-rewritten body, compiled once at declaration time — run
        // it directly instead of rewriting and OTF-compiling the AST on
        // every call. `state` in the proto body (the caching-proto pattern
        // `proto cached($a) { state %cache; ... {*} }`) is safe here: the
        // compiled routine is one fixed bytecode object shared by every call
        // (not re-derived per call), so its `state` cell's identity — keyed
        // by compiled opcode position — is as stable as any ordinary
        // routine's. A proto declared with signature *alternates* shares one
        // `state` cell across them the same way an ordinary multi does
        // (`t/multi-signature-alternates.t`).
        let (cf, pkg) = if let Some(compiled) = proto.compiled.clone() {
            (compiled, proto.package.resolve())
        } else {
            // Fallback for a proto with no plan-compiled body — defensive; every
            // non-trivial package proto sub is plan-derived once C8 is complete,
            // but this keeps the OTF-compile path available for any def built
            // outside declaration-plan registration (e.g. a hand-built
            // `FunctionDef`). Rewrite `{*}` -> `__PROTO_DISPATCH__()` and
            // require the resulting body + the proto's own signature to be
            // OTF-compilable.
            let rewritten = crate::runtime::Interpreter::rewrite_proto_dispatch_stmts(&proto.body);
            let mut proto_def = proto.clone();
            proto_def.body = rewritten;
            // The clone carried the ORIGINAL body's memoized identity; the rewrite
            // gave this def a different body, so drop it.
            proto_def.invalidate_body_fingerprint();
            if !Self::def_is_otf_compilable(&proto_def) {
                return None;
            }
            let cf = self.otf_compile_function_def(&proto_def);
            let pkg = proto_def.package.resolve();
            (cf, pkg)
        };
        // `{*}` redispatch reads the args from `proto_dispatch_stack` (the
        // ORIGINAL proto args, matching the interpreter's
        // `call_proto_function`), so push that before the body runs and pop
        // after. No multi-dispatch frame is pushed for the proto body itself
        // — it is the dispatcher, not a candidate; the candidate's own
        // `nextsame` frame is set up by the proto-dispatch handler when `{*}`
        // runs.
        self.push_proto_dispatch_frame(name.to_string(), args.clone());
        // Prefer the proto body's own nested-sub table over the caller's
        // (ADR-0019 C6e-3c, mirrors `call_shared_state_body`): a proto body
        // that declares its own nested sub/multi/proto must resolve its own
        // `RegisterDecl` keys, not the caller's unrelated table.
        let fns = cf.compiled_fns.as_deref().unwrap_or(compiled_fns);
        let result = self.call_compiled_function_named(&cf, args, fns, &pkg, name);
        self.pop_proto_dispatch_frame();
        Some(result)
    }

    /// VM-native `{*}` redispatch (ledger §D, multi-dispatch VM-ization step ②③):
    /// reached when a compiled proto body executes its rewritten `__PROTO_DISPATCH__()`
    /// call. Resolves the winning multi candidate from the proto-dispatch frame's
    /// original args and runs it as compiled bytecode via
    /// `compile_and_call_function_def` — the same path the trivial-proto fork uses —
    /// so the candidate body and its `nextsame`/`callsame`/`samewith` redispatch all
    /// run VM-natively instead of tree-walking through interpreter
    /// `call_proto_dispatch` and `run_block`. (The `is rw` writeback *through* a
    /// non-trivial proto body is a separate pre-existing gap — the proto-dispatch
    /// frame carries the proto's original args, not the caller's containers — and is
    /// unchanged here; it fails identically on the interpreter path.)
    ///
    /// Falls back to the interpreter's `call_proto_dispatch` (which owns the full
    /// `X::Multi::NoMatch` / `X::Multi::Ambiguous` reporting, `proto method`
    /// invocant handling, and the tree-walk `run_block`) whenever a VM-native run
    /// would not be byte-identical: a `proto method` `{*}` (invocant context), no
    /// resolvable / ambiguous candidate, an empty-sig candidate called with args, or
    /// a non-OTF-compilable / `state`-declaring candidate.
    pub(super) fn vm_call_proto_dispatch(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        let Some((proto_name, args, method_ctx)) = self.proto_dispatch_last() else {
            // `{*}` outside a proto — let the interpreter raise the proper error.
            return self.loan_env_for(|i| i.call_proto_dispatch());
        };
        // `proto method` redispatch needs the invocant + the boundary-resolved
        // dispatch (ADR-0019 E9c-2) the interpreter owns; only proto *subs* run
        // compiled here.
        if method_ctx.is_some() {
            return self.loan_env_for(|i| i.call_proto_dispatch());
        }
        // Clear any stale pending dispatch error (mirrors the trivial-proto fork)
        // so a prior call's ambiguity can't leak into this resolution.
        let _ = self.take_pending_dispatch_error();
        // `{*}` rw-redispatch (ledger §D): when the proto declares a scalar
        // `is rw`/`is raw` parameter, Rakudo redispatches `{*}` using the proto's
        // CURRENT (body-mutated) parameter, so a candidate's own rw write chains
        // back through the proto parameter to the caller. Rebuild the args from
        // the proto's current parameter values (live body locals) and pass
        // arg_sources naming the proto params, so the candidate's writeback lands
        // in the proto frame and the proto's own rw binding propagates it to the
        // caller at proto exit. `None` => unchanged (the common non-rw case).
        let (args, rw_arg_sources) = match self
            .resolve_proto_function(&proto_name)
            .and_then(|proto| self.proto_rw_redispatch_args(&proto.param_defs, &args, Some(code)))
        {
            Some((rebuilt, sources)) => (rebuilt, Some(sources)),
            None => (args, None),
        };
        // For rw redispatch the rebuilt args are plain values; a candidate's
        // `is rw` param requires a *writable* argument, which the multi-dispatch
        // writability check satisfies from `pending_call_arg_sources` (a named
        // source is as good as a VarRef). Set it before resolving so the rw
        // candidate matches, and clear it again on any interpreter fallthrough.
        let had_rw_sources = rw_arg_sources.is_some();
        if had_rw_sources {
            self.set_pending_call_arg_sources(rw_arg_sources);
        }
        if let Some(def) = self.resolve_proto_candidate_with_types(&proto_name, &args)
            && (!def.empty_sig || args.is_empty())
            // A proto candidate is a genuine multi candidate: its caching profile
            // in `compile_and_call_function_def` is identical with or without a
            // default param (caching keys on `has_multi_candidates_cached`, not on
            // defaults), so default-bearing candidates are as safe to OTF here as
            // the non-default ones already are. Permit defaults (see
            // `def_is_otf_compilable_multi_candidate`).
            && Self::def_is_otf_compilable_multi_candidate(&def)
        {
            // pending_call_arg_sources is still set (resolution only reads it);
            // `compile_and_call_function_def`'s bind consumes it for the rw chain.
            let result = self.compile_and_call_function_def(&def, args, compiled_fns);
            if had_rw_sources {
                self.set_pending_call_arg_sources(None);
            }
            return result;
        }
        // No candidate / ambiguous / empty-sig-with-args / non-OTF / state: the
        // interpreter re-resolves and produces the exact error or tree-walk result.
        if had_rw_sources {
            self.set_pending_call_arg_sources(None);
        }
        self.loan_env_for(|i| i.call_proto_dispatch())
    }

    /// `{*}` rw-redispatch helper (ledger §D, multi-dispatch VM-ization): Rakudo
    /// redispatches `{*}` using the proto's CURRENT parameter, so a candidate's
    /// own `is rw` write chains back through the (possibly body-mutated) proto
    /// parameter to the caller's container. mutsu instead passes the proto's
    /// entry-time args, so a candidate's rw write either targets the caller
    /// directly (colliding with the proto's own writeback) or is lost.
    ///
    /// When the proto declares a scalar `is rw`/`is raw` positional parameter and
    /// its signature is a simple all-positional one, return rebuilt args (read
    /// from the proto's current parameter values — the live body locals when
    /// `code` is given, else env) and arg_sources naming the proto params, so the
    /// candidate binds its rw param with `source = <proto param>` and its
    /// writeback lands in the proto frame. Returns `None` (no rebuild, current
    /// behavior) for protos without a scalar rw/raw param or with a non-simple
    /// signature.
    pub(crate) fn proto_rw_redispatch_args(
        &self,
        proto_param_defs: &[crate::ast::ParamDef],
        orig_args: &[Value],
        code: Option<&CompiledCode>,
    ) -> Option<(Vec<Value>, Vec<Option<String>>)> {
        // The fixed positional params: drop the invocant and the variadic /
        // named catch-alls (a `proto method` always carries an implicit `%_`),
        // so a simple positional signature still qualifies. We only rebuild when
        // these fixed params exactly consume the call's positional args.
        let positional: Vec<&crate::ast::ParamDef> = proto_param_defs
            .iter()
            .filter(|pd| {
                !pd.is_invocant
                    && !pd.named
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && !pd.onearg
                    && pd.sub_signature.is_none()
                    && pd.name != "%_"
                    && pd.name != "@_"
            })
            .collect();
        // All args must be positional, and the fixed positional params must
        // exactly consume them (no slurpy mopping up extras).
        if positional.len() != orig_args.len()
            || orig_args.iter().any(|a| {
                matches!(
                    crate::runtime::types::unwrap_varref_value(a.clone()).view(),
                    ValueView::Pair(..) | ValueView::ValuePair(..)
                )
            })
        {
            return None;
        }
        // A scalar param is stored sigil-less, so its name starts with an ASCII
        // letter / `_` (not `@`/`%`/`&`). Require at least one scalar rw/raw param
        // — the only case where the current value differs or a writeback link is
        // needed; `@`/`%` rw params propagate in-place by name already.
        let is_scalar_rw = |pd: &crate::ast::ParamDef| {
            pd.traits.iter().any(|t| t == "rw" || t == "raw")
                && pd.name != "_"
                && pd
                    .name
                    .as_bytes()
                    .first()
                    .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
        };
        if !positional.iter().any(|pd| is_scalar_rw(pd)) {
            return None;
        }
        let mut new_args = Vec::with_capacity(positional.len());
        let mut sources = Vec::with_capacity(positional.len());
        for (pd, orig) in positional.iter().zip(orig_args.iter()) {
            if is_scalar_rw(pd) {
                // Rebuild from the proto's CURRENT parameter value: a scalar rw
                // param is slot-only mid-body (not yet flushed to env), so read
                // the live body local slot first, then env, then the entry-time
                // arg as a last resort. arg_sources names the proto param so the
                // candidate's writeback chains back through it.
                let cur = code
                    .and_then(|c| c.locals.iter().position(|n| n == &pd.name))
                    .map(|slot| self.locals[slot].clone())
                    .or_else(|| self.env().get(&pd.name).cloned())
                    .unwrap_or_else(|| crate::runtime::types::unwrap_varref_value(orig.clone()));
                new_args.push(crate::runtime::types::unwrap_varref_value(cur));
                sources.push(Some(pd.name.clone()));
            } else {
                // Non-rw params keep their original argument (a VarRef / container
                // carries the caller's aliasing for `@`/`%`/`is raw`-by-name
                // params); rebuilding to a plain current value would sever it.
                new_args.push(orig.clone());
                sources.push(None);
            }
        }
        Some((new_args, sources))
    }

    pub(super) fn def_is_otf_compilable(def: &crate::ast::FunctionDef) -> bool {
        // `where` constraints are NOT excluded: the winning multi candidate is
        // resolved by `resolve_function_with_types` / `resolve_proto_candidate_with_types`,
        // which already evaluate `where` (via `args_match_param_types`) to pick
        // the winner, so the resolved def already satisfies its `where`. The
        // compiled binding path (`call_compiled_function_named` ->
        // `bind_function_args_values`) re-checks `where` and raises the same
        // `X::TypeCheck::Binding::Parameter` the interpreter would on failure
        // (for single candidates), and merges the `&name` Sub's captured env so
        // a `where` referencing closure variables resolves them — byte-identical
        // to the interpreter fallback (ledger §D, multi-dispatch VM-ization).
        //
        // A `&callback` parameter is also NOT excluded: it binds and is invoked
        // (`cb()`, `cb($x)`) exactly like any compiled local, including blocks,
        // `&name`-passed subs, and closures over outer lexicals. A `&cb` with an
        // explicit code signature (`&cb:(Int)`, `code_signature`) is now allowed
        // too: the winning candidate is picked by `resolve_function_with_types`
        // (which matches the callback's signature against `code_signature`), so the
        // resolved def already satisfies it and the compiled binding just binds the
        // callable — byte-identical to the interpreter. Any cross-candidate
        // ambiguity (`&c:(Int)` vs untyped `&c`) is a *resolution*-level gap that
        // fires identically with or without OTF, so it is unaffected. Only a param
        // with a default value stays excluded here (the name-cache-pollution /
        // builtin-shadow hazard, PR #3546 — allowed at genuine multi sites via
        // `def_is_otf_compilable_multi_candidate`).
        !Self::routine_body_facts(def).needs_interpreter
            && def.param_defs.iter().all(|pd| pd.default.is_none())
    }

    /// Like `def_is_otf_compilable`, but also permits a parameter with a default
    /// value. Safe ONLY at a genuine *multi*-candidate dispatch site (the
    /// `has_multi_candidates_cached` branch): there `compile_and_call_function_def`
    /// does NOT name-cache the compiled candidate (the name is multi-cached, so
    /// its `!has_multi_candidates_cached` guard is false), so a default-bearing
    /// candidate cannot pollute the name-keyed `otf_call_cache` and mis-bind a
    /// later same-named call — the builtin-shadow hazard that deferred the blanket
    /// default-OTF (PR #3546: Test::Util's `our sub run(Str, Str = '')` shadowing
    /// the `run` builtin). The compiled binding (`bind_function_args_values`)
    /// evaluates defaults exactly as the interpreter does, so this is
    /// byte-identical (ledger §D, multi-dispatch VM-ization). (For non-builtin
    /// single module/dynamic subs, the name-cache is also safe — see
    /// `def_is_otf_compilable_module_single`, which carries extra body/signature
    /// gates those subs need.)
    pub(super) fn def_is_otf_compilable_multi_candidate(def: &crate::ast::FunctionDef) -> bool {
        // `code_signature` params (`&cb:(Int)`) are allowed: the multi resolver
        // already picked this candidate by matching the callback's signature, so
        // the compiled binding only binds the callable (byte-identical). Defaults
        // are allowed because a multi site does not name-cache the candidate.
        !Self::routine_body_facts(def).needs_interpreter
    }

    /// The OTF-compilation gates' body predicates for `def`, computed once and
    /// memoized on the def itself (`FunctionDef::body_facts_cache`).
    ///
    /// Each predicate walks the whole body AST, and the gates are evaluated on
    /// every slow-path call to the routine, so recomputing them per call was pure
    /// repeat work over immutable data. This is also the single place that reads
    /// `def.body` for these facts, so ADR-0019 C6 can later feed them from the
    /// compiler by changing one function.
    pub(crate) fn routine_body_facts(
        def: &crate::ast::FunctionDef,
    ) -> crate::ast::RoutineBodyFacts {
        *def.body_facts_cache
            .get_or_init(|| crate::ast::RoutineBodyFacts {
                needs_interpreter: Self::function_body_needs_interpreter(&def.body),
                declares_state: Self::function_body_declares_state(&def.body),
                registration_identity: crate::ast::registration_identity_fingerprint(
                    &def.params,
                    &def.param_defs,
                    &def.body,
                ),
            })
    }

    /// Check if a function body contains constructs that require the full
    /// interpreter path — now only a top-level `class`/`role` declaration.
    ///
    /// A `start { … }` in the body used to force the interpreter here too, on
    /// the theory that a spawned block needs the tree-walk path "for proper
    /// thread spawning". That exclusion is gone (2026-08-22): `start` compiles
    /// like any other call. Three facts made it indefensible:
    ///
    ///   - The single largest dispatch arm never consulted it. Ordinary module/
    ///     dynamic single subs are gated by `def_is_otf_compilable_module_single`,
    ///     which has admitted `start`-containing bodies since ADR-0019 C6e-2c —
    ///     the compiled caller-env merge excludes the callee's own params
    ///     (`routine_writeback_excluded_names`), so a recursive sub's param
    ///     re-bind can no longer clobber a spawned closure's capture
    ///     (t/start-body-param-compiled.t). Only the *multi* candidate, proto
    ///     and builtin-shadow gates still saw this predicate, so identical
    ///     bodies compiled or tree-walked purely by declaration form.
    ///   - It only ever caught `start` in expression-statement position (or
    ///     inside a call/method-call argument). `my $p = start { … };` is a
    ///     `Stmt::VarDecl`, which this walk does not descend into at all, so the
    ///     most common way to write a `start` block already compiled.
    ///   - Removing it is behaviour-preserving in practice: the recursive
    ///     multi/proto shapes the gate was protecting (fib, fan-out, a `Str`
    ///     param read after an `await`) produce raku-identical results on the
    ///     compiled path — pinned by t/start-multi-candidate-compiled.t.
    pub(crate) fn function_body_needs_interpreter(body: &[crate::ast::Stmt]) -> bool {
        use crate::ast::Stmt;
        for stmt in body {
            match stmt {
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => return true,
                Stmt::Expr(expr) if Self::expr_needs_interpreter(expr) => return true,
                _ => {}
            }
        }
        false
    }

    /// Recurse into expression positions that can *host statements* (a `do`
    /// statement, a bare block, or either as a call/method-call argument), so a
    /// `class`/`role` declaration nested there is still seen. No expression is
    /// itself interpreter-coupled any more.
    fn expr_needs_interpreter(expr: &crate::ast::Expr) -> bool {
        use crate::ast::Expr;
        match expr {
            Expr::DoStmt(stmt) => Self::function_body_needs_interpreter(std::slice::from_ref(stmt)),
            Expr::Block(body) => Self::function_body_needs_interpreter(body),
            Expr::MethodCall { target, args, .. } => {
                Self::expr_needs_interpreter(target)
                    || args.iter().any(Self::expr_needs_interpreter)
            }
            Expr::Call { args, .. } => args.iter().any(Self::expr_needs_interpreter),
            _ => false,
        }
    }

    /// Whether a *non-builtin* single module/dynamic sub (reached via the
    /// `user_function_matches_call` branch) is safe to OTF-compile to bytecode
    /// instead of tree-walking through `call_function_fallback`.
    ///
    /// A genuine builtin shadow is gated by `def_is_otf_compilable` (PR-2), but
    /// an ordinary module sub may contain interpreter-coupled constructs that
    /// `def_is_otf_compilable` does not catch, whose semantics are NOT preserved
    /// when the def is compiled standalone on-the-fly:
    ///   - a `state` variable shared across `start` threads (a routine's state
    ///     lives in a shared cell that a per-thread OTF recompile would sever —
    ///     t/concurrent-state-var; admitted via the cross-thread shared captured
    ///     body, `imported_state_body_for_def`).
    ///
    /// `start` bodies were excluded until ADR-0019 C6e-2c: a recursive sub whose
    /// start closure captured a param used to get its capture clobbered by the
    /// recursive call's param re-bind under OTF (t/start-block-return-value.t
    /// test 3). The compiled caller-env merge now excludes the callee's own
    /// params (`routine_writeback_excluded_names`), so each invocation's binding
    /// stays isolated from the thread env the closure reads — verified by A/B
    /// (full `t/` + all whitelisted S17/S07-hyperrace/integration roast files,
    /// zero failures; pinned by t/start-body-param-compiled.t). The *other*
    /// gates (`def_is_otf_compilable`, `def_is_otf_compilable_multi_candidate`)
    /// kept a blanket `start` exclusion until 2026-08-22, when it was dropped
    /// from `function_body_needs_interpreter` for the same reason — see that
    /// function's doc.
    ///
    /// A sigilless *scalar* (`\x`) param whose alias writeback crosses an `EVAL`
    /// boundary was also historically excluded (t/sigilless-params) — compiled-
    /// safe since C6e-2a.
    ///
    /// Formerly-excluded body constructs verified OTF-safe and now admitted
    /// (§3 fallback removal, 2026-07-11/12): nested sub/proto/token decls
    /// (non-local `when` control flow stays inside the nested routine —
    /// Test::Util's `is-deeply-junction`), `subtest`, `CATCH`/`CONTROL`
    /// handlers, phasers (ENTER/LEAVE), `once`, and nested class/role/grammar
    /// decls (same-named `my` classes stay distinct via `decl_id`; captured
    /// lexicals, inheritance, parameterized roles and grammar parsing all
    /// match raku). All run through the same VM ops the precompiled path
    /// uses; pinned by t/module-sub-otf-interpreter-constructs.t.
    ///
    /// `EVAL`/`EVALFILE` are also admitted (2026-07-12, after the #4435 EVAL
    /// CALLER-frame fix): `eval_eval_string` runs on `self` with the live
    /// `self.env`, so an EVAL inside an OTF-compiled body sees the same
    /// lexical scope and CALLER:: frame layout as under the tree-walk path —
    /// param/lexical reads and writes, nested sub declarations, CALLER::
    /// depth resolution, `$_`, module-private sibling calls, and CATCH around
    /// a dying EVAL all verified raku-identical OTF vs interpreter (pinned by
    /// t/module-sub-otf-interpreter-constructs.t).
    ///
    /// `is rw`/`is raw`/`is copy`/`is readonly`/`is required` params are NOW
    /// allowed (§2 multi-dispatch VM-ization): the compiled binding already
    /// honored them for builtin shadows, and the rw/raw caller writeback carries
    /// a compile-time caller slot (#4091). `is encoded(...)` (NativeCall
    /// marshalling) is allowed too (ADR-0019 C6e-3c): it is inert on both
    /// dispatch arms — nothing reads the trait for marshalling, since actual
    /// string encoding for a native call happens explicitly via `.encode(...)`
    /// in the prelude (`nativecall_manage.rs`), and the shared compiled binder
    /// (`bind_function_args_values`) only branches on `rw`/`raw`/`copy`/
    /// `invocant`. A genuine `is native(...)` sub never reaches this gate at
    /// all — `native_call_specs` is checked by name before body dispatch.
    ///
    /// Defaults ARE allowed (name-cache-safe: no same-named builtin to mis-bind,
    /// and a single candidate always resolves to this def). Bodies and
    /// signatures that pass this gate compile and run identically OTF vs
    /// precompiled (ledger §D, multi-dispatch VM-ization). Being too strict here
    /// is harmless — it just keeps the sub on the interpreter fallback.
    ///
    /// `is test-assertion` IS allowed: `call_compiled_function_named` pushes the
    /// test-assertion line context, so an assertion failing inside an OTF-compiled
    /// helper reports the same caller line the interpreter path would (whatever
    /// the parser stamped on the call's caller-line marker).
    /// The signature/body gates a module single sub must pass to be run as
    /// compiled bytecode, EXCLUDING the `state` check. Factored out of
    /// `def_is_otf_compilable_module_single` so the shared-captured-body path
    /// (`imported_state_body_for_def`) can admit a `state`-bearing module sub —
    /// which the per-call OTF path excludes (a per-thread recompile severs the
    /// shared `state` cell), but which the cross-thread shared captured body
    /// handles correctly.
    pub(crate) fn def_module_single_sig_body_ok_ignoring_state(
        def: &crate::ast::FunctionDef,
    ) -> bool {
        // Parameter shapes no longer gate compilation. ADR-0019 C6e-2a made a
        // sigilless *scalar* (`\x`) compiled-safe (the compiled return path
        // flushes its final value through the `__mutsu_sigilless_alias::`
        // chain before the caller-env merge — vm_call_named_inner — which
        // covers the EVAL-boundary caller-alias writeback that used to require
        // the interpreter arm; t/sigilless-params.t test 3). C6e-2b lifted the
        // sub-signature (destructuring) exclusion too: binding runs through
        // the shared `bind_function_args_values` on both arms, and the
        // destructured elements bind read-only, so the historical exclusion
        // reason (caller-alias writeback) never applied to them
        // (t/subsig-param-compiled.t). C6e-2c lifted the last *body* exclusion
        // (`start`-containing bodies — see the gate doc above). C6e-3c lifted
        // the last param-trait exclusion (`is encoded(...)`, see the gate doc
        // above), so no parameter shape or trait gates compilation anymore.
        def.param_defs.iter().all(|pd| {
            pd.traits.iter().all(|t| {
                matches!(
                    t.as_str(),
                    "copy" | "rw" | "raw" | "readonly" | "required" | "encoded"
                )
            })
        })
    }

    /// Return the shared captured body for a resolved module sub def IF routing it
    /// through that body (instead of a per-call OTF recompile) is both safe and
    /// beneficial. Currently narrowed to `state`-declaring module subs: the shared
    /// body — snapshotted into every thread's clone — lets `await (^N).map: { start
    /// f() }` accumulate `f`'s `state` into one cross-thread cell (the per-thread
    /// OTF path gives each thread its own body and its own cell). Non-`state` subs
    /// keep their existing OTF/tree-walk routing unchanged (zero blast radius). The
    /// body must already be captured (`imported_compiled_fns`) and must clear the
    /// same signature/body gate the OTF path requires (minus the `state` exclusion).
    pub(super) fn imported_state_body_for_def(
        &self,
        def: &crate::ast::FunctionDef,
    ) -> Option<std::sync::Arc<CompiledFunction>> {
        if self.imported_compiled_fns.is_empty()
            || !Self::routine_body_facts(def).declares_state
            || !Self::def_module_single_sig_body_ok_ignoring_state(def)
        {
            return None;
        }
        let fp = def.body_fingerprint();
        self.imported_compiled_fns.get(&fp).cloned()
    }

    /// Invoke a shared captured module-sub body (`imported_state_body_for_def`)
    /// with `state_scope_id` reset to `None` for the duration of the body. A named
    /// routine's `state` is scoped to the *routine* (one cell keyed by the body's
    /// compiled position), NOT to any enclosing closure instance. When such a sub
    /// is called from inside a `start { … }` block, the ambient `state_scope_id`
    /// is the enclosing closure's id; letting it leak into the body would append a
    /// per-closure `#c<id>` suffix to the `state` key that `normalize_state_key`
    /// does not strip, giving each thread (and the parent) a *distinct* cross-thread
    /// cell — the exact reason `await (^N).map: { start f() }` failed to accumulate.
    /// Resetting to `None` makes every caller reach the same normalized key, so the
    /// shared cell accumulates (matching how a same-unit compiled `sub` is called).
    pub(super) fn call_shared_state_body(
        &mut self,
        shared: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &CompiledFns,
        pkg: &str,
        name: &str,
    ) -> Result<Value, RuntimeError> {
        let saved_scope = self.state_scope_id.take();
        // Prefer the routine's own nested-sub table over the caller's
        // (ADR-0019 C6e-3c, mirrors `compile_and_call_function_def`): a
        // module sub with a nested declaration (e.g. a `proto ... {*}`
        // synthesizes a `state` var, routing it through this shared-body
        // path even with no user-written `state`) must resolve its own
        // `RegisterDecl` keys, not the caller's unrelated table.
        let fns = shared.compiled_fns.as_deref().unwrap_or(compiled_fns);
        let result = self.call_compiled_function_named(shared, args, fns, pkg, name);
        self.state_scope_id.set(saved_scope);
        result
    }

    pub(super) fn def_is_otf_compilable_module_single(def: &crate::ast::FunctionDef) -> bool {
        // §2 (multi-dispatch VM-ization): `is rw`/`is raw` no longer force the
        // interpreter fallback. The non-module gate (`def_is_otf_compilable`) already
        // OTF-compiles rw subs, and the rw-arg writeback now carries a compile-time
        // caller slot (#4091), so the compiled binding refreshes the caller variable
        // identically to the interpreter — including across an EVAL call boundary.
        //
        // Return types — plain, definite, subset AND coercion (`--> Foo:D()`)
        // — are all OTF-safe. The coercion exclusion was lifted 2026-07-12:
        // the compiled return path now drives the same COERCE dispatch the
        // interpreter does (custom COERCE multis, Nil/Failure passed as-is,
        // X::TypeCheck::Return / X::Coerce::Impossible on failure), verified
        // against raku with several coercion subs coexisting
        // (roast/S12-coercion/coercion-return.t + t/module-sub-otf-coercion-
        // return.t). Plain/definite returns already let Test::Util's
        // `make-temp-file`/`make-rand-path` (`--> IO::Path:D`, calling a
        // module-private sibling) OTF-compile (PR closure-env #3899/#3902
        // made module-level lexical + private-sibling reads work under OTF;
        // the chmod-IntStr allomorph fix unblocked S32-io/chdir.t).
        // Signature/body gates (shared with the cross-thread shared-body path):
        //   - parameter shapes no longer exclude: captures (`|c`) bind
        //     read-only, sigilless scalars (`\x`) are compiled-safe since
        //     ADR-0019 C6e-2a (the compiled return path flushes the alias
        //     chain before the caller-env merge, covering the EVAL-boundary
        //     writeback), and sub-signature destructuring since C6e-2b (shared
        //     binder, read-only elements);
        //   - standard binding-time traits (`is copy`/`is rw`/`is raw`/`is
        //     readonly`/`is required`) are OTF-safe (compiled binding honors them,
        //     rw/raw writeback carries the #4091 caller slot); NativeCall
        //     marshalling (`is encoded('utf8')`) is OTF-safe too since C6e-3c
        //     (the trait is inert on both dispatch arms).
        // Plus the `state` exclusion: a per-call OTF recompile would sever a
        // module sub's shared `state` cell (the cross-thread shared captured body,
        // `imported_state_body_for_def`, is the path that admits `state`).
        Self::def_module_single_sig_body_ok_ignoring_state(def)
            && !Self::routine_body_facts(def).declares_state
    }

    /// True if the body declares a `state` variable anywhere (recursing through
    /// nested blocks).
    pub(crate) fn function_body_declares_state(body: &[crate::ast::Stmt]) -> bool {
        body.iter().any(Self::stmt_declares_state)
    }

    fn stmt_declares_state(stmt: &crate::ast::Stmt) -> bool {
        use crate::ast::Stmt;
        match stmt {
            Stmt::VarDecl { is_state, .. } => *is_state,
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                Self::function_body_declares_state(then_branch)
                    || Self::function_body_declares_state(else_branch)
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::Loop { body, .. }
            | Stmt::Given { body, .. }
            | Stmt::When { body, .. }
            | Stmt::Whenever { body, .. }
            | Stmt::React { body, .. }
            | Stmt::Subtest { body, .. } => Self::function_body_declares_state(body),
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Default(body) => {
                Self::function_body_declares_state(body)
            }
            Stmt::Expr(e) => Self::expr_declares_state(e),
            _ => false,
        }
    }

    fn expr_declares_state(expr: &crate::ast::Expr) -> bool {
        use crate::ast::Expr;
        match expr {
            Expr::Block(body) => Self::function_body_declares_state(body),
            Expr::DoStmt(stmt) => Self::stmt_declares_state(stmt),
            _ => false,
        }
    }
}
