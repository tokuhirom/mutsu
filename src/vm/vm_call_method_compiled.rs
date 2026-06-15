use super::*;
use crate::ast::{CallArg, Expr, Stmt};

impl VM {
    fn try_eval_simple_protect_expr(
        &self,
        outer_code: &CompiledCode,
        expr: &Expr,
    ) -> Option<Value> {
        let local_value = |name: &str| {
            outer_code
                .locals
                .iter()
                .position(|candidate| candidate == name)
                .and_then(|slot| self.locals.get(slot).cloned())
        };
        match expr {
            Expr::Literal(value) => Some(value.clone()),
            // Decont via into_deref: a captured-mutated `$` scalar may be boxed
            // into a ContainerRef (see box_captured_lexicals), and this direct
            // locals/env read bypasses the GetLocal/GetGlobal chokepoint, so it
            // must deref here to avoid leaking a raw cell into the method
            // fast-path receiver/args.
            Expr::Var(name) => local_value(name)
                .or_else(|| self.get_env_with_main_alias(name))
                .map(Value::into_deref),
            Expr::ArrayVar(name) => {
                let sigiled = format!("@{name}");
                local_value(&sigiled).or_else(|| self.get_env_with_main_alias(&sigiled))
            }
            Expr::HashVar(name) => {
                let sigiled = format!("%{name}");
                local_value(&sigiled).or_else(|| self.get_env_with_main_alias(&sigiled))
            }
            _ => None,
        }
    }

    pub(super) fn try_exec_simple_shared_protect_block(
        &mut self,
        outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let Value::Sub(data) = code_val else {
            return Ok(None);
        };
        let [Stmt::Call { name, args }] = data.body.as_slice() else {
            return Ok(None);
        };
        let method = name.resolve();
        if method != "push" {
            return Ok(None);
        }
        let Some(CallArg::Positional(Expr::ArrayVar(target_name))) = args.first() else {
            return Ok(None);
        };
        let sigiled_target = format!("@{target_name}");
        if !self.interpreter.shared_vars_active
            || !matches!(
                self.interpreter.get_shared_var(&sigiled_target),
                Some(Value::Array(..))
            )
        {
            return Ok(None);
        }
        let mut values = Vec::with_capacity(args.len().saturating_sub(1));
        for arg in args.iter().skip(1) {
            let CallArg::Positional(expr) = arg else {
                return Ok(None);
            };
            let Some(value) = self.try_eval_simple_protect_expr(outer_code, expr) else {
                return Ok(None);
            };
            values.push(value);
        }
        if let Some(result) = loan_env!(
            self,
            push_to_existing_shared_array(&sigiled_target, values.clone())
        ) {
            return Ok(Some(result));
        }
        let Some(target_value) =
            self.try_eval_simple_protect_expr(outer_code, &Expr::ArrayVar(target_name.clone()))
        else {
            return Ok(None);
        };
        let result = loan_env!(
            self,
            push_to_shared_var(&sigiled_target, values, &target_value)
        );
        Ok(Some(result))
    }

    /// Try compiled method fast path; fall back to interpreter.
    pub(super) fn try_compiled_method_or_interpret(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Native default construction: `Foo.new(...)` for a simple user-defined
        // class is pure data assembly (named args + attribute defaults), so the
        // VM builds the instance directly instead of routing through the
        // interpreter's generic constructor dispatch (lever A: shrink method-call
        // interpreter fallback).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) = loan_env!(self, try_native_default_construct(*class_name, &args))
        {
            // Native construction is pure data assembly: it returns a fresh
            // instance and writes nothing to the caller env (Slice 6.3).
            self.method_dispatch_pure = true;
            return result;
        }
        // Native built-in construction: `Buf`/`Blob` (byte overlay), `utf8`/
        // `utf16` (code units), `Uni` (codepoints), `Version`/`Duration`/
        // `StrDistance`/`Stash`/empty-instance handles — pure data builds the VM
        // performs directly instead of routing through `dispatch_new`.
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                crate::runtime::Interpreter::try_native_builtin_construct(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native built-in *class* method (a pure type-object method other than
        // `.new`, e.g. `Instant.from-posix`) — built directly instead of routing
        // through the interpreter's class-method dispatch.
        if let Value::Package(class_name) = &target
            && let Some(result) = crate::runtime::Interpreter::try_native_builtin_class_method(
                *class_name,
                method,
                &args,
            )
        {
            self.method_dispatch_pure = true;
            return result;
        }
        if let Value::Instance { class_name, .. } = &target {
            let class = class_name.resolve();
            // VM-native pure-handle IO dispatch (PLAN.md ③ native IO PR-C/PR-D):
            // resolve `IO::Handle`'s state-only methods (close/tell/eof/seek/
            // opened/t and the Tier-1 setters/getters chomp/nl-out/out-buffer/
            // encoding/native-descriptor) in the VM through its own `io_handles`
            // handle, *before* the generic native-method fallback below would
            // otherwise pre-empt them. Returns `None` (falling through to that
            // fallback unchanged) for any receiver/method/args it cannot handle
            // natively, so this is behavior-invariant.
            if let Some(result) = self.try_native_io_handle_method(&target, method, &args) {
                return result;
            }
            // VM-native text output to a File+UTF8 `IO::Handle` (print/put/say/
            // print-nl): the write touches only handle state (PR-D Tier-2a).
            // Stdout/Stderr (need emit_output) and non-UTF8 File fall through.
            if let Some(result) = self.try_native_io_handle_output(&target, method, &args) {
                return result;
            }
            // VM-native raw byte output to a File `IO::Handle` (write/spurt):
            // raw file write, no buffering/encoding (PR-D Tier-2c). Stdout/Stderr
            // and a non-UTF8 spurt of a Str fall through.
            if let Some(result) = self.try_native_io_handle_byte_output(&target, method, &args) {
                return result;
            }
            // VM-native line read from a File+UTF8 `IO::Handle` (get): reads via
            // the handle's record reader (PR-D read side). ArgFiles/Stdin/non-UTF8
            // (which need @*ARGS / decode) fall through.
            if let Some(result) = self.try_native_io_handle_read(&target, method, &args) {
                return result;
            }
            if self.interpreter.is_native_method(&class, method) {
                // TODO: compile to bytecode — Instance native-method fork (ledger §1).
                crate::vm::vm_stats::record_method_fallback(method);
                return loan_env!(self, call_method_with_values(target, method, args));
            }
        }
        // CARRIER: MOP pseudo-methods (reflection; no bytecode form). See ledger §C.
        // Pseudo-methods must always go through the interpreter which handles
        // them specially — never intercept via the compiled fast path.
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            crate::vm::vm_stats::record_method_fallback(method);
            return loan_env!(self, call_method_with_values(target, method, args));
        }
        // Private method fast path: resolve private candidate and run compiled code
        // when caller context clearly allows direct dispatch.
        if method.starts_with('!') {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name {
                let resolved = loan_env!(self, resolve_private_method_for_vm(&cn, method, &args));
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self
                        .interpreter
                        .can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match &target {
                            Value::Instance { id, .. } => Some(*id),
                            _ => None,
                        };
                        let attrs_cell = match &target {
                            Value::Instance { attributes, .. } => Some(attributes.clone()),
                            _ => None,
                        };
                        let attributes = match &target {
                            Value::Instance { attributes, .. } => attributes.to_map(),
                            _ => std::collections::HashMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::Package(crate::symbol::Symbol::intern(&cn))
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = loan_env!(
                            self,
                            push_method_dispatch_frame(&cn, method, &args, invocant_for_dispatch,)
                        );
                        let invocant = Some(target);
                        let empty_fns = HashMap::new();
                        let method_result = self.call_compiled_method(
                            &cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            attributes,
                            args,
                            invocant,
                            &empty_fns,
                        );
                        if pushed_dispatch {
                            self.interpreter.pop_method_dispatch();
                        }
                        self.interpreter.pop_method_samewith_context();
                        let (result, new_attrs, attrs_adjusted) = method_result?;
                        if let Some(id) = target_id {
                            // Commit only a `:=`-adjusted snapshot: an unadjusted
                            // one equals the cell and the whole-map write would
                            // race with concurrent cell-CAS (lost updates).
                            if attrs_adjusted && let Some(cell) = &attrs_cell {
                                cell.commit_attrs(new_attrs.clone());
                            }
                            if !self.interpreter.in_lvalue_assignment
                                && let Value::Proxy { ref fetcher, .. } = result
                            {
                                // Without a `:=` adjustment the triple's map is
                                // the stale entry snapshot (the lazy reconcile
                                // skips the cell clone) — re-snapshot the live
                                // cell for the proxy fetcher.
                                let proxy_attrs = if !attrs_adjusted && let Some(cell) = &attrs_cell
                                {
                                    cell.to_map()
                                } else {
                                    new_attrs
                                };
                                return loan_env!(
                                    self,
                                    proxy_fetch(fetcher, None, &cn, &proxy_attrs, id)
                                );
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        // User-defined ^method (metamethod) dispatch:
        // Foo.^bar passes Foo as the first positional argument.
        if method.starts_with('^')
            && method.len() > 1
            && !crate::runtime::Interpreter::is_classhow_method(&method[1..])
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.interpreter.has_user_method(&cn, method)
            {
                let mut how_args = vec![target.clone()];
                how_args.extend(args);
                // CARRIER: user-defined ^metamethod dispatch (MOP). See ledger §C.
                crate::vm::vm_stats::record_method_fallback(method);
                return loan_env!(self, call_method_with_values(target, method, how_args));
            }
        }
        // Only attempt compiled path for Instance or Package targets
        let class_name = match &target {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        if let Some(ref cn) = class_name {
            let class_sym = crate::symbol::Symbol::intern(cn);
            let method_sym = crate::symbol::Symbol::intern(method);
            let cache_key = (class_sym, method_sym);

            // Fast method dispatch cache: skip wrap chain check, compiled_code
            // extraction, and param_def eligibility scans for known-fast methods.
            if !self.interpreter.has_any_wrap_chains()
                && let Some(entry) = self.fast_method_cache.get(&cache_key)
                && args.len() <= entry.positional_count
            {
                let needs_default_eval =
                    entry.has_defaults && args.len() < entry.positional_count && {
                        let mut pos = 0;
                        entry.method_def.param_defs.iter().any(|pd| {
                            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                                return false;
                            }
                            let result = pos >= args.len() && pd.default.is_some();
                            pos += 1;
                            result
                        })
                    };
                let has_attr_aliases = match &target {
                    Value::Instance { attributes, .. } => attributes
                        .as_map()
                        .keys()
                        .any(|k| k.starts_with(super::vm_method_dispatch::ATTR_ALIAS_META_PREFIX)),
                    _ => false,
                };
                if !needs_default_eval && !has_attr_aliases {
                    let owner_class = entry.owner_class.resolve();
                    let method_def = entry.method_def.clone();
                    let cc = entry.compiled_code.clone();
                    let can_skip_merge = entry.can_skip_merge;
                    return self.dispatch_compiled_method(
                        cn,
                        &owner_class,
                        method,
                        &method_def,
                        &cc,
                        target,
                        args,
                        Some(can_skip_merge),
                    );
                }
            }

            if let Some((owner_class, method_def)) = {
                // Monomorphic inline cache: single-entry check before HashMap.
                if let Some((cc, cm, ref co, ref cd)) = self.last_method_resolve
                    && cc == class_sym
                    && cm == method_sym
                    && !cd.is_multi
                {
                    Some((co.clone(), cd.clone()))
                } else {
                    let cached = self.method_resolve_cache.get(&cache_key).cloned();
                    let result: Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> =
                        if let Some(ref hit) = cached
                            && let Some((_, def)) = hit
                            && !def.is_multi
                        {
                            hit.clone()
                        } else {
                            let resolved = loan_env!(
                                self,
                                resolve_method_with_owner_invocant(cn, method, &args, &target)
                            );
                            let resolved_arc =
                                resolved.map(|(owner, def)| (owner, std::sync::Arc::new(def)));
                            if resolved_arc.as_ref().is_none_or(|(_, def)| !def.is_multi) {
                                self.method_resolve_cache
                                    .insert(cache_key, resolved_arc.clone());
                            }
                            resolved_arc
                        };
                    if let Some((ref owner, ref def)) = result
                        && !def.is_multi
                    {
                        self.last_method_resolve =
                            Some((class_sym, method_sym, owner.clone(), def.clone()));
                    }
                    result
                }
            } {
                if let Some(result) = self.check_method_wrap_chain(
                    cn,
                    &owner_class,
                    method,
                    &method_def,
                    &target,
                    &args,
                ) {
                    return result;
                }
                // Resolve to a method def that has compiled bytecode. Normally
                // `compiled_code` is already populated at class registration. A
                // few sites add methods after that compile pass (e.g.
                // `.^add_method`, ledger §1) and leave `compiled_code = None`;
                // for those, compile on demand here so the method runs as
                // bytecode instead of tree-walking through the interpreter.
                let compiled_def: Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> =
                    if method_def.compiled_code.is_some() {
                        Some((owner_class.clone(), method_def.clone()))
                    } else if !method_def.body.is_empty()
                        && let Some((owner, def)) = self.populate_uncompiled_method(
                            cn,
                            &owner_class,
                            method,
                            &args,
                            &target,
                        )
                    {
                        // Refresh the resolve caches so future calls take the
                        // already-compiled fast path without re-resolving.
                        self.method_resolve_cache
                            .insert(cache_key, Some((owner.clone(), def.clone())));
                        if !def.is_multi {
                            self.last_method_resolve =
                                Some((class_sym, method_sym, owner.clone(), def.clone()));
                        }
                        Some((owner, def))
                    } else {
                        None
                    };
                if let Some((owner_class, method_def)) = compiled_def {
                    let cc = method_def.compiled_code.clone().expect("compiled_code set");

                    // Try to populate fast_method_cache for future calls
                    if !method_def.is_multi {
                        self.try_populate_fast_cache(cache_key, cn, &owner_class, &method_def, &cc);
                    }

                    return self.dispatch_compiled_method(
                        cn,
                        &owner_class,
                        method,
                        &method_def,
                        &cc,
                        target,
                        args,
                        None,
                    );
                }
            }
        }
        // Native `.map` / `.grep` over a concrete array with a simple block: run
        // the iteration loop in the VM instead of the interpreter (lever A). No
        // `target_name` here (the receiver is a value, not a mutable variable),
        // so `$_`-mutating blocks fall back to the interpreter.
        if let Some(result) = self.try_native_array_map(None, &target, method, &args) {
            return result;
        }
        // Native `.subst` over a Str with a simple pattern/replacement (lever A).
        if let Some(result) = self.try_native_subst(&target, method, &args) {
            return result;
        }
        // Native `.sort` over a plain array with no/simple comparator (lever A).
        if let Some(result) = self.try_native_sort(&target, method, &args) {
            return result;
        }
        // Native `.min` / `.max` over a plain list, including `:by` blocks.
        if let Some(result) = self.try_native_extrema(&target, method, &args) {
            return result;
        }
        // Native `.minmax` over a plain list, including `:by` blocks.
        if let Some(result) = self.try_native_minmax(&target, method, &args) {
            return result;
        }
        // Native `.first` over a plain list (no-adverb forms), including blocks.
        if let Some(result) = self.try_native_first(&target, method, &args) {
            return result;
        }
        // Native QuantHash coercion `.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`
        // over a list-like receiver (ledger §1: native receiver dispatch ->
        // VM-native). Pure element-folding shared with the interpreter via
        // `builtins::quanthash_coerce`. `.MixHash` falls through (it registers
        // container type metadata, which is interpreter-owned state).
        if args.is_empty()
            && let Some(result) = Self::try_native_quanthash_coerce(&target, method)
        {
            return result;
        }
        // Native `.iterator` construction over a plain receiver (Range/Set/Bag/
        // Mix/List/Array/...): builds the `Iterator` instance via the single
        // `builtins::iterator_construct` impl the interpreter also uses. `Seq`
        // (consumed-state + `squish` env mutation) and an already-built Iterator
        // fall through to the interpreter.
        if args.is_empty()
            && let Some(result) = Self::try_native_iterator_construct(&target, method)
        {
            return Ok(result);
        }
        // TODO: compile to bytecode — native/Buf/Failure method fork (ledger §1).
        // User-defined Instance methods now always run as bytecode (compiled at
        // registration, or on demand above via `populate_uncompiled_method`), so
        // what remains here is native receiver dispatch (Buf/Blob/Failure/IO and
        // the `dispatch_method_by_name_*` machinery) that depends on interpreter-
        // owned state (③ state ownership / first-class container Phase 2).
        crate::vm::vm_stats::record_method_fallback(method);
        self.vm_call_method_with_values(target, method, args)
    }

    /// VM-native dispatch for the pure-handle methods of an `IO::Handle`
    /// (`close`/`tell`/`eof`/`seek`/`opened`/`t` plus the PR-D Tier-1
    /// setters/getters `chomp`/`nl-out`/`out-buffer`/`encoding` and
    /// `native-descriptor`) — the ones that touch only the handle's own state,
    /// with no `emit_output` / env / encoding-helper dependency.
    /// Operates on the VM's own [`io_handles`](VM::io_handles_mut) handle and the
    /// shared `IoHandleState` methods (the single authoritative impl the
    /// interpreter's `*_handle_value` wrappers also use), so behavior is identical
    /// to the interpreter's native fork.
    ///
    /// Returns `None` (fall through to the interpreter) for any other receiver,
    /// any other method, unexpected arity, or junction arguments (which need
    /// interpreter autothreading). Restricted to the exact `"IO::Handle"` class:
    /// `IO::Socket::INET` (socket-semantic close) and `IO::Pipe` (process-reaping
    /// close) are intentionally excluded.
    fn try_native_io_handle_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };

        // Pure-handle setters/getters whose argument touches only handle state
        // (PLAN.md ③ native IO PR-D Tier-1). Junction arguments fall through to
        // the interpreter for autothreading.
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }

        // `.flush` is target-agnostic and pure (flush pending out-buffer + the OS
        // file buffer); shared `flush_for_method` (PR-D Tier-2b). A handle id not
        // in the table falls through — the interpreter shapes that into an
        // `X::IO::Flush` Failure (a value, not an error), which this Err-on-absent
        // path below cannot reproduce.
        if method == "flush" && args.is_empty() {
            let mut table = self.io_handles_mut();
            return table
                .map
                .get_mut(&id)
                .map(|state| state.flush_for_method().map(|_| Value::Bool(true)));
        }

        // `.encoding` returns Nil for binary mode and a Str otherwise, so its
        // result shaping is method-specific (mirrors the interpreter's
        // `native_io` arm exactly).
        enum EncodingOp {
            /// Getter: shape the current encoding into Nil("bin") / Str.
            Get,
            /// Setter to binary mode (`:bin`, `"bin"`, or `Nil` arg) -> Nil.
            SetBin,
            /// Setter to a named encoding -> Str(encoding).
            Set(String),
        }

        enum Op {
            Tell,
            Eof,
            Opened,
            Tty,
            Close,
            Seek(i64, i32),
            Chomp(Option<bool>),
            NlOut(Option<String>),
            OutBuffer(Option<Option<usize>>),
            Encoding(EncodingOp),
            NativeDescriptor,
        }
        let op = match method {
            "tell" if args.is_empty() => Op::Tell,
            "eof" if args.is_empty() => Op::Eof,
            "opened" if args.is_empty() => Op::Opened,
            "t" if args.is_empty() => Op::Tty,
            "close" if args.is_empty() => Op::Close,
            "native-descriptor" if args.is_empty() => Op::NativeDescriptor,
            "chomp" => Op::Chomp(args.first().map(|a| a.truthy())),
            "nl-out" => Op::NlOut(args.first().map(|a| a.to_string_value())),
            "out-buffer" => Op::OutBuffer(
                args.first()
                    .map(crate::runtime::Interpreter::parse_out_buffer_size),
            ),
            "encoding" => Op::Encoding(match args.first() {
                None => EncodingOp::Get,
                Some(Value::Nil) => EncodingOp::SetBin,
                Some(arg) => {
                    let enc = arg.to_string_value();
                    if enc == "bin" {
                        EncodingOp::SetBin
                    } else {
                        EncodingOp::Set(enc)
                    }
                }
            }),
            "seek" => {
                // seek($offset, $whence = SeekFromBeginning). Mirror the
                // interpreter's `native_io_handle` arg handling exactly: a
                // non-Int offset coerces to 0 (`unwrap_or(0)`), and the whence
                // string maps to 0/1/2 (anything else -> 0). Defer to the
                // interpreter when an arg is a junction (needs autothreading) or
                // the arity is unexpected.
                if args.len() > 2 || args.iter().any(|a| matches!(a, Value::Junction { .. })) {
                    return None;
                }
                let pos = match args.first() {
                    Some(Value::Int(i)) => *i,
                    _ => 0,
                };
                let mode = match args.get(1) {
                    Some(v) => match v.to_string_value().as_str() {
                        "SeekFromCurrent" => 1,
                        "SeekFromEnd" => 2,
                        _ => 0,
                    },
                    None => 0,
                };
                Op::Seek(pos, mode)
            }
            _ => return None,
        };

        let mut table = self.io_handles_mut();
        let Some(state) = table.map.get_mut(&id) else {
            return Some(Err(RuntimeError::new("Invalid IO::Handle")));
        };
        let result = match op {
            Op::Tell => state.tell().map(Value::Int),
            Op::Eof => state.eof().map(Value::Bool),
            Op::Opened => Ok(Value::Bool(state.is_opened())),
            Op::Tty => Ok(Value::Bool(state.is_tty())),
            Op::Close => state.close().map(Value::Bool),
            Op::Seek(pos, mode) => state.seek(pos, mode).map(Value::Int),
            Op::Chomp(set) => Ok(Value::Bool(state.chomp_setting(set))),
            Op::NlOut(set) => Ok(Value::str(state.nl_out_setting(set))),
            Op::OutBuffer(set) => state.out_buffer_setting(set).map(|n| Value::Int(n as i64)),
            Op::NativeDescriptor => state.native_descriptor().map(Value::Int),
            Op::Encoding(enc_op) => Ok(match enc_op {
                EncodingOp::Get => {
                    let cur = state.encoding_setting(None);
                    if cur == "bin" {
                        Value::Nil
                    } else {
                        Value::str(cur)
                    }
                }
                EncodingOp::SetBin => {
                    state.encoding_setting(Some("bin".to_string()));
                    Value::Nil
                }
                EncodingOp::Set(enc) => {
                    state.encoding_setting(Some(enc.clone()));
                    Value::str(enc)
                }
            }),
        };
        Some(result)
    }

    /// VM-native text output for a `File`+UTF8 `IO::Handle` receiver
    /// (`print`/`put`/`say`/`printf`/`print-nl`): build the payload exactly as
    /// the interpreter's `native_io` handlers do — `render_str_value` for
    /// print/put, `render_gist_value` for say (byte-identical; the same helpers
    /// the VM's own `say`/`print` ops use), the pure `sprintf` helpers for
    /// printf — and write it through the VM's `io_handles` handle via the shared
    /// `IoHandleState::native_text_write` (PLAN.md ③ native IO PR-D Tier-2a/2b).
    ///
    /// Returns `None` (fall through to the interpreter) for any non-`IO::Handle`
    /// receiver, any other method, a junction argument (autothreading), or a
    /// handle whose target/encoding is not File+UTF8 — Stdout/Stderr need
    /// `emit_output`/`stderr_output` and a non-UTF8 File needs
    /// `encode_with_encoding`, neither VM-reachable yet (③ 後段/④). The
    /// target/encoding gate is read *before* the payload is built so a
    /// fall-through never double-runs the argument stringification.
    fn try_native_io_handle_output(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };

        enum Kind {
            Print,
            Put,
            Say,
            Printf,
            PrintNl,
        }
        let kind = match method {
            "print" => Kind::Print,
            "put" => Kind::Put,
            "say" => Kind::Say,
            "printf" => Kind::Printf,
            "print-nl" => Kind::PrintNl,
            _ => return None,
        };
        // Junction args autothread in the interpreter; fall through for those
        // (printf's first-arg junction also threads there).
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }

        // Resolve the target *before* building the payload, so falling through
        // (Socket / non-UTF8 File / Stdin) never runs the arg stringification
        // twice. Stdout/Stderr emit via the VM's shared output sink (③後段 PR-C);
        // File writes its handle state (Tier-2a).
        enum Tgt {
            File,
            Stdout,
            Stderr,
        }
        let tgt = {
            let table = self.io_handles_mut();
            let state = table.map.get(&id)?;
            if state.can_native_text_write() {
                Tgt::File
            } else if state.is_stdout_target() {
                Tgt::Stdout
            } else if state.is_stderr_target() {
                Tgt::Stderr
            } else {
                return None;
            }
        };

        // Build the argument content. `print-nl` has no args — its payload is the
        // handle's `nl_out`, which `prepare_text_payload` appends via `newline`
        // (content = "", newline = true). Its closed-handle error uses "write",
        // matching the interpreter's `print-nl` (which calls `write_to_handle_value`).
        let (content, newline, trying): (String, bool, &str) = match kind {
            Kind::PrintNl => (String::new(), true, "write"),
            // printf: validate the directives then format, exactly as the
            // interpreter's `printf` arm (pure `sprintf` helpers, no handle state).
            Kind::Printf => {
                let fmt = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let rest = if args.is_empty() { &[][..] } else { &args[1..] };
                if let Err(e) =
                    crate::runtime::sprintf::validate_sprintf_directives(&fmt, rest.len())
                {
                    return Some(Err(e));
                }
                (
                    crate::runtime::sprintf::format_sprintf_args(&fmt, rest),
                    false,
                    method,
                )
            }
            Kind::Say => {
                let mut c = String::new();
                for arg in args {
                    c.push_str(&loan_env!(self, render_gist_value(arg)));
                }
                (c, true, method)
            }
            // print / put use `render_str_value`.
            Kind::Print => {
                let mut c = String::new();
                for arg in args {
                    c.push_str(&loan_env!(self, render_str_value(arg)));
                }
                (c, false, method)
            }
            Kind::Put => {
                let mut c = String::new();
                for arg in args {
                    c.push_str(&loan_env!(self, render_str_value(arg)));
                }
                (c, true, method)
            }
        };

        // File: prepare + buffered file write in one confined guard.
        if matches!(tgt, Tgt::File) {
            let mut table = self.io_handles_mut();
            let Some(state) = table.map.get_mut(&id) else {
                return Some(Err(RuntimeError::new("Invalid IO::Handle")));
            };
            return Some(
                state
                    .native_text_write(&content, newline, trying)
                    .map(|_| Value::Bool(true)),
            );
        }

        // Stdout/Stderr: prepare the payload on the receiver handle (closed check
        // + nl_out + bytes_written), then emit via the shared output sink.
        let payload = {
            let mut table = self.io_handles_mut();
            let Some(state) = table.map.get_mut(&id) else {
                return Some(Err(RuntimeError::new("Invalid IO::Handle")));
            };
            match state.prepare_text_payload(&content, newline, trying) {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            }
        };
        if matches!(tgt, Tgt::Stdout) {
            self.vm_emit_stdout(&payload);
        } else {
            self.vm_emit_stderr(&payload);
        }
        Some(Ok(Value::Bool(true)))
    }

    /// VM-native raw byte output for a `File` `IO::Handle` receiver
    /// (`write` / `spurt`): build the bytes exactly as the interpreter's
    /// `native_io` handlers do and write them straight to the file via the
    /// shared `IoHandleState::native_write_bytes_file` (raw, `:out-buffer`- and
    /// encoding-bypassing — same semantics as `write_bytes_to_handle_value`).
    /// PLAN.md ③ native IO PR-D Tier-2c.
    ///
    /// `write` concatenates each arg's bytes — buffer types (Buf/Blob/utf8/
    /// utf16) via `supply_chunk_to_bytes` (utf16-aware), non-buffers via
    /// `render_str_value` (their UTF-8 bytes). `spurt` writes its single
    /// argument: a Buf's raw bytes, or a Str's UTF-8 bytes.
    ///
    /// Returns `None` (fall through) for any non-`IO::Handle` receiver, any other
    /// method, a junction argument, a non-File target (Stdout/Stderr need
    /// `emit_output`), or a `spurt` of a Str on a non-UTF-8 handle (needs
    /// `encode_with_encoding`). The File gate is read before the bytes are built.
    fn try_native_io_handle_byte_output(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };
        if !matches!(method, "write" | "spurt") {
            return None;
        }
        // Junction args autothread in the interpreter; fall through for those.
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }

        // File-target gate, read before building the bytes. For `spurt` of a Str
        // the handle encoding must also be UTF-8 (a non-UTF-8 encoding re-enters
        // `encode_with_encoding`); `write` and a Buf `spurt` ignore encoding.
        let spurt_str_needs_utf8 =
            method == "spurt" && !args.first().map(Self::is_buf_value).unwrap_or(false);
        {
            let table = self.io_handles_mut();
            let state = table.map.get(&id)?;
            if !state.is_file_target() {
                return None;
            }
            if spurt_str_needs_utf8 && !state.can_native_text_write() {
                return None;
            }
        }

        let bytes: Vec<u8> = if method == "spurt" {
            let content_value = args
                .first()
                .cloned()
                .unwrap_or_else(|| Value::Str(String::new().into()));
            if Self::is_buf_value(&content_value) {
                Self::extract_buf_bytes(&content_value)
            } else {
                // Gate guaranteed UTF-8, so the Str's bytes are its UTF-8 bytes.
                content_value.to_string_value().into_bytes()
            }
        } else {
            // write: concatenate each arg — buffer types via supply_chunk_to_bytes
            // (utf16-aware), non-buffers via render_str_value (UTF-8 bytes).
            let mut out = Vec::new();
            for arg in args {
                if Self::is_buf_value(arg) {
                    out.extend(self.interpreter.supply_chunk_to_bytes(arg, "utf-8"));
                } else {
                    out.extend(loan_env!(self, render_str_value(arg)).into_bytes());
                }
            }
            out
        };

        let mut table = self.io_handles_mut();
        let Some(state) = table.map.get_mut(&id) else {
            return Some(Err(RuntimeError::new("Invalid IO::Handle")));
        };
        Some(
            state
                .native_write_bytes_file(&bytes)
                .map(|_| Value::Bool(true)),
        )
    }

    /// VM-native reads from a `File`+UTF8 `IO::Handle` receiver (③後段 PR-D read
    /// side): `get` (next line → `Str`/`Nil`), `slurp` (rest of file → `Str`),
    /// `read` (up to N bytes → `Buf`). Each delegates to the shared
    /// `IoHandleState` reader and shapes the result exactly as the interpreter's
    /// handler.
    ///
    /// Returns `None` (fall through) for any non-`IO::Handle` receiver, any other
    /// method, or a handle the native path can't serve — Stdin / ArgFiles (need
    /// `@*ARGS`), a non-UTF-8 / `:bin` slurp (needs `Buf` / `decode_with_encoding`),
    /// junction args — keeping the interpreter's richer read.
    fn try_native_io_handle_read(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }
        match method {
            "get" => {
                if !args.is_empty() {
                    return None;
                }
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                // File + UTF-8/binary only; everything else falls through.
                if !state.can_native_text_write() {
                    return None;
                }
                Some(
                    state
                        .read_line_native()
                        .map(|line| line.map(Value::str).unwrap_or(Value::Nil)),
                )
            }
            "slurp" => {
                // `:bin` falls through (returns a Buf via the interpreter).
                let has_bin = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "bin" && v.truthy()));
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                if !state.can_native_slurp_string(has_bin) {
                    return None;
                }
                Some(state.slurp_string_native().map(Value::str))
            }
            "read" => {
                // `.read` returns a Buf. Parse the byte count exactly as the
                // interpreter (positive Int → that many bytes; else read to EOF).
                let count = match args.first() {
                    Some(Value::Int(i)) if *i > 0 => *i as usize,
                    None => 0,
                    // Any other first arg (0/negative Int, non-Int) → the
                    // interpreter coerces to 0 (read all); match that.
                    Some(Value::Int(_)) => 0,
                    _ => return None,
                };
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                if !state.is_file_target() {
                    return None;
                }
                Some(
                    state
                        .read_bytes_native(count)
                        .map(crate::runtime::Interpreter::make_buf),
                )
            }
            "getc" => {
                if !args.is_empty() {
                    return None;
                }
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                // File + UTF-8/binary only (utf16/non-UTF8 need the interpreter).
                if !state.can_native_text_write() {
                    return None;
                }
                // One character, possibly multi-byte; empty -> Nil (as `getc`).
                Some(state.read_chars_native(Some(1)).map(|s| {
                    if s.is_empty() {
                        Value::Nil
                    } else {
                        Value::str(s)
                    }
                }))
            }
            "readchars" => {
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                if !state.can_native_text_write() {
                    return None;
                }
                // count: a non-negative integer (parsed as the interpreter does);
                // an invalid arg is the same error; no arg reads to EOF.
                let count = match args.first() {
                    None => None,
                    Some(arg) => match crate::runtime::Interpreter::parse_out_buffer_size(arg) {
                        Some(n) => Some(n),
                        None => {
                            return Some(Err(RuntimeError::new(
                                "readchars count must be a non-negative integer",
                            )));
                        }
                    },
                };
                Some(state.read_chars_native(count).map(Value::str))
            }
            _ => None,
        }
    }

    /// VM-native QuantHash coercion for `.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`
    /// over a list-like receiver (List/Array/Seq/Slip/Hash/Set/Bag/Mix/Pair/Range).
    /// Delegates the element folding to the single authoritative pure
    /// implementation in `builtins::quanthash_coerce`, the same one the
    /// interpreter's `dispatch_method_by_name_2` uses, so the result is
    /// behavior-invariant. The `*Hash` variants flip the mutable flag exactly as
    /// the interpreter does.
    ///
    /// Returns `None` (fall through to the interpreter) for `.MixHash` (it
    /// registers container type metadata — interpreter-owned state) and for
    /// non-list-like receivers (`Instance`/`Package`/scalars), leaving those rarer
    /// cases — `__baggy_data__` instances, type objects, user coercion — to the
    /// interpreter.
    fn try_native_quanthash_coerce(
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "Set" | "SetHash" | "Bag" | "BagHash" | "Mix") {
            return None;
        }
        // Only list-like / QuantHash receivers go native; everything else
        // (Instance/Package/scalars) keeps the interpreter's richer handling.
        let list_like = matches!(
            target,
            Value::Array(..)
                | Value::Seq(_)
                | Value::Slip(_)
                | Value::Hash(_)
                | Value::Set(..)
                | Value::Bag(..)
                | Value::Mix(..)
                | Value::Pair(..)
                | Value::ValuePair(..)
        ) || target.is_range();
        if !list_like {
            return None;
        }
        let target = target.clone();
        let result = match method {
            "Set" => crate::builtins::quanthash_coerce::to_set(target),
            "SetHash" => crate::builtins::quanthash_coerce::to_set(target).map(|r| match r {
                Value::Set(items, _) => Value::Set(items, true),
                other => other,
            }),
            "Bag" => crate::builtins::quanthash_coerce::to_bag(target, "Bag"),
            "BagHash" => {
                crate::builtins::quanthash_coerce::to_bag(target, "BagHash").map(|r| match r {
                    Value::Bag(items, _) => Value::Bag(items, true),
                    other => other,
                })
            }
            "Mix" => crate::builtins::quanthash_coerce::to_mix(target),
            _ => unreachable!(),
        };
        Some(result)
    }

    /// VM-native `.iterator` construction, mirroring
    /// `Interpreter::dispatch_iterator_method`: an already-built `Iterator`
    /// instance returns itself; a `Seq` falls through (consumed-state tracking +
    /// `squish` env mutation are interpreter-owned); any other receiver builds an
    /// `Iterator` instance via the single `builtins::iterator_construct` impl.
    /// Returns `None` to fall through to the interpreter.
    fn try_native_iterator_construct(target: &Value, method: &str) -> Option<Value> {
        if method != "iterator" {
            return None;
        }
        // Already an Iterator instance: identity (interpreter's early return).
        if let Value::Instance { class_name, .. } = target
            && class_name == "Iterator"
        {
            return Some(target.clone());
        }
        // Seq: consumed-state + `squish` env mutation are interpreter-owned.
        if matches!(target, Value::Seq(_)) {
            return None;
        }
        Some(crate::builtins::iterator_construct::build_iterator_instance(target))
    }

    /// VM-native Iterator protocol over a self-contained, array-backed `Iterator`
    /// instance (`items` Array + `index` Int, no `squish_source` callbacks).
    /// Handles the index-advancing protocol methods `pull-one`/`skip-one`/
    /// `skip-at-least`/`skip-at-least-pull-one`/`sink-all`, mirroring the
    /// interpreter's mutating iterator dispatch in `methods_mut.rs` exactly,
    /// including the identity-based writeback (env + locals) so the receiver
    /// variable and any aliases observe the advance. Behavior-invariant.
    ///
    /// Returns `None` (fall through to the interpreter) for: non-`Iterator`
    /// receivers; squish iterators (which invoke user `as`/`with` callbacks);
    /// predictive / coroutine iterators (no concrete `items` array); the
    /// `push-*` family (it writes pulled elements into an external buffer arg,
    /// needing array-identity writeback handled by the interpreter); and
    /// `count-only`/`bool-only` (left to the interpreter's predictive handling).
    pub(super) fn try_native_iterator(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "pull-one" | "skip-one" | "skip-at-least" | "skip-at-least-pull-one" | "sink-all"
        ) {
            return None;
        }
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = target
        else {
            return None;
        };
        if class_name.resolve() != "Iterator"
            || attributes.contains_key("squish_source")
            || attributes.contains_key("is_lazy")
        {
            // squish iterators invoke user callbacks; lazy iterators (gather /
            // lazy Seq) pull through interpreter-owned coroutine state rather than
            // a materialized `items` snapshot — leave both to the interpreter.
            return None;
        }
        // Only a concrete array-backed iterator (excludes predictive/coroutine
        // iterators whose state lives off the instance). Snapshot the (cheap Arc
        // clone of the) backing array and the start index, then drop the read
        // guard before the writeback below write-locks the same cell.
        let (items, start_index) = {
            let map = attributes.as_map();
            let Some(Value::Array(items, ..)) = map.get("items") else {
                return None;
            };
            let start_index = match map.get("index") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => 0,
            };
            (items.clone(), start_index)
        };
        let len = items.len();
        let mut index = start_index;
        let ret = match method {
            "pull-one" => {
                if index < len {
                    let out = items[index].clone();
                    index += 1;
                    out
                } else {
                    Value::str_from("IterationEnd")
                }
            }
            "sink-all" => {
                index = len;
                Value::str_from("IterationEnd")
            }
            "skip-one" => {
                if index < len {
                    index += 1;
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                }
            }
            "skip-at-least" => {
                let want = args.first().map(crate::runtime::to_int).unwrap_or(0).max(0) as usize;
                if len.saturating_sub(index) >= want {
                    index += want;
                    Value::Bool(true)
                } else {
                    index = len;
                    Value::Bool(false)
                }
            }
            "skip-at-least-pull-one" => {
                let want = args.first().map(crate::runtime::to_int).unwrap_or(0).max(0) as usize;
                if len.saturating_sub(index) >= want {
                    index += want;
                    if index < len {
                        let out = items[index].clone();
                        index += 1;
                        out
                    } else {
                        Value::str_from("IterationEnd")
                    }
                } else {
                    index = len;
                    Value::str_from("IterationEnd")
                }
            }
            _ => unreachable!(),
        };
        // Write the advanced index back by instance identity (env + locals), as
        // the interpreter's mutating iterator dispatch does, so the receiver
        // variable and aliases see the advance.
        if index != start_index {
            // Write through the shared cell in place: every alias of this
            // iterator instance (caller var, locals) sees the advance directly.
            attributes.insert("index".to_string(), Value::Int(index as i64));
            self.env_dirty = true;
        }
        Some(Ok(ret))
    }

    /// Compile a resolved user method's body on demand when it lacks bytecode,
    /// then dispatch as bytecode instead of through the interpreter bridge.
    /// Almost all user methods are already compiled at class registration
    /// (`compile_class_methods`); the gap this closes is methods inserted after
    /// that pass without their own bytecode — notably `.^add_multi_method`
    /// (which hardcodes `compiled_code = None`) and any future such site.
    /// (`.^add_method` already carries the method literal's compiled code.)
    /// Populates `compiled_code` in the canonical registry (idempotent) and
    /// re-resolves so the returned def carries the bytecode. Returns `None`
    /// when the owner is not a user class/role (native receiver) or the body
    /// stays uncompilable, preserving the interpreter fallback. Ledger §1.
    fn populate_uncompiled_method(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        args: &[Value],
        target: &Value,
    ) -> Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> {
        // Both compile passes are no-ops if the name is absent from the
        // respective registry, so calling both safely covers class- and
        // role-owned methods. Neither re-enters user code (pure compilation),
        // so the registry re-entrancy discipline (②) is respected.
        self.interpreter.compile_class_methods(owner_class);
        self.interpreter.compile_role_methods(owner_class);
        let (owner, def) = loan_env!(
            self,
            resolve_method_with_owner_invocant(cn, method, args, target)
        )?;
        if def.compiled_code.is_some() {
            Some((owner, std::sync::Arc::new(def)))
        } else {
            None
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dispatch_compiled_method(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        method_def: &std::sync::Arc<crate::runtime::MethodDef>,
        cc: &std::sync::Arc<CompiledCode>,
        target: Value,
        args: Vec<Value>,
        can_skip_merge: Option<bool>,
    ) -> Result<Value, RuntimeError> {
        let target_id = match &target {
            Value::Instance { id, .. } => Some(*id),
            _ => None,
        };
        let attrs_cell = match &target {
            Value::Instance { attributes, .. } => Some(attributes.clone()),
            _ => None,
        };
        let attributes = match &target {
            Value::Instance { attributes, .. } => attributes.to_map(),
            _ => std::collections::HashMap::new(),
        };
        let empty_fns = HashMap::new();
        let method_result = if let Some(csm) = can_skip_merge {
            // Fast path: move target directly as base (avoid extra clone).
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = loan_env!(
                self,
                push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
            );
            let result = self.call_compiled_method_fast(
                cn,
                owner_class,
                method,
                method_def,
                cc,
                attributes,
                args,
                target,
                &empty_fns,
                csm,
            );
            if pushed_dispatch {
                self.interpreter.pop_method_dispatch();
            }
            self.interpreter.pop_method_samewith_context();
            result
        } else {
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = loan_env!(
                self,
                push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
            );
            let invocant = Some(target);
            let result = self.call_compiled_method(
                cn,
                owner_class,
                method,
                method_def,
                cc,
                attributes,
                args,
                invocant,
                &empty_fns,
            );
            if pushed_dispatch {
                self.interpreter.pop_method_dispatch();
            }
            self.interpreter.pop_method_samewith_context();
            result
        };
        let (result, new_attrs, attrs_adjusted) = method_result?;
        if let Some(id) = target_id {
            // Commit only a `:=`-adjusted snapshot: an unadjusted one equals the
            // cell and the whole-map write would race with concurrent cell-CAS
            // from another thread (lost updates).
            if attrs_adjusted && let Some(cell) = &attrs_cell {
                cell.commit_attrs(new_attrs.clone());
            }
            if !self.interpreter.in_lvalue_assignment
                && let Value::Proxy { ref fetcher, .. } = result
            {
                // Without a `:=` adjustment the triple's map is the stale entry
                // snapshot (the lazy reconcile skips the cell clone) —
                // re-snapshot the live cell for the proxy fetcher.
                let proxy_attrs = if !attrs_adjusted && let Some(cell) = &attrs_cell {
                    cell.to_map()
                } else {
                    new_attrs
                };
                return loan_env!(self, proxy_fetch(fetcher, None, cn, &proxy_attrs, id));
            }
        }
        Ok(result)
    }

    /// Pre-compute and cache fast dispatch eligibility for a method.
    fn try_populate_fast_cache(
        &mut self,
        cache_key: (crate::symbol::Symbol, crate::symbol::Symbol),
        receiver_class: &str,
        owner_class: &str,
        method_def: &std::sync::Arc<crate::runtime::MethodDef>,
        cc: &std::sync::Arc<CompiledCode>,
    ) {
        let has_rw_params = method_def
            .param_defs
            .iter()
            .any(|pd| pd.traits.iter().any(|t| t == "rw"));
        if has_rw_params {
            return;
        }
        let has_invocant_constraint = method_def.param_defs.iter().any(|pd| {
            (pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                && pd.type_constraint.is_some()
        });
        let has_complex_params = method_def.param_defs.iter().any(|pd| {
            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                return false;
            }
            if pd.slurpy && pd.name == "%_" {
                return false;
            }
            pd.slurpy
                || pd.double_slurpy
                || pd.named
                || pd.where_constraint.is_some()
                || pd.sub_signature.is_some()
                || pd.outer_sub_signature.is_some()
                || pd.code_signature.is_some()
                || pd
                    .type_constraint
                    .as_ref()
                    .is_some_and(|tc| tc.contains('('))
        });
        let has_role_bindings = self
            .interpreter
            .class_role_param_bindings(owner_class)
            .is_some()
            || self
                .interpreter
                .class_role_param_bindings(receiver_class)
                .is_some();
        if has_invocant_constraint || has_complex_params || has_role_bindings {
            return;
        }
        let positional_count = method_def
            .param_defs
            .iter()
            .filter(|pd| {
                !pd.is_invocant
                    && !pd.traits.iter().any(|t| t == "invocant")
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && !pd.named
            })
            .count();
        let can_skip_merge = !cc.has_env_writes;
        let has_defaults = method_def.param_defs.iter().any(|pd| {
            !pd.is_invocant && !pd.traits.iter().any(|t| t == "invocant") && pd.default.is_some()
        });
        self.fast_method_cache.insert(
            cache_key,
            super::FastMethodCacheEntry {
                owner_class: crate::symbol::Symbol::intern(owner_class),
                method_def: method_def.clone(),
                compiled_code: cc.clone(),
                can_skip_merge,
                positional_count,
                has_defaults,
            },
        );
    }

    pub(super) fn try_compiled_method_mut_or_interpret(
        &mut self,
        target_name: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Native default construction (see `try_compiled_method_or_interpret`).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) = loan_env!(self, try_native_default_construct(*class_name, &args))
        {
            // Pure construction: fresh instance, no caller-env write (Slice 6.3).
            self.method_dispatch_pure = true;
            return result;
        }
        // Native built-in construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                crate::runtime::Interpreter::try_native_builtin_construct(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native built-in class method (mut path twin of the above).
        if let Value::Package(class_name) = &target
            && let Some(result) = crate::runtime::Interpreter::try_native_builtin_class_method(
                *class_name,
                method,
                &args,
            )
        {
            self.method_dispatch_pure = true;
            return result;
        }
        if let Value::Instance { class_name, .. } = &target {
            let class = class_name.resolve();
            // VM-native pure-handle IO dispatch (PLAN.md ③ native IO PR-C/PR-D),
            // mut path: `$fh.method` on a variable receiver routes here, so the
            // same state-only `IO::Handle` methods must be intercepted before the
            // generic native-method fallback below. These methods mutate only the
            // shared handle-table state (not the receiver binding), so the native
            // path returns the result directly; `None` falls through unchanged.
            if let Some(result) = self.try_native_io_handle_method(&target, method, &args) {
                return result;
            }
            // VM-native text output to a File+UTF8 `IO::Handle` (print/put/say/
            // print-nl), mut path (PR-D Tier-2a). See the non-mut twin above.
            if let Some(result) = self.try_native_io_handle_output(&target, method, &args) {
                return result;
            }
            // VM-native raw byte output to a File `IO::Handle` (write/spurt):
            // raw file write, no buffering/encoding (PR-D Tier-2c). Stdout/Stderr
            // and a non-UTF8 spurt of a Str fall through.
            if let Some(result) = self.try_native_io_handle_byte_output(&target, method, &args) {
                return result;
            }
            // VM-native line read from a File+UTF8 `IO::Handle` (get): reads via
            // the handle's record reader (PR-D read side). ArgFiles/Stdin/non-UTF8
            // (which need @*ARGS / decode) fall through.
            if let Some(result) = self.try_native_io_handle_read(&target, method, &args) {
                return result;
            }
            if self.interpreter.is_native_method(&class, method) {
                // TODO: compile to bytecode — Instance native-method fork, mut (ledger §1).
                crate::vm::vm_stats::record_method_fallback(method);
                return self.vm_call_method_mut_with_values(target_name, target, method, args);
            }
        }
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            // CARRIER: MOP pseudo-methods, mut (reflection). See ledger §C.
            crate::vm::vm_stats::record_method_fallback(method);
            return loan_env!(
                self,
                call_method_mut_with_values(target_name, target, method, args)
            );
        }
        // User-defined ^method (metamethod) dispatch:
        // Foo.^bar passes Foo as the first positional argument.
        if method.starts_with('^')
            && method.len() > 1
            && !crate::runtime::Interpreter::is_classhow_method(&method[1..])
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.interpreter.has_user_method(&cn, method)
            {
                let mut how_args = vec![target.clone()];
                how_args.extend(args);
                // CARRIER: user-defined ^metamethod dispatch, mut (MOP). See ledger §C.
                crate::vm::vm_stats::record_method_fallback(method);
                return loan_env!(self, call_method_with_values(target, method, how_args));
            }
        }
        if method.starts_with('!') {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name {
                let resolved = loan_env!(self, resolve_private_method_for_vm(&cn, method, &args));
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self
                        .interpreter
                        .can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match &target {
                            Value::Instance { id, .. } => Some(*id),
                            _ => None,
                        };
                        let attrs_cell = match &target {
                            Value::Instance { attributes, .. } => Some(attributes.clone()),
                            _ => None,
                        };
                        let attributes = match &target {
                            Value::Instance { attributes, .. } => attributes.to_map(),
                            _ => std::collections::HashMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::Package(crate::symbol::Symbol::intern(&cn))
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = loan_env!(
                            self,
                            push_method_dispatch_frame(&cn, method, &args, invocant_for_dispatch,)
                        );
                        let invocant = Some(target);
                        let empty_fns = HashMap::new();
                        let method_result = self.call_compiled_method(
                            &cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            attributes,
                            args,
                            invocant,
                            &empty_fns,
                        );
                        if pushed_dispatch {
                            self.interpreter.pop_method_dispatch();
                        }
                        self.interpreter.pop_method_samewith_context();
                        let (result, new_attrs, attrs_adjusted) = method_result?;
                        if let Some(id) = target_id {
                            // Commit only a `:=`-adjusted snapshot: an unadjusted
                            // one equals the cell and the whole-map write would
                            // race with concurrent cell-CAS (lost updates).
                            if attrs_adjusted && let Some(cell) = &attrs_cell {
                                cell.commit_attrs(new_attrs.clone());
                            }
                            if !self.interpreter.in_lvalue_assignment
                                && let Value::Proxy { ref fetcher, .. } = result
                            {
                                // Without a `:=` adjustment the triple's map is
                                // the stale entry snapshot (the lazy reconcile
                                // skips the cell clone) — re-snapshot the live
                                // cell for the proxy fetcher.
                                let proxy_attrs = if !attrs_adjusted && let Some(cell) = &attrs_cell
                                {
                                    cell.to_map()
                                } else {
                                    new_attrs
                                };
                                return loan_env!(
                                    self,
                                    proxy_fetch(fetcher, None, &cn, &proxy_attrs, id)
                                );
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        let class_name = match &target {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        if let Some(cn) = class_name
            && let Some((owner_class, method_def)) = loan_env!(
                self,
                resolve_method_with_owner_invocant(&cn, method, &args, &target)
            )
        {
            // Ambiguous multi dispatch: two or more candidates matched equally
            // well. Raise X::Multi::Ambiguous instead of silently picking one.
            if self.interpreter.dispatch_ambiguous {
                self.interpreter.dispatch_ambiguous = false;
                let sigs = self
                    .interpreter
                    .format_method_candidate_signatures(&cn, method);
                return Err(
                    crate::runtime::methods_signature::make_multi_ambiguous_error(
                        method, &cn, &sigs,
                    ),
                );
            }
            if let Some(result) =
                self.check_method_wrap_chain(&cn, &owner_class, method, &method_def, &target, &args)
            {
                return result;
            }
            // Resolve to a def carrying compiled bytecode, compiling on demand
            // for methods added after the registration compile pass (e.g.
            // `.^add_method`, ledger §1) so they run as bytecode rather than
            // tree-walking. None → native receiver, keep interpreter fallback.
            let resolved: Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> =
                if method_def.compiled_code.is_some() {
                    Some((owner_class, std::sync::Arc::new(method_def)))
                } else if !method_def.body.is_empty() {
                    self.populate_uncompiled_method(&cn, &owner_class, method, &args, &target)
                } else {
                    None
                };
            if let Some((owner_class, method_def)) = resolved {
                let cc = method_def.compiled_code.clone().expect("compiled_code set");
                let target_id = match &target {
                    Value::Instance { id, .. } => Some(*id),
                    _ => None,
                };
                let attrs_cell = match &target {
                    Value::Instance { attributes, .. } => Some(attributes.clone()),
                    _ => None,
                };
                let attributes = match &target {
                    Value::Instance { attributes, .. } => attributes.to_map(),
                    _ => std::collections::HashMap::new(),
                };
                let invocant_for_dispatch = if attributes.is_empty() {
                    Value::Package(crate::symbol::Symbol::intern(&cn))
                } else {
                    target.clone()
                };
                let pushed_dispatch = loan_env!(
                    self,
                    push_method_dispatch_frame(&cn, method, &args, invocant_for_dispatch,)
                );
                let invocant = Some(target);
                let empty_fns = HashMap::new();
                let method_result = self.call_compiled_method(
                    &cn,
                    &owner_class,
                    method,
                    &method_def,
                    &cc,
                    attributes,
                    args,
                    invocant,
                    &empty_fns,
                );
                if pushed_dispatch {
                    self.interpreter.pop_method_dispatch();
                }
                self.interpreter.pop_method_samewith_context();
                let (result, new_attrs, attrs_adjusted) = method_result?;
                if let Some(id) = target_id {
                    // Commit only a `:=`-adjusted snapshot (cell-CAS race
                    // avoidance — see the primary dispatch site).
                    if attrs_adjusted && let Some(cell) = &attrs_cell {
                        cell.commit_attrs(new_attrs.clone());
                    }
                    if !self.interpreter.in_lvalue_assignment
                        && let Value::Proxy { ref fetcher, .. } = result
                    {
                        // Without a `:=` adjustment the triple's map is the stale
                        // entry snapshot (lazy reconcile) — re-snapshot the live
                        // cell for the proxy fetcher.
                        let proxy_attrs = if !attrs_adjusted && let Some(cell) = &attrs_cell {
                            cell.to_map()
                        } else {
                            new_attrs
                        };
                        return loan_env!(self, proxy_fetch(fetcher, None, &cn, &proxy_attrs, id));
                    }
                }
                return Ok(result);
            }
        }
        // Native `.map` / `.grep` over a concrete array with a simple block (lever A).
        // `target_name` is the receiver variable, enabling rw-binding writeback
        // for `$_`-mutating blocks (`@a.map({ $_++ })` mutates `@a`).
        if let Some(result) = self.try_native_array_map(Some(target_name), &target, method, &args) {
            return result;
        }
        // Native `.subst` over a Str with a simple pattern/replacement (lever A).
        if let Some(result) = self.try_native_subst(&target, method, &args) {
            return result;
        }
        // Native `.sort` over a plain array with no/simple comparator (lever A).
        if let Some(result) = self.try_native_sort(&target, method, &args) {
            return result;
        }
        // Native `.min` / `.max` over a plain list, including `:by` blocks.
        if let Some(result) = self.try_native_extrema(&target, method, &args) {
            return result;
        }
        // Native `.minmax` over a plain list, including `:by` blocks.
        if let Some(result) = self.try_native_minmax(&target, method, &args) {
            return result;
        }
        // Native `.first` over a plain list (no-adverb forms), including blocks.
        if let Some(result) = self.try_native_first(&target, method, &args) {
            return result;
        }
        // TODO: compile to bytecode — native/Buf/Failure method fork, mut (ledger §1).
        // User-defined methods run as bytecode (compiled at registration or on
        // demand above); what remains is native receiver dispatch blocked on
        // ③ state ownership / first-class container Phase 2.
        crate::vm::vm_stats::record_method_fallback(method);
        self.vm_call_method_mut_with_values(target_name, target, method, args)
    }

    /// Execute a protect block inline in the current VM, avoiding the overhead
    /// of creating a new VM.
    pub(super) fn exec_protect_block_inline(
        &mut self,
        outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Value, RuntimeError> {
        let outer_local_slots: std::collections::HashMap<&str, usize> = outer_code
            .locals
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.as_str(), idx))
            .collect();
        let (block_cc, block_fns, captured_env, captured_bindings, writeback_bindings) =
            match code_val {
                Value::Sub(data) => {
                    let (
                        block_cc,
                        block_fns,
                        captured_bindings,
                        writeback_bindings,
                        captured_names,
                    ) = self
                        .interpreter
                        .get_or_compile_protect_block_with_slots(data);
                    self.interpreter.sync_shared_vars_for_names(
                        captured_names.iter().map(|name| name.as_str()),
                    );
                    (
                        block_cc,
                        block_fns,
                        Some(&data.env),
                        captured_bindings,
                        writeback_bindings,
                    )
                }
                _ => {
                    // TODO: Handle non-Sub protect blocks (e.g. WeakSub, Routine)
                    // in the VM. Currently these are rare and delegate to interpreter.
                    return self.interpreter.call_protect_block(code_val);
                }
            };

        // Save/swap stack and locals for the block
        let mut saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_env_dirty = self.env_dirty;

        // Initialize locals for the block
        self.locals = vec![Value::Nil; block_cc.locals.len()];
        self.env_dirty = false;
        if captured_env.is_some() {
            for (slot, name) in captured_bindings.iter() {
                if (name.starts_with('@') || name.starts_with('%'))
                    && self.interpreter.get_shared_var(name).is_some()
                {
                    // Leave shared collections unmaterialized in locals.
                    // GetLocal will read the shared value on demand.
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(val) = saved_locals.get(*outer_slot)
                {
                    self.locals[*slot] = val.clone();
                    continue;
                }
                if let Some(val) = self.env().get(name) {
                    self.locals[*slot] = val.clone();
                }
            }
        }

        // Execute the block's opcodes inline
        let mut sub_ip = 0;
        let mut exec_err = None;
        while sub_ip < block_cc.ops.len() {
            if let Err(e) = self.exec_one(&block_cc, &mut sub_ip, &block_fns) {
                exec_err = Some(e);
                break;
            }
        }

        // Sync locals back to env
        if let Some(captured) = captured_env {
            for (slot, name) in writeback_bindings.iter() {
                if matches!(
                    self.interpreter.get_shared_var(name),
                    Some(Value::Array(..) | Value::Hash(..))
                ) {
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(target) = saved_locals.get_mut(*outer_slot)
                {
                    *target = self.locals[*slot].clone();
                }
                if captured.contains_key(name)
                    && !matches!(
                        self.interpreter.get_shared_var(name),
                        Some(Value::Array(..) | Value::Hash(..))
                    )
                {
                    {
                        let __v = self.locals[*slot].clone();
                        self.env_mut().insert(name.clone(), __v);
                    }
                }
            }
        }

        // Get return value before restoring state
        let ret_val = self.stack.pop().unwrap_or(Value::Nil);

        // Restore outer state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = saved_env_dirty;

        match exec_err {
            Some(e) => Err(e),
            None => Ok(ret_val),
        }
    }

    /// Check if a method candidate has a wrap chain from ^lookup().candidates[N].wrap().
    /// If so, dispatch through the wrapper and return Some(result).
    fn check_method_wrap_chain(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        method_def: &crate::runtime::MethodDef,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !self.interpreter.has_any_wrap_chains() || self.interpreter.is_inside_wrap_dispatch() {
            return None;
        }
        let cand_idx =
            self.interpreter
                .find_method_candidate_index(owner_class, method, method_def)?;
        let chain = self
            .interpreter
            .get_method_wrap_chain(owner_class, method, cand_idx)?
            .clone();
        let invocant_for_dispatch = target.clone();
        let pushed_dispatch = loan_env!(
            self,
            push_method_dispatch_frame(cn, method, args, invocant_for_dispatch)
        );
        let mut orig_env = crate::env::Env::new();
        orig_env.insert(
            "__mutsu_method_wrap_original".to_string(),
            Value::Bool(true),
        );
        let original_sub = Value::make_sub(
            crate::symbol::Symbol::intern(owner_class),
            crate::symbol::Symbol::intern(method),
            method_def.params.clone(),
            method_def.param_defs.clone(),
            (*method_def.body).clone(),
            method_def.is_rw,
            orig_env,
        );
        let outermost = chain.last().unwrap().1.clone();
        let mut remaining: Vec<Value> = Vec::new();
        for i in (0..chain.len() - 1).rev() {
            remaining.push(chain[i].1.clone());
        }
        remaining.push(original_sub);
        let mut call_args = vec![target.clone()];
        call_args.extend(args.to_vec());
        let frame = crate::runtime::WrapDispatchFrame {
            sub_id: 0,
            remaining,
            args: call_args.clone(),
        };
        let wrapper_id = if let Value::Sub(ref wd) = outermost {
            Some(wd.id)
        } else {
            None
        };
        self.interpreter.push_wrap_dispatch_frame(frame);
        let result = self.vm_call_sub_value(outermost, call_args, false);
        self.interpreter.pop_wrap_dispatch_frame();
        // Propagate closure variable mutations from the wrapper back to the
        // current env so captured variables are visible to the caller.
        if let Some(wid) = wrapper_id
            && let Some(persisted) = self.interpreter.get_closure_env_override(wid)
        {
            for (k, v) in persisted.iter() {
                if self.env().contains_key_sym(*k) {
                    self.env_mut().insert_sym(*k, v.clone());
                }
            }
            self.env_dirty = true;
        }
        if pushed_dispatch {
            self.interpreter.pop_method_dispatch();
        }
        self.interpreter.pop_method_samewith_context();
        Some(result)
    }
}
