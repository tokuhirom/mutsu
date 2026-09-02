use super::*;

impl Interpreter {
    pub(super) fn mark_failure_handled_on_stack(stack: &mut [Value]) {
        if let Some(ValueView::Instance {
            class_name,
            id,
            attributes,
        }) = stack.last().map(Value::view)
            && class_name == "Failure"
        {
            attributes.insert("handled".to_string(), Value::TRUE);
            crate::value::mark_failure_handled(id);
        }
    }

    fn runtime_error_from_exception_value(
        &mut self,
        value: Value,
        default_message: &str,
        is_fail: bool,
    ) -> RuntimeError {
        if value.is_nil() {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "payload".to_string(),
                Value::str(default_message.to_string()),
            );
            attrs.insert(
                "message".to_string(),
                Value::str(default_message.to_string()),
            );
            let exception = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
            let mut err = RuntimeError::new(default_message);
            if is_fail {
                err.control = Some(crate::value::Control::Fail);
            }
            err.exception = Some(Box::new(exception));
            return err;
        }

        // See through a `but role` mixin (`X::Foo.new but role { … }`) to the
        // wrapped instance so a mixed-in exception is still recognized as an
        // exception (and matched by `CATCH { when X::Foo }`) instead of being
        // wrapped in X::AdHoc. Walk nested mixins to the innermost instance.
        let underlying_class: Option<Symbol> = {
            let mut cur = value.clone();
            loop {
                match cur.view() {
                    ValueView::Instance { class_name, .. } => break Some(class_name),
                    ValueView::Mixin(inner, _) => cur = inner.as_ref().clone(),
                    _ => break None,
                }
            }
        };

        let message = if let ValueView::Instance { attributes, .. } = value.view() {
            attributes
                .as_map()
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    // Try calling the user-defined .Str method
                    self.vm_call_method_with_values(value.clone(), "Str", vec![])
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|_| value.to_string_value())
                })
        } else if matches!(value.view(), ValueView::Mixin(..)) {
            // A mixed-in exception may override `.message`/`.Str`; dispatch through
            // the mixin so the override is honored, falling back to stringification.
            self.vm_call_method_with_values(value.clone(), "message", vec![])
                .or_else(|_| self.vm_call_method_with_values(value.clone(), "Str", vec![]))
                .map(|v| v.to_string_value())
                .unwrap_or_else(|_| value.to_string_value())
        } else if let ValueView::Array(items, _) = value.view() {
            // Multi-arg die: concatenate .Str of each element
            let mut parts = Vec::new();
            for item in items.iter() {
                let s = loan_env!(self, call_method_with_values(item.clone(), "Str", vec![]))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|_| item.to_string_value());
                parts.push(s);
            }
            parts.join("")
        } else {
            value.to_string_value()
        };

        let mut err = RuntimeError::new(&message);
        if is_fail {
            err.control = Some(crate::value::Control::Fail);
        }
        if let Some(class_name) = underlying_class {
            let cn = class_name.resolve();
            let is_exception = cn == "Exception"
                || cn.starts_with("X::")
                || cn.starts_with("CX::")
                || self
                    .mro_readonly(&cn)
                    .iter()
                    .any(|p| p == "Exception" || p.starts_with("X::") || p.starts_with("CX::"));
            if is_exception {
                // Preserve the value verbatim (including a `but role` mixin) so its
                // type still matches `when X::Foo` and any overridden `.message`
                // dispatches through the mixin.
                err.exception = Some(Box::new(value));
            } else {
                // Non-exception instance: wrap in X::AdHoc with payload
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("payload".to_string(), value);
                attrs.insert("message".to_string(), Value::str(message));
                err.exception = Some(Box::new(Value::make_instance(
                    Symbol::intern("X::AdHoc"),
                    attrs,
                )));
            }
        } else {
            // Non-instance value (Str, Int, etc.): wrap in X::AdHoc with payload
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("payload".to_string(), value);
            attrs.insert("message".to_string(), Value::str(message));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::AdHoc"),
                attrs,
            )));
        }
        err
    }

    pub(crate) fn exec_one(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let mut result = self.exec_one_dispatch(code, ip, compiled_fns);
        // Only genuine runtime errors get a backtrace here: control-flow
        // signals (return/next/warn/fail/...) and parse errors (which carry
        // their own line/column and render as ===SORRY!===) are excluded.
        // The innermost exec_one frame observes the error first, while the
        // routine stack is still intact; outer frames see backtrace()
        // already set and skip. die/throw attach theirs at the throw site.
        if let Err(ref mut e) = result
            // A control signal nothing can consume (`next` with no loop, `take`
            // with no gather) is an error in all but its routing flag, so it
            // gets the same backtrace an ordinary runtime error does — rakudo
            // reports the frames for those too.
            && (e.control.is_none() || e.is_illegal_control())
            && e.backtrace().is_none()
            && !e.code().is_some_and(|c| c.is_parse())
        {
            self.attach_backtrace_to_error(e);
        }
        result
    }

    fn exec_one_dispatch(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        crate::trace::trace_log!(
            "vm",
            "exec_one[{}]: {:?}",
            ip,
            std::mem::discriminant(&code.ops[*ip])
        );
        // Per-opcode execution histogram (MUTSU_VM_STATS=1 only; a single
        // cached bool load when off). Feeds instruction-set tuning decisions.
        crate::vm::vm_stats::record_opcode(&code.ops[*ip]);
        // Track the currently-executing frame's code so the lazy-force machinery
        // can reconcile this (caller) frame's local slots from env after a reify
        // that mutated a captured-outer lexical (Slice F). See `current_code`.
        self.current_code = code as *const CompiledCode as usize;
        match &code.ops[*ip] {
            // -- Constants --
            OpCode::LoadConst(idx) => {
                self.stack.push(code.constants[*idx as usize].clone());
                *ip += 1;
            }
            OpCode::LoadRegexClosure {
                const_idx,
                captures,
            } => {
                let v = self.capture_regex_closure(
                    code,
                    &code.constants[*const_idx as usize],
                    captures,
                );
                self.stack.push(v);
                *ip += 1;
            }
            OpCode::LoadNil => {
                self.stack.push(Value::NIL);
                *ip += 1;
            }
            OpCode::LoadTrue => {
                self.stack.push(Value::TRUE);
                *ip += 1;
            }
            OpCode::LoadFalse => {
                self.stack.push(Value::FALSE);
                *ip += 1;
            }

            // -- Variables --
            OpCode::GetUpvalue { index, name_idx } => {
                self.exec_get_upvalue_op(code, *index, *name_idx, ip)?;
            }
            OpCode::GetGlobal(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                if name == "?CALLER::LINE" {
                    let line = self.get_caller_line(1).unwrap_or(Value::NIL);
                    self.stack.push(line);
                    *ip += 1;
                    return Ok(());
                }
                // $*THREAD: dynamically create a Thread instance with current thread ID
                if name == "*THREAD" || name == "$*THREAD" {
                    self.stack.push(Self::make_thread_instance());
                    *ip += 1;
                    return Ok(());
                }
                // Atomic-variable read: only possible once some `atomicint`/atomic
                // storage has been registered. Skip the whole check (a `format!`
                // plus two `var_type_constraint` lookups) on the hot read path when
                // no atomics exist, which is the overwhelmingly common case.
                if self.atomic_var_seen() {
                    let atomic_name = name.strip_prefix('$').unwrap_or(name);
                    let atomic_name_key = format!("__mutsu_atomic_name::{atomic_name}");
                    let is_atomic_int = loan_env!(self, var_type_constraint(name)).as_deref()
                        == Some("atomicint")
                        || loan_env!(self, var_type_constraint(atomic_name)).as_deref()
                            == Some("atomicint")
                        || self.get_shared_var(&atomic_name_key).is_some();
                    if is_atomic_int {
                        let fetched = self.vm_call_function(
                            "__mutsu_atomic_fetch_var",
                            vec![Value::str(atomic_name.to_string())],
                        )?;
                        self.stack.push(fetched);
                        *ip += 1;
                        return Ok(());
                    }
                }
                // Phase 3 Stage 2c (ii): a sigilless attribute (`has $x`) compiles
                // to a bare `Var("x")` that reads via GetGlobal (it is not a method
                // local), so route it to `self`'s shared cell here too — otherwise
                // a read after a nested-frame mutation sees the stale entry copy.
                // `read_self_attr_cell` is gated on `sigilless_attrs_active`, so
                // non-sigilless programs pay only a string check.
                if let Some(cell_val) = self.read_self_attr_cell(name) {
                    self.stack.push(cell_val);
                    *ip += 1;
                    return Ok(());
                }
                // Rakudo parity: a private-attribute read on a concrete invocant
                // whose class does not carry the attribute throws (P6opaque
                // no-such-attribute) instead of yielding Nil.
                if name.starts_with('!')
                    && name.len() > 1
                    && !name.starts_with("__")
                    && let Some(err) = self.missing_private_attr_read_error(name)
                {
                    return Err(err);
                }
                // Fast scalar read (J4 helper hot path): the dominant GetGlobal
                // shape is a plain env hit (topic `$_` reads in loop bodies,
                // dynamic vars). Serve it through the memoized per-slot Symbol —
                // no per-read intern, and no kebab-alias re-probe for the 1-char
                // topic name `_` (whose underscore otherwise sends every read
                // through the `_`->`-` pre-pass of get_env_with_main_alias).
                // Gate order reproduces the slow chain's precedence: the stores
                // consulted before env must be provably inactive (escaping-our
                // captures empty, no real current package, no ADR-0024 mainline
                // capture active), @/% names keep the slow path's atomic/
                // thread-clone arms, and Nil / LazyThunk / ContainerRef hits
                // fall through for the slow tail's default/type-object, force
                // and deref handling.
                //
                // ADR-0024: `mainline_lexical_frame_active()` must also gate
                // this shortcut, for a reason `is_container_ref()` alone does
                // NOT cover: a mainline named sub's captured cell is installed
                // into BOTH its own local slot and the env key at
                // registration, but an ordinary later `my $name = ...` in an
                // unrelated (shadow-slot) scope — even one the sub was NOT
                // declared inside — explicitly clears a stale `ContainerRef`
                // sitting in env under its own name before writing its fresh
                // value (`exec_set_local_op_inner`'s redeclaration guard), so
                // by the time the sub runs the env key can hold a perfectly
                // plain (non-cell) value that is NOT the sub's own captured
                // binding. `unit_lexical_slot`/`get_env_with_main_alias`
                // consult `unit_lexicals[MAINLINE_UNIT_KEY]` before env
                // precisely to survive that; this shortcut must not bypass it.
                let fast_hit = {
                    let b0 = name.as_bytes().first().copied();
                    if !matches!(b0, Some(b'@' | b'%'))
                        && (name == "_" || !name.contains('_'))
                        && self.escaping_our_lexical_names.is_empty()
                        && !self.mainline_lexical_frame_active()
                        && {
                            let cur = self.current_package();
                            cur.is_empty() || cur == "GLOBAL"
                        }
                    {
                        match self.env().get_sym(code.const_sym(*name_idx)) {
                            Some(val)
                                if !val.is_nil()
                                    && !val.is_container_ref()
                                    && !matches!(val.view(), ValueView::LazyThunk(_)) =>
                            {
                                Some(val.clone())
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                };
                if let Some(val) = fast_hit {
                    self.stack.push(val);
                    *ip += 1;
                    return Ok(());
                }
                let val = self
                    // A PER-CALL anonymous state (`$` inside a block inside a
                    // routine) is authoritative in the state store: its `env`
                    // entry, written by `SetGlobal`, outlives the block clone
                    // and is always the previous call's. See `anon_state_key`.
                    .per_call_anon_state_read(name, Value::NIL)
                    // A package-block `my` lexical is stored in `package_lexicals`;
                    // it is the authoritative store for a bare free-variable read from
                    // inside that package's named subs, and must be read BEFORE `env`.
                    // Two stale `env` shadows would otherwise win: a boxed lexical's
                    // prior-call return-merge copy, and the package block's own
                    // `my $x` top-level local slot flushed to `env` as the type object
                    // after the block exits (`sync_env_from_locals`). Gated on a real
                    // `current_package`, so a bare reference after the block (under
                    // GLOBAL) does not resolve here.
                    // An `our sub` declared in a bare block reads a block `my`
                    // lexical that is out of scope by the time the registry routine
                    // runs (no per-sub closure env), and the shared `env` may hold an
                    // unrelated leaked value from a sibling block. Resolve such a
                    // capture through its persisted shared cell ONLY — see
                    // `escaping_our_read` — short-circuiting the env lookup.
                    .or_else(|| self.escaping_our_read(name))
                    .or_else(|| self.package_scope_lexical(name))
                    // NB: `get_env_with_main_alias` is also where a file-scope `my`
                    // of the running routine's own compunit resolves — `env` is not
                    // authoritative for that name. See `unit_lexicals`.
                    .or_else(|| self.get_env_with_main_alias(name))
                    .or_else(|| {
                        // Fall back to the persistent our_vars store for `our`-scoped
                        // variables accessed via package-qualified names (e.g., $Pkg::var).
                        // Bare variable names should NOT fall back to our_vars — the
                        // lexical alias for `our` variables is block-scoped.
                        if name.contains("::") {
                            self.get_our_var(name)
                                .cloned()
                                .or_else(|| self.our_var_pseudo_unqualified(name))
                                .or_else(|| {
                                    // Nested package shorthand: when looking up
                                    // `$D2::d3` from inside package `D1::D2` (or any
                                    // ancestor of it), also try the fully-qualified
                                    // forms by prepending each ancestor prefix.
                                    let cur = self.current_package().to_string();
                                    if cur.is_empty() || cur == "GLOBAL" {
                                        return None;
                                    }
                                    let (sigil, bare) = if let Some(rest) = name.strip_prefix('$') {
                                        ("$", rest)
                                    } else if let Some(rest) = name.strip_prefix('@') {
                                        ("@", rest)
                                    } else if let Some(rest) = name.strip_prefix('%') {
                                        ("%", rest)
                                    } else if let Some(rest) = name.strip_prefix('&') {
                                        ("&", rest)
                                    } else {
                                        ("", name)
                                    };
                                    // Walk up the current package, trying each prefix
                                    // joined with the requested name.
                                    let parts: Vec<&str> = cur.split("::").collect();
                                    for i in (0..=parts.len()).rev() {
                                        let prefix = parts[..i].join("::");
                                        let candidate = if prefix.is_empty() {
                                            format!("{sigil}{bare}")
                                        } else {
                                            format!("{sigil}{prefix}::{bare}")
                                        };
                                        if candidate == name {
                                            continue;
                                        }
                                        if let Some(v) = self.get_our_var(&candidate).cloned() {
                                            return Some(v);
                                        }
                                        if let Some(v) = self.get_env_with_main_alias(&candidate) {
                                            return Some(v);
                                        }
                                    }
                                    None
                                })
                        } else {
                            None
                        }
                    })
                    .or_else(|| {
                        // Outer-lexical fallback: when a package-qualified name
                        // (e.g. `A::x`) is not found in any package store, fall
                        // back to looking up just the bare component (`x`) in env.
                        // This handles class body statements that access outer
                        // lexical variables which are stored in env under their
                        // unqualified names (not as `A::x`).
                        if !name.contains("::") {
                            return None;
                        }
                        // Only apply when the qualifier matches the current package
                        // (i.e. the name was auto-qualified by the compiler, not
                        // explicitly written as a package-qualified access).
                        let cur = self.current_package().to_string();
                        if cur.is_empty() || cur == "GLOBAL" {
                            return None;
                        }
                        // Extract bare component after the last `::`
                        let bare = if let Some(pos) = name.rfind("::") {
                            &name[pos + 2..]
                        } else {
                            return None;
                        };
                        if bare.is_empty() {
                            return None;
                        }
                        self.get_env_with_main_alias(bare)
                    })
                    // Bare-name package-chain fallback: `our` variables and
                    // package-block `my` lexicals of the enclosing package
                    // chain (a named sub's `current_package` is its package; a
                    // method's is its owner class, whose qualified name walks
                    // up to the enclosing module). See
                    // `package_chain_var_fallback`.
                    .or_else(|| self.package_chain_var_fallback(name))
                    // Anonymous state variable (`$`): fall back to persisted
                    // state so the value survives across closure calls.
                    .or_else(|| self.anon_state_value(name))
                    // `$0`/`$1`/... are `$/[0]`/`$/[1]`/...  A successful match
                    // exports each positional capture as its own digit env key,
                    // but a directly bound/assigned `$/` (`my $/ := "foobar"`)
                    // has none — derive the value by indexing the current `$/`,
                    // matching Raku's `$0 == $/[0]` for any object (a non-Match
                    // scalar self-indexes: `.[0]` is the value, `.[N>0]` is Nil).
                    .or_else(|| {
                        if name.is_empty() || !name.bytes().all(|b| b.is_ascii_digit()) {
                            return None;
                        }
                        let slash = self.get_env_with_main_alias("/")?;
                        if slash.is_nil() {
                            return None;
                        }
                        let i: usize = name.parse().ok()?;
                        Some(Self::bound_slash_positional(&slash, i))
                    })
                    // A file-scope lexical of the module the running routine
                    // belongs to. A module body executes in the env of whatever
                    // frame loaded it, so that binding dies with the frame (a
                    // `require` inside a method); `module_scope_lexicals` keeps it
                    // attached to the module. Last resort, after every live store.
                    .or_else(|| self.module_scope_lexical(name).cloned())
                    .map(Ok)
                    .unwrap_or_else(|| {
                        if name.starts_with('^') {
                            Ok(Value::TRUE)
                        } else if name == "self" || name.ends_with("::self") {
                            Err(RuntimeError::new(
                                "'self' used where no object is available".to_string(),
                            ))
                        } else if name.starts_with('!')
                            && name.len() > 1
                            && name[1..]
                                .chars()
                                .next()
                                .is_some_and(|c| c.is_alphanumeric() || c == '_')
                        {
                            if self.get_env_with_main_alias("self").is_some() {
                                Ok(Value::NIL)
                            } else {
                                Err(RuntimeError::new(format!(
                                    "Variable $!{} used where no 'self' is available",
                                    &name[1..]
                                )))
                            }
                        } else if name == "_" {
                            // An UNSET topic (no `$_` entry in any scope) reads
                            // as Any, not Nil (`$_ === Any` at the top level,
                            // S02-types/nil.t 39). Only the not-found fallback:
                            // a topic explicitly set to Nil (e.g. `Xorelse`
                            // topicalizing a Nil operand) must stay Nil.
                            Ok(Value::package(Symbol::intern("Any")))
                        } else if name.starts_with("__ANON_STATE_") {
                            // An anonymous scalar (`$`) is a declared but
                            // uninitialized scalar: it reads as the Any type
                            // object, like `my $x` (S03-operators/context.t).
                            Ok(Value::package(Symbol::intern("Any")))
                        } else if self.strict_mode && !Self::strict_read_exempt(name) {
                            // Read-side counterpart of the `SetGlobal` write
                            // check above: a plain scalar name that resolved
                            // through NONE of the real stores tried above
                            // (env, unit/package/module lexicals, `our`-vars,
                            // per-call state, ...) is genuinely undeclared —
                            // `use strict` must reject it instead of silently
                            // yielding Nil (`my $x = $y;` under `use strict`).
                            // `strict_read_exempt` carves out the pseudo-
                            // variable / dynamic-scope shapes `GetGlobal`
                            // also carries that are not ordinary lexicals.
                            Err(self.strict_undeclared_error(name))
                        } else {
                            Ok(Value::NIL)
                        }
                    })?;
                // `OUR::`-qualified variable reads are scoped to the CURRENT
                // package (`$OUR::x` inside `package A {}` is `A::x`, bare `x`
                // at file scope). Authoritative: a miss is an undefined package
                // variable, so it overrides any same-named GLOBAL `our` the
                // generic chain above may have leaked (`our_pseudo_var_read`
                // returns None for non-`OUR::` names, leaving `val` untouched).
                let val = self.our_pseudo_var_read(name).unwrap_or(val);
                // When the value is Nil and the variable has a type constraint,
                // return the type object (consistent with GetLocal behavior).
                let val = if val.is_nil() {
                    if let Some(def) = self.var_default(name) {
                        def.clone()
                    // Global-map-only on purpose: an env-scoped constraint (a
                    // typed param / `SetVarTypeScoped` lexical) must not turn a
                    // genuinely-Nil read (a `= Nil` param default) into the
                    // type object; see the matching comment in
                    // `vm_var_assign_local_get.rs`.
                    } else if let Some(constraint) = self.var_type_constraint_fast(name).cloned() {
                        let nominal =
                            loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                        Value::package(Symbol::intern(&nominal))
                    } else {
                        val
                    }
                } else {
                    val
                };
                // Force lazy thunks transparently on access. Tag-probed first: a
                // `view()` would materialize a lazy Match (see `exec_get_local_op`).
                let val = if val.is_lazy_thunk_value()
                    && let ValueView::LazyThunk(thunk_data) = val.view()
                {
                    self.force_lazy_thunk(&thunk_data)?
                } else {
                    val
                };
                // Auto-deref ContainerRef for stack use (ContainerRef axis of the
                // decont family; moves through for the common non-container case).
                let val = val.into_deref();
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetSelfOrNoSelf(name_idx) => {
                // Load `self` for a `$.attr` accessor from the captured env.
                if let Some(self_val) = self.get_env_with_main_alias("self") {
                    self.stack.push(self_val);
                    *ip += 1;
                } else {
                    // No enclosing method/submethod: X::Syntax::NoSelf.
                    let variable = Self::const_str(code, *name_idx).to_string();
                    let message =
                        format!("Variable {} used where no 'self' is available", variable);
                    let mut err = RuntimeError::new(message.clone());
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("variable".to_string(), Value::str(variable));
                    attrs.insert("message".to_string(), Value::str(message));
                    err.exception = Some(Box::new(Value::make_instance(
                        Symbol::intern("X::Syntax::NoSelf"),
                        attrs,
                    )));
                    return Err(err);
                }
            }
            OpCode::GetArrayVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                // Reject @!attr (private attribute twigil) when no self is available
                if let Some(bare) = name.strip_prefix("@!")
                    && !bare.is_empty()
                    && bare.as_bytes()[0].is_ascii_alphabetic()
                    && self.get_env_with_main_alias("self").is_none()
                {
                    return Err(RuntimeError::new(format!(
                        "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                        name
                    )));
                }
                // Phase 3 Stage 2b: array attributes (`@!a`/`@.a`) read straight
                // from `self`'s shared cell so a mutation in a nested method frame
                // is visible here.
                if let Some(cell_val) = self.read_self_attr_cell(name) {
                    let val = match cell_val.view() {
                        ValueView::Hash(map) => Value::real_array(
                            map.iter()
                                .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                                .collect(),
                        ),
                        _ => cell_val,
                    };
                    self.stack.push(val);
                    *ip += 1;
                    return Ok(());
                }
                // A `:=`-bound gather may read the array currently being
                // produced. MakeGather tags that exact self-reference; expose
                // the live take collector instead of recursively forcing the
                // LazyList (or reading its pre-declaration env snapshot).
                if self
                    .env()
                    .get(&format!("__mutsu_gather_self_ref::{name}"))
                    .is_some()
                {
                    self.stack
                        .push(Value::real_array(self.current_gather_items()));
                    *ip += 1;
                    return Ok(());
                }
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        // Fallback: check bare name in env (for closures capturing params)
                        name.strip_prefix('@')
                            .and_then(|bare| self.env().get(bare).cloned())
                    })
                    .or_else(|| {
                        // Fallback for fast-path method dispatch (skip_env_setup=true):
                        // @.attr and @!attr are not set in env, so read directly from
                        // self's instance attributes when available.
                        let attr_name = name
                            .strip_prefix("@.")
                            .or_else(|| name.strip_prefix("@!"))?;
                        if attr_name.is_empty() {
                            return None;
                        }
                        let self_val = self.get_env_with_main_alias("self")?;
                        if let ValueView::Instance { attributes, .. } = self_val.view() {
                            attributes.as_map().get(attr_name).cloned()
                        } else {
                            None
                        }
                    })
                    // A file-scope `my @a` of the running routine's own module
                    // (see `module_scope_lexicals`; keys keep the `@` sigil).
                    .or_else(|| self.module_scope_lexical(name).cloned())
                    // Class-body outer-lexical fallback — see the GetHashVar twin.
                    .or_else(|| self.auto_qualified_bare_env_read(name))
                    .unwrap_or_else(|| {
                        // An undeclared `@`-sigil variable defaults to an empty
                        // Array (raku auto-declares it as Array under `no strict`):
                        // `@x[2]` is `(Any)`, `@x.end` is `-1`, `@x.raku` is `[]`.
                        // Anonymous `@`-sigil variables share this default.
                        Value::real_array(vec![])
                    });
                // A whole-container `:=` bind (`my @b := @a`) stores a shared
                // `ContainerRef` cell in the slot so both aliases observe
                // mutations. Decontainerize the top-level cell here so the read
                // yields the inner Array/Hash (the cell is a binding alias, not
                // an array element). Element-level cells are handled at Index.
                let val = val.into_deref();
                // When @-sigil dereferences a Hash, convert to a list of pairs
                let val = match val.view() {
                    // An `@`-sigil read strips the Scalar container: `@$x` on an
                    // itemized `$x = [1,2,3]` yields the plain Array (flattens /
                    // iterates element-wise).
                    ValueView::Array(items, kind) if kind.is_itemized() => {
                        Value::array_with_kind(items.clone(), kind.decontainerize())
                    }
                    ValueView::Hash(map) => {
                        let pairs: Vec<Value> = map
                            .iter()
                            .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                            .collect();
                        Value::real_array(pairs)
                    }
                    // NOTE: a Buf/Blob is deliberately NOT unwrapped to its
                    // element list here. `@$blob` lowers to `$blob.list` (see
                    // `@$x` in the parser), so it never reaches this opcode;
                    // what does reach it is a genuine `@`-sigil variable whose
                    // container IS a Buf (`my @a is Buf`), and there `@a` must
                    // stay the Buf itself so `@a ~~ Buf` holds (S02-types/is-type.t).
                    // Array-contextualizing a Seq (`@$s`) caches it, so it may be
                    // read repeatedly. If the Seq's iterator was already taken
                    // (e.g. by `.skip`/`.iterator`) and not cached, throw.
                    ValueView::Seq(items) => {
                        if items.is_consumed() && !items.is_cached() {
                            return Err(crate::value::seq_consumed_error());
                        }
                        items.mark_cache_requested();
                        val
                    }
                    _ => val,
                };
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetHashVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                // Reject %!attr (private attribute twigil) when no self is available
                if let Some(bare) = name.strip_prefix("%!")
                    && !bare.is_empty()
                    && bare.as_bytes()[0].is_ascii_alphabetic()
                    && self.get_env_with_main_alias("self").is_none()
                {
                    return Err(RuntimeError::new(format!(
                        "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                        name
                    )));
                }
                // %?RESOURCES — build from the current package's distribution context
                if name == "%?RESOURCES" {
                    let resources = self.build_resources_for_package();
                    self.stack.push(resources);
                    *ip += 1;
                    return Ok(());
                }
                // Phase 3 Stage 2b: hash attributes (`%!h`/`%.h`) read straight
                // from `self`'s shared cell (cross-frame visibility).
                if let Some(cell_val) = self.read_self_attr_cell(name) {
                    self.stack.push(cell_val);
                    *ip += 1;
                    return Ok(());
                }
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        name.strip_prefix('%')
                            .and_then(|bare| self.env().get(bare).cloned())
                    })
                    .or_else(|| {
                        // Fallback for fast-path method dispatch (skip_env_setup=true):
                        // %.attr and %!attr are not set in env, so read directly from
                        // self's instance attributes when available.
                        let attr_name = name
                            .strip_prefix("%.")
                            .or_else(|| name.strip_prefix("%!"))?;
                        if attr_name.is_empty() {
                            return None;
                        }
                        let self_val = self.get_env_with_main_alias("self")?;
                        if let ValueView::Instance { attributes, .. } = self_val.view() {
                            attributes.as_map().get(attr_name).cloned()
                        } else {
                            None
                        }
                    })
                    // A file-scope `my %h` of the module the running routine
                    // belongs to (see `module_scope_lexicals`; the table keys
                    // keep the `%` sigil). Last resort, after every live store —
                    // mirrors the scalar fallback in `GetGlobal`.
                    .or_else(|| self.module_scope_lexical(name).cloned())
                    // Outer-lexical fallback, mirroring GetGlobal: each class-body
                    // statement compiles as its own chunk, so a read of a `my`
                    // declared by an earlier body statement was auto-qualified
                    // (`%C::predef`) while the declaration flushed to env under the
                    // bare sigiled name (`%predef`). Strip the qualifier and retry
                    // when it names the current package.
                    .or_else(|| self.auto_qualified_bare_env_read(name));
                match val {
                    // Decontainerize a top-level `ContainerRef` cell from a
                    // whole-container `:=` bind (`my %h2 := %h`); the read
                    // yields the inner Hash.
                    Some(v) => self.stack.push(v.into_deref()),
                    None => {
                        // %ENV (without * twigil) is not declared in Raku;
                        // only %*ENV is valid. Throw an undeclared error for %ENV specifically.
                        if name == "%ENV" {
                            return Err(RuntimeError::undeclared("name", "%ENV"));
                        }
                        // An undeclared `%`-sigil variable defaults to an empty
                        // Hash (raku auto-declares it as Hash under `no strict`):
                        // `%h<k>` is `(Any)`, `%h.raku` is `{}`. Anonymous
                        // `%`-sigil variables share this default.
                        self.stack
                            .push(Value::hash(std::collections::HashMap::new()));
                    }
                }
                *ip += 1;
            }
            OpCode::GetBareWord(name_idx) => {
                self.exec_get_bare_word_op(code, *name_idx, compiled_fns)?;
                // Slice F: a bareword that resolved to a qualified/`our` sub call
                // (`M::foo`) may have recorded captured-outer writes; drain them
                // through to this caller frame's local slots.
                self.apply_pending_rw_writeback(code);
                *ip += 1;
            }
            OpCode::GetPseudoStash(name_idx) => {
                self.exec_get_pseudo_stash_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::RoleGroupToCandidate => {
                self.exec_role_group_to_candidate_op();
                *ip += 1;
            }
            OpCode::PushLastRegisteredClass => {
                self.exec_push_last_registered_class_op();
                *ip += 1;
            }
            OpCode::PushLastRegisteredRole => {
                self.exec_push_last_registered_role_op();
                *ip += 1;
            }
            OpCode::GetOurVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .get_our_var(name)
                    .cloned()
                    .or_else(|| self.get_env_with_main_alias(name))
                    .unwrap_or(Value::NIL);
                // Auto-deref ContainerRef for stack use (ContainerRef axis of
                // the decont family — mirrors GetGlobal). `our_vars` can now
                // hold a `ContainerRef` cell for a plain scalar `our`
                // (`OpCode::DeclareOurScalar`); without this, an `our`
                // REDECLARATION with no initializer (`our $foo;` after `our
                // $foo = 3`, e.g. roast S04-declarations/our.t) — which reads
                // via GetOurVar and re-stores what it read — would push the
                // raw cell and re-store IT INTO ITSELF, making the cell hold a
                // `ContainerRef` pointing at itself: any later deref of that
                // cell locks its own Mutex twice on the same thread and hangs
                // forever.
                let val = val.into_deref();
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::CheckDynamicVarDeclared(name_idx) => {
                // A genuine assignment to a dynamic variable (`$*x = ...`) that is
                // not present in the dynamic scope throws X::Dynamic::NotFound
                // (Raku semantics — a dynamic var must be declared with `my $*x`
                // first). Built-in dynamic vars (`$*OUT`, `$*CWD`, ...) are seeded
                // into env and a caller's `my $*x` propagates into the callee env,
                // so this only fires for a never-declared dynamic variable.
                let name = Self::const_str(code, *name_idx);
                if !self.env().contains_key(name) && !self.is_var_dynamic(name) {
                    let display = if name.starts_with(['@', '%', '&']) {
                        name.to_string()
                    } else {
                        format!("${}", name)
                    };
                    return Err(runtime::utils::dynamic_not_found_error(&display));
                }
                *ip += 1;
            }
            OpCode::SetGlobalRaw(name_idx) | OpCode::SetGlobal(name_idx) => {
                let raw_mode = matches!(code.ops[*ip], OpCode::SetGlobalRaw(_));
                let is_bind_ctx = self.bind_context.get();
                let is_rebind = self.rebind_context.get();
                self.bind_context.set(false);
                // Consume the scalar-bind marker here too: a topic bind
                // (`$_ := $d`) compiles MarkScalarBindContext + SetGlobal, so
                // without this the flag leaks into the NEXT SetLocal (e.g. a
                // following `my $a = 0`), which would spuriously treat it as a
                // value-bind and mark it readonly.
                let was_scalar_bind = self.scalar_bind_context.get();
                self.scalar_bind_context.set(false);
                // A sigilless-target bind (`-> \v` loop-param bind stmts):
                // skip itemization only, no other bind semantics.
                let was_param_raw_bind = self.param_raw_bind_context.get();
                self.param_raw_bind_context.set(false);
                // Slice 2a: `our $n = @z` / a global scalar target reaches SetGlobal,
                // not SetLocal/AssignExpr. Consume the array-share flag here (the
                // global copies for now — reference sharing for globals is Slice 2d)
                // so it cannot leak into the next SetLocal.
                self.array_share_context.set(false);
                self.array_share_source.set(None);
                // Only clear rebind_context if this is actually a binding operation
                if is_rebind {
                    self.rebind_context.set(false);
                }
                let name_str = match code.constants[*name_idx as usize].as_str() {
                    Some(s) => s,
                    None => unreachable!("SetGlobal name must be a string constant"),
                };
                // Fast path for the anonymous state scalar (`$` and `$.` desugaring).
                // `__ANON_STATE__` is a synthetic internal name that can never be a
                // private attribute, package/class, sigilless-bound alias, or strict-
                // undeclared symbol, so the heavy general store path below (including
                // the O(env) reverse-alias scan, shared-var sync, and our-var store)
                // is unnecessary. This is extremely hot in tight loops assigning to `$`.
                // The remaining special cases (typed/readonly anon scalar, fatal-mode
                // Failure explosion, `:=` container write-through, capture RHS) are
                // excluded by the guards and fall through to the general path.
                // ADR-0042 slice 1: route the constraint check through
                // `element_constraint_for` (container-embedded metadata
                // first, name-keyed map as fallback) like the other mutation
                // chokepoints in this ADR's table, for mechanical
                // consistency. `__ANON_STATE__` is always a scalar, so this
                // falls straight through to the name-keyed lookup — the
                // scalar cell-carried `of` is ADR-0042 slice 2. The name
                // comparison stays first so a non-`__ANON_STATE__` SetGlobal
                // (the overwhelming majority) never pays the env lookup.
                if name_str == "__ANON_STATE__"
                    && !raw_mode
                    && !is_rebind
                    && !self.fatal_mode
                    && {
                        let anon_state_val =
                            self.env().get(name_str).cloned().unwrap_or(Value::NIL);
                        self.element_constraint_for(name_str, &anon_state_val)
                            .is_none()
                    }
                    && !self.is_readonly(name_str)
                    // A `VarRef` RHS (a `:=` bind of `$` to a variable) must reach
                    // the general path, which is where the wrapper is unwrapped and
                    // its `bind_source` recorded; storing the wrapper itself into the
                    // env slot would corrupt `$`. It used to be caught by the
                    // `Capture` arm here, back when a varref *was* a `Capture`.
                    && !matches!(
                        self.stack.last().map(Value::view),
                        Some(ValueView::Capture { .. } | ValueView::VarRef { .. })
                    )
                    && !matches!(
                        self.env().get(name_str).map(Value::view),
                        Some(ValueView::ContainerRef(_))
                    )
                {
                    let val = self.stack.pop().unwrap_or(Value::NIL);
                    // Preserve `$` state persistence across closure calls.
                    self.sync_anon_state_value("__ANON_STATE__", &val);
                    let sym = Symbol::intern("__ANON_STATE__");
                    if let Some(slot) = self.env_mut().get_mut_sym(sym) {
                        *slot = val;
                    } else {
                        self.env_mut().insert_sym(sym, val);
                    }
                    *ip += 1;
                    return Ok(());
                }
                let mut name = name_str.to_string();
                // Outer-lexical write fallback (symmetric with the GetGlobal read
                // fallback above): the compiler auto-qualifies a bare free variable
                // with the current package (`$x` inside `grammar G { ... }` compiles
                // to `SetGlobal("G::x")`). When that qualified name is NOT a real
                // package/`our` variable but the BARE name IS a captured lexical in
                // env (an outer `my $x` the block closes over — e.g. an embedded
                // `regex TOP { x { $x = 42 } }` mutating a top-level lexical),
                // redirect the write to the bare lexical so the outer variable is
                // updated in place instead of stranding a stray `G::x` package var.
                // Confined to embedded regex code blocks so ordinary `our`/
                // package-qualified writes are never redirected.
                if self.in_regex_code_block
                    && !is_rebind
                    && let Some(pos) = name.rfind("::")
                {
                    // Split an optional leading sigil, then `Qualifier::tail`.
                    let sigil = match name.as_bytes().first().copied() {
                        Some(b @ (b'$' | b'@' | b'%' | b'&')) => Some(b as char),
                        _ => None,
                    };
                    let sig_len = sigil.map(|_| 1).unwrap_or(0);
                    let qualifier = &name[sig_len..pos];
                    let bare_after = &name[pos + 2..];
                    if !qualifier.is_empty() && !bare_after.is_empty() && !bare_after.contains("::")
                    {
                        let cur = self.current_package();
                        let bare = match sigil {
                            Some(s) => format!("{s}{bare_after}"),
                            None => bare_after.to_string(),
                        };
                        if !cur.is_empty()
                            && cur != "GLOBAL"
                            && qualifier == cur
                            && self.get_our_var(&name).is_none()
                            && !self.env().contains_key(&name)
                            && self.env().contains_key(&bare)
                        {
                            name = bare;
                        }
                    }
                }
                // A `%h = ...` / `@a = ...` where the env slot holds a *tied*
                // instance (`my %h is Foo` closed over into a block, so the store
                // reaches SetGlobal instead of a local slot) must route through the
                // class's STORE, exactly like the local-slot path. Statement context
                // — `maybe_tied_store_reassign_named` leaves the bound instance on
                // the stack, so discard it after routing.
                if !raw_mode
                    && !is_bind_ctx
                    && !is_rebind
                    && (name.starts_with('%') || name.starts_with('@'))
                    && self.maybe_tied_store_reassign_named(&name)?.is_some()
                {
                    self.stack.pop();
                    *ip += 1;
                    return Ok(());
                }
                // Reject private attribute twigil (!) assignment when no self is available
                {
                    let bare = name.trim_start_matches(['$', '@', '%', '&']);
                    if bare.starts_with('!')
                        && bare.len() > 1
                        && bare.as_bytes()[1].is_ascii_alphabetic()
                        && self.get_env_with_main_alias("self").is_none()
                    {
                        // Reconstruct the display name with sigil
                        let display = if name.starts_with('!') {
                            format!("${}", name)
                        } else {
                            name.clone()
                        };
                        return Err(RuntimeError::new(format!(
                            "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                            display
                        )));
                    }
                }
                // Attribute twigils (`$!x`/`@!x`/`%!x`/`$.x`/`@.x`/`%.x`) are not
                // lexical variables — they are attribute accesses declared by the
                // enclosing class and stored through the self-attribute cell, not
                // `env`. `use strict` must not flag them as undeclared (it would
                // wrongly reject e.g. `%!types := %types.Map` in a class method
                // whenever strict was switched on by an outer module — MIME::Types
                // loaded transitively under Humming-Bird's `use strict`).
                let bare_no_sigil = name.trim_start_matches(['$', '@', '%', '&']);
                let is_attr_twigil = (bare_no_sigil.starts_with('!')
                    || bare_no_sigil.starts_with('.'))
                    && bare_no_sigil.len() > 1
                    && bare_no_sigil.as_bytes()[1].is_ascii_alphabetic();
                // Synthetic compiler temporaries (rw index/argument desugaring,
                // `with`/`without` topic temps `__with_tmp_*`, for-loop element
                // sources, constant hoists, `__mutsu_*`, ...) are all named with a
                // leading `__` and are stored straight into env at runtime — they
                // are never user-declared, so `use strict` must skip them too.
                let is_internal_temp = bare_no_sigil.starts_with("__");
                // A store arriving with `vardecl_context` set IS the declaration
                // (an expression-position `my` — `ok my $x = 7, 'desc'` — compiles
                // to MarkVarDeclContext + SetGlobal when no local slot exists), so
                // `use strict` must not reject it as an undeclared write.
                // A file-scope `my` of the running routine's own compunit lives in
                // `unit_lexicals`, not `env` (that is the store's purpose — see
                // `unit_scope_lexical`), so an `env`-only test reports a module
                // writing its own module-level lexical as undeclared. It is
                // declared; the write below goes to the same store.
                if self.strict_mode
                    && !self.vardecl_context.get()
                    && !is_attr_twigil
                    && !is_internal_temp
                    && !name.contains("::")
                    && !self.env().contains_key(&name)
                    && !self.has_unit_scope_lexical(&name)
                    && !code.param_bind_names.iter().any(|n| n == &name)
                {
                    return Err(self.strict_undeclared_error(&name));
                }
                // Check readonly variables (e.g., $*USAGE).
                // Skip readonly check for SetGlobalRaw which is used for constant
                // declarations — the constant will be re-marked readonly after this.
                // Also skip during a `:=` bind: an `our` container bind (`our %g := %h`)
                // marks the var readonly as a bind signal, not a true RO restriction.
                // Likewise skip for a `:=`-bound container reached by name (e.g. a
                // captured `%b` whole-reassigned inside a closure): it carries the
                // `__mutsu_bound::` marker and writes through to the bound source,
                // unlike a genuinely immutable `constant`. The slot-based path uses
                // CheckReadOnly for the same exemption (vm.rs CheckReadOnly).
                // Note: an immutable Set/Bag/Mix bound via `:=` (`my %m := mix <a b>`)
                // is genuinely read-only — whole-reassignment must still throw
                // X::Assignment::RO — so the exemption applies only to mutable
                // Hash/Array bound containers.
                let is_bound_container = name.starts_with(['@', '%'])
                    && matches!(
                        self.env()
                            .get(&format!("__mutsu_bound::{}", name))
                            .map(Value::view),
                        Some(ValueView::Bool(true))
                    )
                    && !matches!(
                        self.env().get(&name).map(Value::view),
                        Some(
                            ValueView::Mix(_, false)
                                | ValueView::Set(_, false)
                                | ValueView::Bag(_, false)
                        )
                    );
                // A store with `vardecl_context` set IS a declaration (an
                // expression-position `my` — `if (my $str = ...)` — compiles to
                // MarkVarDeclContext + SetGlobal): it creates a FRESH variable,
                // so a readonly marker left by a same-named CALLER binding (a
                // method's readonly parameter `$str`) must not reject it, and
                // the new variable is writable — unmark (journaled, so the
                // caller's mark is restored when this frame exits).
                // (Text::IO::String's `print` declaring `my Str $str` while
                // called from `new (Str $str!)`.)
                if !raw_mode && !is_bind_ctx && !is_bound_container {
                    if self.vardecl_context.get() {
                        self.unmark_readonly(&name);
                    } else {
                        self.check_readonly_for_modify(&name)?;
                    }
                } else if raw_mode {
                    // Clear any previous readonly marking so this constant
                    // redeclaration can proceed (e.g., `constant sym` followed
                    // by `constant $sym` which share the same env name).
                    let bare = name
                        .rsplit("::")
                        .next()
                        .unwrap_or(&name)
                        .trim_start_matches(['$', '@', '%', '&']);
                    self.unmark_readonly(bare);
                }
                // Prevent re-assignment of immutable containers (Mix, Set, Bag)
                // Only when the variable has an explicit immutable type constraint
                // (e.g., `my %h is Mix`), not for regular scalar variables holding
                // an immutable value.
                if let Some(constraint) = loan_env!(self, var_type_constraint(&name)) {
                    let base = constraint.split('[').next().unwrap_or(&constraint);
                    if matches!(base, "Mix" | "Set" | "Bag")
                        && let Some(existing) = self.env().get(&name)
                        && matches!(
                            existing.view(),
                            ValueView::Mix(_, false)
                                | ValueView::Set(_, false)
                                | ValueView::Bag(_, false)
                        )
                    {
                        let type_name = match existing.view() {
                            ValueView::Mix(..) => "Mix",
                            ValueView::Set(..) => "Set",
                            ValueView::Bag(..) => "Bag",
                            _ => unreachable!(),
                        };
                        return Err(RuntimeError::new(format!(
                            "Cannot modify an immutable {} ({})",
                            type_name,
                            existing.to_string_value()
                        )));
                    }
                }
                // Reject assignment to immutable type objects (e.g., `Foo .= new`),
                // the bare `Nil` term, and an enum value (`enum Fo <A B>; A = 3`).
                // A `constant` DECLARATION (`raw_mode`) is exempt: it binds the
                // name rather than modifying whatever the name currently means,
                // so `constant Int = 5` shadows the builtin (raku prints 5) and
                // `our Mu constant D = Metamodel::ClassHOW.new_type(:name<D>)`
                // -- the documented manual-MOP idiom, whose RHS registers a
                // class literally named `D` before the binding runs -- is legal.
                //
                // These three are one mechanism, not three special cases: each is
                // a bareword whose name denotes an immutable VALUE rather than a
                // container (X::Assignment::RO, per the readonly-assign-exception
                // taxonomy's rule 3), and the check was previously scoped to
                // user-declared classes only (`self.has_class`), silently
                // no-opping on a builtin type (`Int = 5`), `Nil = 5`, and an enum
                // value. It also used to throw an untyped `X::AdHoc` instead of
                // `X::Assignment::RO` even for the user-class case it did catch.
                // `_` is excluded even though it carries no sigil in its own
                // storage key: it is the TOPIC's env key (`$_`, never
                // slot-allocated -- it is dynamically resolved via env by
                // design, unlike an ordinary `my`/`our` variable, which either
                // gets a local slot or keeps its own twigil in its key), and a
                // bare `_` is never a valid Raku term at all (rejected at
                // `exec_get_bare_word_op`'s very first check). So whenever a
                // regular variable's CURRENT value happens to be a copy of an
                // enum member or type object (`$_ = $state` where `$state`
                // holds an enum value from an earlier iteration), only `$_`'s
                // reassignment can reach this bareword-shaped check at all --
                // and it must never be treated as reassigning that term.
                if !raw_mode
                    && name != "_"
                    && !name.starts_with('$')
                    && !name.starts_with('@')
                    && !name.starts_with('%')
                    && !name.starts_with('&')
                    && !name.contains("::")
                {
                    // A bareword that has never been (re)bound to something
                    // else still resolves to the type object it names (an
                    // unreferenced builtin type is never actually stored in
                    // env, and a pre-seeded slot holds `Nil` -- both mirror
                    // the fallback `exec_get_bare_word_op` uses for reads).
                    // Anything else (e.g. a sigilless `\Int := 5` shadow)
                    // means the name was rebound to a real value and must NOT
                    // be treated as the type object anymore.
                    //
                    // The "never referenced" (env has no entry at all) AND the
                    // "pre-seeded Nil slot" cases are trustworthy ONLY for a
                    // TitleCase name. A lowercase bareword reaching `SetGlobal`
                    // with `None`/`Nil` currently stored is overwhelmingly more
                    // likely to be an ordinary variable's write than a genuine
                    // reference to a lowercase native-type synonym (`int`,
                    // `str`, `num`, `array`, `bool`, ...) -- `str = 5` as a bare
                    // statement is not idiomatic Raku, while `$str`/`$int` are
                    // extremely common variable names whose sigil-stripped
                    // storage key is indistinguishable from the type name at
                    // this point. TWO separate shapes hit this: a `for`-loop
                    // sub-signature destructure leaf (`for @tests -> ($str,
                    // $expected, |args) {...}`, roast S32-str/comb.t), which is
                    // bound directly by `SetGlobal` rather than a local slot,
                    // so its first-ever write sees `None`; and an uninitialized
                    // outer `my $str;` (Nil) captured and assigned INSIDE a
                    // closure (`my $str; lives-ok { $str = 1 }, "..."` --
                    // `Test`'s `lives-ok` catches the resulting spurious
                    // X::Assignment::RO and reports a false test failure,
                    // caught by `t/immutable-lvalue-assignment-gaps.t`'s
                    // regression control), whose free-variable write also
                    // reaches `SetGlobal` and sees the captured `Some(Nil)`.
                    // A TitleCase name (`Int`, `Nil`, a user class) has no such
                    // realistic collision — Raku convention never uses a
                    // TitleCase bareword as an ordinary variable's storage key
                    // -- so the check requires it for those shapes; only a
                    // REAL `Package(SomeType)` current value (set exclusively
                    // by genuine class/type registration, e.g. `class Foo {}`
                    // sets env["Foo"] = Package("Foo") directly) is trusted
                    // unconditionally.
                    //
                    // `Package(Any)` is NOT that -- it is a generic "not yet
                    // materialized" placeholder `SetVarDynamic` pre-seeds for
                    // ANY closure-captured variable regardless of its name
                    // (see the matching special case in
                    // `exec_get_bare_word_op`, "A `my $Buf = Buf.new`
                    // declaration pre-seeds env[\"Buf\"] with the placeholder
                    // `Package(Any)`"), so it needs the SAME uppercase gate as
                    // `None`/a genuine `Nil` slot: `my $str; lives-ok { $str =
                    // 1 }, "..."` captures the outer, not-yet-assigned `$str`
                    // this way, and without the gate its free-variable write
                    // was misidentified as assigning to the lowercase native
                    // type `str` (a spurious `X::Assignment::RO` that
                    // `lives-ok` correctly caught and reported as a test
                    // failure, even though nothing in the block actually
                    // "died").
                    let current_view = self.env().get(&name).map(Value::view);
                    let first_letter_uppercase = name.starts_with(|c: char| c.is_uppercase());
                    let unbound_type_slot = match current_view {
                        Some(ValueView::Package(p)) if p == "Any" && name != "Any" => {
                            first_letter_uppercase
                        }
                        Some(ValueView::Package(_)) => true,
                        Some(ValueView::Nil) | None => first_letter_uppercase,
                        _ => false,
                    };
                    if unbound_type_slot && name == "Nil" {
                        return Err(RuntimeError::assignment_ro_nil());
                    }
                    if unbound_type_slot && (self.has_class(&name) || Self::is_builtin_type(&name))
                    {
                        return Err(RuntimeError::assignment_ro_type_object(&name));
                    }
                    // A GENUINE enum-constant reassignment (`Red = 5`) writes to
                    // the bareword global that IS the constant's own binding, so
                    // its name equals the currently-stored member's own `key`
                    // (`env["Red"] == Enum { key: "Red", .. }`). Without the
                    // `name == key` check this also fired for an ORDINARY
                    // variable that merely holds an enum value transiently --
                    // e.g. a for-loop's second `.kv` param (`for %h.kv -> $k, $v
                    // {...}`) rebinding `$v` via `SetGlobal("v", ...)` when the
                    // slot's PREVIOUS content (from the prior iteration) happened
                    // to be an Enum member: `env.get("v")` returned
                    // `Enum{key:"Red",..}` from iteration 1, and the unguarded
                    // check misread iteration 2's ordinary rebind as "assigning
                    // over the `Red` constant", raising a spurious
                    // X::Assignment::RO (`roast/S12-enums/misc.t`'s
                    // `X::Enum::NoValue` throws-like case, only reachable once a
                    // hash's random iteration order put an enum value before a
                    // later key).
                    if let Some(ValueView::Enum { enum_type, key, .. }) = current_view
                        && name == key.resolve()
                    {
                        return Err(RuntimeError::assignment_ro_typename(
                            &enum_type.resolve(),
                            &key.resolve(),
                        ));
                    }
                }
                let raw_val = self.stack.pop().unwrap_or(Value::NIL);
                // The compiler's own resolution of a `:=` bind source (see
                // `bind_source_is_own_frame_lexical`), kept before the wrapper
                // is stripped.
                let bind_source_slot = raw_val.varref_slot();
                let (raw_val, bind_source) = match raw_val.as_varref() {
                    Some((source_name, inner, _)) => (inner.clone(), Some(source_name.resolve())),
                    None => (raw_val, None),
                };
                let mut val = if raw_mode && name.starts_with('@') {
                    // Constants with @ sigil coerce to List (not Array).
                    // `constant @x = 42` gives `(42,)`, not `[42]`.
                    // Explicit Arrays ([1,2,3]) are preserved.
                    // Instance objects that do Positional are kept as-is
                    // (they already went through CoerceToList).
                    match raw_val.view() {
                        ValueView::Array(_, kind) if kind.is_real_array() => raw_val,
                        ValueView::Array(items, _) => {
                            Value::array_with_kind(items.clone(), crate::value::ArrayKind::List)
                        }
                        ValueView::Instance { class_name, .. } => {
                            let cn = class_name.resolve();
                            let does_positional = matches!(
                                cn.as_str(),
                                "Array"
                                    | "List"
                                    | "Slip"
                                    | "Seq"
                                    | "Range"
                                    | "Buf"
                                    | "Blob"
                                    | "utf8"
                                    | "buf8"
                                    | "buf16"
                                    | "buf32"
                            ) || self
                                .class_composed_roles(&cn)
                                .is_some_and(|roles| roles.iter().any(|r| r == "Positional"));
                            if does_positional {
                                raw_val
                            } else {
                                Value::array_with_kind(
                                    crate::gc::Gc::new(crate::value::ArrayData::new(vec![raw_val])),
                                    crate::value::ArrayKind::List,
                                )
                            }
                        }
                        // `CoerceToList` already decided a lazy list stays lazy
                        // (an infinite `constant @primes = grep …` cannot be
                        // reified); re-wrapping it here would undo that.
                        ValueView::LazyList(_) | ValueView::Seq(_) => raw_val,
                        _ => Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(vec![raw_val])),
                            crate::value::ArrayKind::List,
                        ),
                    }
                } else if raw_mode && name.starts_with('%') {
                    // `constant %x` coerces non-Associative values to Map.
                    self.coerce_constant_hash_value(&name, raw_val)?
                } else if raw_mode {
                    raw_val
                } else if name.starts_with('%') {
                    // Apply quant-hash (SetHash/BagHash/MixHash) coercion first
                    // so that typed container assignment sees a Set/Bag/Mix,
                    // not a Hash with element-level type errors.
                    self.coerce_hash_var_value(&name, raw_val)?
                } else if name.starts_with('@') {
                    if is_bind_ctx || is_rebind {
                        // `:=` bind (e.g. `@!attr := @x.List`, `our @a := ...`)
                        // preserves the container type instead of copying into
                        // a fresh Array — same semantics as the SetLocal path.
                        self.bind_positional_value(&name, &raw_val)?
                    } else if raw_val.is_nil() {
                        // `@!attr = Nil` (a private attribute twigil reaches
                        // the store by name, not by slot) resets to the
                        // outgoing container's own `is default(...)` -- see
                        // `array_assign_nil_container_default`.
                        let old = self.env().get(&name).cloned().unwrap_or(Value::NIL);
                        let coerced = runtime::coerce_to_array(raw_val);
                        self.array_assign_nil_container_default(&name, &old, coerced)
                    } else {
                        runtime::coerce_to_array(raw_val)
                    }
                } else if !is_bind_ctx
                    && !is_rebind
                    && !was_scalar_bind
                    && !was_param_raw_bind
                    && bind_source.is_none()
                    && !is_internal_temp
                {
                    // A plain `=` into a `$` scalar reached by name (for-loop
                    // multi-param binds, `our $x`, closure-captured scalars)
                    // installs a Scalar container, exactly like the SetLocal
                    // path: itemize the stored aggregate so `.raku` shows
                    // `$[...]` and list context sees ONE element. Binds (`:=`),
                    // rebinds, and internal `__*` temporaries (for-loop element
                    // sources, `with` topic temps) keep the raw value — an
                    // itemized loop source would iterate as a single item.
                    Self::itemize_scalar_store(&name, raw_val)
                } else {
                    raw_val
                };
                if raw_mode {
                    // `constant @x = ...` / `constant %x = ...` already applied
                    // their own List/Map coercion above (raw_mode's own `@`/`%`
                    // branches) — a fresh `constant` declaration is never a
                    // write into an existing container, so neither the typed
                    // re-coercion nor the writethrough metadata preservation
                    // below apply. Running them anyway (verified 2026-08-15)
                    // called `array_container_writethrough_value` on an
                    // already-correct `does Positional` instance, whose
                    // non-Array input falls through to a generic
                    // `coerce_to_array` wrap and loses the custom class.
                } else if name.starts_with('%')
                    && (loan_env!(self, var_type_constraint(&name)).is_some()
                        || loan_env!(self, var_hash_key_constraint(&name)).is_some())
                {
                    val = self.coerce_typed_container_assignment(&name, val, false)?;
                } else if name.starts_with('@') && name.len() > 1 && !name.contains("__") {
                    // `@a = list` reached by name (a closure/nested-sub write to
                    // a captured free var, `our @a`, a for-loop multi-param
                    // bind, ...): the assignment writes INTO whatever container
                    // `@a` already is, so its declared/inherited element type
                    // (`array[int]`, `Array[Int]`) must survive rather than
                    // collapsing to a plain `Array`. `array_container_writethrough_value`
                    // is the same helper the SetLocal ContainerRef writethrough
                    // path uses for the identical scenario (`my @b := @a; @a =
                    // ...`): it re-coerces elements to the declared/inherited
                    // element type and stamps the result with the matching
                    // `value_type`/`declared_type` metadata, whether that type
                    // comes from a `var_type_constraint` (`my int @a`) or from
                    // the container currently bound to the name (a bare `for
                    // @src -> @a { }` alias, no declared constraint of its
                    // own). Internal/anonymous names (`@__ANON_ARRAY__`,
                    // `@__mutsu_*`) are excluded: they are fresh per use and
                    // must not inherit a stale slot's type.
                    let old = self.get_env_with_main_alias(&name).unwrap_or(Value::NIL);
                    val = self.array_container_writethrough_value(&name, val, &old)?;
                }
                // An attribute twigil (`@!c = ...` as a statement lands on
                // SetGlobal): the element type lives in the class registry,
                // which none of the name-keyed lookups above can see.
                val = self.apply_attr_container_element_type(&name, val)?;
                if let Some(constraint) = loan_env!(self, var_type_constraint(&name))
                    && !name.starts_with('%')
                    && !name.starts_with('@')
                {
                    // A Nil ASSIGNED to a typed scalar resets it to its type
                    // object (`my Str $x = "a"; $x = Nil` leaves `$x === Str`),
                    // mirroring `exec_set_local_op`'s STORE-time reset. This
                    // SetGlobal path is reached when the writer has no local
                    // slot for the name (a closure/embedded-regex-code-block
                    // write to a captured typed scalar, `$Foo::x = Nil`, ...):
                    // without the reset here, the raw Nil got stored as-is,
                    // and the GetGlobal read path deliberately does NOT
                    // convert a Nil read into the type object via an
                    // env-scoped constraint (a genuine `Mu $b = Nil` parameter
                    // default must stay Nil) — so the value never became the
                    // type object at all (roast S02-types/nil.t,
                    // S02-types/subset-6e.t "assigns to subset type object").
                    if val.is_nil()
                        && !is_bind_ctx
                        && !is_rebind
                        && constraint != "Nil"
                        && self.var_default(&name).is_none()
                    {
                        val = self.typed_scalar_nil_seed_value(&name, &constraint);
                    } else {
                        if !val.is_nil() && !self.type_matches_value(&constraint, &val) {
                            // When assigning an unhandled Failure to a typed variable
                            // that can't hold it, explode the Failure first (Raku behavior)
                            if let ValueView::Instance { class_name, .. } = val.view()
                                && class_name.resolve() == "Failure"
                                && !val.is_failure_handled()
                                && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
                            {
                                return Err(err);
                            }
                            return Err(runtime::utils::type_check_assignment_typed_error(
                                &name,
                                &constraint,
                                &val,
                            ));
                        }
                        if !val.is_nil() {
                            val =
                                loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
                        }
                        // Wrap native integer values on assignment (overflow wrapping)
                        val = Self::wrap_native_int_by_constraint(&constraint, val)?;
                    }
                }
                if self.fatal_mode
                    && !name.contains("__mutsu_")
                    && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
                {
                    return Err(err);
                }
                let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
                let alias_key = format!("__mutsu_sigilless_alias::{}", name);
                if matches!(
                    self.env().get(&readonly_key).map(Value::view),
                    Some(ValueView::Bool(true))
                ) && !matches!(
                    self.env().get(&alias_key).map(Value::view),
                    Some(ValueView::Str(_))
                ) {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if let Some(source_name) = bind_source.as_ref() {
                    let mut resolved_source = source_name.clone();
                    let mut seen = std::collections::HashSet::new();
                    while seen.insert(resolved_source.clone()) {
                        let key = format!("__mutsu_sigilless_alias::{}", resolved_source);
                        let Some(ValueView::Str(next)) = self.env().get(&key).map(Value::view)
                        else {
                            break;
                        };
                        resolved_source = next.to_string();
                    }
                    // Frame-ownership gate for the ancestor-frame splice below.
                    // MUST be read here, before the container is written into
                    // the env under the source's name — that write would make
                    // the own-tier half of the test trivially true. See
                    // `bind_source_is_own_frame_lexical`.
                    let resolved_source_is_own_lexical = self.bind_source_is_own_frame_lexical(
                        code,
                        source_name,
                        &resolved_source,
                        bind_source_slot,
                    );
                    self.env_mut()
                        .insert(alias_key.clone(), Value::str(resolved_source.clone()));
                    self.mark_sigilless_alias_seen();
                    // Propagate readonly status from the source variable.
                    // Binding to a readonly parameter should make the target
                    // readonly as well (persisted in env for cross-scope survival).
                    let source_kind = self.readonly_kind(source_name);
                    let source_readonly = source_kind.is_some();
                    self.env_mut()
                        .insert(readonly_key.clone(), Value::truth(source_readonly));
                    if let Some(kind) = source_kind {
                        self.mark_readonly_with(&name, kind);
                    }
                    // Create a shared ContainerRef for cross-scope binding persistence.
                    if !name.starts_with('@')
                        && !name.starts_with('%')
                        && !name.starts_with('&')
                        && !source_readonly
                    {
                        // Reuse the source's existing cell when it already has
                        // one, so the bind joins the LIVE cell instead of
                        // minting a disconnected snapshot: the source read that
                        // produced `val` derefs its cell, so `val` is the plain
                        // value and the `ContainerRef` arm below cannot catch
                        // this. Without the reuse, `sub bindit { $alias := $var }`
                        // (a free-var `:=` inside a named sub, routed through
                        // SetGlobal) bound `$alias` to a fresh cell holding the
                        // bind-time value, while every later `$var = ...` write
                        // went through `$var`'s own authoritative cell (its env
                        // entry or its ADR-0024 mainline capture cell) — so
                        // `$alias` never tracked the source again. Mirrors the
                        // cell-reuse the SetLocal `bind_source` twin already
                        // does (`vm_var_assign_set_local.rs`, both the
                        // whole-container and the scalar branch).
                        let container = match val.view() {
                            ValueView::ContainerRef(arc) => Value::container_ref(arc.clone()),
                            _ => {
                                let existing =
                                    match self.env().get(&resolved_source).map(Value::view) {
                                        Some(ValueView::ContainerRef(arc)) => Some(arc.clone()),
                                        _ => None,
                                    }
                                    // ADR-0024: an intervening frame can shadow the
                                    // source in the plain env chain; fall back to the
                                    // mainline capture store before minting a
                                    // disconnected cell (see `mainline_lexical_cell`).
                                    .or_else(|| self.mainline_lexical_cell(&resolved_source));
                                match existing {
                                    Some(arc) => Value::container_ref(arc),
                                    None => val.clone().into_container_ref(),
                                }
                            }
                        };
                        // Store ContainerRef in target and source env
                        self.set_env_with_main_alias(&name, container.clone());
                        self.env_mut()
                            .insert(resolved_source.clone(), container.clone());
                        // If the target is an attribute alias (`has $x` makes `x`
                        // an alias for `!x`), also store the ContainerRef under
                        // the private attribute key so writeback picks it up when
                        // the method returns. Check via the reverse alias:
                        // `__mutsu_sigilless_alias::!x` → `"x"`.
                        {
                            let reverse_key = format!("__mutsu_sigilless_alias::!{}", name);
                            if let Some(reverse_val) = self.env().get(&reverse_key).cloned()
                                && let ValueView::Str(target) = reverse_val.view()
                                && target.as_str() == name
                            {
                                let priv_key = format!("!{}", name);
                                self.env_mut().insert(priv_key, container.clone());
                            }
                        }
                        // When rebinding to a new source, the old alias target
                        // keeps its existing value/ContainerRef — we only break
                        // the alias, we do NOT propagate the new ContainerRef to
                        // the old target.
                        // Update source local if present
                        if let Some(source_idx) =
                            code.locals.iter().rposition(|n| n == &resolved_source)
                        {
                            self.locals[source_idx] = container.clone();
                            self.flush_local_to_env(code, source_idx);
                        }
                        // Propagate to saved call frame envs so the binding
                        // survives method returns (env restore) instead of
                        // reverting to a stale value. See
                        // `propagate_bind_to_ancestor_frames`'s doc comment
                        // for what actually carries the binding across the
                        // call chain.
                        self.propagate_bind_to_ancestor_frames(
                            &resolved_source,
                            resolved_source_is_own_lexical,
                            &container,
                        );
                        // Persist ContainerRef in our_vars for `our` variables.
                        // Store under both the bare name and any existing
                        // package-qualified variants (e.g., "K::x" for bare "x")
                        // so GetGlobal fallback (which uses qualified keys) can
                        // find the binding.
                        self.set_our_var(name.clone(), container.clone());
                        // Update the package-qualified our_var key (e.g., "K::x"
                        // for bare "x" in class K) so GetGlobal fallback can find
                        // the binding. Only match the exact class from the method
                        // class stack to avoid clobbering unrelated package vars.
                        if let Some(method_class) = self.method_class_stack_top() {
                            let qualified = format!("{}::{}", method_class, name);
                            if self.get_our_var(&qualified).is_some() {
                                self.set_our_var(qualified.clone(), container.clone());
                                self.env_mut().insert(qualified, container.clone());
                            }
                        }
                        *ip += 1;
                        return Ok(());
                    }
                    // Record pending alias bind for the caller to create
                    // local_bind_pairs after the closure returns.
                    if !source_readonly {
                        self.pending_alias_bind_names
                            .push((name.clone(), resolved_source));
                    }
                }
                // ADR-0024: a mainline named sub's write to one of its OWN
                // captured lexicals must route through the shared cell in
                // `unit_lexicals[MAINLINE_UNIT_KEY]`, checked BEFORE the
                // generic "any ContainerRef in env" write-through right below
                // — that shortcut reads `env` by the SAME bare name, and for
                // a call made inside a shadowing block, env currently holds
                // the SHADOW's own boxed cell (`box_decl_local_cell` rewrote
                // the env key when the shadow's `my` declared it), not the
                // sub's captured mainline cell. Writing through whichever
                // cell env happens to hold would clobber the shadow instead
                // of the real lexical (row 2a) and lose the write entirely
                // once the shadow's scope ends (row 2b). No-op (and falls
                // through to the checks below) for every other name — this is
                // the same resolver `unit_scope_lexical_write` calls again,
                // unconditionally, further down for the general `unit`
                // compunit case.
                if self.unit_scope_lexical_write(&name, &val) {
                    *ip += 1;
                    return Ok(());
                }
                // A genuinely fresh binding (an expression-position `my`, e.g.
                // `if (my $a = 0) {...}`) whose bare name happens to collide
                // with an outer captured lexical's shared `ContainerRef` cell
                // must NOT write through that cell — it is a new variable, not
                // a write to the outer one. `expr_declared_syms` is the
                // compile-time discriminator recorded for exactly this case
                // (deliberately excluding the synthesized `WhateverCode`
                // "promoted" declaration, which DOES belong to the enclosing
                // block and must keep writing through — see
                // `roast/S02-types/whatever.t` #45 /
                // `t/expression-position-my-scope.t` #8). Method bodies are the
                // main beneficiary: a class/role method's `CompiledCode` is
                // registered separately from its enclosing frame, so it never
                // appears in that frame's `closure_compiled_codes` and none of
                // the OTHER `expr_declared_syms`-based protections (capture
                // filter, free-var-write drain) ever run for it — this check is
                // the one that does.
                let fresh_binding_decl = self.vardecl_context.get()
                    && code.expr_declared_syms.contains(&Symbol::intern(&name));
                // Write through ContainerRef: update inner value for env-based variables.
                // Return early to avoid overwriting the ContainerRef in env with a plain value.
                if !is_rebind && !raw_mode {
                    // Check env directly (not through alias resolution to avoid circular lookups)
                    if !fresh_binding_decl
                        && let Some(cell_val) = self.env().get(&name).cloned()
                        && let ValueView::ContainerRef(arc) = cell_val.view()
                    {
                        self.check_container_cell_constraint(&arc, &val)?;
                        // Preserve the inner container's identity (§3): a boxed
                        // captured `@a`/`%h` whole-reassigned here must keep its
                        // backing `Gc` so by-value holders observe the update.
                        Self::cell_store_preserving_container_identity(&name, &arc, &val);
                        *ip += 1;
                        return Ok(());
                    }
                    // A block `my` lexical captured by an escaped `our` sub: reads
                    // resolve through the persisted shared cell (`escaping_our_read`
                    // short-circuits env), so a plain assignment must reach the SAME
                    // cell — a by-name env write would land only on this call's env
                    // copy (or a stale leaked entry) and be dropped on return.
                    if let Some(cell_val) = self.escaping_our_write_cell(code, &name)
                        && let ValueView::ContainerRef(arc) = cell_val.view()
                    {
                        self.check_container_cell_constraint(&arc, &val)?;
                        Self::cell_store_preserving_container_identity(&name, &arc, &val);
                        *ip += 1;
                        return Ok(());
                    }
                    // Also check alias target for sigilless attributes
                    let alias_key_check = format!("__mutsu_sigilless_alias::{}", name);
                    if let Some(alias_val) = self.env().get(&alias_key_check).cloned()
                        && let ValueView::Str(alias_target) = alias_val.view()
                        && let Some(cell_val) = self.env().get(alias_target.as_str()).cloned()
                        && let ValueView::ContainerRef(arc) = cell_val.view()
                    {
                        self.check_container_cell_constraint(&arc, &val)?;
                        Self::cell_store_preserving_container_identity(&name, &arc, &val);
                        *ip += 1;
                        return Ok(());
                    }
                    // First write through a missing-key bind reached as a captured
                    // free variable (an env entry holding a `HashEntryRef` deferred
                    // token, e.g. a `\target` bound to `%h{$a;$b;$c}` written from
                    // a closure invoked by name): materialize the path into a
                    // shared `ContainerRef` cell — the SetGlobal counterpart of the
                    // SetLocal / AssignExpr materialization.
                    if !name.starts_with('@')
                        && !name.starts_with('%')
                        && let Some(token) = self.env().get(&name).cloned()
                        && matches!(token.view(), ValueView::HashEntryRef { .. })
                        && let Some(terminal) = token.hash_entry_terminal()
                    {
                        let cell = self.materialize_entry_cell(&terminal, val.clone())?;
                        self.set_env_with_main_alias(&name, Value::container_ref(cell));
                        *ip += 1;
                        return Ok(());
                    }
                }
                // A plain assignment to an atomic scalar de-registers its shared
                // cell so the next atomic op re-seeds from the freshly-stored
                // value. SetLocal does this at its own store site; SetGlobal is
                // the path taken when the atomic is a captured free variable
                // written from a nested closure frame (e.g. `$r = 0` inside a
                // `subtest {...}` block), and must reset the shared cell too, or
                // a later atomic-fetch-add reads the stale shared value instead
                // of the freshly-assigned one (roast S03-metaops/hyper.t #408).
                if !is_bind_ctx
                    && !is_rebind
                    && self.atomic_var_seen()
                    && !name.starts_with('@')
                    && !name.starts_with('%')
                    && !name.starts_with('&')
                {
                    let atomic_name = name.strip_prefix('$').unwrap_or(&name).to_string();
                    loan_env!(self, reset_atomic_var_key(&atomic_name));
                }
                // Container identity (§3, splice.t): a plain whole-container
                // *reassignment* of a free/outer `@`/`%` variable reached through
                // SetGlobal (e.g. `@a = ...` inside a nested sub where `@a` is a
                // top-level lexical) must mutate the EXISTING backing container in
                // place, so any by-value holder of the same `Gc` (an `@a` captured
                // into a list `(0, @a)` / a `\param`) observes the update — Raku's
                // stable container identity. Gate on the env already holding a
                // matching container (so a fresh declaration, whose env slot is
                // absent/Nil, keeps fresh identity) and on a plain assignment.
                // A `my @a = …` *declaration* reaching SetGlobal in expression
                // position (`push @a2, my @o = $_`) must NOT reuse the previous
                // iteration's container — each `my` is a fresh array, so a value
                // captured by an earlier iteration keeps its own contents.
                let sg_is_vardecl = self.vardecl_context.get();
                self.vardecl_context.set(false);
                if sg_is_vardecl
                    && !is_bind_ctx
                    && !is_rebind
                    && bind_source.is_none()
                    && (name.starts_with('@') || name.starts_with('%'))
                    && matches!(val.view(), ValueView::Array(..) | ValueView::Hash(..))
                {
                    // Fresh `my @a`/`my %h` declaration: own a DISTINCT container
                    // (Raku `=` copy semantics), never reuse the prior iteration's
                    // backing `Gc`. Detaching a shared source (`my @o = @b`) also
                    // preserves copy independence.
                    val = Self::detach_shared_container(val);
                    // Container-descriptor naming: the declaration names its
                    // fresh container (`@kh.VAR.name` reports "@kh" through any
                    // later pass-by-binding chain). Safe to stamp: the detach
                    // above guarantees an unshared node.
                    if !name.contains("__ANON") {
                        val.stamp_descriptor_name(&name);
                    }
                }
                if !is_bind_ctx
                    && !is_rebind
                    && !raw_mode
                    && !sg_is_vardecl
                    && bind_source.is_none()
                    && !name.contains("__ANON")
                    && (name.starts_with('@') || name.starts_with('%'))
                {
                    match (self.env().get(&name).map(Value::view), val.view()) {
                        (Some(ValueView::Array(old_gc, _)), ValueView::Array(new_gc, kind))
                            if !crate::gc::Gc::ptr_eq(&old_gc, &new_gc) =>
                        {
                            let (old_gc, new_gc, kind) = (old_gc.clone(), new_gc.clone(), kind);
                            val = Self::array_inplace_reassign(&old_gc, &new_gc, kind);
                        }
                        (Some(ValueView::Hash(old_gc)), ValueView::Hash(new_gc))
                            if !crate::gc::Gc::ptr_eq(&old_gc, &new_gc) =>
                        {
                            let (old_gc, new_gc) = (old_gc.clone(), new_gc.clone());
                            val = Self::hash_inplace_reassign(&old_gc, &new_gc);
                        }
                        // No reusable same-typed container already stored under this
                        // name (a first assignment): the `@`/`%` var must own a
                        // DISTINCT container per Raku `=` copy semantics, so detach
                        // from any shared backing `Gc`.
                        (existing, ValueView::Array(..) | ValueView::Hash(..))
                            if !matches!(
                                existing,
                                Some(ValueView::Array(..)) | Some(ValueView::Hash(..))
                            ) =>
                        {
                            val = Self::detach_shared_container(val);
                        }
                        _ => {}
                    }
                }
                // A file-scope `my` of the running routine's own compunit lives in
                // its shared cell, NOT under the bare env key: that key belongs to
                // whatever scope loaded the module (see `unit_lexicals`). Writing
                // env as well would re-create the collision the store removes, so
                // this write is exclusive — the env/`our`/shared-var stores below
                // are skipped for it.
                let unit_lexical_write = self.unit_scope_lexical_write(&name, &val);
                // An `our $x` of the package the running routine belongs to is
                // reached by its BARE name from inside that package's own
                // routines (the sub-body state-scope package disables
                // qualification), but the bare env key belongs to whatever
                // scope loaded the module. Writing it is what made a module's
                // `our $s = ...` land on the consumer's same-named `my $s`.
                // The variable's canonical home is the shared cell
                // `DeclareOurScalar` published under the package-qualified
                // name, so write THROUGH that cell and skip the bare-name
                // stores entirely — the same exclusivity `unit_lexical_write`
                // has, for the same reason. A `:=` bind rebinds the name
                // rather than assigning the variable, so it keeps the normal
                // path.
                let our_scalar_write = !unit_lexical_write
                    && !is_bind_ctx
                    && !is_rebind
                    && bind_source.is_none()
                    && self.our_package_scalar_write(&name, &val);
                // A DECLARATION reaching SetGlobal (an expression-position `my`,
                // e.g. `if (my $file = ...)`) creates a fresh binding — it is
                // never a write to a carrier-caller's lexical, so it must not
                // enter the carrier log: the carrier-return writeback would
                // copy the callee's env entry over a same-named caller slot
                // (Text::CSV's `method csv` clobbering the caller's `$file`).
                // Only remove what THIS write would have added: a name already
                // logged by an earlier genuine write stays logged.
                let carrier_logged_before = sg_is_vardecl
                    && self
                        .carrier_writes
                        .as_ref()
                        .is_some_and(|s| s.contains(name.as_str()));
                if unit_lexical_write || our_scalar_write {
                    // nothing further: the cell is the only home for this name
                } else if raw_mode && name.starts_with('@') {
                    // For `constant @x`, bypass set_shared_var's List→Array
                    // normalization so the container type (List) is preserved.
                    self.env_mut().insert(name.clone(), val.clone());
                } else if fresh_binding_decl {
                    // See the `fresh_binding_decl` comment above: this store
                    // must not write through a same-named outer captured
                    // lexical's `ContainerRef` cell either — this helper is
                    // reached (unlike the write-through checks above, which
                    // `vardecl_context` has already been cleared past by the
                    // time this line runs, hence capturing the flag earlier).
                    self.set_env_with_main_alias_fresh_binding(&name, val.clone());
                } else {
                    self.set_env_with_main_alias(&name, val.clone());
                }
                if sg_is_vardecl
                    && !carrier_logged_before
                    && let Some(set) = self.carrier_writes.as_mut()
                {
                    set.remove(name.as_str());
                }
                // Slice F (env<->locals coherence): a callee assigning to a
                // caller-declared dynamic variable (`$*foo = v`) reaches SetGlobal
                // and writes only `env` by name; the caller's local slot was kept
                // coherent solely by the reverse `sync_locals_from_env` pull.
                // Record the dynamic name so the call-site drain writes it through
                // to the caller frame's slot (no-op when the caller has no such
                // slot, e.g. a built-in like `$*OUT` that lives only in `env`).
                if name.starts_with('*') {
                    self.pending_rw_writeback_sources.push(name.clone());
                }
                // Persist anonymous state variable (`$`) so it survives
                // across closure calls (e.g. `$ ~= $_` in classify block).
                self.sync_anon_state_value(&name, &val);
                // Persist `our`-scoped variables so they survive block-scope
                // restoration (which only preserves env keys that existed
                // before the block).  `::('name')` falls back to this store.
                if !unit_lexical_write && !our_scalar_write {
                    self.set_our_var(name.clone(), val.clone());
                }
                // Eager `our`-alias sync: a package-qualified store (`$Foo::b = v`)
                // must be visible immediately through the lexical alias (`$b`)
                // inside the package, not only after block exit. If a local slot
                // is `our`-linked to this qualified name, refresh it now.
                self.sync_our_local_from_qualified(code, &name, &val);
                // A plain assignment to a package-scope free variable (`our $X`
                // or a `package { my $X }` lexical) reached by bare name from
                // inside a named sub must reach the canonical package store too,
                // otherwise the write lands only on the bare env/our key that the
                // `GetGlobal` read fallback never consults. No-op otherwise.
                if !our_scalar_write {
                    self.writeback_package_scope_var(&name, &val);
                }
                // Track topic mutations for map rw writeback
                if name == "_" {
                    self.env_mut()
                        .insert("__mutsu_rw_map_topic__".to_string(), val.clone());
                }
                // Sync to shared_vars for cross-thread visibility.
                // Skip for raw_mode @-variables to preserve List kind.
                if !(unit_lexical_write || our_scalar_write || raw_mode && name.starts_with('@')) {
                    loan_env!(self, set_shared_var(&name, val.clone()));
                }
                let mut alias_name = self.env().get(&alias_key).and_then(|v| {
                    if let ValueView::Str(name) = v.view() {
                        Some(name.to_string())
                    } else {
                        None
                    }
                });
                let mut seen_aliases = std::collections::HashSet::new();
                while let Some(current_alias) = alias_name {
                    if !seen_aliases.insert(current_alias.clone()) {
                        break;
                    }
                    self.set_env_with_main_alias(&current_alias, val.clone());
                    self.update_local_if_exists(code, &current_alias, &val);
                    // Sigilless attribute write: mirror an attr-twigil alias (`!x`)
                    // into self's shared cell so a same-method cell-direct read of
                    // the sigilless attr sees the new value (Phase 3 Stage 2c (ii)).
                    self.write_self_attr_cell(&current_alias, val.clone());
                    let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
                    alias_name = self.env().get(&next_key).and_then(|v| {
                        if let ValueView::Str(name) = v.view() {
                            Some(name.to_string())
                        } else {
                            None
                        }
                    });
                }
                if name == "_"
                    && !Self::is_topic_ro_assignment(&val)
                    && let Some(ref source_var) = self.topic_source_var
                    && !source_var.starts_with('@')
                    && !source_var.starts_with('%')
                    // A sigiled "$h" tag is the deref'd-container source
                    // (`for @$h`): the per-element loop writeback owns it; the
                    // whole-topic scalar write would pollute a "$h" env key.
                    && !source_var.starts_with('$')
                {
                    let source_name = source_var.clone();
                    self.set_env_with_main_alias(&source_name, val.clone());
                    self.update_local_if_exists(code, &source_name, &val);
                    // An attribute topic (`with $!result { .PQclear; $_ = Nil }`
                    // — DBDish::Pg's StatementHandle.finish) must reach self's
                    // attribute cell, not just the env mirror: the stale cell
                    // otherwise keeps the freed C pointer and the next finish
                    // double-frees it (SEGV).
                    if Self::attr_twigil_base(&source_name).is_some()
                        && !Self::is_non_mirrorable_attr_value(&val)
                    {
                        self.write_self_attr_cell(&source_name, val.clone());
                    }
                }
                // Reverse alias propagation: find all variables that are
                // bound TO this variable (i.e. `my $x := $name`) and update
                // them so the alias stays in sync.
                {
                    let prefix = "__mutsu_sigilless_alias::";
                    let reverse_targets: Vec<String> = self
                        .env()
                        .iter()
                        .filter_map(|(k, v)| {
                            if let Some(var_name) = k.strip_prefix_str(prefix)
                                && let ValueView::Str(target) = v.view()
                                && target.as_str() == name
                            {
                                Some(var_name)
                            } else {
                                None
                            }
                        })
                        .collect();
                    for target_var in reverse_targets {
                        // The alias table is process-global (it even reaches the
                        // cross-thread shared store), but an alias only means
                        // anything in the frame that made the binding. Propagate
                        // only to a name THIS frame owns as a slot.
                        //
                        // Without that, `given EXPR -> $y { … }` — which binds its
                        // parameter as `y := _` — left a permanent "`y` aliases the
                        // topic" entry, so *any* later `$_ = …` in *any* frame or
                        // thread overwrote `$y`. `given Cro::HTTP::Client.new ->
                        // $client { await $client.get(…) }` lost `$client` to an
                        // `Expecting` enum value the moment Cro's response parser —
                        // a supply body running `$_ = $expecting; when StatusLine {
                        // … }` — advanced its state, and the next `$client.get`
                        // died with "No such method 'get' for invocant of type
                        // 'Int'". raku keeps them separate too: in `given $v -> $y
                        // { $_ = 5 }`, `$y` is still 1.
                        //
                        // Same-frame aliasing (`my $c := $_; $_ = 5` — `$c` is 5)
                        // is unaffected: `c` is a local of the assigning frame.
                        if self.find_local_slot(code, &target_var).is_none() {
                            continue;
                        }
                        self.set_env_with_main_alias(&target_var, val.clone());
                        self.update_local_if_exists(code, &target_var, &val);
                    }
                }
                // Phase 3 Stage 2b: mirror a whole-container assign to an
                // array/hash attribute (`@!a = (...)`, `%!h = (...)`, and the
                // public `@.a`/`%.h` twigils) into self's shared cell. A scalar
                // attribute is parsed sigil-stripped to the local `!x` and mirrors
                // via the SetLocal path; an array/hash attribute keeps its `@`/`%`
                // sigil and is stored here through SetGlobal, which otherwise never
                // reaches the cell — so the write was silently lost (a same-method
                // `@!a` read goes cell-direct and saw the unchanged default). This
                // is a cheap prefix-check no-op for every non-attribute name.
                self.mirror_attr_env_to_cell(code, *name_idx, None);
                *ip += 1;
            }
            OpCode::SetVarType { name_idx, tc_idx } => {
                self.exec_set_var_type(code, ip, *name_idx, *tc_idx, false)?;
            }
            OpCode::SetVarTypeScoped { name_idx, tc_idx } => {
                self.exec_set_var_type(code, ip, *name_idx, *tc_idx, true)?;
            }
            OpCode::SetTopic => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                self.last_topic_value = Some(val.clone());
                self.env_mut().insert("_".to_string(), val);
                *ip += 1;
            }
            OpCode::PushEnterResult => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                self.enter_result_stack.push(val);
                *ip += 1;
            }
            OpCode::LoadEnterResult => {
                let val = self.enter_result_stack.pop().unwrap_or(Value::NIL);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::SaveTopic => {
                let current = self.env().get("_").cloned().unwrap_or(Value::NIL);
                self.topic_save_stack.push(current);
                *ip += 1;
            }
            OpCode::RestoreTopic => {
                if let Some(saved) = self.topic_save_stack.pop() {
                    self.env_mut().insert("_".to_string(), saved);
                }
                *ip += 1;
            }
            OpCode::EnterPointyTopic => {
                let saved_topic = self.env().get("_").cloned().unwrap_or(Value::NIL);
                let saved_source = self.topic_source_var.take();
                self.topic_source_save_stack
                    .push((saved_topic, saved_source));
                *ip += 1;
            }
            OpCode::ExitPointyTopic => {
                if let Some((saved_topic, saved_source)) = self.topic_source_save_stack.pop() {
                    self.env_mut().insert("_".to_string(), saved_topic);
                    self.topic_source_var = saved_source;
                }
                *ip += 1;
            }

            // -- Arithmetic --
            OpCode::Add => {
                self.exec_add_op()?;
                *ip += 1;
            }
            OpCode::Sub => {
                self.exec_sub_op()?;
                *ip += 1;
            }
            OpCode::Mul => {
                self.exec_mul_op()?;
                *ip += 1;
            }
            OpCode::Div => {
                self.exec_div_op()?;
                *ip += 1;
            }
            OpCode::Mod => {
                self.exec_mod_op()?;
                *ip += 1;
            }
            OpCode::Pow => {
                self.exec_pow_op()?;
                *ip += 1;
            }
            OpCode::Negate => {
                self.exec_negate_op()?;
                *ip += 1;
            }
            OpCode::IntBitNeg => {
                self.exec_int_bit_neg_op()?;
                *ip += 1;
            }
            OpCode::BoolBitNeg => {
                self.exec_bool_bit_neg_op();
                *ip += 1;
            }
            OpCode::StrBitNeg => {
                self.exec_str_bit_neg_op();
                *ip += 1;
            }
            OpCode::MakeSlip => {
                self.exec_make_slip_op()?;
                *ip += 1;
            }
            OpCode::DeSlip => {
                // A `.Slip`/`slip(...)` VALUE handed to a `**@`-slurpy consumer
                // (say/put/print/note) stays a single argument and gists as a
                // list `(...)`. Demote it to a Seq so the consumer's Slip-flatten
                // pass leaves it whole; `|EXPR` pipe-slips skip this op and still
                // flatten. Non-slip values pass through untouched.
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let demoted = match val.view() {
                    ValueView::Slip(items) => Value::seq(items.iter().cloned().collect()),
                    _ => val,
                };
                self.stack.push(demoted);
                *ip += 1;
            }
            OpCode::DerefContainer => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                self.stack.push(val.into_deref());
                *ip += 1;
            }
            OpCode::Decont => {
                self.exec_decont_op();
                *ip += 1;
            }
            OpCode::DecontListElems => {
                self.exec_decont_list_elems_op();
                *ip += 1;
            }
            OpCode::Itemize => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                self.stack.push(Self::itemize_value(val));
                *ip += 1;
            }
            OpCode::DeitemizeZen => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let deitemized = match val.view() {
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name.resolve() == "IO::Path::Parts" => {
                        let attrs = attributes.as_map();
                        Value::array(
                            crate::runtime::utils::io_path_parts_keys()
                                .iter()
                                .map(|key| {
                                    Value::pair(
                                        (*key).to_string(),
                                        attrs.get(*key).cloned().unwrap_or(Value::NIL),
                                    )
                                })
                                .collect(),
                        )
                    }
                    ValueView::Array(items, kind) if kind.is_itemized() => {
                        Value::array_with_kind(items.clone(), kind.decontainerize())
                    }
                    ValueView::Scalar(inner) => (*inner).clone(),
                    _ => val,
                };
                self.stack.push(deitemized);
                *ip += 1;
            }
            OpCode::DeitemizeForBind => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let deitemized = self.deitemize_for_bind(val)?;
                self.stack.push(deitemized);
                *ip += 1;
            }
            OpCode::ItemizeVar(name_idx) => {
                // Itemize a scalar variable's value for `@a = $var`, UNLESS the
                // scalar was bound (`:=`) to a Positional. A bound scalar is not
                // a Scalar container, so its value must flatten on `@`-assignment.
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let is_bound_decont = if self.bound_decont_active.get() {
                    let var_name = code.constants[*name_idx as usize].as_str().unwrap_or("");
                    let key = format!("__mutsu_bound_decont::{}", var_name);
                    matches!(
                        self.env().get(&key).map(Value::view),
                        Some(ValueView::Bool(true))
                    )
                } else {
                    false
                };
                let result = if is_bound_decont {
                    val
                } else {
                    match val.view() {
                        // A scalar holding a Set/Bag/Mix assigned to an `@`
                        // variable stays a single item (`my $h = set(...); my @a
                        // = $h` -> `@a.elems == 1`). These have no itemized
                        // container kind, so wrap them in a Scalar — but ONLY on
                        // this `@`-assignment path, not in general `$(...)`
                        // itemization (which must not leak the wrapper into set
                        // ops). A Hash uses its `itemized` flag (no wrapper).
                        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
                            Value::scalar(val)
                        }
                        // A scalar holding a Range assigned to an `@` variable
                        // stays a single item (`my $r = 1..5; my @a = $r` ->
                        // `@a.raku eq "[1..5,]"`). Like Set/Bag/Mix, a Range has
                        // no itemized container kind, so wrap it in a Scalar on
                        // this `@`-assignment path so it does not flatten.
                        ValueView::Range(..)
                        | ValueView::RangeExcl(..)
                        | ValueView::RangeExclStart(..)
                        | ValueView::RangeExclBoth(..)
                        | ValueView::GenericRange { .. } => Value::scalar(val),
                        _ => Self::itemize_value(val),
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::WrapScalar => {
                // Wrap the top-of-stack value in a Scalar container.
                // Used for `my $ = expr` (anonymous scalar) in argument position
                // so the container is preserved when stored in an immutable List.
                let val = self.stack.pop().unwrap_or(Value::NIL);
                self.stack.push(Value::scalar(val));
                *ip += 1;
            }
            OpCode::WrapTypedContainer(type_idx) => {
                // Wrap a typed anonymous scalar (`my T $`) in a ContainerRef cell
                // and record its `of`-type, so the constraint travels with the
                // value (e.g. into a Pair value) and is enforced on assignment.
                let type_name = Self::const_str(code, *type_idx).to_string();
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(val));
                crate::value::register_container_constraint(&cell, &type_name);
                self.stack.push(Value::container_ref(cell));
                *ip += 1;
            }
            OpCode::FlattenSlurpy => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let mut items = Vec::new();
                Self::flatten_value_for_slurpy(&val, &mut items);
                self.stack.push(Value::real_array(items));
                *ip += 1;
            }

            // -- Logic / coercion --
            OpCode::Not => {
                self.exec_not_op();
                *ip += 1;
            }
            OpCode::BoolCoerce => {
                self.sync_source_line(code, *ip);
                self.exec_bool_coerce_op();
                *ip += 1;
            }
            OpCode::WrapVarRef { name_idx, slot } => {
                self.exec_wrap_var_ref_op(code, *name_idx, *slot);
                *ip += 1;
            }
            OpCode::CaptureVarCell => {
                self.exec_capture_var_cell_op(code);
                *ip += 1;
            }
            OpCode::MarkBindContext => {
                self.bind_context.set(true);
                *ip += 1;
            }
            OpCode::MarkParamRawBindContext => {
                self.param_raw_bind_context.set(true);
                *ip += 1;
            }
            OpCode::MarkScalarBindContext => {
                self.scalar_bind_context.set(true);
                *ip += 1;
            }
            OpCode::MarkRebindContext => {
                self.rebind_context.set(true);
                *ip += 1;
            }
            OpCode::MarkAccessorRefContext => {
                self.accessor_ref_pending = true;
                *ip += 1;
            }
            OpCode::MarkArrayShareSource(name_idx) => {
                self.array_share_context.set(true);
                self.array_share_source
                    .set(Some(Self::const_str(code, *name_idx).to_string()));
                *ip += 1;
            }
            OpCode::MarkElementShare => {
                self.element_share_pending = true;
                *ip += 1;
            }
            OpCode::MarkConstantContext => {
                self.constant_context.set(true);
                *ip += 1;
            }
            OpCode::MarkExplicitInitializerContext => {
                self.explicit_initializer_context.set(true);
                *ip += 1;
            }
            OpCode::MarkVarDeclContext => {
                self.vardecl_context.set(true);
                *ip += 1;
            }
            OpCode::MarkShapedDeclContext => {
                self.shaped_decl_context = true;
                *ip += 1;
            }

            // -- String --
            OpCode::Concat => {
                self.sync_source_line(code, *ip);
                self.exec_concat_op()?;
                *ip += 1;
            }

            // -- Numeric comparison --
            OpCode::NumEq => {
                self.exec_num_eq_op()?;
                *ip += 1;
            }
            OpCode::NumNe => {
                self.exec_num_ne_op()?;
                *ip += 1;
            }
            OpCode::NumNeNative(flags) => {
                let flags = *flags;
                self.exec_num_ne_native_op(flags)?;
                *ip += 1;
            }
            OpCode::NumLt => {
                self.exec_num_lt_op()?;
                *ip += 1;
            }
            OpCode::NumLe => {
                self.exec_num_le_op()?;
                *ip += 1;
            }
            OpCode::NumGt => {
                self.exec_num_gt_op()?;
                *ip += 1;
            }
            OpCode::NumGe => {
                self.exec_num_ge_op()?;
                *ip += 1;
            }
            OpCode::ApproxEq => {
                self.exec_approx_eq_op()?;
                *ip += 1;
            }
            OpCode::ContainerEq(flags) => {
                let flags = *flags;
                self.exec_container_eq_op(flags);
                *ip += 1;
            }
            OpCode::ContainerEqNamed {
                left_name_idx,
                right_name_idx,
            } => {
                self.exec_container_eq_named_op(code, *left_name_idx, *right_name_idx);
                *ip += 1;
            }
            OpCode::ContainerEqIndexed {
                left_name_idx,
                right_name_idx,
            } => {
                self.exec_container_eq_indexed_op(code, *left_name_idx, *right_name_idx);
                *ip += 1;
            }
            OpCode::ContainerEqRaw => {
                self.exec_container_eq_raw_op();
                *ip += 1;
            }

            // -- String comparison --
            OpCode::StrEq => {
                self.exec_str_eq_op()?;
                *ip += 1;
            }
            OpCode::StrNe => {
                self.exec_str_ne_op()?;
                *ip += 1;
            }
            OpCode::StrLt => {
                self.exec_str_lt_op()?;
                *ip += 1;
            }
            OpCode::StrGt => {
                self.exec_str_gt_op()?;
                *ip += 1;
            }
            OpCode::StrLe => {
                self.exec_str_le_op()?;
                *ip += 1;
            }
            OpCode::StrGe => {
                self.exec_str_ge_op()?;
                *ip += 1;
            }

            // -- Three-way comparison --
            OpCode::Spaceship => {
                self.exec_spaceship_op()?;
                *ip += 1;
            }
            OpCode::Before | OpCode::After => {
                let is_before = matches!(code.ops[*ip], OpCode::Before);
                self.exec_before_after_op(is_before)?;
                *ip += 1;
            }
            OpCode::Cmp => {
                self.exec_cmp_op()?;
                *ip += 1;
            }
            OpCode::Coll => {
                self.exec_coll_op()?;
                *ip += 1;
            }
            OpCode::Unicmp => {
                self.exec_unicmp_op()?;
                *ip += 1;
            }
            OpCode::Leg => {
                self.exec_leg_op()?;
                *ip += 1;
            }

            // -- Identity/value equality --
            OpCode::StrictEq => {
                self.exec_strict_eq_op()?;
                *ip += 1;
            }
            OpCode::StrictNe => {
                self.exec_strict_ne_op()?;
                *ip += 1;
            }
            OpCode::Eqv => {
                self.exec_eqv_op()?;
                *ip += 1;
            }
            OpCode::SmartMatchExpr {
                rhs_end,
                negate,
                lhs,
                rhs_is_match_regex,
                lhs_is_literal,
                rhs_pure_regex,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_smart_match_expr_op(
                    code,
                    ip,
                    *rhs_end,
                    *negate,
                    lhs.as_deref(),
                    *rhs_is_match_regex,
                    *lhs_is_literal,
                    *rhs_pure_regex,
                    compiled_fns,
                )?;
            }
            OpCode::ScalarizeRegexMatchResult => {
                self.exec_scalarize_regex_match_result_op()?;
                *ip += 1;
            }

            // -- Divisibility --
            OpCode::DivisibleBy => {
                self.exec_divisible_by_op()?;
                *ip += 1;
            }
            OpCode::NotDivisibleBy => {
                self.exec_not_divisible_by_op()?;
                *ip += 1;
            }

            // -- Keyword math --
            OpCode::IntDiv => {
                self.exec_int_div_op()?;
                *ip += 1;
            }
            OpCode::IntMod => {
                self.exec_int_mod_op()?;
                *ip += 1;
            }
            OpCode::Gcd => {
                self.exec_gcd_op();
                *ip += 1;
            }
            OpCode::Lcm => {
                self.exec_lcm_op();
                *ip += 1;
            }
            OpCode::InfixMin => {
                self.exec_infix_min_op();
                *ip += 1;
            }
            OpCode::InfixMax => {
                self.exec_infix_max_op();
                *ip += 1;
            }

            // -- Repetition --
            OpCode::StringRepeat => {
                self.exec_string_repeat_op()?;
                *ip += 1;
            }
            OpCode::ListRepeat => {
                self.exec_list_repeat_op()?;
                *ip += 1;
            }
            OpCode::FunctionCompose => {
                self.sync_source_line(code, *ip);
                self.exec_function_compose_op();
                *ip += 1;
            }

            // -- Mixin / Type check --
            OpCode::ButMixin => {
                self.exec_but_mixin_op(code)?;
                *ip += 1;
            }
            OpCode::ButMixinTupleElem => {
                self.exec_but_mixin_tuple_elem_op()?;
                *ip += 1;
            }
            OpCode::Isa => {
                self.exec_isa_op();
                *ip += 1;
            }
            OpCode::Does => {
                self.exec_does_op(code)?;
                *ip += 1;
            }
            OpCode::DoesVar(name_idx, slot) => {
                self.exec_does_var_op(code, *name_idx, *slot)?;
                *ip += 1;
            }
            OpCode::SetDoesContext(flag) => {
                self.in_does_rhs = *flag;
                *ip += 1;
            }

            // -- Pair --
            OpCode::MakePair => {
                self.exec_make_pair_op(code);
                *ip += 1;
            }
            OpCode::MakeNamedArg => {
                self.exec_make_named_arg_op(code);
                *ip += 1;
            }
            OpCode::ContainerizePair => {
                let val = self.stack.pop().unwrap();
                let containerized = match val.view() {
                    ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
                    _ => val,
                };
                self.stack.push(containerized);
                *ip += 1;
            }

            // -- Bitwise --
            OpCode::BitAnd => {
                self.exec_bit_and_op()?;
                *ip += 1;
            }
            OpCode::BitOr => {
                self.exec_bit_or_op()?;
                *ip += 1;
            }
            OpCode::BitXor => {
                self.exec_bit_xor_op()?;
                *ip += 1;
            }
            OpCode::BitShiftLeft => {
                self.exec_bit_shift_left_op()?;
                *ip += 1;
            }
            OpCode::BitShiftRight => {
                self.exec_bit_shift_right_op()?;
                *ip += 1;
            }
            OpCode::BoolBitOr => {
                self.exec_bool_bit_or_op();
                *ip += 1;
            }
            OpCode::BoolBitAnd => {
                self.exec_bool_bit_and_op();
                *ip += 1;
            }
            OpCode::BoolBitXor => {
                self.exec_bool_bit_xor_op();
                *ip += 1;
            }
            OpCode::StrBitAnd => {
                self.exec_str_bit_and_op()?;
                *ip += 1;
            }
            OpCode::StrBitOr => {
                self.exec_str_bit_or_op()?;
                *ip += 1;
            }
            OpCode::StrBitXor => {
                self.exec_str_bit_xor_op()?;
                *ip += 1;
            }
            OpCode::StrShiftLeft => {
                self.exec_str_shift_left_op();
                *ip += 1;
            }
            OpCode::StrShiftRight => {
                self.exec_str_shift_right_op();
                *ip += 1;
            }

            // -- Set operations --
            OpCode::SetElem => {
                self.exec_set_elem_op()?;
                *ip += 1;
            }
            OpCode::SetCont => {
                self.exec_set_cont_op()?;
                *ip += 1;
            }
            OpCode::SetUnion => {
                self.exec_set_union_op()?;
                *ip += 1;
            }
            OpCode::SetAddition => {
                self.exec_set_addition_op()?;
                *ip += 1;
            }
            OpCode::SetIntersect => {
                self.exec_set_intersect_op()?;
                *ip += 1;
            }
            OpCode::SetMultiply => {
                self.exec_set_multiply_op()?;
                *ip += 1;
            }
            OpCode::SetDiff => {
                self.exec_set_diff_op();
                *ip += 1;
            }
            OpCode::SetSymDiff => {
                self.exec_set_sym_diff_op();
                *ip += 1;
            }
            OpCode::SetSubset => {
                self.exec_set_subset_op();
                *ip += 1;
            }
            OpCode::SetSuperset => {
                self.exec_set_superset_op();
                *ip += 1;
            }
            OpCode::SetStrictSubset => {
                self.exec_set_strict_subset_op();
                *ip += 1;
            }
            OpCode::SetStrictSuperset => {
                self.exec_set_strict_superset_op();
                *ip += 1;
            }
            OpCode::JunctionAny => {
                self.exec_junction_any_op();
                *ip += 1;
            }
            OpCode::JunctionAll => {
                self.exec_junction_all_op();
                *ip += 1;
            }
            OpCode::JunctionOne => {
                self.exec_junction_one_op();
                *ip += 1;
            }
            OpCode::JunctionAnyN(count) => {
                self.exec_junction_n_op(*count, JunctionKind::Any, "infix:<|>")?;
                *ip += 1;
            }
            OpCode::JunctionAllN(count) => {
                self.exec_junction_n_op(*count, JunctionKind::All, "infix:<&>")?;
                *ip += 1;
            }
            OpCode::JunctionOneN(count) => {
                self.exec_junction_n_op(*count, JunctionKind::One, "infix:<^>")?;
                *ip += 1;
            }

            // -- Sequence --
            OpCode::Sequence { exclude_end } => {
                self.sync_source_line(code, *ip);
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let out = loan_env!(self, eval_sequence_values(left, right, *exclude_end))?;
                self.stack.push(out);
                *ip += 1;
            }

            // -- Control flow --
            OpCode::Label(_) => {
                *ip += 1;
            }
            OpCode::Goto => {
                let target = self.stack.pop().unwrap_or(Value::NIL).to_string_value();
                if let Some(target_ip) = self.find_label_target(code, &target) {
                    *ip = target_ip;
                } else {
                    return Err(RuntimeError::goto_signal(target));
                }
            }
            OpCode::Jump(target) => {
                *ip = *target as usize;
            }
            OpCode::JumpIfFalse(target) => {
                // Mark Failures as handled when tested for truthiness (e.g. && operator)
                Self::mark_failure_handled_on_stack(&mut self.stack);
                let val = self.stack.pop().unwrap();
                if !self.eval_truthy(&val) {
                    // Also mark the original (below dup) as handled
                    Self::mark_failure_handled_on_stack(&mut self.stack);
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfTrue(target) => {
                Self::mark_failure_handled_on_stack(&mut self.stack);
                let val = self.stack.last().unwrap().clone();
                if self.eval_truthy(&val) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfNotNil(target) => {
                Self::mark_failure_handled_on_stack(&mut self.stack);
                let val = self.stack.last().unwrap().clone();
                if self.value_is_defined_dispatch(&val) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }

            OpCode::CallDefined => {
                self.sync_source_line(code, *ip);
                let val = self.stack.pop().unwrap();
                // A role-composed mixin (`but role { method defined {...} }`)
                // keeps its `.defined` override in a role, not a class MRO, so
                // the Instance/Package `has_user_method` path below can't see
                // it. Route it through the shared dispatch helper (Mixin +
                // caller reconciliation) exactly as `//` (`JumpIfNotNil`) does,
                // so `orelse`/`andthen`/`notandthen` agree on the override.
                if matches!(val.view(), ValueView::Mixin(..))
                    && self.mixin_role_has_method(&val, "defined")
                {
                    let defined = self.value_is_defined_dispatch(&val);
                    self.stack.push(Value::truth(defined));
                    *ip += 1;
                    return Ok(());
                }
                // Check if the value has a user-defined .defined method
                let class_name = match val.view() {
                    ValueView::Package(name) => Some(name),
                    ValueView::Instance { class_name, .. } => Some(class_name),
                    _ => None,
                };
                let has_user_defined = class_name
                    .as_ref()
                    .is_some_and(|cn| self.has_user_method(&cn.resolve(), "defined"));
                // A user `.defined` mutates a captured-outer lexical by name in env
                // via the interpreter slow path (`run_instance_method`), which
                // records nothing this site can drain. Snapshot the caller frame's
                // slot-backing env values before the call so only the changed slots
                // are written through after.
                let armed = has_user_defined;
                let pre_env: Vec<Option<Value>> = if armed {
                    code.locals
                        .iter()
                        .map(|n| {
                            self.env().get(n).cloned().or_else(|| {
                                n.strip_prefix('$')
                                    .or_else(|| n.strip_prefix('@'))
                                    .or_else(|| n.strip_prefix('%'))
                                    .or_else(|| n.strip_prefix('&'))
                                    .and_then(|b| self.env().get(b).cloned())
                            })
                        })
                        .collect()
                } else {
                    Vec::new()
                };
                let defined = if has_user_defined {
                    // Call user method directly, bypassing native method dispatch
                    let cn = class_name.unwrap();
                    let attrs = match val.view() {
                        ValueView::Instance { attributes, .. } => attributes.to_map(),
                        _ => AttrMap::new(),
                    };
                    match self.vm_run_instance_method(
                        &cn.resolve(),
                        attrs,
                        "defined",
                        Vec::new(),
                        Some(val.clone()),
                    ) {
                        Ok((result, _)) => result,
                        Err(_) => Value::truth(runtime::types::value_is_defined(&val)),
                    }
                } else {
                    Value::truth(runtime::types::value_is_defined(&val))
                };
                // Stage 3: a user-defined `.defined` (dispatched above for
                // `andthen`/`notandthen`) runs interpreter code that can mutate a
                // captured-outer caller lexical by name (`my $calls; method
                // defined { $calls++ }`). Reconcile the caller's slots so the
                // write is visible without the reverse `sync_locals_from_env`
                // pull (only on the user-method path; the native check is pure).
                if armed {
                    for (i, name) in code.locals.iter().enumerate() {
                        if name.starts_with('!')
                            || matches!(self.locals[i].view(), ValueView::HashEntryRef { .. })
                        {
                            continue;
                        }
                        let cur = self.env().get(name).cloned().or_else(|| {
                            name.strip_prefix('$')
                                .or_else(|| name.strip_prefix('@'))
                                .or_else(|| name.strip_prefix('%'))
                                .or_else(|| name.strip_prefix('&'))
                                .and_then(|b| self.env().get(b).cloned())
                        });
                        if let Some(cur) = cur
                            && pre_env.get(i).map(|p| p.as_ref()) != Some(Some(&cur))
                        {
                            self.locals[i] = cur;
                        }
                    }
                }
                self.stack.push(defined);
                *ip += 1;
            }

            // -- Stack manipulation --
            OpCode::XorXor => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                let a_truthy = a.truthy();
                let b_truthy = b.truthy();
                let result = if a_truthy && !b_truthy {
                    a
                } else if !a_truthy && b_truthy {
                    b
                } else if a_truthy && b_truthy {
                    Value::NIL
                } else {
                    // both falsy: return the last falsy value
                    b
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::Dup => {
                let val = self.stack.last().unwrap().clone();
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::CoerceToList => {
                self.sync_source_line(code, *ip);
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let list_val = match val.view() {
                    // Explicit Arrays ([1,2,3]) are preserved as-is.
                    ValueView::Array(_, kind) if kind.is_real_array() => val,
                    // Comma lists and other non-real arrays become Lists.
                    ValueView::Array(items, _) => {
                        Value::array_with_kind(items.clone(), crate::value::ArrayKind::List)
                    }
                    ValueView::Seq(items) => Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(items.to_vec())),
                        crate::value::ArrayKind::List,
                    ),
                    // Hash values are flattened to pairs for constant @.
                    ValueView::Hash(map) => {
                        let pairs: Vec<Value> = map
                            .iter()
                            .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                            .collect();
                        Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(pairs)),
                            crate::value::ArrayKind::List,
                        )
                    }
                    // Instance objects: check if Positional; if so keep as-is,
                    // otherwise call .cache for coercion (constant @ semantics).
                    ValueView::Instance { class_name, .. } => {
                        let cn = class_name.resolve();
                        let does_positional = matches!(
                            cn.as_str(),
                            "Array"
                                | "List"
                                | "Slip"
                                | "Seq"
                                | "Range"
                                | "Buf"
                                | "Blob"
                                | "utf8"
                                | "buf8"
                                | "buf16"
                                | "buf32"
                        ) || self
                            .class_composed_roles(&cn)
                            .is_some_and(|roles| roles.iter().any(|r| r == "Positional"));
                        if does_positional {
                            val
                        } else {
                            // Call .cache on non-Positional to coerce.
                            // Skip native methods so user-defined .cache is called.
                            let cached =
                                self.call_method_all_with_fallback(&val, "cache", &[], true)?;
                            let cached_val = cached.into_iter().next().unwrap_or(Value::NIL);
                            // Check that .cache returned a Positional
                            let is_pos = matches!(
                                cached_val.view(),
                                ValueView::Array(..)
                                    | ValueView::Seq(_)
                                    | ValueView::Slip(_)
                                    | ValueView::LazyList(_)
                            );
                            if !is_pos {
                                let got_type = crate::runtime::utils::value_type_name(&cached_val);
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert("got".to_string(), cached_val);
                                attrs.insert(
                                    "expected".to_string(),
                                    Value::package(crate::symbol::Symbol::intern("Positional")),
                                );
                                attrs.insert(
                                    "message".to_string(),
                                    Value::str(format!(
                                        "Type check failed in assignment; expected Positional but got {}",
                                        got_type
                                    )),
                                );
                                let ex = Value::make_instance(
                                    crate::symbol::Symbol::intern("X::TypeCheck"),
                                    attrs,
                                );
                                let mut err = RuntimeError::new(format!(
                                    "Type check failed in assignment; expected Positional but got {}",
                                    got_type
                                ));
                                err.exception = Some(Box::new(ex));
                                return Err(err);
                            }
                            // Coerce cached result to List
                            match cached_val.view() {
                                ValueView::Array(items, _) => Value::array_with_kind(
                                    items.clone(),
                                    crate::value::ArrayKind::List,
                                ),
                                ValueView::Seq(items) => Value::array_with_kind(
                                    crate::gc::Gc::new(crate::value::ArrayData::new(
                                        items.to_vec(),
                                    )),
                                    crate::value::ArrayKind::List,
                                ),
                                _ => Value::array_with_kind(
                                    crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                                        cached_val,
                                    ])),
                                    crate::value::ArrayKind::List,
                                ),
                            }
                        }
                    }
                    // A lazy list keeps its laziness behind `constant @x`, exactly
                    // as behind `my @x`: `constant @primes = grep *.is-prime, 2 .. *`
                    // (Digest::SHA2) is infinite, and wrapping it as a single
                    // element made `@primes[^8]` read `((...) Nil Nil …)`.
                    ValueView::LazyList(list) if list.preserve_lazy_on_array_assign() => val,
                    ValueView::LazyList(list) => Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(
                            self.force_lazy_list_vm(&list)?,
                        )),
                        crate::value::ArrayKind::List,
                    ),
                    _ => Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![val])),
                        crate::value::ArrayKind::List,
                    ),
                };
                self.stack.push(list_val);
                *ip += 1;
            }
            OpCode::Pop => {
                if let Some(popped) = self.stack.pop()
                    && let ValueView::LazyList(list) = popped.view()
                {
                    // Sink context must realize lazy gathers for side effects.
                    self.force_lazy_list_vm(&list)?;
                }
                *ip += 1;
            }
            OpCode::PushBlockFrame => {
                let call_line = self.current_source_line();
                let call_file = self.current_source_file_sym();
                self.push_block_routine_with_location(
                    self.current_package_sym(),
                    Symbol::intern(""),
                    call_line,
                    call_file,
                    // An inlined bare block belongs to its enclosing routine.
                    None,
                );
                *ip += 1;
            }
            OpCode::PopBlockFrame => {
                self.pop_routine();
                *ip += 1;
            }
            OpCode::ThrowIfFailure => {
                self.sync_source_line(code, *ip);
                // Peek (do not pop): a trailing unhandled Failure must be thrown
                // so the enclosing CATCH handler (or `try`) sees it, while a
                // normal value remains on the stack as the block's return value.
                if let Some(val) = self.stack.last()
                    && let Some(err) = self.failure_to_runtime_error_if_unhandled(val)
                {
                    return Err(err);
                }
                // Deliberately no `unhandled_failure_in_list_for_fatal` descent
                // here: unlike a bare Failure (created directly in this frame,
                // so `self.fatal_mode` here really does describe the state it
                // was made under), a reified list/Seq may be the *return value*
                // of a call that crossed its own `call_compiled_closure` save/
                // restore boundary — by the time control gets back here,
                // `self.fatal_mode` has been restored to *this* frame's state,
                // which can differ from the state the list's elements were
                // actually produced under (e.g. `try { c() }` where `c`'s own
                // body ran with fatal off, but `try` restores fatal on for its
                // own tail-position check). Checking the restored, ambient
                // value here retroactively imposed the try's fatal-ness on a
                // Failure the callee legitimately created as soft
                // (`t/whatever-code-fixes.t`, "without fatal, a map of
                // Failures is a soft list"). `.map`/`.grep`'s own native loop
                // (`resolution_map_grep.rs`) already throws at the correct,
                // per-element time using the fatal state active while each
                // element is actually computed, so this redundant recheck can
                // only ever be *wrong*, never additionally correct.
                *ip += 1;
            }
            OpCode::WarnSuppressPush => {
                self.push_warn_suppression();
                *ip += 1;
            }
            OpCode::WarnSuppressPop => {
                self.pop_warn_suppression();
                *ip += 1;
            }
            OpCode::SinkPopAssign => {
                self.sync_source_line(code, *ip);
                if let Some(val) = self.stack.pop() {
                    match val.view() {
                        // Keep SinkPop's lazy handling: mutsu's lazy closures do
                        // not yet track later mutations of captured outer
                        // lexicals, so leaving `@a[$i] = gather ... for ...;`
                        // unreified until first access would read the captures'
                        // final values (gather.t 31-32). Reifying here matches
                        // the pre-SinkPopAssign behavior exactly.
                        ValueView::LazyList(list) if list.is_cached_no_sink() => {}
                        ValueView::LazyList(list) => {
                            self.force_lazy_list_vm(&list)?;
                        }
                        ValueView::Seq(body) if body.needs_touch() => {
                            let body = std::sync::Arc::clone(&body);
                            self.sink_seq_body(&body)?;
                        }
                        _ => {
                            // An assignment statement is wanted, not sunk: the
                            // assigned Failure stays soft — unless `use fatal`
                            // is in effect.
                            if self.fatal_mode
                                && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
                            {
                                return Err(err);
                            }
                        }
                    }
                }
                *ip += 1;
            }
            OpCode::SinkPop(user_sink, may_explode_failure) => {
                self.sync_source_line(code, *ip);
                let user_sink = *user_sink;
                let may_explode_failure = *may_explode_failure;
                if let Some(val) = self.stack.pop() {
                    // A bare statement value whose class defines its own `sink`
                    // method invokes it in sink context (Raku semantics:
                    // `class C { method sink {...} }; C.new;` runs the sink).
                    // Gated on:
                    //  - `user_sink` (compile-time): the value is a fresh rvalue
                    //    (method call / term), not a bare variable or function
                    //    return that Raku keeps container-wrapped (mutsu decont's
                    //    those before SinkPop, losing the distinction);
                    //  - a user-defined `sink` method, so built-in / container
                    //    sink behavior is untouched;
                    //  - the class has no `STORE` method (a STORE class is itself
                    //    a container — Raku sinks the container, not the inner;
                    //    sink.t "we don't sink the result of thing().=method").
                    // TODO: a normal (non-`is rw`) sub returning a fresh instance
                    // should also sink it; that needs first-class container
                    // identity to tell an `is rw` (container) return from a plain
                    // one. Until then function-call returns are conservatively
                    // not auto-sunk.
                    // A `but`/`does` role mixin is a `ValueView::Mixin`, not an
                    // `Instance`, so the class lookup below never saw one and a
                    // composed `method sink` was silently skipped
                    // (`(1) does R;` ran nothing). The composition is dispatched
                    // through the role-aware path instead; the `STORE` exemption
                    // applies to the wrapped class exactly as it does to a bare
                    // instance.
                    let mixin_sink = user_sink
                        && val.is_mixin_value()
                        && !matches!(val.view(), ValueView::Mixin(inner, _)
                            if matches!(inner.view(), ValueView::Instance { class_name, .. }
                                if self.has_user_method(&class_name.resolve(), "STORE")))
                        && self.mixin_composes_method(&val, "sink");
                    let sink_class = if !user_sink || mixin_sink {
                        None
                    } else if let ValueView::Instance { class_name, .. } = val.view() {
                        let cn = class_name.resolve();
                        if self.has_user_method(&cn, "sink") && !self.has_user_method(&cn, "STORE")
                        {
                            Some(cn)
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    if mixin_sink {
                        // Same captured-outer writeback dance as the class arm
                        // below: the doc idiom `($b + 1) does role { method sink
                        // { $b++ } }` mutates a caller lexical from inside sink.
                        let pre_env: Vec<Option<Value>> = code
                            .locals
                            .iter()
                            .map(|n| {
                                self.env().get(n).cloned().or_else(|| {
                                    n.strip_prefix('$')
                                        .or_else(|| n.strip_prefix('@'))
                                        .or_else(|| n.strip_prefix('%'))
                                        .or_else(|| n.strip_prefix('&'))
                                        .and_then(|b| self.env().get(b).cloned())
                                })
                            })
                            .collect();
                        let _ = self.dispatch_mixin_method_call(&val, "sink", Vec::new());
                        self.reconcile_locals_from_env(code, &pre_env);
                        *ip += 1;
                        return Ok(());
                    }
                    if let Some(cn) = sink_class {
                        let attrs = match val.view() {
                            ValueView::Instance { attributes, .. } => attributes.to_map(),
                            _ => AttrMap::new(),
                        };
                        // `sink` can mutate a captured-outer caller lexical by
                        // name (`my @reg; method sink { @reg.push(...) }`) via
                        // the slow path, which records nothing this site drains.
                        // Snapshot slot-backing env before, reconcile after
                        // (same dance as CallDefined).
                        let pre_env: Vec<Option<Value>> = code
                            .locals
                            .iter()
                            .map(|n| {
                                self.env().get(n).cloned().or_else(|| {
                                    n.strip_prefix('$')
                                        .or_else(|| n.strip_prefix('@'))
                                        .or_else(|| n.strip_prefix('%'))
                                        .or_else(|| n.strip_prefix('&'))
                                        .and_then(|b| self.env().get(b).cloned())
                                })
                            })
                            .collect();
                        let _ = self.vm_run_instance_method(
                            &cn,
                            attrs,
                            "sink",
                            Vec::new(),
                            Some(val.clone()),
                        );
                        self.reconcile_locals_from_env(code, &pre_env);
                        *ip += 1;
                        return Ok(());
                    }
                    match val.view() {
                        // A `.cache`-returned view is a cached, re-iterable list;
                        // sinking it is a no-op and must NOT drain the underlying
                        // source (e.g. `(my $l = $cat.lines).cache;` keeps the cat
                        // unread). A `$s = SEQ` scalar assignment itemized this
                        // value (`LazyList::itemized`) — raku's `sink` never
                        // forces an itemized Scalar, only a genuinely bare Seq
                        // (measured: `my $s = (gather die)[]; $s;` lives). A bare
                        // lazy Seq still drains below.
                        ValueView::LazyList(list)
                            if list.is_cached_no_sink() || list.is_itemized() => {}
                        ValueView::LazyList(list) => {
                            self.force_lazy_list_vm(&list)?;
                        }
                        ValueView::Seq(body) if body.needs_touch() => {
                            // Sinking a not-yet-read Seq source must drain it
                            // so that side effects (read position, .eof for
                            // an IO::Handle.lines source) are observable.
                            let body = std::sync::Arc::clone(&body);
                            self.sink_seq_body(&body)?;
                        }
                        _ => {
                            // Sinking an unhandled Failure throws (Raku behavior) —
                            // except inside a regex `{ ... }` code block, whose
                            // statements rakudo compiles as wanted, not sunk: a
                            // stored Failure there stays soft (only a DESTROY-time
                            // warning), e.g. Cro's generated route matcher relies
                            // on the Failure reaching the signature-bind check.
                            // Also except a bare container read (`may_explode_failure
                            // == false`): Raku's optimizer never actually sinks a
                            // pure variable mention (the "Useless use of ... in sink
                            // context" case), so reaching it here must not
                            // retroactively explode a Failure that was created
                            // without `use fatal` in effect (`t/failure-fatal-mode-
                            // creation-time.t`) — Raku decides a Failure's fate at
                            // *construction* time, not at every later mention.
                            if !self.in_regex_code_block
                                && may_explode_failure
                                && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
                            {
                                return Err(err);
                            }
                            // Deliberately no `unhandled_failure_in_list_for_fatal`
                            // descent here — see the identical note on
                            // `OpCode::ThrowIfFailure` above: the ambient
                            // `self.fatal_mode` at this sink can be the
                            // *caller's* restored state, not the state the
                            // sunk list's elements were actually produced
                            // under, and `.map`/`.grep`'s own native loop
                            // already enforces `use fatal` at the correct,
                            // per-element time.
                            // Sinking a Proc with non-zero exitcode throws X::Proc::Unsuccessful
                            if let ValueView::Instance {
                                class_name,
                                attributes,
                                ..
                            } = val.view()
                                && class_name.resolve() == "Proc"
                            {
                                let exitcode =
                                    match attributes.as_map().get("exitcode").map(Value::view) {
                                        Some(ValueView::Int(i)) => i,
                                        _ => 0,
                                    };
                                // A still-"live" Proc (from `run(:in, ...)`)
                                // carries a placeholder exitcode of -1 until it
                                // is finalized; sinking it must not throw.
                                let is_live = matches!(
                                    attributes.as_map().get("live").map(Value::view),
                                    Some(ValueView::Bool(true))
                                );
                                if exitcode != 0 && !is_live {
                                    let signal =
                                        match attributes.as_map().get("signal").map(Value::view) {
                                            Some(ValueView::Int(i)) => i,
                                            _ => 0,
                                        };
                                    let command = attributes
                                        .as_map()
                                        .get("command")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    // When the command could not be spawned at all
                                    // (exit code -1), rakudo reports the underlying
                                    // OS error in the message.
                                    let os_error = attributes
                                        .as_map()
                                        .get("os-error")
                                        .map(|v| v.to_string_value())
                                        .filter(|s| !s.is_empty());
                                    let msg = match &os_error {
                                        Some(oe) => format!(
                                            "The spawned command '{}' exited unsuccessfully (exit code: {}, signal: {}, OS error = {})",
                                            command, exitcode, signal, oe
                                        ),
                                        None => format!(
                                            "The spawned command '{}' exited unsuccessfully (exit code: {}, signal: {})",
                                            command, exitcode, signal
                                        ),
                                    };
                                    let mut ex_attrs = std::collections::HashMap::new();
                                    ex_attrs.insert("message".to_string(), Value::str(msg.clone()));
                                    ex_attrs.insert("proc".to_string(), val);
                                    let exception = Value::make_instance(
                                        crate::symbol::Symbol::intern("X::Proc::Unsuccessful"),
                                        ex_attrs,
                                    );
                                    let mut err = RuntimeError::new(msg);
                                    err.exception = Some(Box::new(exception));
                                    return Err(err);
                                }
                            }
                        }
                    }
                }
                *ip += 1;
            }

            // -- Range creation --
            OpCode::MakeRange => {
                self.exec_make_range_op()?;
                *ip += 1;
            }
            OpCode::MakeRangeExcl => {
                self.exec_make_range_excl_op()?;
                *ip += 1;
            }
            OpCode::MakeRangeExclStart => {
                self.exec_make_range_excl_start_op()?;
                *ip += 1;
            }
            OpCode::MakeRangeExclBoth => {
                self.exec_make_range_excl_both_op()?;
                *ip += 1;
            }

            // -- Composite --
            OpCode::MakeArray(n) => {
                // A user-overloaded list-associative `infix:<,>` intercepts the
                // bare value-list before it becomes a List.
                if !self.try_comma_overload(*n)? {
                    self.exec_make_array_op(code, *n, false)?;
                }
                *ip += 1;
            }
            OpCode::MakeRealArray(n) => {
                self.exec_make_array_op(code, *n, true)?;
                *ip += 1;
            }
            OpCode::MakeRealArrayNoFlatten(n) => {
                self.exec_make_array_no_flatten_op(*n)?;
                *ip += 1;
            }
            OpCode::MakeHash(n) => {
                self.exec_make_hash_op(*n)?;
                *ip += 1;
            }
            OpCode::MakeHashFromPairs(n) => {
                self.exec_make_hash_from_pairs_op(*n)?;
                *ip += 1;
            }
            OpCode::MakeCapture(n) => {
                self.exec_make_capture_op(code, *n);
                *ip += 1;
            }

            // -- I/O --
            OpCode::Say(n) => {
                self.sync_source_line(code, *ip);
                self.sync_env_from_locals_declared(code);
                self.exec_say_op(*n)?;
                *ip += 1;
            }
            OpCode::Put(n) => {
                self.sync_source_line(code, *ip);
                self.sync_env_from_locals_declared(code);
                self.exec_put_op(*n)?;
                *ip += 1;
            }
            OpCode::Print(n) => {
                self.sync_source_line(code, *ip);
                self.sync_env_from_locals_declared(code);
                self.exec_print_op(*n)?;
                *ip += 1;
            }
            OpCode::Note(n) => {
                self.sync_source_line(code, *ip);
                self.sync_env_from_locals_declared(code);
                self.exec_note_op(*n)?;
                *ip += 1;
            }

            // -- Calls --
            OpCode::CallFunc {
                name_idx,
                arity,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: explode an unhandled Failure produced by one of
                // this call's argument expressions before the callee runs.
                // See `explode_if_fatal_failure_in_call_args`.
                self.explode_if_fatal_failure_in_call_args(
                    Self::const_str(code, *name_idx),
                    *arity as usize,
                )?;
                match self.exec_call_func_op(
                    code,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    compiled_fns,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // Record a resume point so a call that raises a
                        // control signal (e.g. `warn`) can be resumed after
                        // the call site by `.resume` in a CONTROL block.
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::CallFuncNamed {
                name_idx,
                arity,
                spec_idx,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above.
                self.explode_if_fatal_failure_in_call_args(
                    Self::const_str(code, *name_idx),
                    *arity as usize,
                )?;
                match self.exec_call_func_named_op(
                    code,
                    *name_idx,
                    *arity,
                    *spec_idx,
                    *arg_sources_idx,
                    compiled_fns,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // Same resume-point recording as CallFunc.
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::CallMethod {
                name_idx,
                arity,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above. The
                // receiver sits below the `arity` argument values on the stack,
                // so this only ever scans the arguments, not the invocant. A
                // method can never be `require` (a bareword sub), so pass "".
                self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
                match self.exec_call_method_op(
                    code,
                    *name_idx,
                    *arity,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // Record a resume point so a method that throws can
                        // be resumed after the call site by .resume in CATCH.
                        // Don't overwrite an existing resume_ip: when the
                        // method call is itself a `.resume`/`.rethrow` that
                        // re-raises a control signal, the original resume
                        // point (e.g. after `warn`) must be preserved.
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                // Slice F: write any `is rw` method-param writeback through to the
                // caller's local slot (no-op unless the dispatch recorded one).
                self.apply_pending_rw_writeback(code);
                // A `Grammar.parse` may run embedded regex `{ ... }` blocks that
                // wrote caller lexicals into `env`; reconcile them into slots.
                self.drain_pending_local_updates_after_call(code);
                *ip += 1;
            }
            OpCode::CallMethodDynamic {
                arity,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above. A
                // method can never be `require` (a bareword sub), so pass "".
                self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
                match self.exec_call_method_dynamic_op(
                    code,
                    *arity,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // Record a resume point so a method that raises a
                        // control signal (e.g. a resumable `warn`) can be
                        // resumed after the call site by `.resume`.
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                // Slice F: drain any `is rw` method-param writeback into the caller's slots.
                self.apply_pending_rw_writeback(code);
                self.drain_pending_local_updates_after_call(code);
                *ip += 1;
            }
            OpCode::CallMethodDynamicMut {
                arity,
                target_name_idx,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above. A
                // method can never be `require` (a bareword sub), so pass "".
                self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
                let pre = self.attr_env_snapshot(code, *target_name_idx);
                match self.exec_call_method_dynamic_mut_op(
                    code,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                self.apply_pending_rw_writeback(code);
                self.drain_pending_local_updates_after_call(code);
                self.mirror_attr_env_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::ArrayPush {
                target_name_idx,
                value_source_idx,
            } => {
                self.sync_source_line(code, *ip);
                let pre = self.attr_env_snapshot(code, *target_name_idx);
                self.exec_array_push_op(code, *target_name_idx, *value_source_idx)?;
                self.mirror_attr_env_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::CallMethodMut {
                name_idx,
                arity,
                target_name_idx,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above. A
                // method can never be `require` (a bareword sub), so pass "".
                self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
                let pre = self.attr_env_snapshot(code, *target_name_idx);
                // The receiver's env binding before the call, so the writeback
                // below can tell whether this method actually rebound it (see
                // there). Compared with `same_binding` — O(1), and it never walks
                // container contents the way `PartialEq` would.
                let receiver_before: Option<Option<Value>> =
                    (!Self::const_str(code, *target_name_idx).is_empty()).then(|| {
                        self.env()
                            .get_sym(code.const_sym(*target_name_idx))
                            .cloned()
                    });
                match self.exec_call_method_mut_op(
                    code,
                    *name_idx,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                // Slice F (env<->locals coherence): a mutating method updates the
                // receiver in env by name (`$s.push` on an `is Array`-backed
                // instance reassigns `env[$s]`; the ~15 `env_mut().insert(target,
                // ..)` branches in exec_call_method_mut_op) and relied on the
                // reverse `sync_locals_from_env` pull to refresh the caller's
                // local slot. Write the receiver through to its slot here so it
                // stays coherent without the pull. (`apply_pending_rw_writeback`
                // mirrors the reverse pull's HashEntryRef-skip invariant.)
                //
                // ONLY when the call actually REBOUND `env[receiver]`. This used
                // to fire after every method call on a named receiver, and
                // `apply_pending_rw_writeback` copies `env[name]` into the local
                // slot by name — but a frame's env also carries every same-named
                // binding it inherited from its caller (the callee env is the
                // flattened caller plus its own writes; parameters live in slots,
                // not in env). So on an unchanged receiver it copied the CALLER's
                // variable over the callee's parameter. A self-recursive routine is
                // exactly that shape:
                //
                //     sub f($tree, $d) { ... ; f($tree[1], $d + 1) }
                //
                // Every frame has a `tree`, so *any* method call on `$tree` in the
                // callee (`.defined`, `.gist`, even inside a `say`) silently
                // reverted `$tree` to the caller's node, the descent never reached
                // a leaf, and the recursion ran until the Rust stack gave out
                // (roast integration/99problems-51-to-60.t P57 — a stack overflow
                // that was really an infinite recursion).
                //
                // A method that mutates the receiver in place through its `Gc`
                // (rather than rebinding the name) leaves the bits equal, and that
                // is correct: the slot already holds the very same `Gc`.
                if let Some(before) = receiver_before {
                    let after = self.env().get_sym(code.const_sym(*target_name_idx));
                    let rebound = match (&before, after) {
                        (Some(b), Some(a)) => !b.same_binding(a),
                        (None, None) => false,
                        _ => true,
                    };
                    if rebound {
                        self.pending_rw_writeback_sources
                            .push(Self::const_str(code, *target_name_idx).to_string());
                    }
                }
                self.apply_pending_rw_writeback(code);
                self.drain_pending_local_updates_after_call(code);
                self.mirror_attr_env_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::CallOnValue {
                arity,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above. The
                // target is a `Value` here, not a static name, so `require`'s
                // exemption can never apply -- pass "".
                self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
                // Set a resume point before propagating a control signal so an
                // enclosing `CONTROL {}` can `.resume` after this call — e.g.
                // `my $w = &warn; $w.("x")` raises a resumable `warn` signal from
                // inside the dispatched callable, exactly like a direct `warn`
                // (which the `ExecCall` arm below already handles).
                match self.exec_call_on_value_op(code, *arity, *arg_sources_idx, compiled_fns) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::CallOnCodeVar {
                name_idx,
                arity,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above.
                self.explode_if_fatal_failure_in_call_args(
                    Self::const_str(code, *name_idx),
                    *arity as usize,
                )?;
                match self.exec_call_on_code_var_op(
                    code,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    compiled_fns,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::ExecCall {
                name_idx,
                arity,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above.
                self.explode_if_fatal_failure_in_call_args(
                    Self::const_str(code, *name_idx),
                    *arity as usize,
                )?;
                match self.exec_exec_call_op(
                    code,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    compiled_fns,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::ExecCallPairs {
                name_idx,
                arity,
                arg_sources_idx,
                keep_value,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above.
                self.explode_if_fatal_failure_in_call_args(
                    Self::const_str(code, *name_idx),
                    *arity as usize,
                )?;
                self.exec_exec_call_pairs_op(
                    code,
                    compiled_fns,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    *keep_value,
                )?;
                *ip += 1;
            }

            // -- Indexing --
            OpCode::Index { is_positional } => {
                self.exec_index_op_with_positional(*is_positional)?;
                *ip += 1;
            }
            OpCode::IndexAutovivifyLazy { is_positional } => {
                self.exec_index_autovivify_lazy_op(false, *is_positional)?;
                *ip += 1;
            }
            OpCode::IndexAutovivifyLazyTerminal { is_positional } => {
                self.exec_index_autovivify_lazy_op(true, *is_positional)?;
                *ip += 1;
            }
            OpCode::DeleteIndexNamed(name_idx, slot) => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_delete_index_named_op(code, *name_idx, *slot)?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::DeleteIndexExpr => {
                self.exec_delete_index_expr_op()?;
                *ip += 1;
            }
            OpCode::MultiDimIndex {
                ndims,
                is_positional,
            } => {
                self.exec_multi_dim_index_op(*ndims, *is_positional)?;
                *ip += 1;
            }
            OpCode::MultiDimIndexAssign {
                name_idx,
                ndims,
                is_positional,
            } => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_multi_dim_index_assign_op(code, *name_idx, *ndims, *is_positional)?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::MultiDimIndexAssignGeneric {
                ndims,
                is_positional,
            } => {
                self.exec_multi_dim_index_assign_generic_op(*ndims, *is_positional)?;
                *ip += 1;
            }
            OpCode::MultiDimIndexBindRef(ndims) => {
                self.exec_multi_dim_index_bind_ref_op(*ndims)?;
                *ip += 1;
            }
            OpCode::HyperSlice(adverb) => {
                self.exec_hyper_slice_op(*adverb)?;
                *ip += 1;
            }
            // -- String interpolation --
            OpCode::StringConcat(n) => {
                self.sync_source_line(code, *ip);
                self.exec_string_concat_op(*n)?;
                *ip += 1;
            }

            // -- Loop control --
            //
            // With no construct on the dynamic chain to act on, a loop-control
            // statement is an ordinary catchable `X::ControlFlow`, not a signal
            // (`try { { next } }` in rakudo leaves `$!` holding it). The check
            // has to be dynamic: `sub f { next }` called from a loop body is
            // legal, so neither the lexical nesting nor the call boundary
            // answers it. See `runtime/loop_handler_depth.rs`.
            OpCode::Last(label) => {
                if !crate::runtime::loop_handler_depth::loop_handler_in_scope() {
                    let illegal = if label.is_some() {
                        "labeled last"
                    } else {
                        "last"
                    };
                    return Err(RuntimeError::control_flow_illegal(
                        crate::value::Control::Last,
                        illegal,
                        "loop construct",
                    ));
                }
                let mut sig = RuntimeError::last_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Next(label) => {
                if !crate::runtime::loop_handler_depth::loop_handler_in_scope() {
                    let illegal = if label.is_some() {
                        "labeled next"
                    } else {
                        "next"
                    };
                    return Err(RuntimeError::control_flow_illegal(
                        crate::value::Control::Next,
                        illegal,
                        "loop construct",
                    ));
                }
                let mut sig = RuntimeError::next_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Redo(label) => {
                if !crate::runtime::loop_handler_depth::loop_handler_in_scope() {
                    let illegal = if label.is_some() {
                        "labeled redo"
                    } else {
                        "redo"
                    };
                    return Err(RuntimeError::control_flow_illegal(
                        crate::value::Control::Redo,
                        illegal,
                        "loop construct",
                    ));
                }
                let mut sig = RuntimeError::redo_signal();
                sig.label = label.clone();
                return Err(sig);
            }

            // -- Given/When control --
            OpCode::Proceed => {
                return Err(RuntimeError::proceed_signal());
            }
            OpCode::Succeed => {
                return Err(RuntimeError::succeed_signal());
            }
            OpCode::ReactDone => {
                // `done` outside any react/supply drive loop is an ordinary
                // catchable `X::ControlFlow`, not an escaping signal — same
                // dynamic-scope reasoning as `next`/`last`/`redo` above. See
                // `runtime::react_done_handler_depth` for why this has to be
                // a per-thread depth rather than an `Interpreter` field: a
                // `whenever` callback often runs on a scheduler worker thread
                // distinct from the one that started the react/supply.
                if !crate::runtime::react_done_handler_depth::react_done_handler_in_scope() {
                    return Err(RuntimeError::react_done_signal());
                }
                return Err(RuntimeError::done_signal());
            }
            OpCode::SupplyBodyDone => {
                return Err(RuntimeError::supply_body_done_signal());
            }
            OpCode::TagContainerRef(name_idx, slot) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.container_ref_var = Some((name, *slot, Self::resume_code_fp(code)));
                self.container_ref_reversed = false;
                *ip += 1;
            }
            OpCode::TagContainerRefReversed(name_idx, slot) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.container_ref_var = Some((name, *slot, Self::resume_code_fp(code)));
                self.container_ref_reversed = true;
                *ip += 1;
            }
            OpCode::TagElementSource {
                container_idx,
                positional,
            } => {
                let container = Self::const_str(code, *container_idx).to_string();
                let positional = *positional;
                let index = self.stack.pop().unwrap_or(Value::NIL);
                // Read the element value `container[index]` and push it as the
                // topic, reusing the standard index op so all container shapes
                // (Array/Hash/ContainerRef/typed) are handled uniformly.
                //
                // Under the (B) per-store env-write gate, a plain lexical in a
                // nested sub is authoritative in its local slot and its env
                // mirror is suppressed, so `get_env_with_main_alias` misses it
                // (returns Nil → indexing Nil yields `Any`). This broke
                // `with $cc<key>` on a grammar Match subcapture held in a nested
                // sub's `my $cc` (the URI dist). Read the live local slot first.
                let cval = self
                    .gate_local_slot_value(code, &container)
                    .or_else(|| self.get_env_with_main_alias(&container))
                    .unwrap_or(Value::NIL);
                self.stack.push(cval);
                self.stack.push(index.clone());
                self.exec_index_op_with_positional(positional)?;
                self.element_source = Some((container, index, positional));
                *ip += 1;
            }

            OpCode::UndefineAggregate(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                if let Some(val) = self.get_env_with_main_alias(name) {
                    match val.view() {
                        ValueView::Array(arc, _) => {
                            // SAFETY: aliased in-place clear of a shared container;
                            // see `gc_contents_mut`.
                            unsafe { crate::value::gc_contents_mut(&arc).items_mut().clear() };
                        }
                        ValueView::Hash(arc) => {
                            // SAFETY: aliased in-place clear; see `gc_contents_mut`.
                            unsafe { crate::value::gc_contents_mut(&arc).map.clear() };
                        }
                        // Slice 2a: a `=`-array-shared source (`my $r = @ary`) holds
                        // the aggregate inside a shared `ContainerRef` cell; clear it
                        // through the cell so every alias (`$r`) observes the empty.
                        ValueView::ContainerRef(cell) => Self::clear_aggregate_cell(&cell),
                        _ => {}
                    }
                }
                // Also update locals if present
                if let Some(slot) = self.find_local_slot(code, name) {
                    match self.locals[slot].view() {
                        ValueView::Array(arc, _) => {
                            // SAFETY: aliased in-place clear; see `gc_contents_mut`.
                            unsafe { crate::value::gc_contents_mut(&arc).items_mut().clear() };
                        }
                        ValueView::Hash(arc) => {
                            // SAFETY: aliased in-place clear; see `gc_contents_mut`.
                            unsafe { crate::value::gc_contents_mut(&arc).map.clear() };
                        }
                        ValueView::ContainerRef(cell) => Self::clear_aggregate_cell(&cell),
                        _ => {
                            self.locals[slot] = Value::NIL;
                            self.flush_local_to_env(code, slot);
                        }
                    }
                }
                self.stack.push(Value::NIL);
                *ip += 1;
            }

            // -- Postfix operators --
            OpCode::PostIncrement(name_idx, slot) => {
                self.exec_post_increment_op(code, *name_idx, *slot)?;
                if let Some(slot) = slot {
                    self.publish_state_local(code, *slot);
                }
                *ip += 1;
            }
            OpCode::PostDecrement(name_idx, slot) => {
                self.exec_post_decrement_op(code, *name_idx, *slot)?;
                if let Some(slot) = slot {
                    self.publish_state_local(code, *slot);
                }
                *ip += 1;
            }
            OpCode::PostIncrementIndex(name_idx, slot) => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_post_increment_index_op(code, *name_idx, *slot)?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::PostDecrementIndex(name_idx, slot) => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_post_decrement_index_op(code, *name_idx, *slot)?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional,
                target_slot,
            } => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_index_assign_expr_named_op(
                    code,
                    *name_idx,
                    *is_positional,
                    *target_slot,
                )?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::IndexElemAutoviv {
                name_idx,
                is_positional,
                target_slot,
                autoviv,
                viv_hash,
            } => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_index_elem_autoviv_op(
                    code,
                    *name_idx,
                    *is_positional,
                    *target_slot,
                    *autoviv,
                    *viv_hash,
                )?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::IndexAssignPseudoStashNamed {
                stash_name_idx,
                key_name_idx,
            } => {
                self.exec_index_assign_pseudo_stash_named_op(code, *stash_name_idx, *key_name_idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignPseudoStashKeyed { stash_name_idx } => {
                self.exec_index_assign_pseudo_stash_keyed_op(code, *stash_name_idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignExprNested {
                name_idx,
                outer_positional,
                inner_positional,
            } => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_index_assign_expr_nested_op(
                    code,
                    *name_idx,
                    *outer_positional,
                    *inner_positional,
                )?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::IndexAssignDeepNested {
                name_idx,
                depth,
                positional_flags_idx,
            } => {
                let pre = self.attr_elem_env_snapshot(code, *name_idx);
                self.exec_index_assign_deep_nested_op(
                    code,
                    *name_idx,
                    *depth,
                    *positional_flags_idx,
                )?;
                self.mirror_attr_elem_env_to_cell(code, *name_idx, pre);
                *ip += 1;
            }

            // -- Unary coercion --
            OpCode::NumCoerce => {
                self.sync_source_line(code, *ip);
                self.exec_num_coerce_op()?;
                *ip += 1;
            }
            OpCode::StrCoerce => {
                self.sync_source_line(code, *ip);
                self.exec_str_coerce_op()?;
                *ip += 1;
            }
            OpCode::UptoRange => {
                self.exec_upto_range_op();
                *ip += 1;
            }
            OpCode::MetaAssignIdentity(identity) => {
                // Only the (cold) no-zero-argument operators throw, so pay the
                // line sync on the error path rather than every `$i += 1`.
                self.exec_meta_assign_identity_op(*identity)
                    .inspect_err(|_| {
                        self.sync_source_line(code, *ip);
                    })?;
                *ip += 1;
            }

            // -- Prefix increment/decrement --
            OpCode::PreIncrement(name_idx, slot) => {
                self.exec_pre_increment_op(code, *name_idx, *slot)?;
                if let Some(slot) = slot {
                    self.publish_state_local(code, *slot);
                }
                *ip += 1;
            }
            OpCode::PreDecrement(name_idx, slot) => {
                self.exec_pre_decrement_op(code, *name_idx, *slot)?;
                if let Some(slot) = slot {
                    self.publish_state_local(code, *slot);
                }
                *ip += 1;
            }
            OpCode::PreIncrementIndex(name_idx, slot) => {
                self.exec_pre_increment_index_op(code, *name_idx, *slot)?;
                *ip += 1;
            }
            OpCode::PreDecrementIndex(name_idx, slot) => {
                self.exec_pre_decrement_index_op(code, *name_idx, *slot)?;
                *ip += 1;
            }

            // -- Variable access --
            OpCode::GetCaptureVar(name_idx) => {
                self.exec_get_capture_var_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::GetCodeVar(name_idx) => {
                self.exec_get_code_var_op(code, *name_idx)?;
                *ip += 1;
            }

            // -- Assignment as expression --
            OpCode::AssignExpr(name_idx) => {
                self.exec_assign_expr_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::TopicDotAssign(name_idx) => {
                self.exec_topic_dot_assign_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::AtomicCompoundVar {
                name_idx,
                op,
                identity,
            } => {
                self.exec_atomic_compound_var_op(code, *name_idx, *op, *identity)?;
                *ip += 1;
            }

            // -- Loops --
            OpCode::WhileLoop {
                cond_end,
                body_end,
                label,
                collect,
                isolate_topic,
            } => {
                self.sync_source_line(code, *ip);
                let spec = vm_control_ops::WhileLoopSpec {
                    cond_end: *cond_end,
                    body_end: *body_end,
                    label: label.clone(),
                    collect: *collect,
                    isolate_topic: *isolate_topic,
                };
                self.exec_while_loop_op(code, &spec, ip, compiled_fns)?;
            }
            OpCode::ForLoop(spec) => {
                self.sync_source_line(code, *ip);
                self.exec_for_loop_op(code, spec, ip, compiled_fns)?;
            }
            OpCode::RestoreForParam => {
                // Restore the single named for-loop param's prior binding now
                // that the loop's LAST/post phasers (which needed the param at
                // its final value) have run. Paired with the push the ForLoop
                // opcode performs on normal completion.
                if let Some((name, saved_val, colliding_slot)) = self.for_param_restore_stack.pop()
                {
                    // A loop param that shares a compile-time local slot with an
                    // enclosing binding of the same bare name overwrote that slot
                    // each iteration; write the saved value back through it too so
                    // a later `GetLocal` read of the outer name (`my \x = 10; for
                    // ... -> \x {}; say x`) sees the restored outer value, not the
                    // loop's last iteration value.
                    if let Some(slot) = colliding_slot
                        && (slot as usize) < self.locals.len()
                    {
                        self.locals[slot as usize] = saved_val.clone().unwrap_or(Value::NIL);
                    }
                    match saved_val {
                        Some(v) => {
                            self.env_mut().insert(name, v);
                        }
                        None => {
                            self.env_mut().remove(&name);
                        }
                    }
                }
                *ip += 1;
            }
            OpCode::CStyleLoop {
                cond_end,
                step_start,
                body_end,
                label,
                collect,
            } => {
                self.sync_source_line(code, *ip);
                let spec = vm_control_ops::CStyleLoopSpec {
                    cond_end: *cond_end,
                    step_start: *step_start,
                    body_end: *body_end,
                    label: label.clone(),
                    collect: *collect,
                };
                self.exec_cstyle_loop_op(code, &spec, ip, compiled_fns)?;
            }

            // -- Given/When/Default --
            OpCode::Given {
                body_end,
                topic_readonly,
                pointy_param_idx,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_given_op(
                    code,
                    *body_end,
                    *topic_readonly,
                    *pointy_param_idx,
                    ip,
                    compiled_fns,
                )?;
            }
            OpCode::When {
                body_end,
                statement_modifier,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_when_op(code, *body_end, *statement_modifier, ip, compiled_fns)?;
            }
            OpCode::Default { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_default_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::PushWhenNonmatch => {
                let v = self.when_nonmatch_value.take().unwrap_or(Value::FALSE);
                self.stack.push(v);
                *ip += 1;
            }

            // -- Repeat loop --
            OpCode::RepeatLoop {
                cond_end,
                body_end,
                label,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_repeat_loop_op(code, *cond_end, *body_end, label, ip, compiled_fns)?;
            }

            // -- Exception handling --
            OpCode::TryCatch {
                catch_start,
                control_start,
                body_end,
                explicit_catch,
                resume_safe,
                control_handles_take,
                is_bare_block,
                traps,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_try_catch_op(
                    code,
                    *catch_start,
                    *control_start,
                    *body_end,
                    *explicit_catch,
                    *resume_safe,
                    *control_handles_take,
                    *is_bare_block,
                    *traps,
                    ip,
                    compiled_fns,
                )?;
            }

            // -- Error handling --
            OpCode::RuntimeHasDecl(spec) => {
                // A `has`-attribute declaration that reached the VM (mainline /
                // EVAL'd source). If a class body is currently being registered
                // (`class Foo { BEGIN EVAL q[has $.x] }`), attach the attribute
                // to that class; otherwise throw the pre-built X::Attribute error.
                if let Some(class_name) = self.defining_class.clone() {
                    self.register_runtime_attribute(&class_name, spec)?;
                    *ip += 1;
                } else {
                    let val = spec.error.clone();
                    self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                    let mut err = self.runtime_error_from_exception_value(val, "Died", false);
                    self.attach_backtrace_to_error(&mut err);
                    return Err(err);
                }
            }
            OpCode::Die => {
                self.sync_source_line(code, *ip);
                let val = self.stack.pop().unwrap_or(Value::NIL);
                // Store the resume point (instruction after Die) for .resume support
                self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                // die() with empty array (from parsing die() with parens) should
                // check $! first, falling back to "Died" default
                let val = if matches!(val.view(), ValueView::Array(items, _) if items.is_empty()) {
                    let current = self.env().get("!").cloned();
                    if let Some(ref c) = current
                        && !c.is_nil()
                    {
                        current.unwrap()
                    } else {
                        Value::NIL
                    }
                } else {
                    val
                };
                let mut err = self.runtime_error_from_exception_value(val, "Died", false);
                self.attach_backtrace_to_error_with_leading(&mut err, &["throw", "die"]);
                return Err(err);
            }
            OpCode::Fail => {
                self.sync_source_line(code, *ip);
                let val = self.stack.pop().unwrap_or(Value::NIL);
                // When fail() receives a Failure:D, extract the inner exception
                // and re-arm it (Raku behavior: fail(Failure:D) re-arms)
                let val = if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = val.view()
                    && class_name.resolve() == "Failure"
                {
                    if let Some(exc) = attributes.as_map().get("exception") {
                        exc.clone()
                    } else {
                        val
                    }
                } else {
                    val
                };
                // Build a backtrace from the routine stack so that
                // Exception.gist can show where the fail originated.
                let backtrace_val = self.build_backtrace_value();
                let current_line = self.current_source_line();
                let current_file = self.current_source_file();
                let err = self.runtime_error_from_exception_value(val, "Failed", true);
                // Attach backtrace, line, and file to the exception value
                if let Some(ref exc_box) = err.exception
                    && let ValueView::Instance { attributes, .. } = exc_box.view()
                {
                    attributes.insert("backtrace".to_string(), backtrace_val);
                    if let Some(line) = current_line {
                        attributes.insert_if_absent("line".to_string(), Value::int(line as i64));
                    }
                    if let Some(ref file) = current_file {
                        attributes.insert_if_absent("file".to_string(), Value::str_from(file));
                    }
                }
                return Err(err);
            }
            OpCode::Return => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                // Check if &return has been lexically rebound; if so, call
                // the rebound function instead of performing a built-in return.
                if let Some(rebound) = self.env().get("&return").cloned()
                    && matches!(
                        rebound.view(),
                        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                    )
                {
                    let result = self.vm_call_on_value(rebound, vec![val], None)?;
                    self.stack.push(result);
                    *ip += 1;
                    return Ok(());
                }
                let mut err = RuntimeError::return_signal(val);
                // ADR-0037 Slice 4: an EVAL unit whose `context => $ctx`
                // named a live routine bakes that routine's id onto its own
                // `CompiledCode` (`compile_block_value_opts`); stamp it here
                // so the signal unwinds past any intervening routine
                // boundary to the frame this id names, instead of being
                // caught by the first one it reaches.
                if let Some(target_id) = code.eval_context_target_callable_id {
                    err.set_return_target_callable_id(Some(target_id));
                }
                return Err(err);
            }
            OpCode::ReturnFromNonRoutine(lexically_in_routine, out_of_dynamic_scope) => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                if *lexically_in_routine {
                    // Closure/block lexically inside a routine: propagate a
                    // CX::Return signal up to the enclosing routine boundary.
                    // If the signal escapes all frames up to the Interpreter top-level,
                    // the lexical target routine is no longer on the dynamic
                    // call stack, so it will surface as
                    // `X::ControlFlow::Return` with `out-of-dynamic-scope`.
                    return Err(RuntimeError::return_signal(val));
                }
                // No lexical routine at all (e.g. top-level `return`), or
                // (ADR-0037 §2.3) an `EVAL ..., context => $ctx` whose `$ctx`
                // named a routine that had already exited when the EVAL ran:
                // throw X::ControlFlow::Return directly, right here.
                let _ = val;
                return Err(RuntimeError::controlflow_return(*out_of_dynamic_scope));
            }

            // -- Environment variable access --
            OpCode::GetEnvIndex(key_idx) => {
                self.exec_get_env_index_op(code, *key_idx);
                *ip += 1;
            }
            OpCode::ExistsEnvIndex(key_idx) => {
                self.exec_exists_env_index_op(code, *key_idx);
                *ip += 1;
            }
            OpCode::ExistsExpr => {
                self.exec_exists_expr_op();
                *ip += 1;
            }
            OpCode::ExistsIndexAdv(flags) => {
                self.exec_exists_index_adv_op(*flags, None)?;
                *ip += 1;
            }
            OpCode::ExistsIndexNamedAdv { name_idx, flags } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.exec_exists_index_adv_op(*flags, Some(name))?;
                *ip += 1;
            }

            // -- Reduction --
            OpCode::Reduction(op_idx) => {
                self.sync_source_line(code, *ip);
                self.exec_reduction_op(code, *op_idx)?;
                *ip += 1;
            }

            // -- Magic variables --
            OpCode::RoutineMagic => {
                self.exec_routine_magic_op()?;
                *ip += 1;
            }
            OpCode::BlockMagic => {
                self.exec_block_magic_op()?;
                *ip += 1;
            }

            // -- Substitution --
            OpCode::Subst {
                pattern_idx,
                replacement_idx,
                samecase,
                sigspace,
                samemark,
                samespace,
                global,
                nth_idx,
                x_idx,
                perl5,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_subst_op(
                    code,
                    *pattern_idx,
                    *replacement_idx,
                    *samecase,
                    *sigspace,
                    *samemark,
                    *samespace,
                    *global,
                    *nth_idx,
                    *x_idx,
                    *perl5,
                )?;
                *ip += 1;
            }
            OpCode::NonDestructiveSubst {
                pattern_idx,
                replacement_idx,
                samecase,
                sigspace,
                samemark,
                samespace,
                global,
                nth_idx,
                x_idx,
                perl5,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_non_destructive_subst_op(
                    code,
                    *pattern_idx,
                    *replacement_idx,
                    *samecase,
                    *sigspace,
                    *samemark,
                    *samespace,
                    *global,
                    *nth_idx,
                    *x_idx,
                    *perl5,
                )?;
                *ip += 1;
            }
            OpCode::Transliterate {
                from_idx,
                to_idx,
                delete,
                complement,
                squash,
                non_destructive,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_transliterate_op(
                    code,
                    *from_idx,
                    *to_idx,
                    *delete,
                    *complement,
                    *squash,
                    *non_destructive,
                )?;
                *ip += 1;
            }

            // -- Take --
            OpCode::Take => {
                self.sync_source_line(code, *ip);
                if let Err(mut e) = self.exec_take_op() {
                    // Stamp the take-limit suspension with this op's exact
                    // location so the innermost enclosing for-loop can resume
                    // the SAME iteration right after this take (statements
                    // after it must not be lost). Keyed by code identity so a
                    // loop in a different code object never claims it.
                    if e.message == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL {
                        e.set_take_suspend_site(Some((code.ops.as_ptr() as usize, *ip)));
                    }
                    // A `CX::Take` a CONTROL block may `.resume`: record where
                    // execution continues (the statement after this take), the
                    // same way every resumable call site does. Without it a
                    // `CONTROL { when CX::Take { .resume } }` silently
                    // abandoned the rest of the block.
                    if e.is_take() && self.resume_ip.is_none() {
                        self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                    }
                    return Err(e);
                }
                *ip += 1;
            }

            // -- Package scope --
            OpCode::PackageScope { name_idx, body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_package_scope_op(code, *name_idx, *body_end, ip, compiled_fns)?;
            }
            OpCode::RegisterPackage { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.shadow_suppressed_type_with_package(&name);
                let pkg_val = Value::package(Symbol::intern(&name));
                self.env_mut().insert(name.clone(), pkg_val.clone());
                self.chain_declared_packages.insert(name.clone());
                self.update_local_if_exists(code, &name, &pkg_val);
                *ip += 1;
            }
            OpCode::SetCurrentPackage { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.set_current_package(name);
                *ip += 1;
            }
            OpCode::SetPackageKind { name_idx, kind } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.registry_mut().package_kinds.insert(name, *kind);
                *ip += 1;
            }
            OpCode::RegisterPackageMy { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.shadow_suppressed_type_with_package(&name);
                let pkg_val = Value::package(Symbol::intern(&name));
                self.env_mut().insert(name.clone(), pkg_val.clone());
                self.chain_declared_packages.insert(name.clone());
                self.update_local_if_exists(code, &name, &pkg_val);
                // Mark as my-scoped so the package is hidden from global
                // lookups and package stash resolution outside its scope.
                self.mark_my_scoped_package_item(name.clone());
                // Mark as block-declared so the name is cleaned up
                // when the enclosing block scope exits.
                let name_sym = code.const_sym(*name_idx);
                if let Some(set) = self.block_declared_vars.last_mut() {
                    set.insert(name_sym);
                }
                // A `my package`/`my module` is lexical exactly like `my class` —
                // its bare binding must die with the enclosing block/EVAL, not
                // stay visible the way a plain (non-`my`) `package Foo {}` does.
                // Reuse the same lexical-class-scope bookkeeping `my class`
                // already relies on (`register_lexical_class` /
                // `pop_lexical_class_scope`, consulted by both the bare-block
                // exit restore in `vm_misc_scope.rs` and by `EVAL`'s own
                // push/pop in `system.rs`) instead of inventing a second
                // mechanism — without this, `{ my package A { } }; A` and
                // `EVAL 'my package A { }'; A` both stayed resolvable outside
                // their scope, and a *stale* out-of-scope `my class`/`my
                // package A` un-suppressed by `shadow_suppressed_type_with_package`
                // above never got re-suppressed either.
                self.register_lexical_class(name);
                *ip += 1;
            }
            OpCode::RegisterPackageStub { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.registry_mut().package_stubs.insert(name);
                *ip += 1;
            }
            OpCode::ClearPackageStub { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.registry_mut().package_stubs.remove(&name);
                self.registry_mut().reported_stub_errors.remove(&name);
                *ip += 1;
            }

            // -- Phaser END --
            OpCode::PhaserEnd { idx, site_id } => {
                self.sync_source_line(code, *ip);
                self.exec_phaser_end_op(code, *idx, *site_id);
                *ip += 1;
            }

            // -- CHECK Phaser scope --
            OpCode::CheckPhaserStart { .. } => {
                self.sync_source_line(code, *ip);
                self.check_phaser_depth += 1;
                *ip += 1;
            }
            OpCode::CheckPhaserEnd => {
                self.check_phaser_depth = self.check_phaser_depth.saturating_sub(1);
                *ip += 1;
            }

            // -- HyperMethodCall --
            OpCode::HyperMethodCall {
                name_idx,
                arity,
                modifier_idx,
                quoted,
                target_name_idx,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above --
                // the shared `arity` extra-args applied to every element. A
                // method can never be `require` (a bareword sub), so pass "".
                self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
                match self.exec_hyper_method_call_op(
                    code,
                    *name_idx,
                    *arity,
                    *modifier_idx,
                    *quoted,
                    *target_name_idx,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // A per-element method may raise a resumable warn (the
                        // hyper op re-raises it carrying the full result); record
                        // the resume point so `.resume` continues after the call.
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::HyperMethodCallDynamic {
                arity,
                modifier_idx,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
                // `use fatal`: see the comment on the `CallFunc` arm above. A
                // method can never be `require` (a bareword sub), so pass "".
                self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
                match self.exec_hyper_method_call_dynamic_op(
                    code,
                    *arity,
                    *modifier_idx,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume() && self.resume_ip.is_none() {
                            self.resume_ip = Some((Self::resume_code_fp(code), *ip + 1));
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }

            // -- HyperOp --
            OpCode::HyperOp {
                op_idx,
                dwim_left,
                dwim_right,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_hyper_op(code, *op_idx, *dwim_left, *dwim_right)?;
                *ip += 1;
            }

            // -- HyperFuncOp --
            OpCode::HyperFuncOp {
                name_idx,
                dwim_left,
                dwim_right,
                writeback,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_hyper_func_op(
                    code,
                    *name_idx,
                    *dwim_left,
                    *dwim_right,
                    *writeback,
                    compiled_fns,
                )?;
                *ip += 1;
            }

            // -- MetaOp --
            OpCode::MetaOp { meta_idx, op_idx } => {
                self.sync_source_line(code, *ip);
                self.exec_meta_op(code, *meta_idx, *op_idx)?;
                *ip += 1;
            }

            OpCode::MetaOpAssign { meta_idx, op_idx } => {
                self.sync_source_line(code, *ip);
                self.exec_meta_op_assign(code, *meta_idx, *op_idx)?;
                *ip += 1;
            }

            OpCode::MetaOpNary {
                meta_idx,
                op_idx,
                count,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_meta_op_nary(code, *meta_idx, *op_idx, *count)?;
                *ip += 1;
            }

            // -- InfixFunc --
            OpCode::InfixFunc {
                name_idx,
                right_arity,
                modifier_idx,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_infix_func_op(code, *name_idx, *right_arity, modifier_idx, compiled_fns)?;
                *ip += 1;
            }
            OpCode::FlipFlopExpr {
                lhs_end,
                rhs_end,
                site_id,
                exclude_start,
                exclude_end,
                is_fff,
            } => {
                self.exec_flip_flop_expr_op(
                    code,
                    ip,
                    *lhs_end,
                    *rhs_end,
                    *site_id,
                    *exclude_start,
                    *exclude_end,
                    *is_fff,
                    compiled_fns,
                )?;
            }

            // -- Type checking --
            OpCode::TypeCheck(tc_idx, var_name_idx) => {
                self.sync_source_line(code, *ip);
                self.exec_type_check_op(code, *tc_idx, *var_name_idx)?;
                *ip += 1;
            }
            OpCode::TypeCheckBind(tc_idx, var_name_idx) => {
                self.sync_source_line(code, *ip);
                self.exec_type_check_bind_op(code, *tc_idx, *var_name_idx)?;
                *ip += 1;
            }
            OpCode::SetPragma(name_idx) => {
                let value = self.stack.pop().unwrap_or(Value::NIL);
                let name = Self::const_str(code, *name_idx);
                if let ValueView::Str(s) = value.view() {
                    if name == "variables" {
                        loan_env!(self, set_variables_pragma(&s));
                    } else if name == "attributes" {
                        loan_env!(self, set_attributes_pragma(&s));
                    }
                }
                *ip += 1;
            }
            OpCode::IndirectTypeLookup => {
                self.exec_indirect_type_lookup_op();
                *ip += 1;
            }
            OpCode::IndirectCodeLookup(name_idx) => {
                self.exec_indirect_code_lookup_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::SymbolicDeref {
                sigil_idx,
                scopes_idx,
            } => {
                self.exec_symbolic_deref_op(code, *sigil_idx, *scopes_idx);
                *ip += 1;
            }
            OpCode::SymbolicDerefStore(sigil_idx) => {
                self.exec_symbolic_deref_store_op(code, *sigil_idx)?;
                *ip += 1;
            }
            OpCode::IndirectTypeLookupStore => {
                self.exec_indirect_type_lookup_store_op(code)?;
                *ip += 1;
            }
            OpCode::StateVarInit(slot, key_idx) => {
                self.exec_state_var_init_op(code, *slot, *key_idx);
                *ip += 1;
            }
            OpCode::StateVarInitGuard(key_idx, jump_to) => {
                let base_key = crate::symbol::Symbol::from_id(*key_idx);
                let scoped_key = self.scoped_state_key(base_key);
                if self.get_state_var(scoped_key).is_some() {
                    // State already initialized: push a placeholder value on the
                    // stack (StateVarInit will discard it and use the stored value)
                    // and skip the RHS initializer.
                    self.stack.push(Value::NIL);
                    *ip = *jump_to as usize;
                } else {
                    // State not yet initialized: fall through to compile RHS
                    *ip += 1;
                }
            }

            // -- Block scope --
            OpCode::RoutineScope { body_end } => {
                self.exec_routine_scope_op(code, *body_end, ip, compiled_fns)?;
            }

            OpCode::BlockScope {
                pre_end,
                enter_end,
                body_end,
                keep_start,
                undo_start,
                post_start,
                end,
                is_bare_block,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_block_scope_op(
                    code,
                    [
                        *pre_end,
                        *enter_end,
                        *body_end,
                        *keep_start,
                        *undo_start,
                        *post_start,
                        *end,
                    ],
                    *is_bare_block,
                    ip,
                    compiled_fns,
                )?;
            }
            OpCode::BlockLocalScope {
                body_end,
                succeed_boundary,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_block_local_scope_op(
                    code,
                    *body_end,
                    *succeed_boundary,
                    ip,
                    compiled_fns,
                )?;
            }
            OpCode::SucceedBarrier { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_succeed_barrier_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::ResetStateLocals { body_end } => {
                self.reset_state_locals_in_range(code, *ip + 1, *body_end as usize);
                *ip += 1;
            }
            OpCode::CheckPhaser {
                is_pre,
                condition_idx,
            } => {
                self.sync_source_line(code, *ip);
                let condition = condition_idx.map(|idx| Self::const_str(code, idx).to_string());
                self.exec_check_phaser_op(*is_pre, condition)?;
                *ip += 1;
            }
            OpCode::LeaveGuard { .. } => {
                // No-op marker; the guarded queue runner uses the `next` field
                // to find the next LEAVE phaser boundary on error.
                *ip += 1;
            }
            OpCode::DoBlockExpr {
                body_end,
                label,
                scope_isolate,
                isolate_decls_idx,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_do_block_expr_op(
                    code,
                    *body_end,
                    label,
                    *scope_isolate,
                    *isolate_decls_idx,
                    ip,
                    compiled_fns,
                )?;
            }
            OpCode::OnceExpr { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_once_expr_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::BeginOnceExpr { body_end, site_id } => {
                self.sync_source_line(code, *ip);
                self.exec_begin_once_expr_op(code, *body_end, *site_id, ip, compiled_fns)?;
            }
            OpCode::DoGivenExpr { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_do_given_expr_op(code, *body_end, ip, compiled_fns)?;
            }

            // -- Closures and registration --
            OpCode::MakeGather(idx, cc_idx) => {
                self.sync_source_line(code, *ip);
                self.exec_make_gather_op(code, *idx, *cc_idx)?;
                *ip += 1;
            }
            OpCode::Eager => {
                self.sync_source_line(code, *ip);
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let result = match val.view() {
                    ValueView::LazyList(ll) => {
                        let items = self.force_lazy_list_vm(&ll)?;
                        // Sync interpreter env changes back to Interpreter locals.
                        // This ensures side effects from gather bodies propagate
                        // to outer-scope variables (e.g., `$was-lazy = 0`).
                        for (i, name) in code.locals.iter().enumerate() {
                            if let Some(v) = self.env().get(name)
                                && i < self.locals.len()
                            {
                                self.locals[i] = v.clone();
                            }
                        }
                        Value::array(items)
                    }
                    ValueView::Seq(body) => {
                        // `.eager` consumes the source (ADR-0034; verified
                        // against raku), unless already reified/cache-requested.
                        let body = std::sync::Arc::clone(&body);
                        let (items, _) = self.take_seq_body(&body)?;
                        Value::array(items)
                    }
                    _ if val.is_range() => Value::array(crate::runtime::utils::value_to_list(&val)),
                    _ => val,
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::MakeAnonSub(idx, cc_idx, is_block) => {
                self.sync_source_line(code, *ip);
                self.exec_make_anon_sub_op(code, *idx, *cc_idx, *is_block)?;
                *ip += 1;
            }
            OpCode::MakeAnonSubParams(idx, cc_idx, is_wc) => {
                self.sync_source_line(code, *ip);
                self.exec_make_anon_sub_params_op(code, *idx, *cc_idx, *is_wc)?;
                *ip += 1;
            }
            OpCode::MakeLambda(idx, cc_idx, is_wc) => {
                self.sync_source_line(code, *ip);
                self.exec_make_lambda_op(code, *idx, *cc_idx, *is_wc)?;
                *ip += 1;
            }
            OpCode::IndexAssignGeneric => {
                self.exec_index_assign_generic_op(code)?;
                *ip += 1;
            }
            OpCode::MakeBlockClosure(idx, cc_idx) => {
                self.sync_source_line(code, *ip);
                self.exec_make_block_closure_op(code, *idx, *cc_idx)?;
                *ip += 1;
            }
            OpCode::RegisterDecl(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_decl_op(code, *idx, compiled_fns)?;
                *ip += 1;
            }
            OpCode::UseModule {
                name_idx,
                tags_idx,
                arg_count,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_use_module_op(code, *name_idx, *tags_idx, *arg_count)?;
                *ip += 1;
            }
            OpCode::ImportModule { name_idx, tags_idx } => {
                self.sync_source_line(code, *ip);
                self.exec_import_module_op(code, *name_idx, *tags_idx)?;
                *ip += 1;
            }
            OpCode::NoModule(name_idx) => {
                self.sync_source_line(code, *ip);
                self.exec_no_module_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::NeedModule(name_idx) => {
                self.sync_source_line(code, *ip);
                self.exec_need_module_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::UseLibPath => {
                self.exec_use_lib_path_op(code)?;
                *ip += 1;
            }
            OpCode::PushImportScope => {
                self.push_import_scope();
                *ip += 1;
            }
            OpCode::PopImportScope => {
                self.pop_import_scope();
                *ip += 1;
            }
            OpCode::RegisterEnum(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_enum_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::AugmentClass(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_augment_class_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterSubset(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_subset_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::SubtestScope { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_subtest_scope_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::ReactScope { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_react_scope_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::WheneverScope {
                body_idx,
                analysis_cc_idx,
                param_idx,
                yields_value,
                param_type_idx,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_whenever_scope_op(
                    code,
                    *body_idx,
                    *analysis_cc_idx,
                    param_idx,
                    *yields_value,
                    param_type_idx,
                )?;
                *ip += 1;
            }

            // -- Local variables --
            OpCode::GetLocal(idx) => {
                self.exec_get_local_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::GetLocalMetaAssign { slot, identity } => {
                self.exec_get_local_op(code, *slot)?;
                self.exec_meta_assign_identity_op(*identity)
                    .inspect_err(|_| {
                        self.sync_source_line(code, *ip);
                    })?;
                *ip += 1;
            }
            OpCode::GetLocalRaw(idx) => {
                self.exec_get_local_raw_op(*idx);
                *ip += 1;
            }
            OpCode::GetLocalDeferred(idx) => {
                self.exec_get_local_deferred_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::GetScalarContainer {
                name_idx,
                local_idx,
            } => {
                self.exec_get_scalar_container_op(code, *name_idx, *local_idx);
                *ip += 1;
            }
            OpCode::SetLocal(idx) => {
                self.exec_set_local_op(code, *idx)?;
                self.publish_state_local(code, *idx);
                *ip += 1;
            }
            OpCode::SetLocalDecl {
                slot,
                explicit_init,
            } => {
                // The fused form of `MarkExplicitInitializerContext;
                // MarkVarDeclContext; SetLocal` (ADR-0006 §2.3): set the very
                // flags those markers set, then run the identical SetLocal body
                // (which reads and clears them).
                self.explicit_initializer_context.set(*explicit_init);
                self.vardecl_context.set(true);
                self.exec_set_local_op(code, *slot)?;
                self.publish_state_local(code, *slot);
                *ip += 1;
            }
            OpCode::DeclareOurScalar {
                slot,
                qualified_idx,
            } => {
                self.exec_declare_our_scalar_op(code, *slot, *qualified_idx);
                *ip += 1;
            }
            OpCode::SetVarDynamic { name_idx, dynamic } => {
                self.exec_set_var_dynamic_op(code, *name_idx, *dynamic);
                *ip += 1;
            }
            OpCode::RegisterVarExport { name_idx, tags_idx } => {
                self.exec_register_var_export_op(code, *name_idx, *tags_idx)?;
                *ip += 1;
            }
            OpCode::ApplyVarTrait {
                name_idx,
                trait_name_idx,
                has_arg,
                slot,
            } => {
                self.exec_apply_var_trait_op(code, *name_idx, *trait_name_idx, *has_arg, *slot)?;
                *ip += 1;
            }
            OpCode::GetCallerVar { name_idx, depth } => {
                let name = Self::const_str(code, *name_idx);
                let val = loan_env!(self, get_caller_var(name, *depth as usize))?;
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetCallersVar {
                name_idx,
                depth,
                cascade,
            } => {
                let name = Self::const_str(code, *name_idx);
                let val = loan_env!(self, get_callers_var(name, *depth as usize, *cascade))?;
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::SetCallerVar { name_idx, depth } => {
                let val = self.stack.pop().unwrap_or(Value::NIL);
                let name = Self::const_str(code, *name_idx);
                loan_env!(self, set_caller_var(name, *depth as usize, val))?;
                *ip += 1;
            }
            OpCode::BindCallerVar {
                target_idx,
                source_idx,
                depth,
            } => {
                let target = Self::const_str(code, *target_idx);
                let source = Self::const_str(code, *source_idx);
                self.bind_caller_var(target, source, *depth as usize)?;
                *ip += 1;
            }
            OpCode::GetOuterVar {
                name_idx,
                depth,
                slot,
            } => {
                let name = Self::const_str(code, *name_idx);
                let val = self.get_outer_var(code, name, *depth as usize, *slot);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetCallerOuterVar {
                name_idx,
                depth,
                slot,
            } => {
                let name = Self::const_str(code, *name_idx);
                // The target scope declares the name (the compiler emits a Nil
                // constant otherwise), so a non-dynamic binding here is the
                // X::Caller::NotDynamic case, not an absent one.
                if !self.is_var_dynamic(name) {
                    return Err(crate::runtime::utils::caller_not_dynamic_error(name));
                }
                let val = self.get_outer_var(code, name, *depth as usize, *slot);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetDynamicVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                // An unfound dynamic via the DYNAMIC:: pseudo-package is undefined,
                // not an error (roast pseudo-6c: `!defined($DYNAMIC::x82)`, and the
                // "no guts spillage" deep-chain lookup must eval-live).
                let val = loan_env!(self, get_dynamic_var(name)).unwrap_or(Value::NIL);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::AssignExprLocal(idx) => {
                self.exec_assign_expr_local_op(code, *idx)?;
                self.publish_state_local(code, *idx);
                *ip += 1;
            }
            OpCode::AssignReadOnly => {
                return Err(RuntimeError::assignment_ro(None));
            }
            OpCode::CheckReadOnly(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                // A `:=`-bound container (`my %a := %b`) is marked readonly as a
                // bind signal, but a whole reassignment (`%a = (...)`) is allowed
                // — it writes through to the bound source. The `__mutsu_bound::`
                // marker distinguishes it from a genuinely immutable `constant`.
                // Both marker probes are gated on their process-global
                // "ever created" flags: this opcode runs on every whole-variable
                // assignment (per iteration in tight loops), and the common
                // program never creates either marker — skipping the two
                // `format!` allocations plus env lookups entirely.
                if crate::env::bound_marker_possible() {
                    let bound_key = format!("__mutsu_bound::{}", name);
                    if matches!(
                        self.env().get(&bound_key).map(Value::view),
                        Some(ValueView::Bool(true))
                    ) {
                        *ip += 1;
                        return Ok(());
                    }
                }
                // Probe through the pre-interned constant Symbol: this opcode
                // runs on every whole-variable assignment (per iteration in
                // tight loops), and `check_readonly_for_modify(name)` would
                // re-intern the name on each execution just to miss the set.
                // The error construction (readonly hit) is the cold path.
                if self.is_readonly_sym(code.const_sym(*name_idx)) {
                    self.check_readonly_for_modify(name)?;
                }
                // Also check env-based readonly status set by cross-scope
                // `:=` binding (e.g. binding to a readonly sub parameter
                // in a closure).  The readonly_vars set is scope-local
                // and gets restored on frame pop, but the env key persists.
                if crate::env::closure_meta_keys_possible() {
                    let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
                    if matches!(
                        self.env().get(&readonly_key).map(Value::view),
                        Some(ValueView::Bool(true))
                    ) {
                        // A sigilless term (`my \\c = 5`) IS the value, so
                        // rakudo names the value in the error: "Cannot modify
                        // an immutable Int (5)".
                        return Err(self.immutable_value_error(name));
                    }
                }
                *ip += 1;
            }
            OpCode::MarkVarReadonly(name_idx, kind) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.mark_readonly_with(&name, *kind);
                *ip += 1;
            }

            // -- Let scope management --
            OpCode::LetSave {
                name_idx,
                index_mode,
                is_temp,
                slot,
            } => {
                self.exec_let_save_op(code, *name_idx, *index_mode, *is_temp, *slot);
                *ip += 1;
            }
            OpCode::LetBlock { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_let_block_op(code, *body_end, ip, compiled_fns)?;
            }
        }
        Ok(())
    }

    /// Adopt the source line of the instruction at `ip` as the current line.
    ///
    /// The line is static per-instruction data (`CompiledCode::op_lines`), so no
    /// dispatched instruction is needed to carry it (the former `SetSourceLine`).
    /// Instead this is called from the instructions that can *observe* a line —
    /// every call/reentry into user code, frame push, declaration registration
    /// and raise site — and from the JIT call shims, whose native code has no
    /// interpreter loop to refresh anything. Instructions that cannot observe it
    /// (arithmetic, local slots, jumps, stack shuffling) pay nothing at all,
    /// which is the point: refreshing on every op costs more than the dispatch it
    /// saves (measured: +7.8% instructions on fib).
    ///
    /// A chunk with no line information for `ip` (hand-built bytecode) leaves the
    /// current line untouched.
    #[inline]
    pub(crate) fn sync_source_line(&mut self, code: &CompiledCode, ip: usize) {
        if let Some(line) = code.line_at(ip) {
            self.cur_source_line = line;
        }
    }

    /// Check if a value represents a "successful" block exit for `let` purposes.
    /// A block is considered successful if it returns a defined value.
    /// Type objects (Package) and Nil are undefined and count as failure.
    pub(crate) fn is_let_success(val: &Value) -> bool {
        crate::runtime::types::value_is_defined(val)
    }

    /// Pull back into the compiler-baked local slots any slot-backing env entry
    /// an internally-dispatched method changed (`pre_env` is the snapshot taken
    /// before the dispatch). Used by the sink-context arms, which run user code
    /// with no surrounding call op to drain the captured-outer writeback.
    fn reconcile_locals_from_env(&mut self, code: &CompiledCode, pre_env: &[Option<Value>]) {
        for (i, name) in code.locals.iter().enumerate() {
            if name.starts_with('!')
                || matches!(self.locals[i].view(), ValueView::HashEntryRef { .. })
            {
                continue;
            }
            let cur = self.env().get(name).cloned().or_else(|| {
                name.strip_prefix('$')
                    .or_else(|| name.strip_prefix('@'))
                    .or_else(|| name.strip_prefix('%'))
                    .or_else(|| name.strip_prefix('&'))
                    .and_then(|b| self.env().get(b).cloned())
            });
            if let Some(cur) = cur
                && pre_env.get(i).map(|p| p.as_ref()) != Some(Some(&cur))
            {
                self.locals[i] = cur;
            }
        }
    }
}
