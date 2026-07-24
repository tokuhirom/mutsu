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
            && e.control.is_none()
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
                // captures empty, no real current package), @/% names keep the
                // slow path's atomic/thread-clone arms, and Nil / LazyThunk /
                // ContainerRef hits fall through for the slow tail's
                // default/type-object, force and deref handling.
                let fast_hit = {
                    let b0 = name.as_bytes().first().copied();
                    if !matches!(b0, Some(b'@' | b'%'))
                        && (name == "_" || !name.contains('_'))
                        && self.escaping_our_lexical_names.is_empty()
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
                    .escaping_our_read(name)
                    .or_else(|| self.package_scope_lexical(name))
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
                    .or_else(|| {
                        // Bare-name fallback: when looking up an unqualified
                        // name (e.g. `msg` or `$msg`) inside a routine whose
                        // current_package is a real package (e.g. `Gee`), try
                        // resolving via the package's `our` store. This makes
                        // `our $msg` accessible from `our sub talk { $msg }`
                        // when `talk` is invoked from outside the package.
                        if name.contains("::") {
                            return None;
                        }
                        let cur = self.current_package().to_string();
                        if cur.is_empty() || cur == "GLOBAL" || cur.contains("::&") {
                            return None;
                        }
                        // Skip special names that shouldn't be package-qualified.
                        let bare_first = name.trim_start_matches(['$', '@', '%', '&']);
                        if bare_first.is_empty() {
                            return None;
                        }
                        let first_ch = bare_first.chars().next().unwrap();
                        if matches!(first_ch, '_' | '/' | '!' | '?' | '*' | '.' | '=')
                            || first_ch.is_ascii_digit()
                        {
                            return None;
                        }
                        let candidate = if let Some(rest) = name.strip_prefix('$') {
                            format!("${cur}::{rest}")
                        } else if let Some(rest) = name.strip_prefix('@') {
                            format!("@{cur}::{rest}")
                        } else if let Some(rest) = name.strip_prefix('%') {
                            format!("%{cur}::{rest}")
                        } else if let Some(rest) = name.strip_prefix('&') {
                            format!("&{cur}::{rest}")
                        } else {
                            format!("{cur}::{name}")
                        };
                        self.get_our_var(&candidate)
                            .cloned()
                            .or_else(|| self.get_env_with_main_alias(&candidate))
                    })
                    .or_else(|| {
                        // Package-block `my` lexical fallback: a named sub defined in
                        // a `package Foo { my $x = ...; sub f { $x } }` block closes
                        // over `$x`, but the block scope is dropped on exit and named
                        // registry subs have no per-sub closure env. When `f` runs
                        // by-name `current_package` is `Foo`, so resolve the miss via
                        // the per-package store recorded by `exec_package_scope_op`.
                        // Gated on `current_package` so it never leaks to bare refs
                        // after the block (those run under `GLOBAL`).
                        if name.contains("::") {
                            return None;
                        }
                        let cur = self.current_package().to_string();
                        if cur.is_empty() || cur == "GLOBAL" {
                            return None;
                        }
                        self.package_lexicals
                            .get(&cur)
                            .and_then(|m| m.get(name))
                            .cloned()
                    })
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
                // Force lazy thunks transparently on access
                let val = if let ValueView::LazyThunk(thunk_data) = val.view() {
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
                    // Array-contextualizing a Seq (`@$s`) caches it, so it may be
                    // read repeatedly. If the Seq's iterator was already taken
                    // (e.g. by `.skip`/`.iterator`) and not cached, throw.
                    ValueView::Seq(items) => {
                        if crate::value::seq_is_consumed(&items)
                            && !crate::value::seq_is_cached(&items)
                        {
                            return Err(crate::value::seq_consumed_error());
                        }
                        crate::value::seq_mark_cached(&items);
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
                    });
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
            OpCode::GetOurVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .get_our_var(name)
                    .cloned()
                    .or_else(|| self.get_env_with_main_alias(name))
                    .unwrap_or(Value::NIL);
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
                let is_bind_ctx = self.bind_context;
                let is_rebind = self.rebind_context;
                self.bind_context = false;
                // Consume the scalar-bind marker here too: a topic bind
                // (`$_ := $d`) compiles MarkScalarBindContext + SetGlobal, so
                // without this the flag leaks into the NEXT SetLocal (e.g. a
                // following `my $a = 0`), which would spuriously treat it as a
                // value-bind and mark it readonly.
                self.scalar_bind_context = false;
                // Slice 2a: `our $n = @z` / a global scalar target reaches SetGlobal,
                // not SetLocal/AssignExpr. Consume the array-share flag here (the
                // global copies for now — reference sharing for globals is Slice 2d)
                // so it cannot leak into the next SetLocal.
                self.array_share_context = false;
                self.array_share_source = None;
                // Only clear rebind_context if this is actually a binding operation
                if is_rebind {
                    self.rebind_context = false;
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
                if name_str == "__ANON_STATE__"
                    && !raw_mode
                    && !is_rebind
                    && !self.fatal_mode
                    && self.var_type_constraint_fast(name_str).is_none()
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
                if self.strict_mode
                    && !is_attr_twigil
                    && !is_internal_temp
                    && !name.contains("::")
                    && !self.env().contains_key(&name)
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
                if !raw_mode && !is_bind_ctx && !is_bound_container {
                    self.check_readonly_for_modify(&name)?;
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
                // Reject assignment to immutable type objects (e.g., `Foo .= new`)
                if !name.starts_with('$')
                    && !name.starts_with('@')
                    && !name.starts_with('%')
                    && !name.starts_with('&')
                    && !name.contains("::")
                    && matches!(
                        self.env().get(&name).map(Value::view),
                        Some(ValueView::Package(_))
                    )
                    && self.has_class(&name)
                {
                    return Err(RuntimeError::new(format!(
                        "Cannot modify an immutable '{}' type object",
                        name
                    )));
                }
                let raw_val = self.stack.pop().unwrap_or(Value::NIL);
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
                    } else {
                        runtime::coerce_to_array(raw_val)
                    }
                } else {
                    raw_val
                };
                if (name.starts_with('@') || name.starts_with('%'))
                    && (loan_env!(self, var_type_constraint(&name)).is_some()
                        || loan_env!(self, var_hash_key_constraint(&name)).is_some())
                {
                    val = self.coerce_typed_container_assignment(&name, val, false)?;
                } else if name.starts_with('@')
                    && name.len() > 1
                    && !name.contains("__")
                    && loan_env!(self, var_type_constraint(&name)).is_none()
                    && matches!(val.view(), ValueView::Array(d, _) if d.value_type.is_none() && d.declared_type.is_none())
                {
                    // `@a = list` where `@a` has no *declared* element type but is
                    // an alias / for-loop binding of an element-typed array
                    // (`array[int]`): the assignment writes INTO that container, so
                    // preserve its declared element type rather than replacing it
                    // with an untyped `Array`. Internal/anonymous names
                    // (`@__ANON_ARRAY__`, `@__mutsu_*`) are excluded: they are
                    // fresh per use and must not inherit a stale slot's type. The
                    // guard `val` is itself untyped is essential: when the RHS
                    // already carries a type (e.g. a `DeitemizeForBind` chunk
                    // element), that type wins — reading a possibly-stale slot here
                    // would clobber it (the cross-type for-loop contamination).
                    let old_info = match self
                        .get_env_with_main_alias(&name)
                        .as_ref()
                        .map(Value::view)
                    {
                        Some(ValueView::Array(old, _))
                            if old.value_type.is_some() || old.declared_type.is_some() =>
                        {
                            Some(crate::runtime::ContainerTypeInfo {
                                value_type: old.value_type.clone().unwrap_or_default(),
                                key_type: None,
                                declared_type: old.declared_type.clone(),
                            })
                        }
                        _ => None,
                    };
                    if let Some(info) = old_info {
                        val = self.tag_container_metadata(val, info);
                    }
                }
                if let Some(constraint) = loan_env!(self, var_type_constraint(&name))
                    && !name.starts_with('%')
                    && !name.starts_with('@')
                {
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
                        val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
                    }
                    // Wrap native integer values on assignment (overflow wrapping)
                    val = Self::wrap_native_int_by_constraint(&constraint, val)?;
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
                    self.env_mut()
                        .insert(alias_key.clone(), Value::str(resolved_source.clone()));
                    self.mark_sigilless_alias_seen();
                    // Propagate readonly status from the source variable.
                    // Binding to a readonly parameter should make the target
                    // readonly as well (persisted in env for cross-scope survival).
                    let source_readonly = self.is_readonly(source_name);
                    self.env_mut()
                        .insert(readonly_key.clone(), Value::truth(source_readonly));
                    if source_readonly {
                        self.mark_readonly(&name);
                    }
                    // Create a shared ContainerRef for cross-scope binding persistence.
                    if !name.starts_with('@')
                        && !name.starts_with('%')
                        && !name.starts_with('&')
                        && !source_readonly
                    {
                        let container = if let ValueView::ContainerRef(arc) = val.view() {
                            Value::container_ref(arc.clone())
                        } else {
                            val.clone().into_container_ref()
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
                        // Propagate to saved call frames (env AND locals)
                        for frame in self.call_frames.iter_mut().rev() {
                            // `code.locals` is this frame's slot layout, not the
                            // parent's; only write a parent frame's `saved_locals`
                            // when that frame owns the source lexical (its saved env
                            // holds the name), else the callee slot index clobbers an
                            // unrelated same-index local.
                            if frame.saved_env.contains_key(&resolved_source) {
                                frame
                                    .saved_env
                                    .insert(resolved_source.clone(), container.clone());
                                // Also update saved locals so a later restore doesn't
                                // overwrite the ContainerRef with a stale plain value.
                                for (i, local_name) in code.locals.iter().enumerate() {
                                    if local_name == &resolved_source
                                        && i < frame.saved_locals.len()
                                    {
                                        frame.saved_locals[i] = container.clone();
                                    }
                                }
                            }
                        }
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
                // Write through ContainerRef: update inner value for env-based variables.
                // Return early to avoid overwriting the ContainerRef in env with a plain value.
                if !is_rebind && !raw_mode {
                    // Check env directly (not through alias resolution to avoid circular lookups)
                    if let Some(cell_val) = self.env().get(&name).cloned()
                        && let ValueView::ContainerRef(arc) = cell_val.view()
                    {
                        self.check_container_cell_constraint(&arc, &val)?;
                        // Preserve the inner container's identity (§3): a boxed
                        // captured `@a`/`%h` whole-reassigned here must keep its
                        // backing `Gc` so by-value holders observe the update.
                        Self::cell_store_preserving_container_identity(&arc, &val);
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
                        Self::cell_store_preserving_container_identity(&arc, &val);
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
                        Self::cell_store_preserving_container_identity(&arc, &val);
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
                        && let Some((arc, key)) = token.hash_entry_terminal()
                    {
                        let cell = crate::gc::Gc::new(std::sync::Mutex::new(val.clone()));
                        // SAFETY: aliased in-place mutation of a shared hash; see
                        // `arc_contents_mut`. No live borrow into the map.
                        let hd = unsafe { crate::value::gc_contents_mut(&arc) };
                        Value::hash_insert_through(
                            &mut hd.map,
                            key,
                            Value::container_ref(cell.clone()),
                        );
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
                let sg_is_vardecl = self.vardecl_context;
                self.vardecl_context = false;
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
                if raw_mode && name.starts_with('@') {
                    // For `constant @x`, bypass set_shared_var's List→Array
                    // normalization so the container type (List) is preserved.
                    self.env_mut().insert(name.clone(), val.clone());
                } else {
                    self.set_env_with_main_alias(&name, val.clone());
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
                self.set_our_var(name.clone(), val.clone());
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
                self.writeback_package_scope_var(&name, &val);
                // Track topic mutations for map rw writeback
                if name == "_" {
                    self.env_mut()
                        .insert("__mutsu_rw_map_topic__".to_string(), val.clone());
                }
                // Sync to shared_vars for cross-thread visibility.
                // Skip for raw_mode @-variables to preserve List kind.
                if !(raw_mode && name.starts_with('@')) {
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
                {
                    let source_name = source_var.clone();
                    self.set_env_with_main_alias(&source_name, val.clone());
                    self.update_local_if_exists(code, &source_name, &val);
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
                self.mirror_array_hash_attr_to_cell(code, *name_idx, None);
                *ip += 1;
            }
            OpCode::SetVarType { name_idx, tc_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let raw_constraint = Self::const_str(code, *tc_idx).to_string();
                // Resolve type capture variables (e.g., `T` → `Int` when `::T`
                // was captured earlier in the signature).
                let constraint = loan_env!(self, resolved_type_capture_name(&raw_constraint));
                // Clear stale atomic CAS state when an @-variable is
                // (re-)declared with a type constraint like atomicint.
                if name.starts_with('@') && constraint == "atomicint" {
                    self.clear_atomic_array_state(&name);
                }
                self.vm_set_var_type_constraint(&name, Some(constraint.clone()));
                // For scalar variables, if the current value is Nil, set it to the type object.
                // Exception: if the constraint is "Nil", keep the value as Nil
                // (the Nil type object is Nil itself, not the Package "Nil").
                if !name.starts_with('@') && !name.starts_with('%') && constraint != "Nil" {
                    let is_nil = matches!(
                        self.env().get(&name).map(Value::view),
                        Some(ValueView::Nil) | None
                    );
                    if is_nil {
                        // Native types get zero/empty defaults instead of type objects.
                        let init_val =
                            if crate::runtime::native_types::is_native_int_type(&constraint) {
                                Value::int(0)
                            } else if matches!(constraint.as_str(), "num" | "num32" | "num64") {
                                Value::num(0.0)
                            } else if constraint == "str" {
                                Value::str(String::new())
                            } else {
                                // A parameterized role constraint (`my Cup of
                                // EggNog $mug` / `my Cup[EggNog] $mug`) resolves
                                // to the ParametricRole type object so .WHAT /
                                // .raku keep the type arguments. The stored
                                // constraint metadata normalizes to the base
                                // name, so probe the raw constraint here.
                                let parametric = constraint.contains('[').then(|| {
                                    loan_env!(self, type_arg_value_from_name(&constraint))
                                });
                                match parametric {
                                    Some(v)
                                        if matches!(v.view(), ValueView::ParametricRole { .. }) =>
                                    {
                                        v
                                    }
                                    _ => Value::package(Symbol::intern(
                                        &loan_env!(self, var_type_constraint(&name))
                                            .unwrap_or(constraint.clone()),
                                    )),
                                }
                            };
                        self.set_env_with_main_alias(&name, init_val.clone());
                        self.update_local_if_exists(code, &name, &init_val);
                    }
                } else if let Some(value) = self.get_env_with_main_alias(&name) {
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type: loan_env!(self, var_type_constraint(&name))
                            .unwrap_or(constraint),
                        key_type: if name.starts_with('%') {
                            loan_env!(self, var_hash_key_constraint(&name))
                        } else {
                            None
                        },
                        declared_type: None,
                    };
                    // Hashes embed metadata in `HashData`; write the tagged value
                    // back (no-op Arc for array/instance side-table containers).
                    // Tagging an object hash also re-keys it by `.WHICH`
                    // (see `tag_container_metadata`).
                    let tagged = self.tag_container_metadata(value, info);
                    self.set_env_with_main_alias(&name, tagged.clone());
                    self.update_local_if_exists(code, &name, &tagged);
                }
                *ip += 1;
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
                self.exec_int_bit_neg_op();
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
                let is_bound_decont = if self.bound_decont_active {
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
                let cell = crate::gc::Gc::new(std::sync::Mutex::new(val));
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
            OpCode::WrapVarRef(name_idx) => {
                self.exec_wrap_var_ref_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::MarkBindContext => {
                self.bind_context = true;
                *ip += 1;
            }
            OpCode::MarkScalarBindContext => {
                self.scalar_bind_context = true;
                *ip += 1;
            }
            OpCode::MarkRebindContext => {
                self.rebind_context = true;
                *ip += 1;
            }
            OpCode::MarkAccessorRefContext => {
                self.accessor_ref_pending = true;
                *ip += 1;
            }
            OpCode::MarkArrayShareSource(name_idx) => {
                self.array_share_context = true;
                self.array_share_source = Some(Self::const_str(code, *name_idx).to_string());
                *ip += 1;
            }
            OpCode::MarkElementShare => {
                self.element_share_pending = true;
                *ip += 1;
            }
            OpCode::MarkConstantContext => {
                self.constant_context = true;
                *ip += 1;
            }
            OpCode::MarkExplicitInitializerContext => {
                self.explicit_initializer_context = true;
                *ip += 1;
            }
            OpCode::MarkVarDeclContext => {
                self.vardecl_context = true;
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
                self.exec_before_after_op(is_before);
                *ip += 1;
            }
            OpCode::Cmp => {
                self.exec_cmp_op();
                *ip += 1;
            }
            OpCode::Coll => {
                self.exec_coll_op();
                *ip += 1;
            }
            OpCode::Unicmp => {
                self.exec_unicmp_op();
                *ip += 1;
            }
            OpCode::Leg => {
                self.exec_leg_op();
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
                lhs_var,
                lhs_slot,
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
                    lhs_var,
                    *lhs_slot,
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
                                    | ValueView::LazyIoLines { .. }
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
                let call_file = self.current_source_file();
                self.push_block_routine_with_location(
                    self.current_package(),
                    String::new(),
                    call_line,
                    call_file,
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
                if let Some(val) = self.stack.last() {
                    if let Some(err) = self.failure_to_runtime_error_if_unhandled(val) {
                        return Err(err);
                    }
                    // Under `use fatal`, a block/routine that returns a reified
                    // list/Seq whose element is an unhandled Failure throws too
                    // (`use fatal; { "a".map: *.Int }()`).
                    if self.fatal_mode
                        && let Some(err) = self.unhandled_failure_in_list_for_fatal(val)
                    {
                        return Err(err);
                    }
                }
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
            OpCode::SinkPop(user_sink) => {
                self.sync_source_line(code, *ip);
                let user_sink = *user_sink;
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
                    let sink_class = if !user_sink {
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
                        *ip += 1;
                        return Ok(());
                    }
                    match val.view() {
                        // A `.cache`-returned view is a cached, re-iterable list;
                        // sinking it is a no-op and must NOT drain the underlying
                        // source (e.g. `(my $l = $cat.lines).cache;` keeps the cat
                        // unread). A bare lazy Seq still drains below.
                        ValueView::LazyList(list) if list.is_cached_no_sink() => {}
                        ValueView::LazyList(list) => {
                            self.force_lazy_list_vm(&list)?;
                        }
                        ValueView::LazyIoLines { handle, words, .. } => {
                            // Sinking a lazy IO lines iterator must drain the
                            // underlying handle so that side effects (read
                            // position, .eof) are observable.
                            loan_env!(self, force_lazy_io_lines(handle, words))?;
                        }
                        _ => {
                            // Sinking an unhandled Failure always throws (Raku behavior)
                            if let Some(err) = self.failure_to_runtime_error_if_unhandled(&val) {
                                return Err(err);
                            }
                            // Under `use fatal`, sinking a reified list/Seq whose
                            // element is an unhandled Failure also throws (e.g.
                            // `use fatal; "a".map: *.Int`). Non-fatal code keeps
                            // its soft-Failure lists (a plain `[Failure]` sink is a
                            // no-op without fatal).
                            if self.fatal_mode
                                && let Some(err) = self.unhandled_failure_in_list_for_fatal(&val)
                            {
                                return Err(err);
                            }
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
                    self.exec_make_array_op(code, *n, false);
                }
                *ip += 1;
            }
            OpCode::MakeRealArray(n) => {
                self.exec_make_array_op(code, *n, true);
                *ip += 1;
            }
            OpCode::MakeRealArrayNoFlatten(n) => {
                self.exec_make_array_no_flatten_op(*n);
                *ip += 1;
            }
            OpCode::MakeHash(n) => {
                self.exec_make_hash_op(*n)?;
                *ip += 1;
            }
            OpCode::MakeHashFromPairs(n) => {
                self.exec_make_hash_from_pairs_op(*n);
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
            } => {
                self.sync_source_line(code, *ip);
                match self.exec_call_method_dynamic_op(code, *arity, *modifier_idx) {
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
            } => {
                self.sync_source_line(code, *ip);
                let pre = self.array_hash_attr_env_snapshot(code, *target_name_idx);
                match self.exec_call_method_dynamic_mut_op(
                    code,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
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
                self.mirror_array_hash_attr_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::ArrayPush {
                target_name_idx,
                value_source_idx,
            } => {
                self.sync_source_line(code, *ip);
                let pre = self.array_hash_attr_env_snapshot(code, *target_name_idx);
                self.exec_array_push_op(code, *target_name_idx, *value_source_idx)?;
                self.mirror_array_hash_attr_to_cell(code, *target_name_idx, pre);
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
                let pre = self.array_hash_attr_env_snapshot(code, *target_name_idx);
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
                self.mirror_array_hash_attr_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::CallOnValue {
                arity,
                arg_sources_idx,
            } => {
                self.sync_source_line(code, *ip);
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
                slip_positions_idx,
                keep_value,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_exec_call_pairs_op(
                    code,
                    compiled_fns,
                    *name_idx,
                    *arity,
                    *slip_positions_idx,
                    *keep_value,
                )?;
                *ip += 1;
            }

            // -- Indexing --
            OpCode::Index { is_positional } => {
                self.exec_index_op_with_positional(*is_positional)?;
                *ip += 1;
            }
            OpCode::IndexAutovivifyLazy => {
                self.exec_index_autovivify_lazy_op(false)?;
                *ip += 1;
            }
            OpCode::IndexAutovivifyLazyTerminal => {
                self.exec_index_autovivify_lazy_op(true)?;
                *ip += 1;
            }
            OpCode::DeleteIndexNamed(name_idx, slot) => {
                let pre = self.array_hash_attr_env_snapshot(code, *name_idx);
                self.exec_delete_index_named_op(code, *name_idx, *slot)?;
                self.mirror_array_hash_attr_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::DeleteIndexExpr => {
                self.exec_delete_index_expr_op()?;
                *ip += 1;
            }
            OpCode::MultiDimIndex(ndims) => {
                self.exec_multi_dim_index_op(*ndims)?;
                *ip += 1;
            }
            OpCode::MultiDimIndexAssign { name_idx, ndims } => {
                let pre = self.array_hash_attr_env_snapshot(code, *name_idx);
                self.exec_multi_dim_index_assign_op(code, *name_idx, *ndims)?;
                self.mirror_array_hash_attr_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::MultiDimIndexAssignGeneric(ndims) => {
                self.exec_multi_dim_index_assign_generic_op(*ndims)?;
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
            OpCode::Last(label) => {
                let mut sig = RuntimeError::last_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Next(label) => {
                let mut sig = RuntimeError::next_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Redo(label) => {
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
                return Err(RuntimeError::react_done_signal());
            }
            OpCode::TagContainerRef(name_idx, slot) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.container_ref_var = Some((name, *slot));
                self.container_ref_reversed = false;
                *ip += 1;
            }
            OpCode::TagContainerRefReversed(name_idx, slot) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.container_ref_var = Some((name, *slot));
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
                            // see `arc_contents_mut`.
                            unsafe { crate::value::gc_contents_mut(&arc).items.clear() };
                        }
                        ValueView::Hash(arc) => {
                            // SAFETY: aliased in-place clear; see `arc_contents_mut`.
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
                            // SAFETY: aliased in-place clear; see `arc_contents_mut`.
                            unsafe { crate::value::gc_contents_mut(&arc).items.clear() };
                        }
                        ValueView::Hash(arc) => {
                            // SAFETY: aliased in-place clear; see `arc_contents_mut`.
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
                *ip += 1;
            }
            OpCode::PostDecrement(name_idx, slot) => {
                self.exec_post_decrement_op(code, *name_idx, *slot)?;
                *ip += 1;
            }
            OpCode::PostIncrementIndex(name_idx) => {
                self.exec_post_increment_index_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PostDecrementIndex(name_idx) => {
                self.exec_post_decrement_index_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional,
                target_slot,
            } => {
                let pre = self.array_hash_attr_env_snapshot(code, *name_idx);
                self.exec_index_assign_expr_named_op(
                    code,
                    *name_idx,
                    *is_positional,
                    *target_slot,
                )?;
                self.mirror_array_hash_attr_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::IndexElemAutoviv {
                name_idx,
                is_positional,
                target_slot,
                autoviv,
                viv_hash,
            } => {
                self.exec_index_elem_autoviv_op(
                    code,
                    *name_idx,
                    *is_positional,
                    *target_slot,
                    *autoviv,
                    *viv_hash,
                )?;
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
                self.exec_index_assign_expr_nested_op(
                    code,
                    *name_idx,
                    *outer_positional,
                    *inner_positional,
                )?;
                *ip += 1;
            }
            OpCode::IndexAssignDeepNested {
                name_idx,
                depth,
                positional_flags_idx,
            } => {
                self.exec_index_assign_deep_nested_op(
                    code,
                    *name_idx,
                    *depth,
                    *positional_flags_idx,
                )?;
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

            // -- Prefix increment/decrement --
            OpCode::PreIncrement(name_idx, slot) => {
                self.exec_pre_increment_op(code, *name_idx, *slot)?;
                *ip += 1;
            }
            OpCode::PreDecrement(name_idx, slot) => {
                self.exec_pre_decrement_op(code, *name_idx, *slot)?;
                *ip += 1;
            }
            OpCode::PreIncrementIndex(name_idx) => {
                self.exec_pre_increment_index_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PreDecrementIndex(name_idx) => {
                self.exec_pre_decrement_index_op(code, *name_idx)?;
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
            OpCode::AtomicCompoundVar { name_idx, op } => {
                self.exec_atomic_compound_var_op(code, *name_idx, *op)?;
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
            OpCode::When { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_when_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::Default { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_default_op(code, *body_end, ip, compiled_fns)?;
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
                is_bare_block,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_try_catch_op(
                    code,
                    *catch_start,
                    *control_start,
                    *body_end,
                    *explicit_catch,
                    *resume_safe,
                    *is_bare_block,
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
                self.attach_backtrace_to_error(&mut err);
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
                return Err(RuntimeError::return_signal(val));
            }
            OpCode::ReturnFromNonRoutine(lexically_in_routine) => {
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
                // No lexical routine at all (e.g. top-level `return`): throw
                // X::ControlFlow::Return directly.
                let _ = val;
                return Err(RuntimeError::controlflow_return(false));
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
            } => {
                self.sync_source_line(code, *ip);
                match self.exec_hyper_method_call_op(
                    code,
                    *name_idx,
                    *arity,
                    *modifier_idx,
                    *quoted,
                    *target_name_idx,
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
            } => {
                self.sync_source_line(code, *ip);
                match self.exec_hyper_method_call_dynamic_op(code, *arity, *modifier_idx) {
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
                self.exec_symbolic_deref_store_op(code, *sigil_idx);
                *ip += 1;
            }
            OpCode::IndirectTypeLookupStore => {
                self.exec_indirect_type_lookup_store_op(code);
                *ip += 1;
            }
            OpCode::StateVarInit(slot, key_idx) => {
                self.exec_state_var_init_op(code, *slot, *key_idx);
                *ip += 1;
            }
            OpCode::StateVarInitGuard(key_idx, jump_to) => {
                let base_key = Self::const_str(code, *key_idx);
                let scoped_key = self.scoped_state_key(base_key);
                if self.get_state_var(&scoped_key).is_some() {
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
            OpCode::BlockLocalScope { body_end } => {
                self.sync_source_line(code, *ip);
                self.exec_block_local_scope_op(code, *body_end, ip, compiled_fns)?;
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
                    ValueView::Seq(items) => {
                        // Consuming the Seq via eager marks it as consumed.
                        crate::value::seq_sink(&items);
                        Value::array(items.to_vec())
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
            OpCode::RegisterSub(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_sub_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterToken(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_token_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterProtoSub(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_proto_sub_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterProtoToken(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_proto_token_op(code, *idx)?;
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
            OpCode::RegisterClass(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_class_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::AugmentClass(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_augment_class_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterRole(idx) => {
                self.sync_source_line(code, *ip);
                self.exec_register_role_op(code, *idx)?;
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
                param_idx,
                target_var_idx,
            } => {
                self.sync_source_line(code, *ip);
                self.exec_whenever_scope_op(code, *body_idx, param_idx, target_var_idx)?;
                *ip += 1;
            }

            // -- Local variables --
            OpCode::GetLocal(idx) => {
                self.exec_get_local_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::GetLocalRaw(idx) => {
                self.exec_get_local_raw_op(*idx);
                *ip += 1;
            }
            OpCode::SetLocal(idx) => {
                self.exec_set_local_op(code, *idx)?;
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
                self.explicit_initializer_context = *explicit_init;
                self.vardecl_context = true;
                self.exec_set_local_op(code, *slot)?;
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
                        return Err(RuntimeError::assignment_ro(Some(name)));
                    }
                }
                *ip += 1;
            }
            OpCode::MarkVarReadonly(name_idx) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.mark_readonly(&name);
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
}
