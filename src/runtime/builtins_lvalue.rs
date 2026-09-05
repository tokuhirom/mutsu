use super::*;
use crate::value::ArrayKind;

impl Interpreter {
    pub(super) fn sub_call_args_from_value(arg: Option<&Value>) -> Vec<Value> {
        match arg {
            Some(v) => match v.view() {
                ValueView::Array(items, _) => items.to_vec(),
                ValueView::Nil => Vec::new(),
                _ => vec![v.clone()],
            },
            None => Vec::new(),
        }
    }

    pub(crate) fn maybe_fetch_rw_proxy(
        &mut self,
        result: Value,
        is_rw: bool,
    ) -> Result<Value, RuntimeError> {
        if !is_rw || self.in_lvalue_assignment {
            return Ok(result);
        }
        if let Some((fetcher, _storer, _subclass, decontainerized)) =
            result.clone().into_proxy_parts()
        {
            if decontainerized {
                return Ok(result);
            }
            if fetcher.is_nil() {
                return Ok(Value::NIL);
            }
            return self.call_sub_value(fetcher, vec![result], true);
        }
        Ok(result)
    }

    /// Auto-FETCH a Proxy value. If the value is a Proxy, call its FETCH callback.
    /// Used when a Proxy-bound variable is read in value context.
    pub(crate) fn auto_fetch_proxy(&mut self, value: &Value) -> Result<Value, RuntimeError> {
        // Tag probe first: a `view()` on a lazy Match would materialize it
        // just to see it is not a Proxy.
        if !value.is_proxy_value() {
            return Ok(value.clone());
        }
        if let ValueView::Proxy { fetcher, .. } = value.view() {
            if fetcher.is_nil() {
                return Ok(Value::NIL);
            }
            // merge_all=true gives the FETCH body caller-priority inputs (it
            // must see the CURRENT value of a captured lexical the STORE side
            // mutates — substr-rw's `$str`). But its post-call whole-env merge
            // would leak the body's captures into the caller: two map-produced
            // Proxies sharing a captured `$v` name would both freeze to the
            // first FETCHed value. FETCH is a READ, so run with caller-priority
            // inputs and DISCARD every env effect afterwards.
            let saved_env = self.env.clone();
            let result = self.call_sub_value(fetcher.clone(), vec![value.clone()], true);
            self.env = saved_env;
            return result;
        }
        Ok(value.clone())
    }

    /// Whether a value is (or contains, one container level deep per recursion
    /// step) a `Proxy` element that a value-context read must FETCH.
    pub(crate) fn value_has_proxy(value: &Value) -> bool {
        Self::value_has_proxy_seen(value, &mut Vec::new())
    }

    /// The body of [`Self::value_has_proxy`], carrying the set of aggregate
    /// nodes already on the walk.
    ///
    /// Raku lets a container hold itself (`my @a; @a = 42, @a;`), and this scan
    /// now runs on user data at every render (ADR-0040 §9.2), so an unguarded
    /// walk overflows the stack on the first circular array it meets —
    /// `@a.raku` and `t/nested-instance-raku.t`'s circular rows aborted the
    /// process. Re-entering a node cannot reveal a Proxy the first visit missed,
    /// so stopping there costs no accuracy. Identity is the `Gc` node pointer,
    /// the same handle the `.raku` renderer's own cycle detection uses.
    fn value_has_proxy_seen(value: &Value, seen: &mut Vec<usize>) -> bool {
        // A cheap guard on the way in: only aggregates can close a cycle, and
        // `seen` is short (cycle depth, not element count), so the linear scan
        // is cheaper than a set.
        macro_rules! enter {
            ($node:expr) => {{
                let ptr = crate::gc::Gc::as_ptr(&$node) as usize;
                if seen.contains(&ptr) {
                    return false;
                }
                seen.push(ptr);
                ptr
            }};
        }
        match value.view() {
            ValueView::Proxy { .. } => true,
            // An element bound with `@a[0] := $p` holds the `Proxy` behind the
            // element's own container cell (ADR-0040 §9.1), so the scan has to
            // look through one — otherwise the whole array reads as Proxy-free
            // and nothing below ever runs.
            //
            // EXACTLY one level, and no recursion past the cell: the bind puts
            // the Proxy DIRECTLY behind it, while a self-reference is a cell
            // pointing back at the structure being scanned. Recursing through
            // cells walks that cycle forever (`my @a; @a = 42, @a; @a.raku`
            // overflowed the stack), and the cell is also where this scan used
            // to stop before the look-through was added.
            ValueView::ContainerRef(_) | ValueView::ContainerView(_) => {
                value.deref_container().is_proxy_value()
            }
            ValueView::Array(items, _) => {
                enter!(items);
                let found = items.iter().any(|v| Self::value_has_proxy_seen(v, seen));
                seen.pop();
                found
            }
            ValueView::Seq(items) => items.iter().any(|v| Self::value_has_proxy_seen(v, seen)),
            ValueView::Slip(items) => items.iter().any(|v| Self::value_has_proxy_seen(v, seen)),
            ValueView::Hash(map) => {
                enter!(map);
                let found = map.values().any(|v| Self::value_has_proxy_seen(v, seen));
                seen.pop();
                found
            }
            ValueView::Pair(_, v) => Self::value_has_proxy_seen(v, seen),
            ValueView::ValuePair(k, v) => {
                Self::value_has_proxy_seen(k, seen) || Self::value_has_proxy_seen(v, seen)
            }
            _ => false,
        }
    }

    /// The native methods that RENDER their receiver's elements — the ones that
    /// stand in for the per-element `.gist`/`.Str`/`.raku` call Rakudo would
    /// make, and so owe that call's decont (ADR-0040 §9.2).
    ///
    /// This is not a heuristic about user code: it enumerates mutsu's own native
    /// renderers, the natives that inline a per-element method call instead of
    /// dispatching one. A method that hands an element to *user* code instead of
    /// rendering it — `map`, `grep`, `sort`, `for` — is deliberately absent: it
    /// binds the element container, Proxy included (ADR-0045), and resolving
    /// would destroy exactly what it is supposed to pass along.
    pub(crate) fn renders_receiver_elements(method: &str) -> bool {
        matches!(
            method,
            "gist" | "Str" | "Stringy" | "raku" | "perl" | "join" | "fmt" | "say" | "put" | "note"
        )
    }

    /// Whether `value` is a *container* holding a `Proxy` somewhere inside it,
    /// as opposed to being a `Proxy` itself.
    ///
    /// This is the gate for ADR-0040 §9.2: rendering a container has to resolve
    /// the Proxies among its elements, while a `Proxy` receiver in its own right
    /// is already deconted by ordinary method dispatch and must keep taking that
    /// path.
    pub(crate) fn holds_nested_proxy(value: &Value) -> bool {
        !value.is_proxy_value() && Self::value_has_proxy(value)
    }

    /// Deep-resolve `Proxy` values for a value-context read: every Proxy —
    /// top-level or inside an Array/List/Seq/Slip/Hash/Pair — is replaced by
    /// its FETCHed value (raku semantics: reading through a container FETCHes;
    /// `((1,2).map({ Proxy.new(...) }).List).raku` renders the values). The
    /// no-Proxy common case is a cheap scan with no allocation. Mirrors
    /// `resolve_bound_array_elements` (the ContainerRef twin), but needs
    /// `&mut self` for the FETCH closure calls.
    pub(crate) fn resolve_proxies_in_value(
        &mut self,
        value: &Value,
    ) -> Result<Value, RuntimeError> {
        if !Self::value_has_proxy(value) {
            return Ok(value.clone());
        }
        match value.view() {
            ValueView::Proxy { .. } => {
                let fetched = self.auto_fetch_proxy(value)?;
                // A FETCH may itself return a Proxy-bearing structure.
                self.resolve_proxies_in_value(&fetched)
            }
            // Read through the element's own container cell to reach a `Proxy`
            // bound into it. The cell is dropped from the result, which is what
            // a value-context read does anyway — and only ever happens when
            // there IS a Proxy inside, because `value_has_proxy` above returns
            // the value untouched otherwise. Bounded to one level for the same
            // reason `value_has_proxy` is: a cell holding anything but a Proxy
            // may be a self-reference.
            ValueView::ContainerRef(_) | ValueView::ContainerView(_) => {
                let inner = value.deref_container();
                if inner.is_proxy_value() {
                    self.resolve_proxies_in_value(&inner)
                } else {
                    Ok(value.clone())
                }
            }
            ValueView::Array(items, kind) => {
                let resolved: Result<Vec<Value>, RuntimeError> = items
                    .iter()
                    .map(|v| self.resolve_proxies_in_value(v))
                    .collect();
                Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(resolved?)),
                    kind,
                ))
            }
            ValueView::Seq(items) => {
                let resolved: Result<Vec<Value>, RuntimeError> = items
                    .iter()
                    .map(|v| self.resolve_proxies_in_value(v))
                    .collect();
                Ok(Value::seq(resolved?))
            }
            ValueView::Slip(items) => {
                let resolved: Result<Vec<Value>, RuntimeError> = items
                    .iter()
                    .map(|v| self.resolve_proxies_in_value(v))
                    .collect();
                Ok(Value::slip_arc(std::sync::Arc::new(resolved?)))
            }
            ValueView::Hash(map) => {
                let mut resolved = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    resolved.insert(k.clone(), self.resolve_proxies_in_value(v)?);
                }
                Ok(Value::hash(resolved))
            }
            ValueView::Pair(k, v) => {
                let rv = self.resolve_proxies_in_value(v)?;
                Ok(Value::pair(k.clone(), rv))
            }
            ValueView::ValuePair(k, v) => {
                let rk = self.resolve_proxies_in_value(k)?;
                let rv = self.resolve_proxies_in_value(v)?;
                Ok(Value::value_pair(rk, rv))
            }
            _ => Ok(value.clone()),
        }
    }

    /// Whether a routine hands its caller a container, so that `f() = v`,
    /// `++f()` and `my $r := f(); $r = v` may write through the call result:
    /// declared `is rw` / `is raw`, or spelling an explicit `return-rw`
    /// anywhere in its body (which is assignable without the trait —
    /// `sub f() { return-rw $v }; f() = 5` writes `$v` in Rakudo).
    ///
    /// This is a property of the *declaration*, never of the call result: a
    /// routine that is not rw-capable still runs (Rakudo evaluates `h()` before
    /// rejecting `h() = 1`), but whatever it returns is a value and the
    /// assignment is refused even when that value happens to be a `Proxy`
    /// (Rakudo: `sub f() { Proxy.new(...) }; f() = 5` is `X::Assignment::RO`).
    pub(crate) fn routine_is_rw_capable(def: &crate::ast::FunctionDef) -> bool {
        def.is_rw || def.is_raw || Self::routine_body_facts(def).uses_return_rw
    }

    /// The method half of `routine_is_rw_capable` — the *same* rule, asked of a
    /// [`MethodDef`] instead of a `FunctionDef` (ADR-0067 slice 2).
    ///
    /// The method lvalue path used to test `is_rw` alone, so `method m(\x) is
    /// raw { x }` and `method m(\x) { return-rw x }` were refused with
    /// `X::Assignment::RO` while the byte-identical `sub` spellings wrote
    /// through — two rules for one declaration property. Rakudo has one:
    /// `is rw`, `is raw`, and an explicit `return-rw` all make a routine hand
    /// its caller a container, and a method is just a routine whose parameter
    /// zero is the invocant.
    pub(crate) fn method_is_rw_capable(def: &crate::runtime::decl_types::MethodDef) -> bool {
        def.is_rw || def.is_raw || crate::opcode::body_uses_return_rw(&def.body)
    }

    /// The write half of `f() = value` once the routine has run: the routine
    /// handed back a container (ADR-0059) and `value` is stored through it, or
    /// it handed back a plain value and the assignment is `X::Assignment::RO`
    /// with Rakudo's "Cannot modify an immutable <Type> (<value>)" wording.
    fn assign_through_rw_result(
        &mut self,
        result: Value,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(assigned) = self.assign_lvalue_container(&result, value) {
            return assigned;
        }
        let typename = crate::runtime::utils::value_type_name(&result);
        let repr = result.to_string_value();
        Err(RuntimeError::assignment_ro_typename(typename, &repr))
    }

    pub(crate) fn assign_proxy_lvalue(
        &mut self,
        proxy: Value,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let Some((fetcher, storer, _subclass, _decontainerized)) = proxy.clone().into_proxy_parts()
        else {
            return Err(RuntimeError::new(
                "X::Assignment::RO: target is not assignable",
            ));
        };
        // A user `Proxy` STORE (`STORE => method ($v) { $realvar = $v }` from an
        // lvalue sub) mutates a captured-outer caller lexical by name. Snapshot the
        // env scalars before STORE so the names it changed can be recorded for the
        // retain-on-miss caller-var writeback, which the assign call site drains.
        let pre_env: Option<std::collections::HashMap<crate::symbol::Symbol, Value>> = Some(
            self.env
                .iter()
                .filter(|(_, v)| Self::is_writeback_safe_scalar(v))
                .map(|(k, v)| (*k, v.clone()))
                .collect(),
        );
        let store_result =
            self.call_sub_value(storer.clone(), vec![proxy.clone(), value.clone()], true);
        if let Err(err) = store_result {
            if err.message.contains("Too many positionals") {
                self.call_sub_value(storer.clone(), vec![value.clone()], true)?;
            } else {
                return Err(err);
            }
        }
        if let Some(pre_env) = pre_env {
            let changed: Vec<String> = self
                .env
                .iter()
                .filter(|(k, v)| {
                    Self::is_writeback_safe_scalar(v)
                        && pre_env.get(*k).map(|p| p != *v).unwrap_or(true)
                })
                .map(|(k, _)| k.resolve())
                .collect();
            for name in changed {
                self.record_caller_var_writeback(&name);
            }
        }
        // After STORE executes, propagate its closure env changes to FETCH's
        // closure env override so that shared captured variables stay in sync.
        // This is needed because mutsu closures capture environments by value
        // (copy-on-write), so two closures from the same scope diverge on mutation.
        self.sync_proxy_closure_envs(&fetcher, &storer);
        if fetcher.is_nil() {
            return Ok(Value::NIL);
        }
        let fetched = self.call_sub_value(fetcher.clone(), vec![proxy.clone()], true);
        match fetched {
            Ok(value) => Ok(value),
            Err(err) if err.message.contains("Too many positionals") => {
                let value = self.call_sub_value(fetcher, Vec::new(), true)?;
                Ok(value)
            }
            Err(err) => Err(err),
        }
    }

    /// Synchronize closure environment overrides between Proxy FETCH and STORE.
    /// After STORE modifies captured variables, propagate those changes to FETCH
    /// so both closures see the same state for shared variables.
    fn sync_proxy_closure_envs(&mut self, fetcher: &Value, storer: &Value) {
        let (Some(fetch_data), Some(store_data)) = (
            match fetcher.view() {
                ValueView::Sub(d) => Some(d),
                _ => None,
            },
            match storer.view() {
                ValueView::Sub(d) => Some(d),
                _ => None,
            },
        ) else {
            return;
        };
        let fetch_id = fetch_data.id;
        let store_id = store_data.id;
        // Get the updated STORE env (after call_sub_value persisted it)
        let store_env = match self.closure_env_overrides.get(&store_id) {
            Some(env) => env.clone(),
            None => return,
        };
        // Find variables that are shared between FETCH and STORE captured envs
        let fetch_base = self
            .closure_env_overrides
            .get(&fetch_id)
            .cloned()
            .unwrap_or_else(|| fetch_data.env.clone());
        let mut updated_fetch = fetch_base.clone();
        let mut changed = false;
        for key in fetch_base.keys() {
            // Skip internal/metadata keys
            if key.starts_with("__mutsu_") || key.starts_with("&?") || key == "?LINE" {
                continue;
            }
            if let Some(store_val) = store_env.get_sym(*key)
                && fetch_base.get_sym(*key) != Some(store_val)
            {
                updated_fetch.insert_sym(*key, store_val.clone());
                changed = true;
            }
        }
        if changed {
            self.closure_env_overrides.insert(fetch_id, updated_fetch);
        }
    }

    pub(super) fn assign_named_sub_lvalue_with_values(
        &mut self,
        name: &str,
        call_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // Perl-style slurp idiom used in roast/t/fudge.t:
        //   local(@ARGV, $/) = $path; <>
        // Preserve support when `local(...) = ...` is lowered as named-sub lvalue assignment.
        if name == "local" {
            self.env
                .insert("ARGV".to_string(), Value::array(vec![value.clone()]));
            self.env.insert("/".to_string(), Value::NIL);
            return Ok(value);
        }

        // undefine($var) = value: undefine the variable, then assign value to it.
        // `undefine` is rw — it returns the container after clearing it.
        if name == "undefine" && call_args.len() == 1 {
            let target = &call_args[0];
            let var_name = {
                let mut found = None;
                for (k, v) in self.env.iter() {
                    if crate::runtime::values_identical(v, target) && !k.starts_with("__") {
                        found = Some(k.resolve());
                        break;
                    }
                }
                found
            };
            if let Some(vname) = var_name {
                // Single-store coherence: refresh the caller's local slot from the
                // env write below (same as substr-rw / object subscript assign).
                self.pending_rw_writeback_sources.push(vname.clone());
                // Also record on the retain-on-miss list so the bound-Proxy form
                // (`my $r := undefine(...); $r = v`) — whose STORE runs a frame
                // below the slot owner — reaches the owner's slot too.
                self.record_caller_var_writeback(&vname);
                self.env.insert(vname.clone(), value.clone());
                return Ok(value);
            }
            return Ok(value);
        }

        // substr-rw as a function: substr-rw($str, from, len) = $value
        if name == "substr-rw" && !call_args.is_empty() {
            let target = call_args[0].clone();
            let method_args = call_args[1..].to_vec();
            let target_var = {
                let mut found = None;
                for (k, v) in self.env.iter() {
                    if crate::runtime::values_identical(v, &target) && !k.starts_with("__") {
                        found = Some(k.resolve());
                        break;
                    }
                }
                found
            };
            // Single-store coherence: `assign_method_lvalue_with_values` writes
            // the mutated string back into `env[target_var]` but not the caller's
            // local slot. The default build's blanket reconcile carried this;
            // record the target so the call-site `apply_pending_rw_writeback`
            // drains the env value into the slot precisely (no blanket pull).
            if let Some(ref tv) = target_var {
                self.pending_rw_writeback_sources.push(tv.clone());
                // Retain-on-miss too, for the bound-Proxy form (`my $r :=
                // substr-rw($s, ...); $r = v`) whose STORE runs below the owner.
                self.record_caller_var_writeback(tv);
            }
            return self.assign_method_lvalue_with_values(
                target_var.as_deref(),
                target,
                "substr-rw",
                method_args,
                value,
                false,
            );
        }

        // subbuf-rw as a function: subbuf-rw($buf, from, len) = $value
        if name == "subbuf-rw" && !call_args.is_empty() {
            let target = call_args[0].clone();
            let method_args = call_args[1..].to_vec();
            // We need to find the variable name for the target to update it.
            // Search the env for a variable whose value matches the target by identity.
            let target_var = {
                let mut found = None;
                for (k, v) in self.env.iter() {
                    if crate::runtime::values_identical(v, &target) && !k.starts_with("__") {
                        found = Some(k.resolve());
                        break;
                    }
                }
                found
            };
            if let Some(ref tv) = target_var {
                self.pending_rw_writeback_sources.push(tv.clone());
                // Retain-on-miss too, for the bound-Proxy form.
                self.record_caller_var_writeback(tv);
            }
            return self.assign_method_lvalue_with_values(
                target_var.as_deref(),
                target,
                "subbuf-rw",
                method_args,
                value,
                false,
            );
        }

        if let Some(def) = self.resolve_function_with_alias(name, &call_args) {
            // ADR-0059: the routine always runs, and the assignment writes
            // through the container it hands back. That container is produced
            // by the compiler — a `return-rw` operand, or the bare tail of an
            // `is rw`/`is raw` routine, is compiled in container mode — so
            // this site never inspects the callee's body: an element reached
            // through one of the routine's OWN parameters, a computed tail and
            // a recursive descent all arrive here as the same `ContainerRef` /
            // `HashEntryRef` / `Proxy`.
            let rw_capable = Self::routine_is_rw_capable(&def);
            let was_lvalue = self.in_lvalue_assignment;
            self.in_lvalue_assignment = true;
            let result = self.call_function(name, call_args);
            self.in_lvalue_assignment = was_lvalue;
            let result = result?;
            if rw_capable {
                return self.assign_through_rw_result(result, value);
            }
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: sub '{}' is not rw",
                name
            )));
        }
        if let Some(err) = self.take_pending_dispatch_error() {
            return Err(err);
        }

        if let Some(callable) = self.env.get(&format!("&{}", name)).cloned() {
            return self.assign_callable_lvalue_with_values(callable, call_args, value);
        }

        Err(RuntimeError::new(format!("Unknown call: {}", name)))
    }

    pub(super) fn assign_callable_lvalue_with_values(
        &mut self,
        callable: Value,
        call_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        match callable.view() {
            ValueView::Routine { name, .. } => {
                self.assign_named_sub_lvalue_with_values(&name.resolve(), call_args, value)
            }
            ValueView::Sub(data) => {
                let data = data.clone();
                // A body-less routine code object (ADR-0019 C6e-3b registers
                // safe-class defs with an empty AST body) cannot answer the
                // `return-rw` question itself; its installed def can. Delegate
                // to the named path, which reads the def.
                if data.body.is_empty() && data.compiled_routine.is_some() && !data.name.is_empty()
                {
                    return self.assign_named_sub_lvalue_with_values(
                        &data.name.resolve(),
                        call_args,
                        value,
                    );
                }
                // Same rule as the named path (ADR-0059): run the routine and
                // write through the container it returns.
                let rw_capable =
                    data.is_rw || data.is_raw || crate::opcode::body_uses_return_rw(&data.body);
                let was_lvalue = self.in_lvalue_assignment;
                self.in_lvalue_assignment = true;
                let result = self.call_sub_value(Value::sub_value(data), call_args, true);
                self.in_lvalue_assignment = was_lvalue;
                let result = result?;
                if rw_capable {
                    return self.assign_through_rw_result(result, value);
                }
                Err(RuntimeError::assignment_ro(Some("sub is not rw")))
            }
            ValueView::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => self.assign_callable_lvalue_with_values(
                    Value::sub_value(strong),
                    call_args,
                    value,
                ),
                None => Err(RuntimeError::new("Callable has been freed")),
            },
            _ => Err(RuntimeError::assignment_ro(Some(
                "cannot assign through non-callable value",
            ))),
        }
    }

    pub(super) fn builtin_assign_named_sub_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_assign_named_sub_lvalue expects name, call args, and value",
            ));
        }
        let name = args[0].to_string_value();
        let call_args = Self::sub_call_args_from_value(args.get(1));
        let value = args[2].clone();
        self.assign_named_sub_lvalue_with_values(&name, call_args, value)
    }

    pub(super) fn builtin_assign_callable_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_assign_callable_lvalue expects callable, call args, and value",
            ));
        }
        let callable = args[0].clone();
        let call_args = Self::sub_call_args_from_value(args.get(1));
        let value = args[2].clone();
        self.assign_callable_lvalue_with_values(callable, call_args, value)
    }

    /// `__mutsu_var_is_writable('name')` -- does the named scalar have a
    /// container that can be assigned through?
    ///
    /// Used by the `//=` / `||=` / `&&=` short-circuit desugar: when the short
    /// circuit KEEPS the current value, rakudo returns the LHS *container* if
    /// it has one (so `my $a = 52; ($a //= 42) += 10` leaves 62 in `$a`) and
    /// the bare *value* if it does not (so `my $a := 42; ($a //= 42) += 10`
    /// dies with X::Assignment::RO on the returned value). Only the runtime
    /// knows which, so the desugar branches on this.
    pub(super) fn builtin_var_is_writable(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Some(name) = args.first().map(Value::to_string_value) else {
            return Ok(Value::TRUE);
        };
        if self.readonly_kind(&name).is_some() {
            return Ok(Value::FALSE);
        }
        if crate::env::closure_meta_keys_possible() {
            let key = crate::runtime::sigilless_readonly_key(&name);
            if matches!(
                self.env.get(&key).map(Value::view),
                Some(ValueView::Bool(true))
            ) {
                return Ok(Value::FALSE);
            }
        }
        Ok(Value::TRUE)
    }

    pub(super) fn builtin_assignment_ro(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // When the read-only left-hand side value is supplied (e.g. assigning to
        // a literal `120 = 3`), report its type and representation to match
        // rakudo: "Cannot modify an immutable Int (120)".
        if let Some(lhs) = args.first() {
            let typename = crate::runtime::utils::value_type_name(lhs);
            let repr = lhs.to_string_value();
            return Err(RuntimeError::assignment_ro_typename(typename, &repr));
        }
        Err(RuntimeError::assignment_ro(None))
    }

    /// `__mutsu_list_assign_rhs(rhs)` — Rakudo List.STORE decont for a paren
    /// destructuring assignment (`my ($a, $b) = RHS`): an ITEMIZED single RHS
    /// container is deitemized so the temp-array assignment flattens it into
    /// elements (`my ($a, $b) = $row`). Every other value — comma lists,
    /// plain arrays, Failures, scalars — passes through untouched: a Failure
    /// RHS must land in the first target, not throw (so a blanket `.list`
    /// call is wrong here).
    pub(super) fn builtin_list_assign_rhs(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Some(v) = args.first() else {
            return Ok(Value::NIL);
        };
        if let ValueView::Array(items, kind) = v.view()
            && kind.is_itemized()
        {
            return Ok(Value::array_with_kind(items.clone(), kind.decontainerize()));
        }
        Ok(v.clone())
    }

    pub(super) fn builtin_star_lvalue_rhs(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_star_lvalue_rhs expects target name and rhs value",
            ));
        }
        let target_name = args[0].to_string_value();
        let marker_key = format!("__mutsu_bound_array_len::{target_name}");
        let Some(limit) = self.env.get(&marker_key).and_then(|v| match v.view() {
            ValueView::Int(i) if i >= 0 => usize::try_from(i).ok(),
            _ => None,
        }) else {
            return Ok(args[1].clone());
        };

        let mut items = crate::runtime::value_to_list(&args[1]);
        if items.len() > limit {
            items.truncate(limit);
        }
        Ok(Value::real_array(items))
    }

    pub(super) fn builtin_record_bound_array_len(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(
                "__mutsu_record_bound_array_len expects target name",
            ));
        }
        let target_name = args[0].to_string_value();
        if !target_name.starts_with('@') {
            return Ok(Value::NIL);
        }
        // For gather-based LazyLists with coroutine support, skip recording
        // the length since it's unknown (the list is lazy / possibly infinite).
        if let Some(ValueView::LazyList(ll)) = self.env.get(&target_name).map(Value::view)
            && ll.coroutine.is_some()
        {
            return Ok(Value::NIL);
        }
        let bound_len = self
            .env
            .get(&target_name)
            .map(|v| crate::runtime::value_to_list(v).len() as i64)
            .unwrap_or(0);
        self.env.insert(
            format!("__mutsu_bound_array_len::{target_name}"),
            Value::int(bound_len),
        );
        Ok(Value::NIL)
    }

    pub(super) fn builtin_record_shaped_array_dims(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(
                "__mutsu_record_shaped_array_dims expects target name",
            ));
        }
        let target_name = args[0].to_string_value();
        if !target_name.starts_with('@') {
            return Ok(Value::NIL);
        }
        let key = format!("__mutsu_shaped_array_dims::{target_name}");
        let dims = self
            .env
            .get(&target_name)
            .and_then(Self::infer_array_shape)
            .filter(|shape| shape.len() > 1);
        if let Some(shape) = dims {
            let dims_val = Value::array_with_kind(
                crate::gc::Gc::new(shape.into_iter().map(|n| Value::int(n as i64)).collect()),
                ArrayKind::List,
            );
            self.env.insert(key, dims_val);
        } else {
            self.env.remove(&key);
        }
        Ok(Value::NIL)
    }
}
