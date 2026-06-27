use super::*;

impl Interpreter {
    /// Like `exec_get_local_op` but does NOT resolve HashEntryRef.
    /// Pushes the raw local value, preserving container references for `=:=` checks.
    pub(super) fn exec_get_local_raw_op(&mut self, idx: u32) {
        let idx = idx as usize;
        let val = self.locals[idx].clone();
        self.stack.push(val);
    }

    /// Read a read-only scalar upvalue (see `OpCode::GetUpvalue`). The fast path
    /// reads this frame's installed upvalue array by index, dereferencing a
    /// shared `ContainerRef` cell so the value tracks the creator's container.
    /// When `index` is out of range — a non-standard path (control handler /
    /// phaser / register-reuse run) executed this closure's ops without installing
    /// its upvalue array — it falls back to a by-name env read; env is retained as
    /// the capture source, so the fallback is always correct for the plain scalar
    /// lexicals that are ever upvalue-promoted.
    pub(super) fn exec_get_upvalue_op(
        &mut self,
        code: &CompiledCode,
        index: u32,
        name_idx: u32,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        let val = match self.upvalues.get(index as usize) {
            Some(Some(v)) => v.clone(),
            // `None` entry (non-cell capture) or out-of-range (non-standard path):
            // read the captured scalar live from env by name.
            _ => {
                let name = Self::const_str(code, name_idx);
                self.get_env_with_main_alias(name).unwrap_or(Value::Nil)
            }
        };
        let val = if let Value::LazyThunk(ref thunk_data) = val {
            self.force_lazy_thunk(thunk_data)?
        } else {
            val
        };
        self.stack.push(val.into_deref());
        *ip += 1;
        Ok(())
    }

    pub(super) fn exec_get_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        // Check if this variable has a binding alias (e.g. from $CALLER::foo := $other_var)
        let name = code.locals.get(idx).cloned().unwrap_or_default();
        if let Some(bound_to) = self.resolve_binding(&name) {
            let bound_to = bound_to.to_string();
            if let Some(val) = self.env().get(&bound_to).cloned() {
                self.stack.push(val);
                return Ok(());
            }
        }
        // Attribute locals (!attr) modified by CAS: env holds the authoritative
        // value since sync_locals_from_env skips !-prefixed names for performance.
        if name.starts_with('!')
            && self.is_shared_var_dirty(&name)
            && let Some(val) = self.env().get(&name).cloned()
        {
            self.locals[idx] = val.clone();
            self.stack.push(val);
            return Ok(());
        }
        // Atomic-variable read: skip entirely (a `format!` plus two
        // `var_type_constraint` lookups) when no atomic storage has ever been
        // registered — the common case on this hot local-read path.
        if self.atomic_var_seen() {
            let atomic_name = name.strip_prefix('$').unwrap_or(&name);
            let atomic_name_key = format!("__mutsu_atomic_name::{atomic_name}");
            // Only use the scalar atomic fast path for scalar ($) variables.
            // Array (@) variables with `atomicint` constraint are element-wise
            // atomic and should go through the normal array read path.
            let is_atomic_int = !name.starts_with('@')
                && (loan_env!(self, var_type_constraint(&name)).as_deref() == Some("atomicint")
                    || loan_env!(self, var_type_constraint(atomic_name)).as_deref()
                        == Some("atomicint")
                    || self.get_shared_var(&atomic_name_key).is_some());
            if is_atomic_int {
                let fetched = loan_env!(
                    self,
                    builtin_atomic_fetch_var(&[Value::str(atomic_name.to_string())])
                )?;
                self.locals[idx] = fetched.clone();
                self.stack.push(fetched);
                return Ok(());
            }
        }
        // Atomic array CAS stores the authoritative array under an internal
        // shared key.  Check it first so reads pick up the latest CAS'd value.
        if name.starts_with('@') {
            let atomic_key = format!("__mutsu_atomic_arr::{name}");
            if let Some(shared_val) = self.get_shared_var(&atomic_key) {
                self.locals[idx] = shared_val.clone();
                self.stack.push(shared_val);
                return Ok(());
            }
        }
        // Shared @/% variables may be mutated by sibling threads while this Interpreter
        // still holds an old local snapshot. Prefer the shared copy so reads
        // observe the latest value without forcing array COW on every push.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(shared_val) = self.get_shared_var(&name)
        {
            self.stack.push(shared_val);
            return Ok(());
        }
        // Lazy sync: if the local is not a ContainerRef but env has one
        // (e.g., a cross-scope `:=` binding was established during a function/method
        // call and propagated back to env but not to locals), adopt the ContainerRef.
        // Skip for type objects and complex values that should not be replaced.
        if !self.locals[idx].is_container_ref()
            && !matches!(
                self.locals[idx],
                Value::Package(_)
                    | Value::Array(..)
                    | Value::Hash(..)
                    | Value::Sub(..)
                    | Value::Instance { .. }
            )
            && let Some(Value::ContainerRef(arc)) = self.env().get(&name).cloned()
        {
            self.locals[idx] = Value::ContainerRef(arc);
        }
        // Phase 3 Stage 2 (scalar slice): scalar instance attributes read straight
        // from `self`'s shared cell, so a mutation made in a nested method frame
        // is visible here. Gated on a non-container slot so `$!x := outer`
        // bindings keep their ContainerRef handling. The cell lookup returns None
        // for non-attribute names and when `self` is not an instance.
        if !self.locals[idx].is_container_ref()
            && let Some(cell_val) = self.read_self_attr_cell(&name)
        {
            self.locals[idx] = cell_val.clone();
            self.stack.push(cell_val);
            return Ok(());
        }
        let val = self.locals[idx].clone();
        // Resolve a deferred bind token to its current value (Any if the path
        // doesn't exist). The raw local slot is unchanged, so a later write still
        // materializes it; `=:=` reads the raw slot via GetLocalRaw.
        if let Value::HashEntryRef { .. } = &val {
            self.stack.push(val.hash_entry_read());
            return Ok(());
        }
        // Force lazy thunks transparently on access
        if let Value::LazyThunk(ref thunk_data) = val {
            let forced = self.force_lazy_thunk(thunk_data)?;
            self.stack.push(forced);
            return Ok(());
        }
        // Auto-deref ContainerRef: read the inner value for stack use (ContainerRef
        // axis of the decont family). Gate on is_container_ref() to preserve the
        // early return AND keep the non-container hot path move-only (into_deref is
        // never reached for non-ContainerRef values).
        if val.is_container_ref() {
            self.stack.push(val.into_deref());
            return Ok(());
        }
        // Fast path: non-Nil values are always valid — skip env lookup
        if matches!(val, Value::Nil) {
            if let Some(shared_val) = self.get_shared_var(&name) {
                self.stack.push(shared_val);
                return Ok(());
            }
            let is_internal = name.starts_with("__");
            let is_special = matches!(name.as_str(), "_" | "/" | "!" | "¢");
            // Private attribute locals (!attr) are populated directly from
            // instance attributes in fast-path method calls; they may not be
            // in the env (when skip_env_setup is active) but are still valid.
            let is_private_attr =
                name.starts_with('!') && name.len() > 1 && !name.starts_with("__");
            if !is_internal && !is_special && !is_private_attr && !self.env().contains_key(&name) {
                return Err(RuntimeError::new(format!(
                    "X::Undeclared::Symbols: Variable '{name}' is not declared"
                )));
            }
            // `is default(...)`: return the default value instead of Nil.
            if let Some(def) = self.var_default(&name) {
                self.stack.push(def.clone());
                return Ok(());
            }
            if let Some(constraint) = self.var_type_constraint_fast(&name).cloned() {
                let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                // Nil type constraint: the type object for Nil is Value::Nil itself,
                // not Value::Package("Nil").
                if nominal == "Nil" {
                    self.stack.push(Value::Nil);
                } else {
                    self.stack.push(Value::Package(Symbol::intern(&nominal)));
                }
                return Ok(());
            }
        }
        self.stack.push(val);
        Ok(())
    }

    /// Box the just-declared scalar local at `idx` into a shared `ContainerRef`
    /// cell placed in BOTH the slot and the env entry, so a directly-nested named
    /// sub that writes the lexical by name (via `SetGlobal` through env) and the
    /// owner that reads it by slot observe one cell — enabling cross-call
    /// accumulation without the `env_dirty` blanket reconcile. The skips mirror
    /// `box_captured_lexicals` exactly: scalars only (`@`/`%`/`&` share already),
    /// never an already-shared cell or a reference/identity-bearing value, and
    /// never a type/`where`-constrained scalar (whose every mutation must re-flow
    /// through the assignment chokepoint, which a cell write-through bypasses).
    fn box_decl_local_cell(&mut self, code: &CompiledCode, idx: usize) {
        let name = &code.locals[idx];
        if name.starts_with('&') {
            return;
        }
        // `@`/`%` containers captured-and-mutated in place by a nested named sub
        // (e.g. a user `trait_mod:<is>` pushing to an outer `@names`) are boxed as
        // a whole-container cell so the sub's by-name mutation and the owner's
        // by-name read alias one cell (docs/captured-outer-cell-sharing.md §7.2).
        if name.starts_with('@') || name.starts_with('%') {
            self.box_decl_local_container_cell(code, idx);
            return;
        }
        if self.locals[idx].is_container_ref() {
            return;
        }
        if matches!(
            self.locals[idx],
            Value::Package(_)
                | Value::Array(..)
                | Value::Hash(..)
                | Value::Sub(..)
                | Value::Instance { .. }
                | Value::Proxy { .. }
        ) {
            return;
        }
        if loan_env!(self, var_type_constraint(name)).is_some()
            || loan_env!(self, var_type_constraint(name.trim_start_matches('$'))).is_some()
        {
            return;
        }
        let container = self.locals[idx].clone().into_container_ref();
        self.locals[idx] = container.clone();
        let nm = code.locals[idx].clone();
        self.env_mut().insert(nm.clone(), container.clone());
        // Track C: keep a running thread's shared snapshot pointing at the cell
        // (mirrors box_captured_lexicals).
        if self.shared_vars_active {
            loan_env!(self, set_shared_var(&nm, container.clone()));
        }
    }

    /// Box a just-declared `@`/`%` container local into a shared `ContainerRef`
    /// cell placed in BOTH the slot and the env entry (the array/hash `:=` cell
    /// shape), so a nested named sub that mutates the container by name and the
    /// owner that reads it by name observe one cell. The mutating-method and
    /// element-assign write-back paths already descend through the cell
    /// (`try_native_array_mut` / `try_native_hash_mut_bound` / `env_root_descended_mut`),
    /// and `GetArrayVar`/`GetHashVar` `into_deref()` the cell on read.
    fn box_decl_local_container_cell(&mut self, code: &CompiledCode, idx: usize) {
        if self.locals[idx].is_container_ref() {
            return;
        }
        if !matches!(self.locals[idx], Value::Array(..) | Value::Hash(..)) {
            return;
        }
        let name = code.locals[idx].clone();
        // Typed containers must keep flowing through the assignment chokepoint.
        if loan_env!(self, var_type_constraint(&name)).is_some()
            || loan_env!(
                self,
                var_type_constraint(name.trim_start_matches(['@', '%']))
            )
            .is_some()
        {
            return;
        }
        let container = self.locals[idx].clone().into_container_ref();
        self.locals[idx] = container.clone();
        self.env_mut().insert(name.clone(), container.clone());
        if self.shared_vars_active {
            loan_env!(self, set_shared_var(&name, container.clone()));
        }
    }

    pub(super) fn exec_set_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        // Named-sub declaration boxing decision: a scalar local that a
        // directly-nested *named sub* writes (`needs_cell_named_sub`) becomes a
        // shared `ContainerRef` cell so the named sub's by-name env writes and the
        // owner's slot reads alias one cell, carrying cross-call accumulation
        // (`via(); via()`) through the cell instead of the `env_dirty` blanket
        // reconcile (see docs/captured-outer-cell-sharing.md).
        //
        // Boxing and the blanket reconcile are MUTUALLY EXCLUSIVE coherence
        // mechanisms — exactly one is active. Gating boxing on
        // `blanket_reconcile_disabled()` keeps the DEFAULT (shipping) build
        // byte-identical to before (reconcile carries coherence, no boxing), while
        // the `MUTSU_NO_BLANKET_RECONCILE` build activates boxing. This is what lets
        // the slice land safely: with the reconcile on, cell-sharing would
        // propagate a captured-outer write that the reconcile happens to drop
        // through some carriers (e.g. `lives-ok { ... }`), and that extra
        // propagation surfaces unrelated latent bugs (a missing `&foo = &foo`
        // self-reference type-check, etc.). Those are fixed as the surface is
        // migrated; until then boxing is exercised only under the toggle, where the
        // pins prove it carries the named-sub accumulation surface.
        //
        // `vardecl_context` is consumed inside the inner handler, so snapshot it
        // here. Gated on a non-empty `needs_cell_named_sub`, so ordinary code (e.g.
        // `fib`) pays nothing. Such a captured local is `needs_env_sync`
        // (non-simple), so the boxing must wrap BOTH the fast and slow inner paths
        // — hence it lives here, not inside one branch. Deliberately keyed on
        // `needs_cell_named_sub`, NOT the closure-driven `needs_cell_locals`:
        // closures are boxed precisely at their creation op, so reusing that set
        // here would over-box unrelated same-named locals (same-named `my` locals
        // share one slot) and break e.g. `let`-restore in a sibling block.
        let box_decl = self.vardecl_context && !code.needs_cell_named_sub.is_empty();
        let r = self.exec_set_local_op_inner(code, idx);
        // Phase 3 Stage 2: write-through scalar attribute writes to the cell.
        if r.is_ok() {
            self.mirror_attr_local_to_cell(code, idx as usize);
            if box_decl
                && let Some(sym) = code.locals_sym.get(idx as usize)
                && code.needs_cell_named_sub.contains(sym)
            {
                self.box_decl_local_cell(code, idx as usize);
            }
        }
        r
    }
}
