use super::*;

impl Interpreter {
    /// Track whether a scalar variable is bound (`:=`) to a Positional value.
    /// A bound scalar is NOT a Scalar container, so `@a = $bound` must flatten
    /// rather than itemize. The `ItemizeVar` opcode reads this marker. Plain
    /// assignment to a scalar clears any stale marker (guarded by
    /// `bound_decont_active` so the common no-bind case stays cheap).
    pub(super) fn update_bound_decont_marker(&mut self, name: &str, is_bind: bool, val: &Value) {
        // Scalar local names carry no sigil (e.g. "y"); `@`/`%`/`&` vars do.
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return;
        }
        if is_bind {
            // A scalar bound to a container (`$x := @a` / `:= %h` / `:= (1,2,3)`)
            // is not a Scalar container of its own: it aliases the container, so
            // `@a = $bound` flattens (ItemizeVar) and `$bound.VAR.^name` reflects
            // the container type (List/Array/Hash/Set/Bag/Mix) rather than Scalar.
            let is_container = match &val {
                Value::Array(_, k) => !k.is_itemized(),
                Value::Seq(_)
                | Value::Slip(_)
                | Value::LazyList(_)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::Hash(_)
                | Value::Set(..)
                | Value::Bag(..)
                | Value::Mix(..) => true,
                // A `:=` bind to a whole-container `@`/`%` variable holds a
                // shared cell whose inner value is the container.
                Value::ContainerRef(cell) => matches!(
                    &*cell.lock().unwrap(),
                    Value::Array(..)
                        | Value::Hash(_)
                        | Value::Set(..)
                        | Value::Bag(..)
                        | Value::Mix(..)
                ),
                _ => false,
            };
            if is_container {
                let key = format!("__mutsu_bound_decont::{}", name);
                self.env_mut().insert(key, Value::Bool(true));
                self.bound_decont_active = true;
                return;
            }
        }
        if self.bound_decont_active {
            let key = format!("__mutsu_bound_decont::{}", name);
            self.env_mut().remove(&key);
        }
    }

    /// Slice 2a (`docs/scalar-array-sharing.md`): is `name` a `$` scalar that
    /// currently shares an array/hash by reference (`$n = @z`)? Such a scalar's
    /// whole reassignment REPLACES its slot instead of writing through the cell.
    pub(super) fn is_array_share_scalar(&self, name: &str) -> bool {
        self.env()
            .get(&format!("__mutsu_array_share::{}", name))
            .is_some()
    }

    pub(super) fn clear_array_share_marker(&mut self, name: &str) {
        self.env_mut()
            .remove(&format!("__mutsu_array_share::{}", name));
    }

    /// Slice 2a: `$n = @z` / `$n = %h`. Promote the source container variable to
    /// a shared `ContainerRef` cell and store that same cell in the scalar
    /// target, so structural mutations (`.push`) through either name are seen by
    /// both (raku reference semantics) — a snapshotting copy would COW-detach on
    /// the first `.push`. Marks the scalar `__mutsu_array_share::` so a later
    /// whole reassignment (`$n = 5`) replaces the slot instead of mutating the
    /// shared cell. Does NOT set the bound-decont marker: the scalar stays
    /// itemized (`@a = $n` itemizes, unlike a `:=` bind which flattens).
    pub(super) fn array_share_assign(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        val: Value,
        source_name: String,
    ) -> Result<(), RuntimeError> {
        let resolved_source = self.resolve_sigilless_alias_source_name(&source_name);
        let name = code.locals[idx].clone();
        // Build (or reuse) the shared cell: reuse an existing cell carried by the
        // value or already held by the source variable, else wrap the snapshot.
        let cell = match &val {
            Value::ContainerRef(arc) => arc.clone(),
            _ => match self.env().get(&resolved_source) {
                Some(Value::ContainerRef(arc)) => arc.clone(),
                _ => std::sync::Arc::new(std::sync::Mutex::new(val.clone())),
            },
        };
        let container = Value::ContainerRef(cell);
        // Promote the SOURCE container variable to the same cell so its own
        // `.push` / whole-reassign (`@z = (...)`) mutate through and stay visible
        // via the scalar.
        if let Some(source_idx) = code.locals.iter().rposition(|n| n == &resolved_source) {
            self.locals[source_idx] = container.clone();
            self.flush_local_to_env(code, source_idx);
        }
        self.set_env_with_main_alias(&resolved_source, container.clone());
        // Propagate the shared cell into saved call frames so the sharing
        // survives method returns (env restore).
        for frame in self.call_frames.iter_mut().rev() {
            if frame.saved_env.contains_key(&resolved_source) {
                frame
                    .saved_env
                    .insert(resolved_source.clone(), container.clone());
            }
            for (i, local_name) in code.locals.iter().enumerate() {
                if local_name == &resolved_source && i < frame.saved_locals.len() {
                    frame.saved_locals[i] = container.clone();
                }
            }
        }
        // Store the shared cell in the scalar target (itemized scalar).
        self.locals[idx] = container.clone();
        // Clear any stale bound-decont marker inherited from an earlier bind of
        // the same name (this `=` share is itemized, not a `:=` decont alias).
        self.update_bound_decont_marker(&name, false, &val);
        // Mark the scalar so a later whole reassignment replaces the slot.
        self.env_mut()
            .insert(format!("__mutsu_array_share::{}", name), Value::Bool(true));
        self.array_share_active = true;
        self.set_env_with_main_alias(&name, container.clone());
        self.flush_local_to_env(code, idx);
        Ok(())
    }

    pub(super) fn normalize_scalar_assignment_value(val: Value) -> Value {
        let is_nilish = |v: &Value| match v {
            Value::Nil => true,
            Value::Package(sym) => sym.resolve() == "Any",
            _ => false,
        };
        match val {
            Value::Array(items, kind) if items.len() == 1 => {
                if items.first().is_some_and(is_nilish) {
                    Value::Nil
                } else {
                    Value::Array(items, kind)
                }
            }
            Value::Seq(items) if items.len() == 1 && items.first().is_some_and(is_nilish) => {
                Value::Nil
            }
            Value::Slip(items) if items.len() == 1 && items.first().is_some_and(is_nilish) => {
                Value::Nil
            }
            other => other,
        }
    }

    pub(super) fn extract_varref_binding(raw_val: Value) -> (Value, Option<String>) {
        if let Value::Capture { positional, named } = &raw_val
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return (inner.clone(), Some(name.to_string()));
        }
        (raw_val, None)
    }

    pub(crate) fn resolve_sigilless_alias_source_name(&self, source_name: &str) -> String {
        let mut resolved = source_name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(resolved.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", resolved);
            let Some(Value::Str(next)) = self.env().get(&key) else {
                break;
            };
            resolved = next.to_string();
        }
        resolved
    }

    /// Try to reconstruct a typed value from a stringified hash key.
    /// Hash keys are always stored as strings, but for key-constrained hashes
    /// (e.g. `%h{Int}`), we need to check the original type. If the string
    /// looks like a valid value of the target type, return that typed value.
    pub(super) fn try_reconstruct_typed_key(key: &str, target_type: &str) -> Value {
        match target_type {
            "Int" => {
                if let Ok(n) = key.parse::<i64>() {
                    return Value::Int(n);
                }
            }
            "Num" => {
                if let Ok(n) = key.parse::<f64>() {
                    return Value::Num(n);
                }
            }
            "Numeric" | "Real" | "Cool" | "Any" | "Mu" => {
                // These accept strings, so just return the string
                return Value::str(key.to_string());
            }
            _ => {}
        }
        Value::str(key.to_string())
    }
}
