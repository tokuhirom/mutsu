//! Container-identity (`=:=`) comparison ops.
use super::*;

impl Interpreter {
    /// Container identity (`=:=`).
    ///
    /// `flags` encodes whether operands are provably fresh containers:
    ///   bit 0 = left operand is a fresh container (e.g. array index),
    ///   bit 1 = right operand is a fresh container.
    ///
    /// When at least one operand is a fresh container and both values
    /// are non-reference types (no `Arc` identity), the two stack
    /// values can never be the same container, so we return `False`.
    /// Reference types (Array, Hash, Sub, Instance, …) have `Arc`
    /// pointer identity which `values_identical` already checks.
    pub(super) fn exec_container_eq_op(&mut self, flags: u8) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let any_fresh = flags != 0;
        let result = if any_fresh && Self::is_value_non_reference(&left, &right) {
            // A fresh container (e.g. from `[$x][0]`) holding a non-reference
            // value can never be the same container as any other expression.
            false
        } else {
            crate::runtime::values_identical(&left, &right)
        };
        self.stack.push(Value::Bool(result));
    }

    /// Returns `true` when **both** values are simple, non-reference
    /// types where stack copies can never carry container identity
    /// (Int, Str, Bool, Nil, Rat, …).
    ///
    /// Note: `Package` is NOT included because type objects are singletons —
    /// two variables holding the same Package should be considered `=:=`.
    fn is_value_non_reference(left: &Value, right: &Value) -> bool {
        fn is_non_ref(v: &Value) -> bool {
            matches!(
                v,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Str(_)
                    | Value::Bool(_)
                    | Value::Nil
                    | Value::Rat(..)
                    | Value::FatRat(..)
                    | Value::BigRat(..)
                    | Value::Complex(..)
                    | Value::Whatever
                    | Value::HyperWhatever
                    | Value::Enum { .. }
                    | Value::Version { .. }
            )
        }
        is_non_ref(left) && is_non_ref(right)
    }

    /// Container identity (`=:=`) when both operands are named variables.
    /// Resolves alias chains to find the binding root of each name, then:
    /// - If both roots are the same name, they share a container → True.
    /// - For reference types (Array, Hash, Sub, Instance), use Arc pointer identity.
    /// - Otherwise → False (distinct scalar containers).
    pub(super) fn exec_container_eq_named_op(
        &mut self,
        code: &CompiledCode,
        left_name_idx: u32,
        right_name_idx: u32,
    ) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let left_name = Self::const_str(code, left_name_idx);
        let right_name = Self::const_str(code, right_name_idx);

        // Resolve alias chains to find the binding root of each variable.
        // =:= tests container identity — two named variables are identical
        // only when they share the same binding root (via `:=`).
        let left_root = self.resolve_alias_root(left_name);
        let right_root = self.resolve_alias_root(right_name);

        let result = if left_root == right_root {
            // Same binding root → same container.
            true
        } else {
            // Different binding roots. For most types, this means different
            // containers → False. But singleton/unboxed types like Package
            // (type objects) and Sub have no container semantics — two
            // variables holding the same Package are considered =:=.
            // Instance values (user objects) are NOT singletons: assignment
            // copies the reference but creates a new container, so =:= is False.
            match (&left, &right) {
                (Value::Package(a), Value::Package(b)) => a == b,
                (Value::Sub(a), Value::Sub(b)) => std::sync::Arc::ptr_eq(a, b),
                (Value::WeakSub(a), Value::WeakSub(b)) => a.ptr_eq(b),
                _ => false,
            }
        };
        self.stack.push(Value::Bool(result));
    }

    /// Walk the `__mutsu_sigilless_alias::` chain to find the ultimate
    /// binding root for a variable name.
    fn resolve_alias_root(&self, name: &str) -> String {
        let mut current = name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(current.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", current);
            if let Some(Value::Str(next)) = self.env().get(&key) {
                current = next.to_string();
            } else {
                break;
            }
        }
        current
    }

    /// Container identity (`=:=`) for indexed expressions (array/hash elements).
    /// A `:=`-bound element holds a shared `ContainerRef` cell, so two bound
    /// elements are the same container iff they hold the same cell `Arc`.
    pub(super) fn exec_container_eq_indexed_op(
        &mut self,
        code: &CompiledCode,
        left_name_idx: u32,
        right_name_idx: u32,
    ) {
        let _right = self.stack.pop().unwrap();
        let _left = self.stack.pop().unwrap();
        let left_source = Self::const_str(code, left_name_idx);
        let right_source = Self::const_str(code, right_name_idx);

        // Same cell `Arc` at both slots → same container.
        if let (Some(Value::ContainerRef(l)), Some(Value::ContainerRef(r))) = (
            self.raw_element_at_encoded(left_source),
            self.raw_element_at_encoded(right_source),
        ) && Arc::ptr_eq(&l, &r)
        {
            self.stack.push(Value::Bool(true));
            return;
        }
        // Same encoded source (e.g. @a[1] =:= @a[1])
        let result = left_source == right_source;
        self.stack.push(Value::Bool(result));
    }

    /// Container identity (`=:=`) using raw container values.
    /// Compares HashEntryRef values to check if they
    /// reference the same hash slot.
    pub(super) fn exec_container_eq_raw_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result =
            Self::containers_same_slot(&left, &right) || Self::containers_same_slot(&right, &left);
        self.stack.push(Value::Bool(result));
    }

    /// Check if two raw container values (HashEntryRef)
    /// point to the same hash slot.
    fn containers_same_slot(a: &Value, b: &Value) -> bool {
        // Phase 2 element container: a `ContainerRef` cell has container identity
        // by its `Arc<Mutex>`. Two cells are the same container only if the same
        // Arc; a cell is never the same container as a non-cell value. (Must be
        // checked here, not in `values_identical`, which `===` uses to compare
        // *values* and therefore must read through a cell.)
        match (a, b) {
            (Value::ContainerRef(x), Value::ContainerRef(y)) => return Arc::ptr_eq(x, y),
            (Value::ContainerRef(cell), other) | (other, Value::ContainerRef(cell)) => {
                // A `:=`-bound hash element is promoted to a `ContainerRef` cell
                // (Phase 2 Stage 1). The OTHER side may still be a HashEntryRef
                // deferred token pointing at that *same* element (e.g. a
                // deferred bind `my $b := %h<a><b>` whose element was later
                // promoted when re-evaluated). They are the same container iff
                // the element currently stored at that slot IS this cell.
                if let Some((arc, key)) = Self::extract_hash_ref(other) {
                    let ptr = Arc::as_ptr(&arc);
                    if let Some(Value::ContainerRef(elem_cell)) =
                        unsafe { (*ptr).get(key.as_str()) }
                    {
                        return Arc::ptr_eq(cell, elem_cell);
                    }
                }
                return false;
            }
            _ => {}
        }
        // Extract (arc, key) from each side
        let a_ref = Self::extract_hash_ref(a);
        let b_ref = Self::extract_hash_ref(b);
        if let (Some((a_arc, a_key)), Some((b_arc, b_key))) = (a_ref, b_ref) {
            Arc::ptr_eq(&a_arc, &b_arc) && a_key == b_key
        } else {
            // Both are the same value identity (for non-hash-ref cases)
            crate::runtime::values_identical(a, b)
        }
    }

    /// Extract the `(terminal hash Arc, terminal key)` a `HashEntryRef` points
    /// to, walking its `path` READ-ONLY. Returns `None` if any intermediate level
    /// is missing or not a hash (the deferred path is not yet materialized, so it
    /// has no stable container identity).
    fn extract_hash_ref(val: &Value) -> Option<(Arc<crate::value::HashData>, String)> {
        let Value::HashEntryRef { hash, path } = val else {
            return None;
        };
        let mut cur = hash.clone();
        for k in &path[..path.len() - 1] {
            let ptr = Arc::as_ptr(&cur);
            match unsafe { (*ptr).get(k.as_str()) } {
                Some(Value::Hash(inner)) => cur = inner.clone(),
                _ => return None,
            }
        }
        Some((cur, path.last().unwrap().clone()))
    }

    /// For an encoded source like "@b\0idx\01" (or "%h\0idx\0k"), look up the
    /// raw element value stored at that array/hash slot — without
    /// decontainerizing, so a `:=`-bound `ContainerRef` cell is returned as-is
    /// for identity comparison.
    fn raw_element_at_encoded(&self, encoded: &str) -> Option<Value> {
        let sep_pos = encoded.find("\x00idx\x00")?;
        let var_name = &encoded[..sep_pos];
        let idx_str = &encoded[sep_pos + 5..];
        match self.env().get(var_name) {
            Some(Value::Array(items, _)) => {
                let i = idx_str.parse::<usize>().ok()?;
                items.get(i).cloned()
            }
            Some(Value::Hash(hash)) => hash.get(idx_str).cloned(),
            _ => None,
        }
    }
}
