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
        self.stack.push(Value::truth(result));
    }

    /// Returns `true` when **both** values are simple, non-reference
    /// types where stack copies can never carry container identity
    /// (Int, Str, Bool, Nil, Rat, …).
    ///
    /// Note: `Package` is NOT included because type objects are singletons —
    /// two variables holding the same Package should be considered `=:=`.
    /// The sole exception is the `Any` type object: it is the seed of every
    /// uninitialized untyped scalar (PLAN 8.5 step 3), so a fresh container
    /// holding it must not compare identical to another Any-holding read —
    /// exactly how the old Nil seed behaved on this path.
    /// `Nil` IS included: two *variable/element reads* both yielding Nil are
    /// distinct containers (uninitialized scalars store Nil pre-PLAN-8.5-step-3),
    /// so they must not compare identical. The `X =:= Nil` literal form is
    /// compiled with `flags == 0` instead (see `compile_binary`'s `=:=` arm),
    /// which routes to `values_identical` where Nil == Nil holds.
    fn is_value_non_reference(left: &Value, right: &Value) -> bool {
        fn is_non_ref(v: &Value) -> bool {
            if v.is_any_type_object() {
                return true;
            }
            matches!(
                v.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Nil
                    | ValueView::Rat(..)
                    | ValueView::FatRat(..)
                    | ValueView::BigRat(..)
                    | ValueView::Complex(..)
                    | ValueView::Whatever
                    | ValueView::HyperWhatever
                    | ValueView::Enum { .. }
                    | ValueView::Version { .. }
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
            match (left.view(), right.view()) {
                // The Any type object is excluded from the Package-singleton
                // shortcut: it is the seed of every uninitialized untyped
                // scalar (PLAN 8.5 step 3), so two distinct uninitialized
                // containers would otherwise wrongly compare identical
                // (S03-operators/identity.t "basic sanity"). The cost — a
                // rare `my $a := Any; my $b := Any` pair no longer comparing
                // True — mirrors how the old Nil seed behaved here.
                (ValueView::Package(a), ValueView::Package(b)) => a == b && a.as_str() != "Any",
                (ValueView::Sub(a), ValueView::Sub(b)) => crate::gc::Gc::ptr_eq(&a, &b),
                (ValueView::WeakSub(a), ValueView::WeakSub(b)) => crate::gc::WeakGc::ptr_eq(&a, &b),
                _ => false,
            }
        };
        self.stack.push(Value::truth(result));
    }

    /// Container identity (`=:=`) when one operand is `EXPR.self` and the
    /// other is a named variable.
    ///
    /// `.self` decontainerizes: it hands back the invocant's value, so its
    /// result never *is* a Scalar container. The named side therefore matches
    /// only when it has no container of its own — a `:=` binding
    /// (`my $i := 42; $i.self =:= $i` is True) — whereas an assigned
    /// `my $a = 42` denotes a Scalar the value can never be identical to
    /// (`$a.self =:= $a` is False, even though both sides read as `42`).
    ///
    /// `@`/`%` names never reach here: `@a` denotes the Array itself, which is
    /// exactly what `.self` hands back, so `@a.self =:= @a` stays True via the
    /// generic value compare.
    pub(super) fn exec_container_eq_deconted_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let name = Self::const_str(code, name_idx);
        let result = if self.name_denotes_scalar_container(name) {
            false
        } else {
            crate::runtime::values_identical(&left, &right)
        };
        self.stack.push(Value::truth(result));
    }

    /// Whether the variable `name` denotes a **Scalar container** — the thing
    /// `=:=` compares — rather than a direct binding to a value.
    ///
    /// An ordinary `my $x = ...` owns a Scalar. A `:=` binding does not: the
    /// name *is* the bound thing, which is why `my $i := 42; $i.VAR.^name` is
    /// `Int` and not `Scalar`. mutsu records the three binding spellings
    /// separately — a literal RHS as [`ReadonlyKind::Immutable`], another
    /// variable in the `__mutsu_sigilless_alias::` chain — so all of them are
    /// consulted here. A readonly *alias* that does own a container (a
    /// non-`is rw` parameter, a `for @a -> $v` alias) is deliberately NOT
    /// excluded: rakudo reports `Scalar` for those.
    ///
    /// `@`/`%`/`&` names answer `false`: an aggregate is its own container, so
    /// `@a.self =:= @a` is True.
    pub(crate) fn name_denotes_scalar_container(&self, name: &str) -> bool {
        if name.starts_with(['@', '%', '&']) {
            return false;
        }
        // `my $b := @x` / `:= %h` aliases the aggregate, which is its own
        // container — no Scalar. `my $b := $a` aliases `$a`'s Scalar, so the
        // question becomes "does the ROOT own one", answered by recursing.
        let root = self.resolve_alias_root(name);
        if root != name {
            return self.name_denotes_scalar_container(&root);
        }
        // `my $i := 42` (recorded readonly-immutable by the parser) and
        // `my $o := C.new` (recorded by the store) both bind straight to a
        // value. A readonly *alias* with a container behind it — a non-`is rw`
        // parameter, a `for @a -> $v` alias — is deliberately not excluded:
        // rakudo reports `Scalar` for those.
        !self.scalar_name_has_no_container(name)
            && !self
                .env()
                .contains_key(&Self::scalar_bind_no_container_key(name))
    }

    /// Env key recording that a `$` name was `:=`-bound straight to a value and
    /// so owns no Scalar container. Written at the declaration's store; see
    /// `bind_marks_no_container` in `vm_var_assign_set_local.rs`.
    pub(crate) fn scalar_bind_no_container_key(name: &str) -> String {
        format!(
            "__mutsu_scalar_bind_no_container::{}",
            name.trim_start_matches('$')
        )
    }

    /// Walk the `__mutsu_sigilless_alias::` chain to find the ultimate
    /// binding root for a variable name.
    pub(crate) fn resolve_alias_root(&self, name: &str) -> String {
        let mut current = name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(current.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", current);
            if let Some(ValueView::Str(next)) = self.env().get(&key).map(Value::view) {
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
        let left_elem = self.raw_element_at_encoded(left_source);
        let right_elem = self.raw_element_at_encoded(right_source);
        if let (Some(ValueView::ContainerRef(l)), Some(ValueView::ContainerRef(r))) = (
            left_elem.as_ref().map(Value::view),
            right_elem.as_ref().map(Value::view),
        ) && crate::gc::Gc::ptr_eq(&l, &r)
        {
            self.stack.push(Value::TRUE);
            return;
        }
        // Same encoded source (e.g. @a[1] =:= @a[1])
        let result = left_source == right_source;
        self.stack.push(Value::truth(result));
    }

    /// Container identity (`=:=`) using raw container values.
    /// Compares HashEntryRef values to check if they
    /// reference the same hash slot.
    pub(super) fn exec_container_eq_raw_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result =
            Self::containers_same_slot(&left, &right) || Self::containers_same_slot(&right, &left);
        self.stack.push(Value::truth(result));
    }

    /// Check if two raw container values (HashEntryRef)
    /// point to the same hash slot.
    fn containers_same_slot(a: &Value, b: &Value) -> bool {
        // Phase 2 element container: a `ContainerRef` cell has container identity
        // by its `Arc<Mutex>`. Two cells are the same container only if the same
        // Arc; a cell is never the same container as a non-cell value. (Must be
        // checked here, not in `values_identical`, which `===` uses to compare
        // *values* and therefore must read through a cell.)
        match (a.view(), b.view()) {
            (ValueView::ContainerRef(x), ValueView::ContainerRef(y)) => {
                return crate::gc::Gc::ptr_eq(&x, &y);
            }
            (ValueView::ContainerRef(cell), _) | (_, ValueView::ContainerRef(cell)) => {
                // A `:=`-bound hash element is promoted to a `ContainerRef` cell
                // (Phase 2 Stage 1). The OTHER side may still be a HashEntryRef
                // deferred token pointing at that *same* element (e.g. a
                // deferred bind `my $b := %h<a><b>` whose element was later
                // promoted when re-evaluated). They are the same container iff
                // the element currently stored at that slot IS this cell.
                let other = if matches!(a.view(), ValueView::ContainerRef(_)) {
                    b
                } else {
                    a
                };
                if let Some(elem) = other.hash_entry_locate().and_then(|t| t.peek())
                    && let ValueView::ContainerRef(elem_cell) = elem.view()
                {
                    return crate::gc::Gc::ptr_eq(&cell, &elem_cell);
                }
                return false;
            }
            _ => {}
        }
        // Locate the terminal slot each side denotes and compare slot identity.
        let a_ref = a.hash_entry_locate();
        let b_ref = b.hash_entry_locate();
        if let (Some(a_slot), Some(b_slot)) = (a_ref, b_ref) {
            a_slot.same_slot(&b_slot)
        } else if a.is_any_type_object() && b.is_any_type_object() {
            // Two raw container reads that both hold the Any type object (the
            // uninitialized-scalar seed, PLAN 8.5 step 3) are distinct
            // containers — `[$foo][0] =:= $foo` with an uninit `$foo` is
            // False (S02-types/array_ref.t 45), matching the old Nil seed.
            false
        } else {
            // Both are the same value identity (for non-hash-ref cases)
            crate::runtime::values_identical(a, b)
        }
    }

    /// For an encoded source like "@b\0idx\01" (or "%h\0idx\0k"), look up the
    /// raw element value stored at that array/hash slot — without
    /// decontainerizing, so a `:=`-bound `ContainerRef` cell is returned as-is
    /// for identity comparison.
    fn raw_element_at_encoded(&self, encoded: &str) -> Option<Value> {
        let sep_pos = encoded.find("\x00idx\x00")?;
        let var_name = &encoded[..sep_pos];
        let idx_str = &encoded[sep_pos + 5..];
        match self.env().get(var_name).map(Value::view) {
            Some(ValueView::Array(items, _)) => {
                let i = idx_str.parse::<usize>().ok()?;
                items.get(i).cloned()
            }
            Some(ValueView::Hash(hash)) => hash.get(idx_str).cloned(),
            _ => None,
        }
    }
}
