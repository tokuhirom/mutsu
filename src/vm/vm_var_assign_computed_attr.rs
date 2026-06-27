use super::*;
use std::sync::Arc;

impl Interpreter {
    /// Assign a single value into a stack-computed Hash/Array `target` at `key`
    /// via interior mutation, writing through an existing `:=`-bound
    /// `ContainerRef` cell. Used by junction/slice autothreading in the generic
    /// index-assign op, where the target is a resolved inner container reached
    /// through a nested subscript (`%h<x>{...}`).
    pub(crate) fn assign_into_computed_target(
        &self,
        target: &Value,
        key: &Value,
        val: Value,
    ) -> Result<(), RuntimeError> {
        match target {
            Value::Hash(arc) => {
                let k = Value::hash_key_encode(key);
                // SAFETY: aliased in-place mutation of a shared hash; see
                // `arc_contents_mut`. No live borrow into the map.
                let hd = unsafe { crate::value::arc_contents_mut(arc) };
                Value::hash_insert_through(&mut hd.map, k, val);
            }
            Value::Array(arc, _) => {
                if let Some(i) = Self::index_to_usize(key) {
                    // SAFETY: aliased in-place mutation of a shared array; see
                    // `arc_contents_mut`.
                    let v = &mut unsafe { crate::value::arc_contents_mut(arc) }.items;
                    Self::autoviv_resize(v, i + 1, Value::Nil)?;
                    Value::assign_element_slot(&mut v[i], val);
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Phase 2 phantom-entry: materialize a missing-key `:=` bind on the first
    /// write through the bound variable. A local holding a `HashEntryRef` deferred
    /// token (single missing key `$e := %m<solo>`, or a multi-key path
    /// `$d := %k<p><q>`) is converted into a shared `ContainerRef` cell: the path
    /// is walk-created (`hash_entry_terminal`) and the cell is installed at the
    /// terminal hash entry, and the local is replaced with the same cell. After
    /// this the bound var and the hash entry alias bidirectionally — the old
    /// plain-value materialization lost the alias so a later cross-write was not
    /// observed (the case-C bug). Returns `true` when it handled `idx`; the
    /// deferred token is unchanged until first write, so `:exists` and pre-write
    /// reads keep their lazy semantics.
    pub(super) fn materialize_bound_slot_to_cell(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        val: Value,
    ) -> bool {
        let cell = match &self.locals[idx] {
            token @ Value::HashEntryRef { .. } => {
                // Walk-create the deferred path (single key for `$e := %m<solo>`,
                // multi-key for `$d := %k<p><q>`) and install the shared cell at
                // the terminal entry so the bound var and the hash entry alias
                // bidirectionally afterwards.
                let Some((arc, key)) = token.hash_entry_terminal() else {
                    return false;
                };
                let cell = Arc::new(std::sync::Mutex::new(val));
                // SAFETY: aliased in-place mutation of a shared hash; see
                // `arc_contents_mut`. No live borrow into the map.
                let hd = unsafe { crate::value::arc_contents_mut(&arc) };
                Value::hash_insert_through(&mut hd.map, key, Value::ContainerRef(cell.clone()));
                cell
            }
            _ => return false,
        };
        self.locals[idx] = Value::ContainerRef(cell);
        self.flush_local_to_env(code, idx);
        true
    }

    // --- Phase 3 Stage 2: scalar instance attributes as cell-direct (slice 1) ---
    //
    // For scalar attribute-twigil locals (`$!x` -> `!x`, `$.x` -> `.x`) the
    // instance's shared attribute cell is the single source of truth. Reads come
    // straight from the cell (so a mutation made in a nested method frame is
    // visible to the caller — the cross-frame bug), and every write mirrors the
    // local back into the cell. This lets the scalar writeback be dropped
    // (`writeback_attributes*` skip scalar attrs). Array/hash attributes still
    // use the materialize+writeback path for now (later slices).

    /// If `name` is an attribute-twigil local — scalar (`!x`/`.x`), array
    /// (`@!x`/`@.x`) or hash (`%!x`/`%.x`) — return `(bare attribute name,
    /// is_private)`. Excludes special vars (`!`, `.`) and internal names. The
    /// cell stores attributes under the bare name, so all six twigil forms of an
    /// attribute resolve to the same cell slot.
    pub(super) fn attr_twigil_base(name: &str) -> Option<(&str, bool)> {
        // Optional `@`/`%` sigil, then the `!` (private) / `.` (public) twigil.
        let rest = name
            .strip_prefix('@')
            .or_else(|| name.strip_prefix('%'))
            .unwrap_or(name);
        let (bare, is_private) = if let Some(b) = rest.strip_prefix('!') {
            (b, true)
        } else if let Some(b) = rest.strip_prefix('.') {
            (b, false)
        } else {
            return None;
        };
        // Attribute names are ordinary identifiers (start alpha/underscore). This
        // filters out `!=`, the bare `!`/`.` special vars, and `__mutsu_` keys.
        match bare.chars().next() {
            Some(c) if c.is_alphabetic() || c == '_' => Some((bare, is_private)),
            _ => None,
        }
    }

    /// Resolve the cell key for an attribute on the given instance, preferring
    /// the method owner class's qualified private key when present (Parent/Child
    /// same-named `$!priv` disambiguation), matching the order used when method
    /// frames materialize attributes. Returns `None` when the attribute does not
    /// exist in the cell (so non-attribute `.foo`/`!foo` lvalues fall through to
    /// the normal local/env handling).
    pub(crate) fn resolve_attr_cell_key(
        &self,
        name: &str,
        attrs: &crate::value::InstanceAttrs,
    ) -> Option<String> {
        let (bare, is_private) = Self::attr_twigil_base(name)?;
        let map = attrs.as_map();
        if is_private && let Some(owner) = self.method_class_stack_top() {
            let qkey = format!("{}\0{}", owner, bare);
            if map.contains_key(&qkey) {
                return Some(qkey);
            }
        }
        if map.contains_key(bare) {
            Some(bare.to_string())
        } else {
            None
        }
    }

    /// The inner instance's shared attribute cell for a `self` value, unwrapping a
    /// `Value::Mixin` (runtime `$obj does Role`) to the wrapped instance. The
    /// Mixin's inner value is held in a shared `Arc`, and the instance's own cell is
    /// an `Arc<RwLock>` — so a write through this reference persists back to the
    /// caller's Mixin (it shares the same inner instance). Returns `None` for a
    /// type object / non-instance.
    pub(crate) fn self_instance_attrs(val: &Value) -> Option<&crate::value::InstanceAttrs> {
        match val {
            Value::Instance { attributes, .. } => Some(attributes),
            Value::Mixin(inner, _) => Self::self_instance_attrs(inner),
            _ => None,
        }
    }

    /// Read a scalar attribute straight from `self`'s shared cell. `Some` only
    /// when `name` is a scalar attr-twigil, `self` is a concrete instance (or a
    /// Mixin wrapping one), and the attribute exists in the cell.
    pub(super) fn read_self_attr_cell(&self, name: &str) -> Option<Value> {
        let twigil = self.canonical_attr_twigil(name)?;
        let self_val = self.get_env_with_main_alias("self")?;
        let attributes = Self::self_instance_attrs(&self_val)?;
        let key = self.resolve_attr_cell_key(&twigil, attributes)?;
        attributes.as_map().get(&key).cloned()
    }

    /// Map a variable name to its canonical attribute-twigil form for cell access:
    /// a direct twigil (`!x`/`@.y`/…) maps to itself; a bare sigilless name
    /// (`has $x` → `Var("x")`) resolves through the runtime alias table to its
    /// `!x` twigil. Returns `None` for ordinary (non-attribute) names. The
    /// sigilless lookup is gated on `sigilless_attrs_active` so the common case
    /// (no sigilless attributes) costs only a string check on the hot read path.
    pub(crate) fn canonical_attr_twigil(&self, name: &str) -> Option<String> {
        if Self::attr_twigil_base(name).is_some() {
            return Some(name.to_string());
        }
        if !self.sigilless_attrs_active {
            return None;
        }
        self.sigilless_attr_twigil(name)
    }

    /// Follow the `__mutsu_sigilless_alias::` chain from a bare sigilless name
    /// until it reaches an attribute twigil (`!x`), returning that twigil. The
    /// alias table is bidirectional (`x ↔ !x`), so the `seen` guard prevents a
    /// cycle; returns `None` if the chain has no attribute-twigil link.
    pub(crate) fn sigilless_attr_twigil(&self, name: &str) -> Option<String> {
        let mut current = name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(current.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", current);
            match self.env().get(&key) {
                Some(Value::Str(next)) => {
                    let next = next.to_string();
                    if Self::attr_twigil_base(&next).is_some() {
                        return Some(next);
                    }
                    current = next;
                }
                _ => return None,
            }
        }
        None
    }

    /// Skip mirroring slot values that keep their legacy handling (`:=` bindings,
    /// Proxy accessors, hash/array slot refs, deferred hash access).
    pub(crate) fn is_non_mirrorable_attr_value(val: &Value) -> bool {
        matches!(
            val,
            Value::ContainerRef(_) | Value::Proxy { .. } | Value::HashEntryRef { .. }
        )
    }

    /// Write `val` into `self`'s shared cell for the scalar attribute named
    /// `name` (`!x`/`.x`), resolving the qualified private key when present.
    /// No-op when `name` is not a scalar attr-twigil, `self` is not a concrete
    /// instance, or the attribute does not exist on `self`.
    pub(super) fn write_self_attr_cell(&self, name: &str, val: Value) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        let Some(self_val) = self.get_env_with_main_alias("self") else {
            return;
        };
        // Unwrap a `Value::Mixin` self to the inner instance's shared cell so a
        // runtime-`does` mixin method's `$.attr`/`$!attr` write persists (the cell
        // is an `Arc<RwLock>` shared with the caller's Mixin).
        let Some(attributes) = Self::self_instance_attrs(&self_val) else {
            return;
        };
        if let Some(key) = self.resolve_attr_cell_key(name, attributes) {
            attributes.insert(key, val);
        }
    }

    /// Mirror the current local slot value into `self`'s shared cell for a scalar
    /// attribute, after the normal write logic has finalized the slot.
    pub(super) fn mirror_attr_local_to_cell(&self, code: &CompiledCode, idx: usize) {
        let Some(name) = code.locals.get(idx) else {
            return;
        };
        if Self::attr_twigil_base(name).is_none()
            || Self::is_non_mirrorable_attr_value(&self.locals[idx])
        {
            return;
        }
        self.write_self_attr_cell(&name.clone(), self.locals[idx].clone());
    }

    /// Mirror the finalized value of the variable named `name` into `self`'s
    /// shared cell, for write ops that dispatch by name (e.g. the name-based
    /// `AssignExpr`). Reads the value back from the local slot or env after the
    /// op completed.
    pub(super) fn mirror_attr_value_to_cell_by_name(&self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        let val = self
            .find_local_slot(code, name)
            .map(|slot| self.locals[slot].clone())
            .or_else(|| self.get_env_with_main_alias(name));
        let Some(val) = val else {
            return;
        };
        if Self::is_non_mirrorable_attr_value(&val) {
            return;
        }
        self.write_self_attr_cell(name, val);
    }

    /// True if `name` is an array/hash attribute twigil (`@!`/`@.`/`%!`/`%.`).
    pub(crate) fn is_array_hash_attr_twigil(name: &str) -> bool {
        (name.starts_with("@!")
            || name.starts_with("@.")
            || name.starts_with("%!")
            || name.starts_with("%."))
            && Self::attr_twigil_base(name).is_some()
    }

    /// Snapshot the env/shared value of an array/hash attribute variable before a
    /// mutating op, so [`mirror_array_hash_attr_to_cell`] can tell a genuine
    /// mutation (env value changed) from a non-mutating method call (`@!a.join`)
    /// on a stale env copy — mirroring the stale copy would clobber a cross-frame
    /// cell mutation. Returns `None` for non-attribute targets (cheap fast path).
    ///
    /// Also refreshes the env/local copy from `self`'s live cell before the op
    /// runs: a closure-captured env copy is a stale snapshot from closure
    /// creation, so letting the op mutate it (and mirroring the result) would
    /// clobber keys written by earlier calls — `%!h{$k} = $v` inside a returned
    /// closure kept only the last write. After the refresh the op starts from
    /// the live cell value and the mirror's pre-snapshot is that same value.
    pub(super) fn array_hash_attr_env_snapshot(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Value> {
        let name = Self::const_str(code, name_idx);
        if !Self::is_array_hash_attr_twigil(name) {
            return None;
        }
        let name = name.to_string();
        let env_val = self
            .get_env_with_main_alias(&name)
            .or_else(|| self.get_shared_var(&name));
        // `:=` bindings / slot refs keep their legacy env handling.
        if env_val
            .as_ref()
            .is_some_and(Self::is_non_mirrorable_attr_value)
        {
            return env_val;
        }
        if let Some(cell_val) = self.read_self_attr_cell(&name) {
            // Env copies share the cell value's Arc until a copy-on-write fork;
            // pointer inequality means the copy is stale (or absent) — adopt the
            // live cell value so the op mutates current state.
            if !env_val
                .as_ref()
                .is_some_and(|v| Self::same_container_arc(v, &cell_val))
            {
                self.env_mut().insert(name.clone(), cell_val.clone());
                if let Some(slot) = self.find_local_slot(code, &name) {
                    self.locals[slot] = cell_val.clone();
                }
            }
            return Some(cell_val);
        }
        env_val
    }

    /// Cheap container identity check: true when both values are the same
    /// Array/Hash Arc (a clone that has not been copy-on-write forked).
    pub(crate) fn same_container_arc(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Array(x, _), Value::Array(y, _)) => Arc::ptr_eq(x, y),
            (Value::Hash(x), Value::Hash(y)) => Arc::ptr_eq(x, y),
            _ => false,
        }
    }

    /// Mirror an array/hash attribute variable's post-mutation value into `self`'s
    /// shared cell (Phase 3 Stage 2b). Used after the mutating array/hash ops
    /// (`@!a.push`, `@!a[i]=`, `%!h<k>=`, …), which write the new container into
    /// env/shared keyed by `name`. Only fires for `@!`/`@.`/`%!`/`%.` twigils and
    /// only when the env value actually changed from `pre` (so a non-mutating
    /// method like `@!a.join` on a stale env copy does not clobber the cell).
    pub(super) fn mirror_array_hash_attr_to_cell(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        pre: Option<Value>,
    ) {
        let name = Self::const_str(code, name_idx);
        if !Self::is_array_hash_attr_twigil(name) {
            return;
        }
        let name = name.to_string();
        // The mutating ops write the new container into env (or shared_vars).
        let val = self
            .get_env_with_main_alias(&name)
            .or_else(|| self.get_shared_var(&name));
        let Some(val) = val else {
            return;
        };
        // No env change -> either a non-mutating method or a no-op; do not write
        // a possibly-stale env copy over a cross-frame cell mutation.
        if pre.as_ref() == Some(&val) {
            return;
        }
        if Self::is_non_mirrorable_attr_value(&val) {
            return;
        }
        self.write_self_attr_cell(&name, val);
    }

    /// Refresh the local slot from `self`'s cell before a read-modify-write on a
    /// scalar attribute (increment/decrement), so the operation sees a mutation
    /// made in a nested frame rather than the materialized snapshot.
    pub(crate) fn sync_attr_local_from_cell(&mut self, code: &CompiledCode, idx: usize) {
        if self.locals[idx].is_container_ref() {
            return;
        }
        let Some(name) = code.locals.get(idx).cloned() else {
            return;
        };
        if let Some(cell_val) = self.read_self_attr_cell(&name) {
            self.locals[idx] = cell_val;
        }
    }

    /// Name-based wrapper: refresh the slot named `name` from `self`'s cell before
    /// a read-modify-write (used by the increment/decrement ops, which dispatch
    /// by name rather than slot index).
    pub(super) fn sync_attr_local_from_cell_by_name(&mut self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        if let Some(slot) = self.find_local_slot(code, name) {
            self.sync_attr_local_from_cell(code, slot);
        }
    }

    /// Name-based wrapper: mirror the slot named `name` into `self`'s cell after a
    /// read-modify-write.
    pub(super) fn mirror_attr_local_to_cell_by_name(&self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        if let Some(slot) = self.find_local_slot(code, name) {
            self.mirror_attr_local_to_cell(code, slot);
        }
    }
}
