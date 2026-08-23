use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// ADR-0049 slice 3: the store-time `Nil` default for a whole-container
    /// (list-)assignment, computed BEFORE the container's own type metadata
    /// has necessarily been tagged onto it -- `tag_container_metadata`/
    /// `coerce_typed_container_assignment` run later in the same opcode
    /// (`vm_var_assign_set_local.rs`, `vm_var_assign_local.rs`).
    ///
    /// Tries the container's OWN already-embedded state first, via the same
    /// two checks `typed_container_default` itself starts with (an explicit
    /// `is default(...)` value, then declared element-type metadata) -- this
    /// covers a value that already carries its container's identity (e.g. a
    /// bind/write-through source). Failing that, consults the *target
    /// variable's* own declared `is default(...)` / element-type constraint
    /// (the same ADR-0042 side table this opcode is about to embed as the
    /// container's own metadata a few lines later), so a fresh typed
    /// declaration decays to the same value the per-element ladder in
    /// `coerce_typed_array_elements` would otherwise have computed -- the two
    /// no longer disagree, and once this hook has run first, `item.is_nil()`
    /// is already false by the time that (still-needed, for other callers
    /// such as write-through reassignment and shaped-array recursion) ladder
    /// runs, so it never double-applies. Only when neither the container nor
    /// the target name is typed does this fall through to
    /// `typed_container_default`'s own generic real-Array/Hash default
    /// (`Any`).
    pub(crate) fn assign_store_nil_default(&mut self, name: &str, container: &Value) -> Value {
        if let Some(def) = self.container_default(container) {
            return def.clone();
        }
        if let Some(info) = self.container_type_metadata(container) {
            if let Some(def) = super::vm_var_ops::native_element_default(&info.value_type) {
                return def;
            }
            return if info.value_type.is_empty() {
                Value::package(Symbol::intern("Any"))
            } else {
                Value::package(Symbol::intern(&info.value_type))
            };
        }
        if let Some(def) = self.var_default(name) {
            return def.clone();
        }
        if let Some(constraint) = loan_env!(self, var_type_constraint(name)) {
            if let Some(def) = super::vm_var_ops::native_element_default(&constraint) {
                return def;
            }
            let base = crate::runtime::types::strip_type_smiley(&constraint).0;
            return Value::package(Symbol::intern(base));
        }
        self.typed_container_default(container)
    }

    /// `@a = Nil` is a one-element *list* assignment whose single `Nil`
    /// element resets to the owning container's own `is default(...)` (raku:
    /// `[42]`, not `[Any]`). `coerce_to_array` is deliberately type-blind and
    /// hardcodes `Any` for a bare `Nil` RHS, and the `is default(...)` hole
    /// fixup on the SetLocal path does not run at all on the by-name
    /// `SetGlobal` store that an attribute twigil (`@!a`) compiles to.
    ///
    /// So consult both sources: the *outgoing* container's embedded default
    /// (tagged at construction for a public container attribute, and the only
    /// source available when the store is by slot) and, failing that, the
    /// name-keyed `var_default` (which method entry populates for
    /// `@!a`/`@.a`, and which is the only source for a private-only
    /// attribute, whose container is not tagged at construction). The fresh
    /// container is re-tagged, because Raku's `=` assigns *into* an Array
    /// rather than replacing it, so `@!a[5]` still yields the default
    /// afterwards.
    ///
    /// Callers gate on the RHS actually being `Nil`, so the (cheap) old-value
    /// clone is only paid on that path.
    pub(crate) fn array_assign_nil_container_default(
        &mut self,
        name: &str,
        old_container: &Value,
        assigned: Value,
    ) -> Value {
        let Some(def) = self
            .container_default(old_container)
            .or_else(|| self.var_default(name).cloned())
        else {
            return assigned;
        };
        let replaced = Value::real_array(vec![def.clone()]);
        self.tag_container_default(replaced, def)
    }

    /// ADR-0049 slice 3: replaces the narrow, hardcoded-`Any`
    /// `nil_elems_to_any` fixup that used to run only for a whole-array
    /// (list-)assignment to an UNTYPED `@` variable (gated on
    /// `var_type_constraint(name).is_none()`, the ADR-0042 side table this
    /// ADR's own decision text names for retirement). Decays each `Nil`
    /// element of a freshly-built real-array value to the target's own
    /// default via [`Self::assign_store_nil_default`] regardless of whether
    /// the target is typed -- a no-op when there is nothing to decay.
    pub(crate) fn decay_nil_elements_for_var_assign(&mut self, name: &str, value: Value) -> Value {
        let has_nil_elements = matches!(
            value.view(),
            ValueView::Array(items, kind)
                if kind.is_real_array() && items.iter().any(Value::is_nil)
        );
        if !has_nil_elements {
            return value;
        }
        let default = self.assign_store_nil_default(name, &value);
        let ValueView::Array(items, kind) = value.view() else {
            unreachable!("has_nil_elements only true for ValueView::Array");
        };
        // Clone the ArrayData so shape/default/type metadata survive; only
        // the items are rewritten.
        let mut data = (**items).clone();
        for item in data.items_mut() {
            if item.is_nil() {
                *item = default.clone();
            }
        }
        Value::array_with_kind(crate::gc::Gc::new(data), kind)
    }

    /// ADR-0049 slice 3: the `Vec<Value>`-based counterpart of
    /// [`Self::decay_nil_elements_for_var_assign`] for call sites that build
    /// a raw item list rather than an already-tagged container value --
    /// lazy-list reification into an array-context slot
    /// (`vm_helpers_lazy.rs`) and the untyped-only `push`/`append`/`unshift`
    /// fast path (`vm_call_method_mut_ops.rs`, which bails to the slow path
    /// for any typed/metadata-tagged target before reaching this call, so
    /// the result is always the untyped `Any` default here). Reuses
    /// [`Self::decay_nil_container_elements`] (the Slice 2 construction-time
    /// hook) via a throwaway untyped-array wrapper instead of re-deriving
    /// the same "untyped real Array defaults to `Any`" rule a second time.
    pub(crate) fn decay_nil_vec_elements(&mut self, items: Vec<Value>) -> Vec<Value> {
        if !items.iter().any(Value::is_nil) {
            return items;
        }
        let wrapped = self.decay_nil_container_elements(Value::real_array(items));
        match wrapped.view() {
            ValueView::Array(items, _) => items.iter().cloned().collect(),
            _ => unreachable!("decay_nil_container_elements preserves the Array shape"),
        }
    }
}
