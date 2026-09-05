//! ADR-0067 slice 4: an lvalue subscript chain takes its step through an
//! object by calling `AT-KEY`/`AT-POS` in **lvalue** mode and descending into
//! the container that comes back.
//!
//! The chain walkers (`exec_index_assign_expr_nested_op` and
//! `exec_index_assign_deep_nested_op`) used to call the accessor as an rvalue
//! and throw its container away on the next line
//! (`call_method_with_values(..).deref_container()`), then fall through to a
//! generic Hash/Array walk against a root that is not a container. The write
//! was silently dropped, and at depth 3 the object was replaced by a freshly
//! autovivified Hash.
//!
//! Nothing new has to be produced to fix that: a rw-capable `AT-KEY` body is
//! already compiled with an rw tail (ADR-0059 / ADR-0067 slice 2), so the call
//! already hands back a `ContainerRef` cell (an existing element) or a
//! `HashEntryRef` token (a not-yet-existent one) — which is exactly why the
//! `:=`-bound spelling `my $e := $q<foo>; $e[0] = 99` has always worked. The
//! producer simply was not consulted. These helpers consult it.
//!
//! The *shape of the returned value* is the discriminator, not a declaration
//! probe: an accessor that is not rw-capable is compiled without an rw tail and
//! returns a plain value, so [`Interpreter::lvalue_object_step_container`]
//! answers `None` and every caller keeps its previous behaviour.

use super::*;

impl Interpreter {
    /// The subscript accessor a user-defined object serves this step with, or
    /// `None` when the value is not such an object.
    ///
    /// `index_positional` picks which of `AT-POS`/`AT-KEY` is tried first; the
    /// other is the fallback, so a class that supplies only one of them serves
    /// both spellings (matching the pre-existing behaviour of the walker this
    /// was extracted from).
    pub(crate) fn object_subscript_accessor(
        &mut self,
        target: &Value,
        index_positional: bool,
    ) -> Option<&'static str> {
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let cn = class_name.resolve();
        let (primary, secondary) = if index_positional {
            ("AT-POS", "AT-KEY")
        } else {
            ("AT-KEY", "AT-POS")
        };
        if self.has_user_method(&cn, primary) {
            Some(primary)
        } else if self.has_user_method(&cn, secondary) {
            Some(secondary)
        } else {
            None
        }
    }

    /// One lvalue subscript step through an object, for a walker that only
    /// needs the container to descend into: call `accessor` and materialize the
    /// Array/Hash the *next* subscript addresses.
    pub(crate) fn lvalue_object_subscript_container(
        &mut self,
        target: Value,
        accessor: &str,
        index: &Value,
        next_positional: bool,
    ) -> Result<Option<Value>, RuntimeError> {
        let returned = self.call_method_with_values(target, accessor, vec![index.clone()])?;
        Ok(Self::lvalue_object_step_container(
            &returned,
            next_positional,
        ))
    }

    /// The Array/Hash a deeper subscript must walk, given the value an object's
    /// `AT-KEY`/`AT-POS` returned.
    ///
    /// A **location** (a `ContainerRef` cell, or a `HashEntryRef` token for a
    /// key/index that does not exist yet) that already holds a container hands
    /// that container back — it shares its `Gc` node with the object's own
    /// storage, so a write through it reaches the object with no write-back. An
    /// **empty** location autovivifies a container of the kind the next step
    /// addresses and installs it, which is what makes `$q<new>[0] = 9` grow
    /// `{new => [9]}` the way raku does.
    ///
    /// An accessor that is **not** rw-capable returns no location at all — but
    /// when what it returns *is* a container, that container is the object's own
    /// (a method return shares its `Gc` node, and raku likewise mutates the
    /// returned `Array`/`Hash` object in place), so it is handed back too.
    ///
    /// Everything else answers `None` and the caller keeps its previous
    /// behaviour. In particular a location holding a defined non-container is
    /// what raku rejects outright ("Cannot modify an immutable Int") —
    /// vivifying over real data would be worse than doing nothing.
    pub(crate) fn lvalue_object_step_container(
        step: &Value,
        next_positional: bool,
    ) -> Option<Value> {
        match step.view() {
            ValueView::ContainerRef(cell) => {
                let held = cell.lock().unwrap().clone();
                if let Some(container) = Self::held_container(&held) {
                    return Some(container);
                }
                if crate::value::is_container_hole(&held) {
                    let fresh = Self::fresh_autoviv_container(next_positional);
                    Value::store_through_cell(&cell, &fresh);
                    return Some(fresh.descalarize().clone());
                }
                None
            }
            ValueView::HashEntryRef { .. } => {
                let held = step.hash_entry_read();
                if let Some(container) = Self::held_container(&held) {
                    return Some(container);
                }
                if crate::value::is_container_hole(&held) {
                    let fresh = Self::fresh_autoviv_container(next_positional);
                    step.hash_entry_write(fresh.clone());
                    return Some(fresh.descalarize().clone());
                }
                None
            }
            _ => Self::held_container(step),
        }
    }

    /// The Array/Hash inside a location's held value, looking through the
    /// `Scalar` an itemized element store leaves behind.
    pub(crate) fn held_container(held: &Value) -> Option<Value> {
        let bare = held.descalarize();
        matches!(bare.view(), ValueView::Array(..) | ValueView::Hash(..)).then(|| bare.clone())
    }

    /// Read an index-assignment root through any `:=` cell / itemizing `Scalar`,
    /// so a bound alias of a subscriptable object (`my $t := $u.query`) is
    /// recognized as one.
    pub(crate) fn index_assign_root_object(&mut self, var_name: &str) -> Option<Value> {
        let root = self.env().get(var_name)?.clone();
        Some(
            if matches!(
                root.view(),
                ValueView::ContainerRef(_) | ValueView::Scalar(_) | ValueView::HashEntryRef { .. }
            ) {
                root.deref_container().into_descalarized()
            } else {
                root
            },
        )
    }
}
