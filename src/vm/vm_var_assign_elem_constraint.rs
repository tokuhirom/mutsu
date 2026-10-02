//! The element constraint an element store (`@a[i] = v`, `%h{k} = v`, and
//! their slice forms) checks the stored value against.
//!
//! The constraint belongs to the container (ADR-0042): `@P::a` reached through
//! its package-qualified name is the same `Array[Int]` as `@a` inside
//! `package P { our Int @a }`, but only the declaring name has an entry in the
//! by-name `__mutsu_type::<name>` lane. Reading the lane alone let
//! `@P::a[5] = "x"` through while `@P::a.push("x")` (which reads the container)
//! was refused (#10411).
use super::*;

impl Interpreter {
    /// The element type an element store into the `@`/`%` variable `var_name`
    /// must satisfy: the by-name constraint when the variable has one,
    /// otherwise the element type its current container carries
    /// (`Array[Int]`, `Hash[Int]`). `None` for an unconstrained container.
    ///
    /// A `$`-sigil name only consults the by-name lane, as before: its lane
    /// constraint describes the scalar, and the callers reduce it to an element
    /// type themselves.
    // Cost: O(1) amortized (one env probe and one metadata read; the
    // returned name is cloned, O(t), t = type-name length).
    pub(crate) fn element_store_constraint(&self, var_name: &str) -> Option<String> {
        if let Some(constraint) = self.var_type_constraint(var_name) {
            return Some(constraint);
        }
        if !var_name.starts_with(['@', '%']) {
            return None;
        }
        let container = self.env().get(var_name)?.deref_container();
        if !matches!(container.view(), ValueView::Array(..) | ValueView::Hash(..)) {
            return None;
        }
        let info = self.container_type_metadata(&container)?;
        if matches!(info.value_type.as_str(), "" | "Any" | "Mu") {
            return None;
        }
        Some(info.value_type)
    }
}
