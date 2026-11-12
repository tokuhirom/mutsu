//! Call-site container identity for first-class infix routines.
use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// A named call argument's live cell, if its binding has one.
    // Cost: O(1) map and slot probes.
    pub(super) fn call_site_source_cell(&self, index: usize) -> Option<&Value> {
        let name = self.pending_call_arg_sources()?.get(index)?.as_deref()?;
        let slot = self
            .pending_call_arg_source_slots
            .get(&crate::symbol::Symbol::intern(name));
        slot.and_then(|slot| self.locals.get(*slot as usize))
            .or_else(|| self.env().get(name))
            .filter(|value| value.is_container_ref())
    }

    /// Whether the two variables the current call site passed share one
    /// container, or `None` when the call site did not name two variables.
    // Cost: O(c), c = length of the `:=` alias chains of the two names.
    pub(super) fn call_site_containers_identical(&self, args: &[Value]) -> Option<bool> {
        let sources = self.pending_call_arg_sources()?;
        if let (Some(left), Some(right)) =
            (self.call_site_source_cell(0), self.call_site_source_cell(1))
            && crate::value::identity::container_identity_identical(left, right)
        {
            return Some(true);
        }
        let plain_value = |v: &Value| {
            let held = v.unwrap_varref().clone().into_deref();
            // An itemized element (`$(3)`) is a Scalar holding the plain value.
            let held = match held.view() {
                ValueView::Scalar(inner) => inner.clone(),
                _ => held,
            };
            matches!(
                held.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Rat(..)
                    | ValueView::FatRat(..)
                    | ValueView::Complex(..)
            )
        };
        let (left, right) = match sources.as_slice() {
            [Some(left), Some(right)] => (left, right),
            // A named scalar variable against an unnamed operand (a call result, a
            // literal): the variable's Scalar is never the operand's, which is a
            // plain value or another call's result -- `&with($first, &as($next))`.
            [Some(named), None] | [None, Some(named)] => {
                return (!named.starts_with(['@', '%', '&']) && args.iter().all(plain_value))
                    .then_some(false);
            }
            _ => return None,
        };
        let root = |name: &str| {
            // A sigilless parameter is stored under its term key.
            let direct = self.resolve_alias_root(name).into_owned();
            if direct != name {
                return direct;
            }
            let keyed = crate::term_names::term_key(name);
            let resolved = self.resolve_alias_root(&keyed).into_owned();
            if resolved == keyed { direct } else { resolved }
        };
        let (left_root, right_root) = (root(left), root(right));
        // A sigilless term is judged like the operator form (`exec_container_eq_named_op`)
        // judges it: two distinct names holding plain values are distinct containers.
        let scalar = |name: &str| !name.starts_with(['@', '%', '&']);
        if left_root == right_root {
            return Some(true);
        }
        // Distinct roots: two scalar variables hold distinct containers when
        // they hold plain values. Type objects, code objects, enum values,
        // aggregates and objects are singletons or may be bare `:=` bindings,
        // which the operator form (`exec_container_eq_named_op`) leaves to
        // value identity, so they keep the value comparison here too.
        (scalar(&left_root) && scalar(&right_root) && args.iter().all(plain_value))
            .then_some(false)
    }
}
