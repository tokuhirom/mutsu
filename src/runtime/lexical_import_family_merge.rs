//! One `multi` family across the `use`s of a scope (#17828).
//!
//! Two imports of a same-named `multi` into one scope are one family in Raku:
//! the name dispatches over every candidate either import brought. An
//! `is export` import's candidates are registry definitions, which
//! `finish_lexical_import_capture` already merges with an earlier set's
//! (`lexical_set_families`). A `sub EXPORT` hook hands out a dispatcher value
//! instead (`Map.new: Mod::EXPORT::ALL::`, Red's `use Red:api<2>`), whose
//! candidates are code objects with no registry definition to merge. Here the
//! binding a set gives such a name takes in the candidates of every earlier
//! set's dispatcher that `lexical_set_families` does not already cover, so
//! the last import binds the whole family whichever mechanism each import
//! used.

use super::*;
use crate::runtime::lexical_imports::LexicalImport;

impl Interpreter {
    /// `binding` (the binding set `id` gives `name`), extended by the
    /// candidates of every multi dispatcher an earlier set of the scope
    /// (`earlier_sets`, in import order) binds `name` to. With `is_family`
    /// (the binding is a dispatcher over this set's registry family, which
    /// already spans the earlier sets' families), only an earlier set that
    /// bound `name` through a hook contributes.
    // Cost: O(s * c), s = earlier sets, c = candidates of the family.
    pub(crate) fn merge_earlier_import_dispatchers(
        &self,
        binding: LexicalImport,
        name: Symbol,
        earlier_sets: &[Symbol],
        is_family: bool,
    ) -> LexicalImport {
        let LexicalImport::Code(value) = &binding else {
            return binding;
        };
        let ValueView::Sub(data) = value.view() else {
            return binding;
        };
        // The list the dispatcher captured; its `routine_cell` dispatchees
        // stay with the clone below.
        let Some((own, _)) = data
            .env
            .get_sym(crate::symbol::well_known::multi_dispatch_candidates())
            .cloned()
            .and_then(Value::into_array)
        else {
            return binding;
        };
        let own: Vec<Value> = own.iter().cloned().collect();
        let mut earlier: Vec<Value> = Vec::new();
        for set in earlier_sets {
            if is_family
                && self
                    .module
                    .lexical_set_families
                    .get(set)
                    .is_some_and(|families| families.contains_key(&name))
            {
                continue;
            }
            let Some(LexicalImport::Code(prior)) = self
                .module
                .lexical_import_sets
                .get(set)
                .and_then(|bindings| bindings.get(&name))
            else {
                continue;
            };
            let ValueView::Sub(prior) = prior.view() else {
                continue;
            };
            if let Some(candidates) = Self::captured_dispatch_candidates(&prior) {
                earlier.extend(candidates);
            }
        }
        let same = |a: &Value, b: &Value| match (a.view(), b.view()) {
            (ValueView::Sub(x), ValueView::Sub(y)) => x.id == y.id,
            _ => false,
        };
        let mut merged: Vec<Value> = Vec::with_capacity(earlier.len() + own.len());
        for candidate in earlier.into_iter().chain(own.iter().cloned()) {
            if !merged.iter().any(|seen| same(seen, &candidate)) {
                merged.push(candidate);
            }
        }
        if merged.len() == own.len() {
            return binding;
        }
        let mut next: crate::value::SubData = (**data).clone();
        // The family is no longer the by-name family of the registry.
        next.env.remove("__mutsu_multi_dispatch_gen");
        next.env.insert_sym(
            crate::symbol::wk::multi_dispatch_candidates(),
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(merged)),
                crate::value::ArrayKind::List,
            ),
        );
        LexicalImport::Code(Value::sub_value(crate::gc::Gc::new(next)))
    }
}
