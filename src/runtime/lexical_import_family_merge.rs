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
    /// bound `name` through a hook contributes, and so does `env_hook`: the
    /// dispatcher a hook handed out for a built-in operator's name
    /// (`&trait_mod:<is>`), which joins the operator through its env alias
    /// instead of a set.
    // Cost: O(s * c), s = earlier sets, c = candidates of the family.
    pub(crate) fn merge_earlier_import_dispatchers(
        &self,
        binding: LexicalImport,
        name: Symbol,
        earlier_sets: &[Symbol],
        is_family: bool,
        env_hook: Option<&Value>,
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
        if is_family
            && let Some(ValueView::Sub(hook)) = env_hook.map(Value::view)
            && let Some(candidates) = Self::captured_dispatch_candidates(&hook)
        {
            earlier.extend(candidates);
        }
        let mut merged: Vec<Value> = Vec::with_capacity(earlier.len() + own.len());
        for candidate in earlier.into_iter().chain(own.iter().cloned()) {
            if !merged.iter().any(|seen| same_routine(seen, &candidate)) {
                merged.push(candidate);
            }
        }
        if merged.len() == own.len() {
            return binding;
        }
        LexicalImport::Code(with_candidates(&data, merged))
    }

    /// The dispatcher the last earlier set of the scope binds the built-in
    /// operator family `name` to, with the candidates of `hook` (the
    /// dispatcher a `sub EXPORT` hook just handed out under `&name`) after
    /// its own. `None` when no earlier set binds `name` as a dispatcher: the
    /// hook's candidates then join the operator through its env alias alone.
    // Cost: O(s + c), s = earlier sets, c = candidates of both.
    pub(crate) fn extend_earlier_family_with_hook(
        &self,
        name: Symbol,
        earlier_sets: &[Symbol],
        hook: Option<&Value>,
    ) -> Option<LexicalImport> {
        let ValueView::Sub(hook) = hook?.view() else {
            return None;
        };
        let hook_candidates = Self::captured_dispatch_candidates(&hook)?;
        let earlier = earlier_sets.iter().rev().find_map(|set| {
            match self.module.lexical_import_sets.get(set)?.get(&name)? {
                LexicalImport::Code(value) if matches!(value.view(), ValueView::Sub(_)) => {
                    Some(value.clone())
                }
                _ => None,
            }
        })?;
        let ValueView::Sub(data) = earlier.view() else {
            return None;
        };
        let (own, _) = data
            .env
            .get_sym(crate::symbol::well_known::multi_dispatch_candidates())
            .cloned()
            .and_then(Value::into_array)?;
        let mut merged: Vec<Value> = own.iter().cloned().collect();
        for candidate in hook_candidates {
            if !merged.iter().any(|seen| same_routine(seen, &candidate)) {
                merged.push(candidate);
            }
        }
        Some(LexicalImport::Code(with_candidates(&data, merged)))
    }
}

/// A copy of the dispatcher `data` over `candidates`. The family is no longer
/// the registry's by-name family, so the generation that lets a call take the
/// by-name path is dropped.
// Cost: O(e), e = the dispatcher's captured env.
fn with_candidates(data: &crate::value::SubData, candidates: Vec<Value>) -> Value {
    let mut next = data.clone();
    next.env.remove("__mutsu_multi_dispatch_gen");
    next.env.insert_sym(
        crate::symbol::wk::multi_dispatch_candidates(),
        Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(candidates)),
            crate::value::ArrayKind::List,
        ),
    );
    Value::sub_value(crate::gc::Gc::new(next))
}

/// Whether two candidate code objects are the same routine: one object, or
/// two code objects one declaration made, which share its parameter list
/// (the same family reached through a hook and through an `is export`
/// import builds a code object for each).
// Cost: O(1).
fn same_routine(a: &Value, b: &Value) -> bool {
    match (a.view(), b.view()) {
        (ValueView::Sub(x), ValueView::Sub(y)) => {
            x.id == y.id
                || (x.name == y.name
                    && x.package == y.package
                    && !x.param_defs.is_empty()
                    && std::sync::Arc::ptr_eq(&x.param_defs, &y.param_defs))
        }
        _ => false,
    }
}
