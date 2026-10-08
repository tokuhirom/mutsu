//! Core candidates typed `IO::Path` outrank a user `multi` that takes an
//! untyped first parameter.
//!
//! mutsu's builtin routines are not registry candidates, so a user
//! `multi sub slurp($source where $source.&is-url, ...)` used to be tried first
//! for `slurp($io-path)` and its `where` clause ran against an `IO::Path`
//! (Data::Importers, `t/02-basic-usage-via-slurp.rakutest`). In Rakudo the
//! setting's `slurp` / `dir` / `spurt` / `unlink` candidates have an
//! `IO::Path`-typed (or `IO()`-coerced) first parameter, which is narrower than
//! the user's `Any`-typed one, so the core candidate wins and the user's `where`
//! clause never runs. Routines whose core signature is untyped (`lines`,
//! `words`) are deliberately not listed: there the user candidate does win.

use super::*;

/// Core routines whose setting candidates take an `IO::Path` first argument.
const CORE_IO_PATH_ROUTINES: &[&str] = &["slurp", "dir", "spurt", "unlink"];

impl Interpreter {
    /// Whether `name` called with `args` reaches a core candidate typed
    /// `IO::Path` (the first positional argument is an `IO::Path`).
    // Cost: O(a), a = arguments up to the first positional.
    pub(crate) fn core_io_path_candidate_applies(name: &str, args: &[Value]) -> bool {
        CORE_IO_PATH_ROUTINES.contains(&name.rsplit("::").next().unwrap_or(name))
            && args
                .iter()
                .find(|v| !v.is_string_pair_value())
                .is_some_and(|v| {
                    let v = match v.view() {
                        ValueView::VarRef { value, .. } => value.clone(),
                        _ => v.clone(),
                    };
                    matches!(v.into_deref().view(), ValueView::Instance { class_name, .. }
                        if class_name == "IO::Path")
                })
    }

    /// Whether `def`'s first positional parameter is untyped but carries a
    /// `where` clause: narrower-than-`Any` core candidates outrank it.
    // Cost: O(p), p = parameters.
    pub(crate) fn candidate_is_untyped_where(def: &FunctionDef) -> bool {
        def.param_defs
            .iter()
            .find(|p| !p.named)
            .is_some_and(|p| p.type_constraint.is_none() && p.where_constraint.is_some() && !p.slurpy)
    }

    /// Drop the user candidates of `name` that a narrower core candidate
    /// outranks for these `args`, so the call reaches the builtin.
    // Cost: O(c), c = candidates.
    pub(crate) fn retain_candidates_not_outranked_by_core(
        &self,
        name: &str,
        args: &[Value],
        candidates: &mut Vec<(String, Arc<FunctionDef>)>,
    ) {
        if Self::core_io_path_candidate_applies(name, args) {
            candidates.retain(|(_, def)| !Self::candidate_is_untyped_where(def));
        }
    }
}
