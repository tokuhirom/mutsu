//! The declared candidates of a routine, by name, in declaration order --
//! the list `&name.candidates` exposes and `RUN-MAIN`'s usage message walks.

use super::*;
use crate::ast::FunctionDef;
use std::sync::Arc;

impl Interpreter {
    /// Every registered candidate of the routine `package`/`name`, in
    /// declaration order, each paired with whether it is a `multi` candidate
    /// (its registry key has the `name/<signature>` shape).
    // Cost: O(f log f), f = registered functions (one registry scan + sort).
    pub(super) fn routine_candidate_defs(
        &self,
        package: &str,
        name: &str,
    ) -> Vec<(Arc<FunctionDef>, bool)> {
        // A package-qualified `&Pkg::name` reference is a handle whose `name`
        // already spells the whole registry key (its package is only the
        // current one), so prefixing the package again looks up
        // `GLOBAL::Pkg::name` and finds no candidate of the proto it names.
        let (exact_local, prefix_local) = if crate::qualified::is_qualified(Symbol::intern(name)) {
            (name.to_string(), format!("{name}/"))
        } else {
            let exact = crate::qualified::qualified_text(package, name).as_str();
            (exact.to_string(), format!("{exact}/"))
        };
        let exact_global = format!("GLOBAL::{name}");
        let prefix_global = format!("GLOBAL::{name}/");
        let mut candidates = Vec::new();
        let registry = self.registry();
        for (key, def) in registry.functions.iter() {
            let key_s = key.resolve();
            // A plain (non-multi) sub is registered only under the exact
            // `pkg::name` key; a `multi sub`'s candidates -- even a single
            // one, with no sibling yet in this compilation unit -- are always
            // keyed `pkg::name/<mangled-signature>` (`registration_sub.rs`'s
            // `multi_prefix`), so the key SHAPE itself is `Code.multi`'s
            // source of truth, not how many candidates happened to survive
            // the dedup below.
            let is_multi_key =
                key_s.starts_with(&prefix_local) || key_s.starts_with(&prefix_global);
            if key_s == exact_local || key_s == exact_global || is_multi_key {
                candidates.push((def.clone(), is_multi_key));
            }
        }
        // Rakudo returns `.candidates` in DECLARATION order. Each candidate is
        // registered TWICE — once by the forward-declaration/hoist pre-pass,
        // once by the in-sequence pass that runs when execution reaches the
        // statement — and the second registration cannot reuse the hoisted
        // registry key (it is keyed by mangled type signature, e.g.
        // `GLOBAL::mm/1:Int`, which the hoist pass already occupied), so it
        // falls back to a `__m{N}`-suffixed key. That leaves TWO registry rows
        // per candidate with the SAME body (`body_fingerprint`) but DIFFERENT
        // `decl_order` stamps. The scan above visits the registry's `HashMap`
        // in bucket order, which is arbitrary and unstable against unrelated
        // statements elsewhere in the file — so naively keeping "whichever row
        // is seen first" per body fingerprint reproduced that instability.
        //
        // Fix: sort ALL rows (both copies of every candidate) by `decl_order`
        // first, then dedupe by body fingerprint keeping the smallest
        // `decl_order` — always the hoist-pass row, since hoisting walks the
        // block's statements top-to-bottom (`Compiler::hoist_sub_decls`) and so
        // stamps candidates in true declaration order, chronologically before
        // any in-sequence stamp. This mirrors the established `decl_order`
        // min-per-key dedup pattern already used for token/grammar proto
        // candidates (`token_key_decl_order`, `sort_sym_keys_by_decl_order` in
        // `resolution.rs`). See
        // todo/tickets/multi-candidates-declaration-order.md.
        drop(registry);
        // A compunit-scoped family the running code cannot see is not one of
        // this routine's candidates (#11081, `runtime/unit_multi_scope.rs`).
        if self.operator_has_import_scope(name) {
            let name_sym = Symbol::intern(name);
            candidates.retain(|(def, _)| self.operator_candidate_visible(name_sym, def));
        }
        candidates.sort_by_key(|(def, _)| def.decl_order);
        let mut seen = std::collections::HashSet::new();
        let mut defs = Vec::new();
        for (def, is_multi_key) in candidates {
            let fp = def.body_fingerprint();
            if seen.insert(fp) {
                defs.push((def, is_multi_key));
            }
        }
        defs
    }

    /// The candidates of the operator `name` (`infix:<+>`) the running unit
    /// declares itself, as code objects: what joins a family a `sub EXPORT`
    /// hook handed the unit when the unit adds a `multi` of its own (a
    /// block's `multi infix:<+>(Red::AST $l, Int $r)`), as rakudo's
    /// block-level `multi` derives the dispatcher in scope from the imported
    /// one. Empty, without a registry scan, when the running unit declares
    /// no candidate of `name`.
    // Cost: O(1) when the running unit declares none; else
    // `routine_candidate_defs` plus building each candidate.
    pub(crate) fn unit_declared_operator_candidates(&self, name: &str) -> Vec<Value> {
        let unit = self.executing_unit_sym();
        let declared_here = self
            .dispatch
            .user_declared_infix_ops
            .get(name)
            .is_some_and(|files| files.contains(&unit) || files.contains(&self.current_unit));
        if !declared_here {
            return Vec::new();
        }
        let defs = self
            .routine_candidate_defs("GLOBAL", name)
            .into_iter()
            .filter(|(def, _)| {
                def.dispatchee.is_none()
                    && def.source_file.is_some()
                    && self.unit_of_declaring_file(def.source_file.as_deref()) == unit
            })
            .collect();
        self.candidate_subs_from_defs(defs)
    }
}
