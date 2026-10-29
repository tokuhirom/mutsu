//! A role body's plain `sub`s, declared once more for each composing class.
//!
//! Rakudo runs a role body once per composing class, and a `sub` the body
//! declares closes over that run's lexicals. `role R { my $count = 10; method m
//! { f() }; sub f { ++$count } }` gives `K` and `L` a counter each, and a call
//! through `K.m` keeps incrementing `K`'s (#13957).
//!
//! mutsu persists the body's lexicals as statics of the composing class
//! (`persist_role_body_lexicals_except`), where the composed methods read and
//! write them. The body's `sub`s, though, are registered in the role's own
//! package and closed over the body run's plain values. So `++$count` read a
//! fresh `10` on every call and wrote to a copy nobody saw. Here each such
//! `sub` is declared again in the composing class's package, with the
//! class's static cells bound for the body's lexicals. A composed method,
//! which runs in that package, then finds the class's own copy first.

use super::*;
use std::collections::HashSet;

impl Interpreter {
    /// Declare `role`'s (named `role_name`) plain body `sub`s again in
    /// `class`'s package, closed
    /// over `class`'s statics for the names in `declared` (the body's
    /// lexicals). Only a plain, non-exported `sub` without custom traits is
    /// re-declared: a `multi` or exported one belongs to the role (it is
    /// registered once, at declaration), and running a trait again would
    /// apply it twice (#13645). When the class declares a `sub` of one of
    /// the names itself, the subs go into the role's composition scope in
    /// `class` instead (`role_composition_scope`), where the composed role
    /// methods run; the class's own methods keep calling the class's (#14177).
    /// `params` are the composition's value parameters (`:$prefix`), bound in
    /// the env as well so a `sub` that reads one sees this composition's value
    /// (#15100).
    // Cost: O(s + v + p), s = the body's statements, v = `declared`, p = `params`; plus one
    // sub registration per re-declared sub.
    pub(super) fn register_role_body_subs_per_composer(
        &mut self,
        class: &str,
        class_body: &[crate::opcode::ClassBodyOp],
        role_name: &str,
        role: &RoleDef,
        declared: &HashSet<String>,
        params: &[(String, Value)],
    ) -> Result<(), RuntimeError> {
        let class_sym = Symbol::intern(class);
        let scope = crate::runtime::registration_role_composition_scope::role_composition_scope(
            class_sym,
            Symbol::intern(role_name),
        );
        let candidates: Vec<(&crate::opcode::DeferredBodyOp, Symbol)> = role
            .deferred_body
            .iter()
            .filter(|op| op.kind == crate::opcode::DeferredBodyOpKind::Plain)
            .filter_map(|op| match &op.raw {
                Stmt::SubDecl {
                    name,
                    multi: false,
                    is_export: false,
                    name_expr: None,
                    custom_traits,
                    ..
                } if custom_traits.is_empty() => Some((op, *name)),
                _ => None,
            })
            .collect();
        // When the class declares a `sub` of one of the names, the body's subs
        // all go into the composition scope, so they keep reaching each other
        // and not the class's; otherwise they go into the class's package.
        let shadowed = candidates.iter().any(|(_, name)| {
            class_body.iter().any(|op| {
                matches!(op, crate::opcode::ClassBodyOp::ClassSub { name: own, .. } if own == name)
            })
        });
        let package = if shadowed { scope } else { class_sym };
        let subs: Vec<&crate::opcode::DeferredBodyOp> = candidates
            .into_iter()
            .filter(|(_, name)| {
                !self
                    .registry()
                    .functions
                    .contains_key(&crate::qualified::qualified(package, *name))
            })
            .map(|(op, _)| op)
            .collect();
        if subs.is_empty() {
            return Ok(());
        }
        // The class's cells for the body's lexicals, bound in place of the
        // body run's plain values while the subs are declared.
        let cells: Vec<(String, Value)> = match self.lexicals.package_lexicals.get(class) {
            Some(statics) => declared
                .iter()
                .filter_map(|name| {
                    statics
                        .get(name.as_str())
                        .filter(|value| value.is_container_ref())
                        .map(|cell| (name.clone(), cell.clone()))
                })
                .collect(),
            None => Vec::new(),
        };
        // The composition's value parameters (`:$prefix`) are bound the same
        // way: the role's own registration was made without them (#15100).
        let mut cells = cells;
        cells.extend(params.iter().cloned());
        // With no body lexicals to close over, the role's own registration
        // already serves every composer that does not shadow one of its subs.
        if subs.is_empty() || (cells.is_empty() && !shadowed) {
            return Ok(());
        }
        if shadowed {
            self.registry_mut().role_composition_scopes.insert(scope);
            self.share_role_body_statics_with_scope(class, scope.as_str(), declared);
        }
        // A sub declaration also binds `&name` in the env, where the class's
        // own same-named sub is bound; that binding is put back afterwards.
        let redeclared: Vec<String> = subs
            .iter()
            .filter_map(|op| match &op.raw {
                Stmt::SubDecl { name, .. } => Some(name.to_string()),
                _ => None,
            })
            .collect();
        let amp_names: Vec<String> = if shadowed {
            subs.iter()
                .filter_map(|op| match &op.raw {
                    Stmt::SubDecl { name, .. } => Some(format!("&{name}")),
                    _ => None,
                })
                .collect()
        } else {
            Vec::new()
        };
        let saved: Vec<(String, Option<Value>)> = cells
            .iter()
            .map(|(name, _)| name)
            .chain(amp_names.iter())
            .map(|name| (name.clone(), self.env.get(name).cloned()))
            .collect();
        for (name, cell) in cells {
            self.env.insert(name, cell);
        }
        let saved_package = self.current_package().to_string();
        self.set_current_package(package.as_str().to_string());
        let saved_file = self.enter_source_file(role.decl_file.as_deref());
        let mut result = Ok(());
        for op in subs {
            result = match &op.chunk {
                Some(chunk) => self.run_compiled_block_raw(&chunk.code, &chunk.fns),
                None => self.run_block_raw(std::slice::from_ref(&op.raw)),
            }
            .map(|_| ());
            if result.is_err() {
                break;
            }
        }
        self.leave_source_file(saved_file);
        self.set_current_package(saved_package);
        for (name, prior) in saved {
            Self::restore_env_entry(&mut self.env, &name, prior);
        }
        // A parametric composition's lexical-package copy of each re-declared
        // sub knows nothing of the composition's parameters (#15100).
        //
        // TODO: a role instantiation composed at run time (`@a does R[:c(1)]`
        // composes the target `R[:c(1)]`) keeps the copy: the mixin's role
        // methods run in the role's own package (`method_body_package`), where
        // the re-declaration under the instantiation's name is not found, so
        // dropping the copy left the sub unknown (Grid). The sub then lacks the
        // composition's value parameters; the right fix is to run the mixin's
        // role methods where its re-declared subs live.
        let role_instantiation = class
            .strip_prefix(role_name)
            .is_some_and(|rest| rest.starts_with('['));
        if result.is_ok() && !params.is_empty() && !role_instantiation {
            for name in &redeclared {
                self.unseclude_role_lexical_routine(name, class, role.decl_file.as_deref());
            }
        }
        result
    }

    /// Make the role body's lexicals that `class` holds as statics (`declared`)
    /// statics of the composition scope `scope` too, sharing the same cells: a
    /// role method or sub running in the scope reads and writes the body's
    /// lexicals there, and must see what one running in `class` did.
    // Cost: O(v), v = `declared`.
    fn share_role_body_statics_with_scope(
        &mut self,
        class: &str,
        scope: &str,
        declared: &HashSet<String>,
    ) {
        let shared: Vec<(String, Value)> = match self.lexicals.package_lexicals.get(class) {
            Some(statics) => declared
                .iter()
                .filter_map(|name| {
                    statics
                        .get(name.as_str())
                        .map(|value| (name.clone(), value.clone()))
                })
                .collect(),
            None => return,
        };
        if shared.is_empty() {
            return;
        }
        let marks = crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_names)
            .entry(scope.to_string())
            .or_default();
        for (name, _) in &shared {
            marks.insert(name.clone());
        }
        let store = self
            .package_lexicals_cow_mut()
            .entry(scope.to_string())
            .or_default();
        for (name, value) in shared {
            store.insert(name, value);
        }
    }
}
