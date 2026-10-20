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
    /// Declare `role`'s plain body `sub`s again in `class`'s package, closed
    /// over `class`'s statics for the names in `declared` (the body's
    /// lexicals). Only a plain, non-exported `sub` without custom traits is
    /// re-declared: a `multi` or exported one belongs to the role (it is
    /// registered once, at declaration), and running a trait again would
    /// apply it twice (#13645). A name the class declares a `sub` of itself
    /// keeps the class's; the composed methods then call that one, as before.
    // TODO: a role method should call the role body's `sub` even when the
    // composing class declares its own of the same name (Rakudo keeps the two
    // lexical scopes apart); that needs a per-method lexical scope for the
    // role body, which the package-keyed registration cannot express.
    // Cost: O(s + v), s = the body's statements, v = `declared`; plus one
    // sub registration per re-declared sub.
    pub(super) fn register_role_body_subs_per_composer(
        &mut self,
        class: &str,
        class_body: &[crate::opcode::ClassBodyOp],
        role: &RoleDef,
        declared: &HashSet<String>,
    ) -> Result<(), RuntimeError> {
        let subs: Vec<&crate::opcode::DeferredBodyOp> = role
            .deferred_body
            .iter()
            .filter(|op| {
                op.kind == crate::opcode::DeferredBodyOpKind::Plain
                    && match &op.raw {
                        Stmt::SubDecl {
                            name,
                            multi: false,
                            is_export: false,
                            name_expr: None,
                            custom_traits,
                            ..
                        } if custom_traits.is_empty() => {
                            let key =
                                crate::qualified::qualified(Symbol::intern(class), *name);
                            !self.registry().functions.contains_key(&key)
                                && !class_body.iter().any(|op| {
                                    matches!(op, crate::opcode::ClassBodyOp::ClassSub {
                                        name: own, ..
                                    } if own == name)
                                })
                        }
                        _ => false,
                    }
            })
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
        if cells.is_empty() {
            return Ok(());
        }
        let saved: Vec<(String, Option<Value>)> = cells
            .iter()
            .map(|(name, _)| (name.clone(), self.env.get(name).cloned()))
            .collect();
        for (name, cell) in cells {
            self.env.insert(name, cell);
        }
        let saved_package = self.current_package().to_string();
        self.set_current_package(class.to_string());
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
        result
    }
}
