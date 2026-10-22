//! The `package EXPORTHOW { ... }` entries a unit declares, kept per loaded
//! module so `CompUnit::Handle.export-how-package` can publish them (#14173).

use super::*;
use crate::ast::Stmt;

impl Interpreter {
    /// The `(directive, declarator)` pairs the unit's top-level
    /// `package EXPORTHOW` declares: `package DECLARE { constant pokemon =
    /// ... }` gives `("DECLARE", "pokemon")`, and a member spelled
    /// `DECLARE::pokemon` the same.
    // Cost: O(m), m = members of the unit's `EXPORTHOW` packages.
    pub(crate) fn collect_exporthow_declares(stmts: &[Stmt]) -> Vec<(String, String)> {
        let mut out = Vec::new();
        for stmt in stmts {
            let Stmt::Package { name, body, .. } = stmt else {
                continue;
            };
            if name.resolve() != "EXPORTHOW" {
                continue;
            }
            for member in body {
                match member {
                    Stmt::Package {
                        name: directive,
                        body: inner,
                        ..
                    } => {
                        for decl in inner {
                            if let Some(declarator) = Self::declared_member_name(decl) {
                                out.push((directive.resolve(), declarator));
                            }
                        }
                    }
                    other => {
                        if let Some(full) = Self::declared_member_name(other)
                            && let Some((directive, declarator)) =
                                crate::qualified::split_first(crate::qualified::known_symbol(&full))
                        {
                            out.push((directive.to_string(), declarator.to_string()));
                        }
                    }
                }
            }
        }
        out
    }

    fn declared_member_name(stmt: &Stmt) -> Option<String> {
        match stmt {
            Stmt::VarDecl { name, .. } => Some(name.clone()),
            Stmt::ClassDecl { name, .. } | Stmt::RoleDecl { name, .. } => Some(name.resolve()),
            _ => None,
        }
    }

    /// The `EXPORTHOW` stash of the loaded unit `unit`: one `DECLARE` /
    /// `SUPERSEDE` package per directive it declares; `None` when the unit
    /// declares none.
    // TODO: the declarators' HOW objects live in the global `EXPORTHOW`
    // package (registration_class_body_exit.rs), not in a stash owned by the
    // unit that declared them, so a directive's stash also shows other units'
    // declarators.
    // Cost: O(d), d = the unit's `EXPORTHOW` entries.
    pub(crate) fn exporthow_stash_of_unit(&self, unit: &str) -> Option<Value> {
        let declares = self.module.exporthow_declares.get(unit)?;
        let mut directives: Vec<&str> = Vec::new();
        for (directive, _) in declares {
            if !directives.contains(&directive.as_str()) {
                directives.push(directive);
            }
        }
        // Each directive is a package whose `.WHO` is its stash
        // (`$stash<DECLARE>.WHO<pokemon>`).
        let mut symbols = ValueMap::default();
        for directive in directives {
            symbols.insert(
                directive.to_string(),
                Value::package(Symbol::intern(&format!("EXPORTHOW::{directive}"))),
            );
        }
        Some(Self::make_stash_instance("EXPORTHOW", symbols))
    }
}
