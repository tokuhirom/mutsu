//! The `EXPORTHOW::DECLARE` keywords a module's scan records.

use super::{DeclScan, package_type_name};
use crate::ast::{Expr, Stmt};

impl DeclScan {
    /// `EXPORTHOW::DECLARE` entries can be constants (`constant kw = SomeHOW`)
    /// or class declarations (`class DECLARE::kw is Metamodel::ClassHOW { }`).
    /// The latter is registered as a type object, so runtime HOW lookup checks
    /// the type registry as well as the env.
    pub(super) fn collect_declare_keywords(&mut self, exporthow_body: &[Stmt]) {
        for inner in exporthow_body {
            match inner {
                Stmt::ClassDecl { name, .. } => {
                    let name = name.resolve();
                    if let Some(keyword) = name.strip_prefix("SUPERSEDE::") {
                        self.out.supersede_keywords.push(keyword.to_string());
                    }
                    if let Some(keyword) = name.strip_prefix("DECLARE::") {
                        self.out
                            .declare_keywords
                            .push((keyword.to_string(), package_type_name("EXPORTHOW", &name)));
                    }
                }
                // `package SUPERSEDE { constant class = SomeHOW }` replaces the
                // HOW of a built-in declarator for the importing unit.
                Stmt::Package { name, body, .. } if name.resolve() == "SUPERSEDE" => {
                    for decl in body {
                        if let Stmt::VarDecl {
                            name,
                            custom_traits,
                            ..
                        } = decl
                            && custom_traits.iter().any(|(t, _)| t == "__constant")
                        {
                            self.out.supersede_keywords.push(name.clone());
                        }
                    }
                }
                Stmt::Package { name, body, .. } if name.resolve() == "DECLARE" => {
                    for decl in body {
                        match decl {
                            Stmt::VarDecl {
                                name,
                                expr: Expr::BareWord(how_type),
                                custom_traits,
                                ..
                            } if custom_traits.iter().any(|(t, _)| t == "__constant") => {
                                self.out
                                    .declare_keywords
                                    .push((name.clone(), how_type.clone()));
                            }
                            Stmt::ClassDecl { name, .. } if !name.resolve().contains("::") => {
                                self.out.declare_keywords.push((
                                    name.resolve(),
                                    package_type_name("EXPORTHOW::DECLARE", &name.resolve()),
                                ));
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
    }
}
