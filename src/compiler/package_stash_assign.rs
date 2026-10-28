//! Compile literal and keyed writes through a named package's stash.

use super::*;
use crate::symbol::Symbol;

impl Compiler {
    pub(super) fn try_compile_named_package_stash_assign(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
    ) -> bool {
        let Expr::PseudoStash(stash_name) = target else {
            return false;
        };
        let named = stash_name.strip_suffix("::").is_some_and(|pkg| {
            !pkg.is_empty()
                && (pkg == "GLOBAL" || !pkg.split("::").any(crate::parser::is_pseudo_package))
        });
        if !named {
            return false;
        }
        // `Pkg::<$v> {=,:=} value` is the stash spelling of `$Pkg::v {=,:=}
        // value`: route it through the qualified-variable assignment, which
        // writes through the variable's container (and so checks its declared
        // constraint, ADR-0042) instead of replacing the stash entry. Only a
        // `$` key: an `@`/`%` stash entry is the Array/Hash itself, not a
        // Scalar, and rakudo refuses `Pkg::<@a> = ...` as an assignment to an
        // immutable value.
        if let Expr::Literal(lit) = index
            && let Some(key) = lit.as_str()
            && !key.starts_with('&')
        {
            // A literal bare-symbol bind addresses the package's symbol slot.
            // Keep the shared qualified binding machinery (including export
            // metadata and the RHS cell), rather than binding a stash snapshot.
            if !key.starts_with(['$', '@', '%', '&'])
                && !crate::qualified::is_qualified_str(key)
                && key.starts_with(|c: char| c.is_alphabetic() || c == '_')
                && let Expr::IndexBindValue(rhs) = value
            {
                self.compile_expr(&Expr::AssignExpr {
                    name: format!("{stash_name}{key}"),
                    expr: rhs.clone(),
                    is_bind: true,
                });
                return true;
            }
            // `Pkg::<@a> = ...` / `Pkg::<%h> = ...` (assignment, not bind).
            if key.starts_with(['@', '%'])
                && key.len() > 1
                && !matches!(value, Expr::IndexBindValue(_))
            {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("die"),
                    args: vec![Expr::Literal(Value::str(
                        "Cannot assign to an immutable value".to_string(),
                    ))],
                    listop: false,
                });
                return true;
            }
            // `Pkg::<@a> := v` / `Pkg::<%h> := v`: the stash spelling of
            // `@Pkg::a := v`, which rebinds the package variable (#10546).
            if let Expr::IndexBindValue(rhs) = value
                && let Some(sigil) = key.chars().next().filter(|c| matches!(c, '@' | '%'))
                && key[1..].starts_with(|c: char| c.is_alphabetic() || c == '_')
            {
                self.compile_expr(&Expr::AssignExpr {
                    name: format!("{sigil}{stash_name}{}", &key[1..]),
                    expr: rhs.clone(),
                    is_bind: true,
                });
                return true;
            }
            let Some(bare) = key
                .strip_prefix('$')
                .filter(|bare| bare.starts_with(|c: char| c.is_alphabetic() || c == '_'))
            else {
                return false;
            };
            let (is_bind, rhs) = match value {
                Expr::IndexBindValue(rhs) => (true, rhs.as_ref().clone()),
                other => (false, other.clone()),
            };
            // `GLOBAL::<$x> = v` (plain assign): the statement form's store, so
            // a never-declared `$GLOBAL::x` lands where the GLOBAL stash reads
            // it back (the expression-form `AssignExpr` op does not register it).
            if !is_bind && stash_name == "GLOBAL::" {
                self.with_escape(true, |c| c.compile_expr(&rhs));
                let qualified = format!("GLOBAL::{bare}");
                self.emit_set_named_var(&qualified);
                let name_idx = self
                    .code
                    .add_constant(Value::str(self.qualify_variable_name(&qualified)));
                self.code.emit(OpCode::GetGlobal(name_idx));
                return true;
            }
            // `$Pkg::v` compiles to AssignExpr name "Pkg::v" (sigil dropped).
            self.compile_expr(&Expr::AssignExpr {
                name: format!("{stash_name}{bare}"),
                expr: Box::new(rhs),
                is_bind,
            });
            return true;
        }
        self.compile_bind_index_value(value);
        self.compile_expr(index);
        let stash_name_idx = self.code.add_constant(Value::str(stash_name.clone()));
        self.code
            .emit(OpCode::IndexAssignPseudoStashKeyed { stash_name_idx });
        true
    }
}
