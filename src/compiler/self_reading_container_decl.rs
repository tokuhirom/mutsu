//! A `my @a` / `my %h` whose initializer reads the container it declares
//! (#18248 shape B, ADR-12529 phase 3).
//!
//! In Raku `my @s = INIT` stores INIT into the Array the declaration put in
//! the pad on scope entry, so a closure INIT creates sees the very container
//! the store fills:
//!
//! ```raku
//! sub f { my @s = 1, { @s[$++] * 2 } ... *; @s[^5] }   # (1 2 4 8 16)
//! ```
//!
//! mutsu seeds a fresh empty container before such an initializer
//! (`DeclReset::Shadow`) but the declaration's store then replaces it with a
//! new one, so the closure held the empty seed. The read only worked because
//! a closure frame that missed a name walked on into its callers and found the
//! declaring frame's slot there, which is the dynamic-scope leak ADR-12529
//! retires.
//!
//! A reassignment `@s = INIT` already stores into the existing container. So
//! such a declaration compiles as the declaration alone followed by that
//! assignment: the closure reads the declared container as an upvalue and the
//! store fills it in place. Only a plain declaration qualifies (no type,
//! trait, `where`, `:=`, `state`/`our`/dynamic, export or other mark), whose
//! value is the same either way.

use super::Compiler;
use crate::ast::{AssignOp, Expr, HashSpelling, Stmt};
use crate::value::Value;

impl Compiler {
    /// `my @a = INIT` / `my %h = INIT` split into `my @a` and `@a = INIT`
    /// when INIT reads the declared container (see the module docs).
    // Cost: O(1) to decide; the clone of INIT is O(n), n = its AST nodes.
    pub(super) fn split_self_reading_container_decl(stmt: &Stmt) -> Option<(Stmt, Stmt)> {
        let Stmt::VarDecl {
            marks,
            name,
            expr,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            custom_traits,
            has_initializer: true,
            constant: None,
            bind: None,
            where_constraint: None,
            anonymous: false,
            ..
        } = stmt
        else {
            return None;
        };
        if !marks.init_sees_self || !custom_traits.is_empty() {
            return None;
        }
        let mut plain = marks.clone();
        plain.init_sees_self = false;
        if !plain.is_plain() {
            return None;
        }
        let empty = match name.chars().next() {
            Some('@') if !name.starts_with("@*") => Expr::Literal(Value::real_array(Vec::new())),
            Some('%') if !name.starts_with("%*") => Expr::Hash(Vec::new(), HashSpelling::Composer),
            _ => return None,
        };
        let mut decl = stmt.clone();
        if let Stmt::VarDecl {
            marks,
            expr,
            has_initializer,
            ..
        } = &mut decl
        {
            marks.init_sees_self = false;
            *has_initializer = false;
            *expr = empty;
        }
        let assign = Stmt::Assign {
            name: name.clone(),
            expr: expr.clone(),
            op: AssignOp::Assign,
            target_is_sigilless: false,
        };
        Some((decl, assign))
    }
}
