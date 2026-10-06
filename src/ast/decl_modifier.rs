//! A declaration carrying a conditional statement modifier,
//! `my $x = 5 if COND` / `my @a = 1, 2 unless COND`.
//!
//! The parser splits it into an unconditional declaration and a gated
//! assignment (`parser::try_split_decl_modifier`), because a declaration takes
//! effect whatever the modifier says. Rakudo has one statement with a
//! `condition-modifier`. [`modified_declaration`] goes back for the converter:
//! it accepts a block only when it has exactly the shape the split builds,
//! and answers with the statement the parser would have split.

use super::{AssignOp, Expr, Stmt};

/// The `Stmt::If` over a declaration that `stmt` is the split of, if it is one.
// Cost: O(n), n = size of the initializer (it is cloned).
pub(crate) fn modified_declaration(stmt: &Stmt) -> Option<Stmt> {
    let Stmt::SyntheticBlock(parts) = stmt else {
        return None;
    };
    let [
        Stmt::VarDecl {
            name,
            type_constraint,
            is_state: false,
            is_our,
            is_dynamic,
            is_export,
            export_tags,
            custom_traits,
            where_constraint,
            ..
        },
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var: None,
            is_statement_modifier: true,
            is_unless,
            with_kind: None,
        },
    ] = parts.as_slice()
    else {
        return None;
    };
    let (
        [
            Stmt::Assign {
                name: assigned,
                expr,
                op: AssignOp::Assign,
                target_is_sigilless: false,
            },
        ],
        true,
    ) = (then_branch.as_slice(), else_branch.is_empty())
    else {
        return None;
    };
    if assigned != name || custom_traits.iter().any(|(t, _)| t == "__has_initializer") {
        return None;
    }
    let mut custom_traits = custom_traits.clone();
    custom_traits.push(("__has_initializer".to_string(), None));
    // The split is only built for a negated condition when the modifier was
    // `unless`, and the flag says so.
    if *is_unless && !matches!(cond, Expr::Unary { .. }) {
        return None;
    }
    Some(Stmt::If {
        cond: cond.clone(),
        then_branch: vec![Stmt::VarDecl {
            name: name.clone(),
            expr: expr.clone(),
            type_constraint: type_constraint.clone(),
            is_state: false,
            is_our: *is_our,
            is_dynamic: *is_dynamic,
            is_export: *is_export,
            export_tags: export_tags.clone(),
            custom_traits,
            where_constraint: where_constraint.clone(),
        }],
        else_branch: Vec::new(),
        binding_var: None,
        is_statement_modifier: true,
        is_unless: *is_unless,
        with_kind: None,
    })
}
