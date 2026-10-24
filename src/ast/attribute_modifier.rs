//! The runtime-only part of a postfix modifier on a class attribute.
//!
//! Attributes are installed while the class is composed. The modifier still
//! evaluates its condition, but cannot gate the declaration. The parser keeps
//! the written form (`ParsedStmt::AttributeModifier`); [`expand`] is its one
//! lowering, which `rakuast::lower` calls.

use super::decl_modifier::DeclarationModifier;
use super::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;

/// Build the class-body statements for an attribute and its postfix modifier.
// Cost: O(n), n = size of the condition.
pub(crate) fn expand(declaration: Stmt, condition: Expr, kind: DeclarationModifier) -> Stmt {
    let modifier = match kind {
        DeclarationModifier::If | DeclarationModifier::Unless => Stmt::If {
            cond: if matches!(kind, DeclarationModifier::Unless) {
                Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(condition.clone()),
                    word: false,
                }
            } else {
                condition.clone()
            },
            then_branch: vec![Stmt::SyntheticBlock(Vec::new())],
            else_branch: Vec::new(),
            binding_var: None,
            is_statement_modifier: true,
            is_unless: matches!(kind, DeclarationModifier::Unless),
            with_kind: None,
        },
        DeclarationModifier::When => Stmt::Given {
            topic: Expr::Var("_".to_string()),
            body: vec![Stmt::When {
                cond: condition.clone(),
                body: vec![Stmt::SyntheticBlock(Vec::new())],
                is_statement_modifier: true,
            }],
            is_statement_modifier: true,
            with_kind: None,
        },
        DeclarationModifier::With | DeclarationModifier::Without => {
            let is_without = matches!(kind, DeclarationModifier::Without);
            let topic = condition.clone();
            let defined = Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name: Symbol::intern("defined"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: false,
            };
            let condition = if is_without {
                Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(defined),
                    word: false,
                }
            } else {
                defined
            };
            Stmt::Given {
                topic,
                body: vec![Stmt::If {
                    cond: condition,
                    then_branch: vec![Stmt::SyntheticBlock(Vec::new())],
                    else_branch: Vec::new(),
                    binding_var: None,
                    is_statement_modifier: true,
                    is_unless: false,
                    with_kind: None,
                }],
                is_statement_modifier: true,
                with_kind: Some(if is_without {
                    crate::ast::GivenWithKind::Without
                } else {
                    crate::ast::GivenWithKind::With
                }),
            }
        }
    };
    Stmt::SyntheticBlock(vec![declaration, modifier])
}
