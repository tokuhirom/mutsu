//! Which declaration a parsed statement is, for declarator docs.

use super::SiteIdent;
use crate::ast::Stmt;
use crate::ast::parsed::ParsedStmt;

/// What a statement declares, if it is a documentable declaration.
pub(super) fn stmt_ident(stmt: &Stmt) -> Option<(SiteIdent, bool)> {
    let routine = |name: &crate::symbol::Symbol, callable, multi, proto| SiteIdent::Routine {
        name: name.resolve(),
        callable,
        multi,
        proto,
    };
    let package = |name: &crate::symbol::Symbol, container, is_role| SiteIdent::Package {
        name: name.resolve(),
        container,
        is_role,
    };
    let ident = match stmt {
        Stmt::SubDecl { name, multi, .. } => routine(name, None, *multi, false),
        Stmt::ProtoDecl {
            name, is_method, ..
        } => routine(name, is_method.then_some("Method"), false, true),
        Stmt::MethodDecl {
            name,
            multi,
            is_submethod,
            ..
        } => routine(
            name,
            Some(if *is_submethod { "Submethod" } else { "Method" }),
            *multi,
            false,
        ),
        Stmt::TokenDecl { name, .. } | Stmt::RuleDecl { name, .. } => SiteIdent::GrammarRule {
            name: name.resolve(),
        },
        Stmt::ClassDecl { name, .. } => package(name, true, false),
        Stmt::RoleDecl { name, .. } => package(name, true, true),
        Stmt::Package { name, is_unit, .. } => return Some((package(name, true, false), *is_unit)),
        Stmt::EnumDecl { name, .. } | Stmt::SubsetDecl { name, .. } => package(name, false, false),
        Stmt::HasDecl { name, sigil, .. } => SiteIdent::Attr {
            sigiled: format!("{sigil}!{}", name.resolve()),
        },
        Stmt::VarDecl { .. } => SiteIdent::Variable,
        // A declaration a `.AST` parse keeps as written (its header, a binding).
        Stmt::Ast(node) => {
            return match node.inner_stmt().and_then(stmt_ident) {
                Some(found) => Some(found),
                None => declares_variable(node).then_some((SiteIdent::Variable, false)),
            };
        }
        _ => return None,
    };
    let named = match &ident {
        SiteIdent::Package { name, .. }
        | SiteIdent::Routine { name, .. }
        | SiteIdent::GrammarRule { name } => !name.is_empty(),
        _ => true,
    };
    named.then_some((ident, false))
}

/// Whether `node` is a variable declaration kept in its RakuAST shape
/// (`my \x = 1`, `my ($a, $b) = ...`, `my $x will begin {...}`, ...): a
/// declarand a trailing `#=` documents, as a `Stmt::VarDecl` is.
// Cost: O(1).
fn declares_variable(node: &ParsedStmt) -> bool {
    matches!(
        node,
        ParsedStmt::BindDecl(_)
            | ParsedStmt::DoesDecl { .. }
            | ParsedStmt::WillDecl { .. }
            | ParsedStmt::DeclTail { .. }
            | ParsedStmt::DeclCompound { .. }
            | ParsedStmt::SigillessDecl(_)
            | ParsedStmt::CommaDecls(_)
            | ParsedStmt::SignatureDecl(_)
            | ParsedStmt::SignatureBind { .. }
            | ParsedStmt::MethodAssignDecl(_)
    )
}
