//! Which routines of an inner scope a lifted BEGIN can declare again, and the
//! declaration it repeats (ADR-0134, slice 2; [`super::routines`]).

use super::token_reads::TokenReads;
use crate::ast::Stmt;

/// A routine a lifted BEGIN can declare again: the name a call spells, whether
/// it is one candidate of a multi (or its proto), and the declaration to
/// repeat. `None` for a redeclaring one, a method that is not lexical, one whose name
/// is computed from anything but a string literal (`sub ::("h")` names `h` as
/// plainly as `sub h` does), and one whose name has a colon without being an operator or
/// other category routine (`infix:<plus>`). Code uses such a routine through
/// the syntax the parser registered for it (`1 plus 2`), which a scan of the
/// called names need not see, so every one of the enclosing scopes is copied
/// ([`super::Walker::add_category_routines`]).
///
/// The repeat is lexical only. An `our sub` would otherwise install into its
/// package ahead of the scope, and an exported one would be exported a second
/// time; the body reaches the copy by its bare name either way, and the
/// in-place declaration still does the installing.
// Cost: O(t), t = traits on the declaration.
pub(super) fn copyable_routine(decl: &Stmt) -> Option<(String, bool, Stmt)> {
    match decl {
        Stmt::SubDecl {
            name,
            name_expr,
            supersede: false,
            multi,
            scope,
            custom_traits,
            ..
        } => {
            let name = name.resolve();
            let internal = *scope == crate::ast::SubScope::Anon
                || custom_traits
                    .iter()
                    .any(|(t, _)| t.starts_with("__") && t != OP_PREC);
            if internal
                || (name.contains(':') && !is_category_routine(&name))
                || name_expr.as_ref().is_some_and(|e| !is_literal_name(e))
            {
                return None;
            }
            let mut copy = decl.clone();
            if let Stmt::SubDecl {
                is_export,
                export_tags,
                scope,
                ..
            } = &mut copy
            {
                *is_export = false;
                export_tags.clear();
                *scope = crate::ast::SubScope::My;
            }
            Some((name, *multi, copy))
        }
        // A lexical method outside a package (`my method m { }`), reached as
        // `$x.&m`, is a lexical routine like a `my sub`. A has-scoped one
        // there belongs to no package, so it is left alone.
        Stmt::MethodDecl {
            name,
            name_expr: None,
            multi,
            is_my: true,
            is_our: false,
            is_private: false,
            is_export: false,
            custom_traits,
            ..
        } if custom_traits.is_empty() => {
            let name = name.resolve();
            (!name.contains(':')).then(|| (name, *multi, decl.clone()))
        }
        // A proto's own traits dispatch to a user `trait_mod:<is>`, which would
        // run again. A method-level one outside a package (`my proto method`)
        // declares the same lexical proto as a sub-level one.
        Stmt::ProtoDecl {
            name,
            custom_traits,
            ..
        } if custom_traits.is_empty() => {
            let name = name.resolve();
            if name.contains(':') {
                return None;
            }
            let mut copy = decl.clone();
            if let Stmt::ProtoDecl {
                is_export,
                export_tags,
                is_our,
                ..
            } = &mut copy
            {
                *is_export = false;
                export_tags.clear();
                *is_our = false;
            }
            Some((name, true, copy))
        }
        _ => None,
    }
}

/// An operator's precedence and associativity, which the copy registers again.
const OP_PREC: &str = "__prec";

/// Whether `name` names an operator or other category routine
/// (`infix:<plus>`, `term:<now>`, `trait_mod:<is>`).
// Cost: O(n), n = the length of `name`.
fn is_category_routine(name: &str) -> bool {
    name.split_once(':').is_some_and(|(category, rest)| {
        !category.is_empty()
            && category.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
            && (rest.starts_with('<') || rest.starts_with('«') || rest.starts_with('['))
    })
}

/// A `token`, `rule` or `regex` a lifted BEGIN can declare again, with what
/// its pattern reads, or `None`. Code uses one through a regex (`/<t>/`),
/// which a scan of the called names need not see, so every one of the
/// enclosing scopes is copied, as operators are. A code block or an
/// interpolation in the pattern reads the scope's lexicals, which a scan of
/// the compiled declaration does not see, so they come from the pattern's
/// regex tree ([`TokenReads`]); a pattern without a static tree is refused.
/// The copy is lexical only, like a routine's ([`copyable_routine`]).
// Cost: O(p), p = the length of the pattern.
pub(super) fn copyable_token(decl: &Stmt) -> Option<(String, Stmt, TokenReads)> {
    let (Stmt::TokenDecl {
        name,
        params,
        body,
        multi: false,
        ..
    }
    | Stmt::RuleDecl {
        name,
        params,
        body,
        multi: false,
        ..
    }) = decl
    else {
        return None;
    };
    let [Stmt::Expr(crate::ast::Expr::Literal(literal))] = body.as_slice() else {
        return None;
    };
    let crate::value::ValueView::Regex(pattern) = literal.view() else {
        return None;
    };
    if !params.is_empty() {
        return None;
    }
    let reads = TokenReads::of(&pattern)?;
    let mut copy = decl.clone();
    match &mut copy {
        Stmt::TokenDecl {
            is_export,
            export_tags,
            is_our,
            ..
        } => {
            *is_export = false;
            export_tags.clear();
            *is_our = false;
        }
        Stmt::RuleDecl {
            is_export,
            export_tags,
            ..
        } => {
            *is_export = false;
            export_tags.clear();
        }
        _ => {}
    }
    Some((name.resolve(), copy, reads))
}

/// Whether a computed routine name (`sub ::(...)`) is a string literal, which
/// the parser already took as the routine's name.
fn is_literal_name(expr: &crate::ast::Expr) -> bool {
    matches!(expr, crate::ast::Expr::Literal(v) if matches!(v.view(), crate::value::ValueView::Str(_)))
}
