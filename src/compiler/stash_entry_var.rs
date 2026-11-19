//! A package stash's sigiled entry written through a subscript below it
//! (#18204).
//!
//! `GLOBAL::<%Z>` is the package variable `%GLOBAL::Z`, and `Foo::<@l>` is
//! `@Foo::l`. An element write below such an entry (`GLOBAL::<%Z><a> = 5`,
//! `GLOBAL::<@L>[2]++`) compiled the entry as a stash read, which answers a
//! fresh value when the variable does not exist yet, so the write vivified a
//! container nobody stored. Written as the qualified variable it names, the
//! chain takes the ordinary package-variable paths, which create the
//! variable on first write as `%GLOBAL::Z<a> = 5` does.
//!
//! Only a package stash qualifies: a pseudo-package (`MY::`, `OUTER::`,
//! `CALLER::`, `PROCESS::`, ...) names lexical or dynamic scopes, not a
//! package's variables, and keeps its own lowering. `GLOBAL::` is the root
//! package itself.

use crate::ast::Expr;

/// The qualified variable a package stash's literal entry names
/// (`GLOBAL::` + `%Z` -> `%GLOBAL::Z`), or `None` when `stash` is a
/// pseudo-package or `key` is not a plain sigiled name.
// Cost: O(|stash| + |key|).
fn package_stash_entry_var(stash: &str, key: &str) -> Option<Expr> {
    let package = stash.strip_suffix("::")?;
    let mut segments = package.split("::");
    let first = segments.next()?;
    if first != "GLOBAL" && crate::parser::is_pseudo_package(first) {
        return None;
    }
    let identifier = |s: &str| {
        s.chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    };
    if !identifier(first) || !segments.all(identifier) {
        return None;
    }
    let mut chars = key.chars();
    let sigil = chars.next()?;
    let name = chars.as_str();
    if !identifier(name) || name.contains("::") {
        return None;
    }
    let qualified = format!("{package}::{name}");
    match sigil {
        '$' => Some(Expr::Var(qualified)),
        '@' => Some(Expr::ArrayVar(qualified)),
        '%' => Some(Expr::HashVar(qualified)),
        _ => None,
    }
}

/// `expr`, an element chain (subscripts, parentheses) whose innermost link
/// is a package stash's literal entry, with that entry replaced by the
/// qualified variable it names; `None` for any other chain.
// Cost: O(d + |stash| + |key|), d = subscript and paren depth.
pub(super) fn chain_with_stash_entry_var(expr: &Expr) -> Option<Expr> {
    // The subscripts from the outermost down, and the link below them.
    let mut levels = Vec::new();
    let mut cur = expr;
    let var = loop {
        match cur {
            Expr::Index {
                target,
                index,
                is_positional,
                spelling,
            } => {
                if let Expr::PseudoStash(stash) = target.as_ref() {
                    let Expr::Literal(lit) = index.as_ref() else {
                        return None;
                    };
                    break package_stash_entry_var(stash, lit.as_str()?)?;
                }
                levels.push((index, *is_positional, *spelling));
                cur = target;
            }
            Expr::Grouped(inner) => cur = inner,
            _ => return None,
        }
    };
    Some(
        levels
            .into_iter()
            .rev()
            .fold(var, |target, (index, is_positional, spelling)| {
                Expr::Index {
                    target: Box::new(target),
                    index: index.clone(),
                    is_positional,
                    spelling,
                }
            }),
    )
}
