//! What the parser wraps around a package-like declaration: the `:ver<1.0>` /
//! `:auth<me>` / `:api<1>` adverbs of its name and its `is export`.
//!
//! A `class`, `grammar`, `module` or `package` that carries either parses to a
//! [`Stmt::SyntheticBlock`] of the meta setters before the declaration and
//! the `__MUTSU_EXPORT_TYPE__` registration after it; with neither it is the
//! bare declaration. Rakudo has neither statement: the adverbs are colonpairs
//! of the declaration's `name` and the export a `Trait::Is`. [`wrap`] is the
//! parser's and the RakuAST lowering's one builder, [`unwrap`] the converter's
//! recognizer.

use super::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

const SET_META: &str = "__MUTSU_SET_META__";
const EXPORT_TYPE: &str = "__MUTSU_EXPORT_TYPE__";

/// The header of a package-like declaration.
#[derive(Debug, Default)]
pub(crate) struct Header {
    /// `:ver<1.0>` and friends, as `(key, value)` in the written order.
    pub(crate) adverbs: Vec<(String, Expr)>,
    /// The tags of `is export(:tag)`, `["DEFAULT"]` for a bare `is export`;
    /// `None` when the declaration is not exported this way.
    pub(crate) export_tags: Option<Vec<String>>,
}

/// `meta setter statements, declaration, export registration`, or the bare
/// declaration when the header is empty. `name` is the registered type name.
// Cost: O(n), n = adverbs and tags.
pub(crate) fn wrap(declaration: Stmt, name: &str, header: Header) -> Stmt {
    if header.adverbs.is_empty() && header.export_tags.is_none() {
        return declaration;
    }
    let mut stmts: Vec<Stmt> = header
        .adverbs
        .into_iter()
        .map(|(key, value)| meta_setter(name, &key, value))
        .collect();
    stmts.push(declaration);
    if let Some(tags) = header.export_tags {
        stmts.push(export_registration(name, &tags));
    }
    Stmt::SyntheticBlock(stmts)
}

/// The `__MUTSU_SET_META__(type, key, value)` statement of one adverb.
// Cost: O(|name| + |key|).
pub(crate) fn meta_setter(type_name: &str, key: &str, value: Expr) -> Stmt {
    Stmt::Expr(Expr::Call {
        name: Symbol::intern(SET_META),
        args: vec![
            Expr::Literal(Value::str(type_name.to_string())),
            Expr::Literal(Value::str(key.to_string())),
            value,
        ],
    })
}

/// The `__MUTSU_EXPORT_TYPE__(type, tags...)` statement of an exported type.
// Cost: O(|name| + t), t = tags.
pub(crate) fn export_registration(type_name: &str, tags: &[String]) -> Stmt {
    let mut args = vec![Expr::Literal(Value::str(type_name.to_string()))];
    for tag in tags {
        args.push(Expr::Literal(Value::str(tag.clone())));
    }
    Stmt::Expr(Expr::Call {
        name: Symbol::intern(EXPORT_TYPE),
        args,
    })
}

fn is_declaration(stmt: &Stmt) -> bool {
    matches!(stmt, Stmt::ClassDecl { .. } | Stmt::Package { .. })
}

/// The declaration `stmt` is, or the one inside its header wrapper (the
/// wrapper is one level deep: [`wrap`] never nests).
// Cost: O(p), p = statements in the wrapper.
pub(crate) fn declaration_mut(stmt: &mut Stmt) -> Option<&mut Stmt> {
    match stmt {
        Stmt::SyntheticBlock(parts) => parts.iter_mut().find(|part| is_declaration(part)),
        other => is_declaration(other).then_some(other),
    }
}

/// The declaration inside `stmt` and the header around it, when `stmt` is the
/// parser's wrapping of a package-like declaration.
// Cost: O(n), n = size of the adverb values (they are cloned).
pub(crate) fn unwrap(stmt: &Stmt) -> Option<(&Stmt, Header)> {
    let Stmt::SyntheticBlock(parts) = stmt else {
        return None;
    };
    let at = parts.iter().position(is_declaration)?;
    let (before, rest) = parts.split_at(at);
    let (declaration, after) = rest.split_first()?;
    let (Stmt::ClassDecl { name, .. } | Stmt::Package { name, .. }) = declaration else {
        return None;
    };
    let type_name = name.resolve();
    let mut header = Header::default();
    for part in before {
        let Stmt::Expr(Expr::Call { name: call, args }) = part else {
            return None;
        };
        let (Some(Expr::Literal(t)), Some(Expr::Literal(k)), Some(value)) =
            (args.first(), args.get(1), args.get(2))
        else {
            return None;
        };
        if call.as_str() != SET_META || t.as_str()? != type_name || args.len() != 3 {
            return None;
        }
        header
            .adverbs
            .push((k.as_str()?.to_string(), value.clone()));
    }
    match after {
        [] => {}
        [Stmt::Expr(Expr::Call { name: call, args })] if call.as_str() == EXPORT_TYPE => {
            let (Expr::Literal(t), tags) = args.split_first()? else {
                return None;
            };
            if t.as_str()? != type_name {
                return None;
            }
            let mut out = Vec::new();
            for tag in tags {
                let Expr::Literal(v) = tag else { return None };
                out.push(v.as_str()?.to_string());
            }
            header.export_tags = Some(out);
        }
        _ => return None,
    }
    Some((declaration, header))
}
