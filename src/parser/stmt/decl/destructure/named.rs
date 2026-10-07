//! Named destructuring (`my (:$a, :@b) := %h`): bind from a hash.

use super::super::super::super::parse_result::PResult;
use super::DestructureVar;
use crate::ast::{Expr, Stmt};
use crate::value::Value;

/// Parse named destructuring: bind from a hash.
pub(super) fn parse_named_destructuring(
    rest: &str,
    vars: Vec<DestructureVar>,
    rhs: Expr,
    type_constraint: Option<String>,
    is_state: bool,
) -> PResult<'_, Stmt> {
    let tmp_name = "%__destructure_tmp__".to_string();
    let hash_bare = "__destructure_tmp__".to_string();
    // The named targets read the source's named part: `.hash` is the Hash
    // itself for a Hash/Map and the named arguments of a Capture
    // (`my (:$path, :@globbers) := @open-list.shift`, IO::Glob).
    let rhs = Expr::MethodCall {
        target: Box::new(rhs),
        name: crate::symbol::Symbol::intern("hash"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
        sugar: false,
    };
    let mut stmts = vec![Stmt::VarDecl {
        name: tmp_name,
        expr: rhs,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    }];
    for dvar in &vars {
        let bare_name = if dvar.name.starts_with('@')
            || dvar.name.starts_with('%')
            || dvar.name.starts_with('&')
        {
            &dvar.name[1..]
        } else {
            &dvar.name
        };
        let index_expr = Expr::Index {
            target: Box::new(Expr::HashVar(hash_bare.clone())),
            index: Box::new(Expr::Literal(Value::str(bare_name.to_string()))),
            is_positional: false,
            spelling: Default::default(),
        };
        // A named `@`-sigil destructure target binds (`:=`) the hash value, which
        // spreads an itemized array (e.g. a `.classify` bucket `$[2,4]`) into the
        // array. mutsu's destructure lowers to assignment, so de-itemize the value
        // via `.list` for `@`-targets — a no-op for a plain list, but it unwraps a
        // single itemized array so `my (:@even) := classify(...)` yields `[2,4]`.
        //
        // When the key is ABSENT the named-array bind must yield an empty array
        // (Rakudo parameter-binding semantics), not `[Any]`: a bare `%h<absent>`
        // is `Any`, and `Any.list` is `(Any,)`, so guard with `:exists` and fall
        // back to an empty list. `my (:@paths, :@uris) := <a>.classify(...)` must
        // leave the unmatched `@uris` empty, or a downstream `%(... )` init sees a
        // stray `(Any,)` and dies with X::Hash::Store::OddNumber.
        let value_expr = if dvar.name.starts_with('@') {
            Expr::Ternary {
                cond: Box::new(Expr::Exists {
                    target: Box::new(index_expr.clone()),
                    negated: false,
                    delete: false,
                    arg: None,
                    adverb: crate::ast::ExistsAdverb::None,
                }),
                then_expr: Box::new(Expr::MethodCall {
                    target: Box::new(index_expr),
                    name: crate::symbol::Symbol::intern("list"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                    sugar: false,
                }),
                else_expr: Box::new(Expr::ArrayLiteral(Vec::new())),
            }
        } else {
            index_expr
        };
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr: value_expr,
            type_constraint: type_constraint.clone(),
            is_state,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        });
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}
