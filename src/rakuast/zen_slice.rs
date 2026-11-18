//! A zen slice written with `<>` or `{}` across the RakuAST boundary.
//!
//! `@a[]`, `@a{}` and `@a<>` are one decontainerizing operation, so the parser
//! keeps one `ZenSlice` node. Rakudo keeps the brackets: `[]` is a
//! `Postcircumfix::ArrayIndex`, `{}` a `HashIndex` over an empty `SemiList`
//! (also on a non-hash target, `*{}`), and `<>` a `LiteralHashIndex` over an
//! empty word quote. A `.AST` parse wraps the node in
//! `Spelling::ZenBrackets` and this module views it back.

use super::RakuAstNode;
use super::view::{subscript_dims_node, view_expr};
use crate::ast::{Expr, IndexSpelling};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// The subscript node of the zen slice `expr` written with `bracket`
/// (`'<'` or `'{'`); `expr` is the `ZenSlice` or, for a hash, the hash itself.
// Cost: O(size of the target).
pub(super) fn view(
    names: &NameScope,
    expr: &Expr,
    bracket: char,
) -> Result<RakuAstNode, RuntimeError> {
    let target = match expr {
        Expr::ZenSlice(target) => target,
        other => other,
    };
    if bracket == '<' {
        let angle = Expr::Index {
            target: Box::new(target.clone()),
            index: Box::new(Expr::Literal(Value::str(String::new()))),
            is_positional: false,
            spelling: IndexSpelling::Angle,
        };
        return view_expr(names, &angle);
    }
    subscript_dims_node(names, target, &[], false, None, Vec::new())
}
