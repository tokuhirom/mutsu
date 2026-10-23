//! Placeholder variables (`$^a`, `@^a`, `%^h`, `&^cb`, `$:foo`) as RakuAST nodes.
//!
//! The execution AST keeps a placeholder as a caret- or colon-prefixed lexical
//! name (`Var("^a")`, `ArrayVar("^a")`, `Var(":foo")`) and a block or routine
//! carries the matching implicit parameters. Rakudo's node is a declaration
//! written with the variable's sigil and bare name (`"$a"`, `"@a"`, `"$foo"`):
//! every positional kind is a `VarDeclaration::Placeholder::Positional`, the
//! twigil `:` a `Placeholder::Named`. Measured on rakudo 2026.09.

use super::convert::leaf_field;
use super::lower::{positional_leaf, unsupported};
use super::{RakuAstClass, RakuAstNode};
pub(super) use crate::ast::placeholders::is_placeholder_name;
use crate::value::{RuntimeError, Value, ValueView};

/// The node for a placeholder variable written `sigil` + twigil + `name`.
// Cost: O(k), k = length of the name.
pub(super) fn placeholder_node(class: RakuAstClass, sigil: &str, name: &str) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class,
        fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
    }
}

/// A placeholder node's sigil and bare name: `"@a"` is `('@', "a")`.
// Cost: O(k), k = length of the name.
pub(super) fn spelling(node: &RakuAstNode) -> Result<(char, String), RuntimeError> {
    let name = positional_leaf(node)?;
    let ValueView::Str(name) = name.view() else {
        return Err(unsupported(node));
    };
    let mut chars = name.chars();
    let Some(sigil) = chars.next() else {
        return Err(unsupported(node));
    };
    let bare = chars.as_str();
    if !is_placeholder_name(bare) {
        return Err(unsupported(node));
    }
    Ok((sigil, bare.to_string()))
}
