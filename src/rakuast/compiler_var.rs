//! Compiler variables (`$?PACKAGE`, `$?LINE`, `$?FILE`, `$?DISTRIBUTION`)
//! across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, a `$?` variable is a
//! `Var::Compiler::Lookup("$?NAME")`, except `$?LINE` and `$?FILE`, which the
//! parse resolves to `Var::Compiler::Line(N)` / `Var::Compiler::File("path")`,
//! and `$?DISTRIBUTION`, a bare `Var::Compiler::Distribution`. mutsu's parser
//! folds `$?LINE`, `$?TABSTOP` and (in a file) `$?FILE` into literals; a
//! `.AST` parse keeps the written name beside the literal
//! (`Spelling::CompilerVar`).

use super::convert::leaf_field;
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The node for the compiler variable the parser keeps as `Var(name)`
/// (`name` without the `$` sigil, `?PACKAGE`), or `None` for any other name.
// Cost: O(n), n = length of the name.
pub(super) fn convert_var(name: &str) -> Option<RakuAstNode> {
    let bare = name.strip_prefix('?')?;
    if bare.is_empty() || !bare.starts_with(|c: char| c.is_ascii_uppercase()) {
        return None;
    }
    Some(match bare {
        "DISTRIBUTION" => node(RakuAstClass::VarCompilerDistribution, None),
        _ => node(RakuAstClass::VarCompilerLookup, Some(Value::str(format!("${name}")))),
    })
}

/// The node for a compiler variable the parser folded into `value`, written
/// as `written` (`$?LINE`, `$?FILE`, `$?TABSTOP`).
// Cost: O(1).
pub(super) fn convert_folded(written: &str, value: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let literal = match value {
        Expr::Literal(v) => v.clone(),
        _ => return Err(super::convert::unsupported("a folded compiler variable")),
    };
    Ok(match written {
        "$?LINE" => node(RakuAstClass::VarCompilerLine, Some(literal)),
        "$?FILE" => node(RakuAstClass::VarCompilerFile, Some(literal)),
        _ => node(RakuAstClass::VarCompilerLookup, Some(Value::str(written.to_string()))),
    })
}

/// The expression a compiler-variable node lowers to: the parser's folded
/// literal for `Line` / `File` / `$?TABSTOP`, its runtime lookup otherwise.
// Cost: O(1).
pub(super) fn lower(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let leaf = || match node.fields.first().map(|f| &f.value) {
        Some(RakuAstFieldValue::Node(v)) => Ok(v.clone()),
        _ => Err(super::lower::unsupported(node)),
    };
    Ok(match node.class {
        RakuAstClass::VarCompilerLine | RakuAstClass::VarCompilerFile => {
            Expr::Literal(leaf()?)
        }
        RakuAstClass::VarCompilerDistribution => Expr::Var("?DISTRIBUTION".to_string()),
        _ => {
            let name = leaf()?;
            let ValueView::Str(name) = name.view() else {
                return Err(super::lower::unsupported(node));
            };
            match name.strip_prefix('$') {
                Some("?TABSTOP") => Expr::Literal(Value::int(8)),
                Some(bare) if bare.starts_with('?') => Expr::Var(bare.to_string()),
                _ => return Err(super::lower::unsupported(node)),
            }
        }
    })
}

// Cost: O(1).
fn node(class: RakuAstClass, value: Option<Value>) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class,
        fields: value.map(|v| leaf_field(None, v)).into_iter().collect(),
    }
}
