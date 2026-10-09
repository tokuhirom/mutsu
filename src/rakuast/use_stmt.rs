//! `use` / `no` statements across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, a `use` statement is one of three nodes:
//!
//! - `RakuAST::Pragma` for a core pragma (`use strict`, `use lib "lib"`,
//!   `use variables :D`), with `off => True` for its `no` form;
//! - `RakuAST::Statement::LanguageVersion` for `use v6.d`;
//! - `RakuAST::Statement::Use` for every other module, including
//!   `experimental` and `newline`.
//!
//! The parser keeps import tags (`:ALL`) apart from any other `use` argument,
//! while raku has a single `argument` expression: one `ColonPair::True` per
//! tag, a comma list for several. Both directions translate between the two.

use super::convert::{convert_expr, leaf_field, name_from_identifier, node_field, plain_infix};
use super::lower::{leaf_str, list_field, lower_expr, named_child, positional_leaf};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The core pragmas that rakudo represents as `RakuAST::Pragma`.
pub(super) fn is_pragma_name(name: &str) -> bool {
    matches!(
        name,
        "strict"
            | "fatal"
            | "nqp"
            | "soft"
            | "MONKEY"
            | "MONKEY-GUTS"
            | "MONKEY-TYPING"
            | "MONKEY-SEE-NO-EVAL"
            | "dynamic-scope"
            | "isms"
            | "precompilation"
            | "worries"
            | "trace"
            | "internals"
            | "lib"
    )
}

/// `use MODULE [ARG | :TAG...]` -> `Pragma`, `Statement::LanguageVersion` or
/// `Statement::Use`.
pub(super) fn convert_use(
    module: &str,
    arg: Option<&Expr>,
    tags: &[String],
) -> Result<RakuAstNode, RuntimeError> {
    // The parser keeps `use v6.d`'s version text as the argument of a `v6`
    // pseudo-module.
    if module == "v6" && tags.is_empty() {
        if let Some(Expr::Literal(version)) = arg
            && let ValueView::Str(text) = version.view()
        {
            return Ok(RakuAstNode {
                class: RakuAstClass::StatementLanguageVersion,
                fields: vec![leaf_field(None, Value::version_from_str(&text))],
            });
        }
        return Err(super::convert::unsupported("language version argument"));
    }
    // The smiley pragmas (`use variables :D`) are parsed into a `":D"` string
    // argument rather than a colonpair; rakudo's node is
    // `Pragma(argument => ColonPair::True("D"))`. Refuse until the parser keeps
    // the colonpair.
    if matches!(
        module,
        "variables" | "attributes" | "parameters" | "invocant"
    ) {
        return Err(super::convert::unsupported("smiley pragma"));
    }
    let argument = use_argument(arg, tags)?;
    let mut node = if is_pragma_name(module) {
        pragma_node(module)
    } else {
        RakuAstNode {
            class: RakuAstClass::StatementUse,
            fields: vec![node_field(
                Some("module-name"),
                name_from_identifier(module),
            )],
        }
    };
    if let Some(argument) = argument {
        node.fields.push(node_field(Some("argument"), argument));
    }
    Ok(node)
}

/// `need MODULE;` -> `Statement::Need(module-names => (Name,))`.
pub(super) fn convert_need(module: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::StatementNeed,
        fields: vec![RakuAstField {
            name: Some("module-names"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(name_from_identifier(
                module,
            )))]),
        }],
    }
}

/// A statically named `require` keeps its package target through the round trip.
// Cost: O(1).
pub(super) fn convert_require(args: &[Expr]) -> Option<RakuAstNode> {
    let [Expr::Literal(target)] = args else {
        return None;
    };
    let ValueView::Package(module) = target.view() else {
        return None;
    };
    Some(RakuAstNode {
        class: RakuAstClass::StatementRequire,
        fields: vec![node_field(
            Some("module-name"),
            name_from_identifier(&module.resolve()),
        )],
    })
}

/// Lower `Statement::Require` to the parser's package-valued call operand.
// Cost: O(1).
pub(super) fn lower_require(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    if node
        .fields
        .iter()
        .any(|field| matches!(field.name, Some("file" | "argument")))
    {
        return Err(super::lower::unsupported(node));
    }
    let module = match name_parts::name_shape(named_child(node, "module-name")?) {
        Some(NameShape::Identifier(name)) => name,
        _ => return Err(super::lower::unsupported(node)),
    };
    Ok(Expr::Call {
        name: Symbol::intern("require"),
        args: vec![Expr::Literal(Value::package(Symbol::intern(&module)))],
        listop: false,
    })
}

/// `import MODULE [:TAG...];` -> `Statement::Import`, its tags as the `use`
/// statement's `argument`.
pub(super) fn convert_import(module: &str, tags: &[String]) -> RakuAstNode {
    let mut node = RakuAstNode {
        class: RakuAstClass::StatementImport,
        fields: vec![node_field(
            Some("module-name"),
            name_from_identifier(module),
        )],
    };
    if let Some(argument) = tag_argument(tags) {
        node.fields.push(node_field(Some("argument"), argument));
    }
    node
}

/// `no PRAGMA` -> `Pragma(off => True, name => PRAGMA)`.
pub(super) fn convert_no(module: &str) -> RakuAstNode {
    let mut node = pragma_node(module);
    node.fields.insert(0, leaf_field(Some("off"), Value::TRUE));
    node
}

fn pragma_node(name: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Pragma,
        fields: vec![leaf_field(Some("name"), Value::str(name.to_string()))],
    }
}

/// The `argument` of a `use` statement. Both an argument and tags at once
/// (`use Foo :tag<x>`) is a shape the parser does not keep distinct.
fn use_argument(arg: Option<&Expr>, tags: &[String]) -> Result<Option<RakuAstNode>, RuntimeError> {
    match (arg, tags) {
        (None, []) => Ok(None),
        (Some(arg), []) => Ok(Some(convert_expr(arg)?)),
        (None, tags) => Ok(tag_argument(tags)),
        (Some(_), _) => Err(super::convert::unsupported(
            "use with both an argument and import tags",
        )),
    }
}

/// Import tags as one `argument`: a `ColonPair::True` for one tag, a comma
/// list of them for several, nothing for none.
fn tag_argument(tags: &[String]) -> Option<RakuAstNode> {
    match tags {
        [] => None,
        [tag] => Some(colonpair_true(tag)),
        tags => Some(RakuAstNode {
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(Some("infix"), plain_infix(",")),
                RakuAstField {
                    name: Some("operands"),
                    value: RakuAstFieldValue::List(
                        tags.iter()
                            .map(|tag| Value::rakuast(Box::new(colonpair_true(tag))))
                            .collect(),
                    ),
                },
            ],
        }),
    }
}

fn colonpair_true(tag: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::ColonPairTrue,
        fields: vec![leaf_field(None, Value::str(tag.to_string()))],
    }
}

/// `RakuAST::Pragma` -> `Stmt::Use`, or `Stmt::No` when it is switched off.
pub(super) fn lower_pragma(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let module = leaf_str(node, "name")?;
    let off = node.fields.iter().any(|f| {
        f.name == Some("off") && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
    });
    let (arg, tags) = lower_argument(node)?;
    if off {
        if arg.is_some() || !tags.is_empty() {
            return Err(super::lower::unsupported(node));
        }
        return Ok(Stmt::No { module, arg: None });
    }
    Ok(Stmt::Use {
        module,
        arg,
        tags,
        condition: None,
        if_imports: Vec::new(),
    })
}

/// `RakuAST::Statement::Use` -> `Stmt::Use`.
pub(super) fn lower_use(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let module = match name_parts::name_shape(named_child(node, "module-name")?) {
        Some(NameShape::Identifier(name)) => name,
        _ => return Err(super::lower::unsupported(node)),
    };
    let (mut arg, mut tags) = lower_argument(node)?;
    // `use newline :crlf` keeps its pair as the argument, where any other
    // module's `:tag` is an import tag.
    if module == "newline"
        && arg.is_none()
        && let [tag] = tags.as_slice()
    {
        arg = Some(Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(tag.clone()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::TRUE)),
            form: crate::ast::BinaryForm::ColonPairTrue,
        });
        tags.clear();
    }
    Ok(Stmt::Use {
        module,
        arg,
        tags,
        condition: None,
        if_imports: Vec::new(),
    })
}

/// `RakuAST::Statement::Need` -> `Stmt::Need`. Only the one-module form the
/// converter renders; several names or a version colonpair stay the boundary.
pub(super) fn lower_need(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let names = list_field(node, "module-names")?;
    let [only] = names else {
        return Err(super::lower::unsupported(node));
    };
    let ValueView::RakuAst(name) = only.view() else {
        return Err(super::lower::unsupported(node));
    };
    match name_parts::name_shape(name) {
        Some(NameShape::Identifier(module)) => Ok(Stmt::Need { module }),
        _ => Err(super::lower::unsupported(node)),
    }
}

/// `RakuAST::Statement::Import` -> `Stmt::Import`.
pub(super) fn lower_import(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let module = match name_parts::name_shape(named_child(node, "module-name")?) {
        Some(NameShape::Identifier(name)) => name,
        _ => return Err(super::lower::unsupported(node)),
    };
    let (arg, tags) = lower_argument(node)?;
    if arg.is_some() {
        return Err(super::lower::unsupported(node));
    }
    Ok(Stmt::Import { module, tags })
}

/// `RakuAST::Statement::LanguageVersion` -> the parser's `use v6` form.
pub(super) fn lower_language_version(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let version = positional_leaf(node)?;
    if !matches!(version.view(), ValueView::Version { .. }) {
        return Err(super::lower::unsupported(node));
    }
    Ok(Stmt::Use {
        module: "v6".to_string(),
        arg: Some(Expr::Literal(Value::str(version.to_string_value()))),
        tags: Vec::new(),
        condition: None,
        if_imports: Vec::new(),
    })
}

/// Split a `use` statement's `argument` back into the parser's argument and
/// import tags: a `ColonPair::True`, or a comma list made only of them, is a
/// tag list; anything else is the argument.
fn lower_argument(node: &RakuAstNode) -> Result<(Option<Expr>, Vec<String>), RuntimeError> {
    let Ok(argument) = named_child(node, "argument") else {
        return Ok((None, Vec::new()));
    };
    if let Some(tag) = colonpair_true_key(argument) {
        return Ok((None, vec![tag]));
    }
    if argument.class == RakuAstClass::ApplyListInfix
        && let Ok(operands) = list_field(argument, "operands")
        && !operands.is_empty()
    {
        let tags = operands
            .iter()
            .map(|operand| match operand.view() {
                ValueView::RakuAst(operand) => colonpair_true_key(operand),
                _ => None,
            })
            .collect::<Option<Vec<_>>>();
        if let Some(tags) = tags {
            return Ok((None, tags));
        }
    }
    Ok((Some(lower_expr(argument)?), Vec::new()))
}

fn colonpair_true_key(node: &RakuAstNode) -> Option<String> {
    if node.class != RakuAstClass::ColonPairTrue {
        return None;
    }
    match positional_leaf(node).ok()?.view() {
        ValueView::Str(key) => Some(key.to_string()),
        _ => None,
    }
}
