//! Internal AST (`Stmt`/`Expr`) → RakuAST node tree (read direction, ADR-0011).
//!
//! Covered so far: the literal + say-call cluster (Phase 1) and variables,
//! plain `my` declarations, and infix/prefix/postfix operators (Phase 2).
//! Constructs outside that set produce an explicit `RuntimeError` (the
//! documented coverage boundary) rather than a silently-wrong node.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::compiler::helpers_ops::token_kind_to_op_name;
use crate::value::{RuntimeError, Value, ValueView};

fn unsupported(what: &str) -> RuntimeError {
    RuntimeError::new(format!(
        "RakuAST: `.AST` does not yet support this construct: {what}"
    ))
}

fn node_field(name: Option<&'static str>, node: RakuAstNode) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
    }
}

fn leaf_field(name: Option<&'static str>, value: Value) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(value),
    }
}

/// Top-level: a parsed program becomes a `RakuAST::StatementList`.
pub(super) fn statement_list(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    for stmt in stmts {
        if let Some(node) = convert_stmt(stmt)? {
            fields.push(node_field(None, node));
        }
    }
    Ok(RakuAstNode {
        class: RakuAstClass::StatementList,
        fields,
    })
}

/// Convert one statement. Returns `Ok(None)` for non-semantic bookkeeping
/// statements (e.g. `SetLine`) that carry no RakuAST representation.
fn convert_stmt(stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    match stmt {
        Stmt::SetLine(_) => Ok(None),
        Stmt::Expr(e) => Ok(Some(statement_expression(convert_expr(e)?))),
        // `say 42` / `put`/`print`/`note` as listops (no parens) parse to a
        // dedicated statement; raku models them as a call in WithoutParentheses
        // form.
        Stmt::Say(args) => Ok(Some(statement_expression(listop_call("say", args)?))),
        Stmt::Put(args) => Ok(Some(statement_expression(listop_call("put", args)?))),
        Stmt::Print(args) => Ok(Some(statement_expression(listop_call("print", args)?))),
        Stmt::Note(args) => Ok(Some(statement_expression(listop_call("note", args)?))),
        Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic,
            custom_traits,
            where_constraint,
            ..
        } => {
            // Phase 2 covers only the plain `my $x [= expr]` form; scoped/typed
            // declarations map to extra RakuAST fields not yet modelled.
            if type_constraint.is_some()
                || *is_state
                || *is_our
                || *is_dynamic
                || where_constraint.is_some()
            {
                return Err(unsupported("scoped/typed variable declaration"));
            }
            let has_initializer = custom_traits
                .iter()
                .any(|(name, _)| name == "__has_initializer");
            Ok(Some(statement_expression(var_declaration(
                name,
                expr,
                has_initializer,
            )?)))
        }
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// `my $x` / `my @a` / `my $x = EXPR` -> `VarDeclaration::Simple`. The sigil is
/// implicit (`$`) when mutsu already stripped it from the name; otherwise the
/// name carries its `@`/`%`/`&` sigil.
fn var_declaration(
    name: &str,
    init_expr: &Expr,
    has_initializer: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(desigil.to_string()))],
    };
    let mut fields = vec![
        leaf_field(Some("sigil"), Value::str(sigil.to_string())),
        node_field(Some("desigilname"), name_node),
    ];
    if has_initializer {
        let assign = RakuAstNode {
            class: RakuAstClass::InitializerAssign,
            fields: vec![node_field(None, convert_expr(init_expr)?)],
        };
        fields.push(node_field(Some("initializer"), assign));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::VarDeclarationSimple,
        fields,
    })
}

/// Split a declaration name into `(sigil, desigilname)`. mutsu keeps the sigil
/// on `@`/`%`/`&` declarations but strips it from `$` ones.
fn split_sigil(name: &str) -> (&str, &str) {
    match name.as_bytes().first() {
        Some(b'@') => ("@", &name[1..]),
        Some(b'%') => ("%", &name[1..]),
        Some(b'&') => ("&", &name[1..]),
        _ => ("$", name),
    }
}

fn statement_expression(expr: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::StatementExpression,
        fields: vec![node_field(Some("expression"), expr)],
    }
}

fn convert_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => convert_literal(v),
        Expr::Call { name, args } => Ok(call_name(name.as_str(), args, false)?),
        Expr::Var(name) => Ok(var_lexical("$", name)),
        Expr::ArrayVar(name) => Ok(var_lexical("@", name)),
        Expr::HashVar(name) => Ok(var_lexical("%", name)),
        Expr::CodeVar(name) => Ok(var_lexical("&", name)),
        Expr::Binary { left, op, right } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), convert_expr(left)?),
                node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                node_field(Some("right"), convert_expr(right)?),
            ],
        }),
        Expr::Unary { op, expr } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(Some("prefix"), operator_node(RakuAstClass::Prefix, op)),
                node_field(Some("operand"), convert_expr(expr)?),
            ],
        }),
        Expr::PostfixOp { op, expr } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPostfix,
            fields: vec![
                node_field(Some("operand"), convert_expr(expr)?),
                node_field(Some("postfix"), postfix_node(op)),
            ],
        }),
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// `$x` / `@a` / `%h` / `&f` usage -> `Var::Lexical("<sigil><name>")`.
fn var_lexical(sigil: &str, name: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::VarLexical,
        fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
    }
}

/// `Infix`/`Prefix` — a single positional operator string (e.g. `Infix.new("+")`).
fn operator_node(class: RakuAstClass, op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        class,
        fields: vec![leaf_field(None, Value::str(token_kind_to_op_name(op)))],
    }
}

/// `Postfix` — a single NAMED `operator` string (e.g. `Postfix.new(operator => "++")`).
fn postfix_node(op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Postfix,
        fields: vec![leaf_field(
            Some("operator"),
            Value::str(token_kind_to_op_name(op)),
        )],
    }
}

fn convert_literal(v: &Value) -> Result<RakuAstNode, RuntimeError> {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Ok(RakuAstNode {
            class: RakuAstClass::IntLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => Ok(RakuAstNode {
            class: RakuAstClass::RatLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Str(_) => Ok(quoted_string(v.clone())),
        other => Err(unsupported(&format!("literal {other:?}"))),
    }
}

/// A string literal renders as `QuotedString.new(segments => (StrLiteral,))`.
fn quoted_string(str_value: Value) -> RakuAstNode {
    let seg = RakuAstNode {
        class: RakuAstClass::StrLiteral,
        fields: vec![leaf_field(None, str_value)],
    };
    RakuAstNode {
        class: RakuAstClass::QuotedString,
        fields: vec![RakuAstField {
            name: Some("segments"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(seg))]),
        }],
    }
}

/// A listop `say EXPR` — `Call::Name::WithoutParentheses`.
fn listop_call(name: &'static str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    call_name(name, args, true)
}

fn call_name(
    name: &str,
    args: &[Expr],
    without_parentheses: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    let arg_list = arg_list(args)?;
    let class = if without_parentheses {
        RakuAstClass::CallNameWithoutParentheses
    } else {
        RakuAstClass::CallName
    };
    Ok(RakuAstNode {
        class,
        fields: vec![
            node_field(Some("name"), name_node),
            node_field(Some("args"), arg_list),
        ],
    })
}

fn arg_list(args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::with_capacity(args.len());
    for a in args {
        fields.push(node_field(None, convert_expr(a)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ArgList,
        fields,
    })
}
