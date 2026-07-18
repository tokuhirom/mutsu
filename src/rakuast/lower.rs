//! RakuAST node tree → internal AST (`Stmt`/`Expr`) — the write direction that
//! backs `EVAL($rakuast)` (ADR-0011 Phase 5). The lowered AST is fed to the
//! existing compiler; there is no second execution engine.
//!
//! Slice 1 covers the literal cluster (and the `StatementList` /
//! `Statement::Expression` wrappers around it). Constructs outside that set
//! produce an explicit `RuntimeError` (the documented coverage boundary).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

fn unsupported(node: &RakuAstNode) -> RuntimeError {
    RuntimeError::new(format!(
        "RakuAST: EVAL does not yet support lowering `{}`",
        node.class.printed_name()
    ))
}

/// Lower a top-level RakuAST node to a statement list. A bare expression node
/// (e.g. `EVAL(RakuAST::IntLiteral.new(42))`) becomes a single expression
/// statement.
pub fn lower(node: &RakuAstNode) -> Result<Vec<Stmt>, RuntimeError> {
    match node.class {
        RakuAstClass::StatementList => {
            let mut stmts = Vec::with_capacity(node.fields.len());
            for f in &node.fields {
                stmts.push(lower_stmt(child_node(&f.value)?)?);
            }
            Ok(stmts)
        }
        _ => Ok(vec![lower_stmt(node)?]),
    }
}

fn lower_stmt(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match node.class {
        // A declaration wrapped in Statement::Expression lowers to its own
        // statement (a `my $x = …` is a `Stmt::VarDecl`, not a `Stmt::Expr`).
        RakuAstClass::StatementExpression => lower_stmt_inner(named_child(node, "expression")?),
        _ => lower_stmt_inner(node),
    }
}

fn lower_stmt_inner(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match node.class {
        RakuAstClass::VarDeclarationSimple => lower_var_decl(node),
        RakuAstClass::StatementIf => lower_if(node),
        RakuAstClass::StatementLoopWhile => lower_while(node),
        // `$x = EXPR` is an `ApplyInfix` whose infix is an `Assignment` node; it is
        // a `Stmt::Assign`, not a general binary expression.
        RakuAstClass::ApplyInfix if infix_is_assignment(node) => lower_assign(node),
        // The listop I/O calls (`say`/`put`/`print`/`note`) are their own
        // statements in the internal AST.
        RakuAstClass::CallName | RakuAstClass::CallNameWithoutParentheses => {
            let name = call_name_str(node)?;
            let args = arg_exprs(node)?;
            match name.as_str() {
                "say" => Ok(Stmt::Say(args)),
                "put" => Ok(Stmt::Put(args)),
                "print" => Ok(Stmt::Print(args)),
                "note" => Ok(Stmt::Note(args)),
                _ => Ok(Stmt::Expr(lower_expr(node)?)),
            }
        }
        _ => Ok(Stmt::Expr(lower_expr(node)?)),
    }
}

/// Lower `if COND { … } else { … }` to `Stmt::If`. `elsif` chains (which carry
/// an `elsifs` list) are the current coverage boundary.
fn lower_if(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    if node.fields.iter().any(|f| f.name == Some("elsifs")) {
        return Err(unsupported(node));
    }
    let cond = lower_expr(named_child(node, "condition")?)?;
    let then_branch = lower_block(named_child(node, "then")?)?;
    let else_branch = match node.fields.iter().find(|f| f.name == Some("else")) {
        Some(_) => lower_block(named_child(node, "else")?)?,
        None => Vec::new(),
    };
    Ok(Stmt::If {
        cond,
        then_branch,
        else_branch,
        binding_var: None,
    })
}

/// Lower `while COND { … }` to `Stmt::While`.
fn lower_while(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let cond = lower_expr(named_child(node, "condition")?)?;
    let body = lower_block(named_child(node, "body")?)?;
    Ok(Stmt::While {
        cond,
        body,
        label: None,
    })
}

/// Lower a `Block` (`body => Blockoid` wrapping a positional `StatementList`) to a
/// statement list.
fn lower_block(block: &RakuAstNode) -> Result<Vec<Stmt>, RuntimeError> {
    let blockoid = named_child(block, "body")?;
    lower(named_child_or_positional(blockoid)?)
}

/// Whether an `ApplyInfix`'s `infix` child is an `Assignment` node (`$x = …`).
fn infix_is_assignment(node: &RakuAstNode) -> bool {
    named_child(node, "infix")
        .map(|c| c.class == RakuAstClass::Assignment)
        .unwrap_or(false)
}

/// Lower `$x = EXPR` to `Stmt::Assign`. The `$` sigil is stripped (matching the
/// parser's naming); `@`/`%`/`&` are kept.
fn lower_assign(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let left = named_child(node, "left")?;
    if left.class != RakuAstClass::VarLexical {
        return Err(unsupported(node));
    }
    let raw = match positional_leaf(left)?.view() {
        ValueView::Str(s) => s.to_string(),
        _ => return Err(unsupported(node)),
    };
    let name = match raw.strip_prefix('$') {
        Some(bare) => bare.to_string(),
        None => raw,
    };
    let expr = lower_expr(named_child(node, "right")?)?;
    Ok(Stmt::Assign {
        name,
        expr,
        op: crate::ast::AssignOp::Assign,
    })
}

/// The identifier string of a call node's `name` (a `Name`) child.
fn call_name_str(node: &RakuAstNode) -> Result<String, RuntimeError> {
    match positional_leaf(named_child(node, "name")?)?.view() {
        ValueView::Str(s) => Ok(s.to_string()),
        _ => Err(unsupported(node)),
    }
}

/// The lowered positional arguments of a call node's `args` (`ArgList`) child, or
/// an empty vec when there are none.
fn arg_exprs(node: &RakuAstNode) -> Result<Vec<Expr>, RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some("args")) {
        Some(f) => {
            let arglist = child_node(&f.value)?;
            arglist
                .fields
                .iter()
                .map(|af| lower_expr(child_node(&af.value)?))
                .collect()
        }
        None => Ok(Vec::new()),
    }
}

/// Lower a plain `my $x = EXPR` declaration to `Stmt::VarDecl`. Scoped/typed/
/// attribute forms (which carry `scope`/`type`/`twigil`/`traits` fields) are the
/// coverage boundary.
fn lower_var_decl(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    if node.fields.iter().any(|f| {
        matches!(
            f.name,
            Some("scope") | Some("type") | Some("twigil") | Some("traits")
        )
    }) {
        return Err(unsupported(node));
    }
    let sigil = leaf_str(node, "sigil")?;
    let desigil_node = named_child(node, "desigilname")?;
    let desigil = match positional_leaf(desigil_node)?.view() {
        ValueView::Str(s) => s.to_string(),
        _ => return Err(unsupported(node)),
    };
    let name = if sigil == "$" {
        desigil
    } else {
        format!("{sigil}{desigil}")
    };
    // The initializer field is present only for `= EXPR`; without it a plain
    // `my $x` declares an undefined value.
    let (expr, has_initializer) = match node.fields.iter().find(|f| f.name == Some("initializer")) {
        Some(_) => {
            let init = named_child(node, "initializer")?;
            (lower_expr(named_child_or_positional(init)?)?, true)
        }
        None => (Expr::Literal(Value::NIL), false),
    };
    let custom_traits = if has_initializer {
        vec![("__has_initializer".to_string(), None)]
    } else {
        Vec::new()
    };
    Ok(Stmt::VarDecl {
        name,
        expr,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits,
        where_constraint: None,
    })
}

/// The value of a leaf-valued named field (e.g. `sigil => "$"`), as a `String`.
fn leaf_str(node: &RakuAstNode, name: &str) -> Result<String, RuntimeError> {
    let field = node
        .fields
        .iter()
        .find(|f| f.name == Some(name))
        .ok_or_else(|| unsupported(node))?;
    match &field.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::Str(s) => Ok(s.to_string()),
            _ => Err(unsupported(node)),
        },
        _ => Err(unsupported(node)),
    }
}

/// The child node of an `Initializer::Assign` — its single positional child.
fn named_child_or_positional(node: &RakuAstNode) -> Result<&RakuAstNode, RuntimeError> {
    match node.fields.first() {
        Some(f) if f.name.is_none() => child_node(&f.value),
        _ => Err(unsupported(node)),
    }
}

fn lower_expr(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match node.class {
        RakuAstClass::IntLiteral | RakuAstClass::RatLiteral | RakuAstClass::StrLiteral => {
            Ok(Expr::Literal(positional_leaf(node)?))
        }
        // `"..."` parses to a QuotedString wrapping StrLiteral segments; a single
        // plain segment lowers to its string literal.
        RakuAstClass::QuotedString => {
            let segments = list_field(node, "segments")?;
            if segments.len() == 1
                && let ValueView::RakuAst(seg) = segments[0].view()
                && seg.class == RakuAstClass::StrLiteral
            {
                return Ok(Expr::Literal(positional_leaf(seg)?));
            }
            Err(unsupported(node))
        }
        RakuAstClass::StatementExpression => lower_expr(named_child(node, "expression")?),
        // `$x` / `@a` / `%h` / `&f` -> the sigil-specific variable expression.
        RakuAstClass::VarLexical => {
            let name = match positional_leaf(node)?.view() {
                ValueView::Str(s) => s.to_string(),
                _ => return Err(unsupported(node)),
            };
            let (sigil, bare) = name.split_at(name.chars().next().map_or(0, char::len_utf8));
            Ok(match sigil {
                "@" => Expr::ArrayVar(bare.to_string()),
                "%" => Expr::HashVar(bare.to_string()),
                "&" => Expr::CodeVar(bare.to_string()),
                _ => Expr::Var(bare.to_string()),
            })
        }
        RakuAstClass::ApplyInfix => {
            let left = lower_expr(named_child(node, "left")?)?;
            let right = lower_expr(named_child(node, "right")?)?;
            let op = infix_token(named_child(node, "infix")?)?;
            Ok(Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            })
        }
        RakuAstClass::ApplyPrefix => {
            let operand = lower_expr(named_child(node, "operand")?)?;
            let op = infix_token(named_child(node, "prefix")?)?;
            Ok(Expr::Unary {
                op,
                expr: Box::new(operand),
            })
        }
        // A postfix method call: `$x.abs` -> ApplyPostfix(operand, Call::Method).
        // Subscripts / hyper-calls carry a different postfix and are deferred.
        RakuAstClass::ApplyPostfix => {
            let operand = lower_expr(named_child(node, "operand")?)?;
            let postfix = named_child(node, "postfix")?;
            if postfix.class != RakuAstClass::CallMethod {
                return Err(unsupported(node));
            }
            Ok(Expr::MethodCall {
                target: Box::new(operand),
                name: crate::symbol::Symbol::intern(&call_name_str(postfix)?),
                args: arg_exprs(postfix)?,
                modifier: None,
                quoted: false,
            })
        }
        _ => Err(unsupported(node)),
    }
}

/// The `TokenKind` for an `Infix`/`Prefix` operator node (its positional operator
/// string), or an error for an operator the lowerer doesn't handle yet.
fn infix_token(node: &RakuAstNode) -> Result<crate::token_kind::TokenKind, RuntimeError> {
    let name = positional_leaf(node)?;
    let ValueView::Str(s) = name.view() else {
        return Err(unsupported(node));
    };
    crate::compiler::helpers_ops::op_name_to_token_kind(&s).ok_or_else(|| unsupported(node))
}

/// The value of a node's single positional (name-less) leaf field.
fn positional_leaf(node: &RakuAstNode) -> Result<Value, RuntimeError> {
    match node.fields.first() {
        Some(f) if f.name.is_none() => match &f.value {
            RakuAstFieldValue::Node(v) => Ok(v.clone()),
            _ => Err(unsupported(node)),
        },
        _ => Err(unsupported(node)),
    }
}

/// The child RakuAST node of a named field.
fn named_child<'a>(node: &'a RakuAstNode, name: &str) -> Result<&'a RakuAstNode, RuntimeError> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::RakuAst(child) => Some(child),
                _ => None,
            },
            _ => None,
        })
        .ok_or_else(|| unsupported(node))
}

/// The child RakuAST node wrapped in a `Node` field value.
fn child_node(fv: &RakuAstFieldValue) -> Result<&RakuAstNode, RuntimeError> {
    if let RakuAstFieldValue::Node(v) = fv
        && let ValueView::RakuAst(child) = v.view()
    {
        return Ok(child);
    }
    Err(RuntimeError::new("RakuAST: EVAL expected a child node"))
}

fn list_field<'a>(node: &'a RakuAstNode, name: &str) -> Result<&'a [Value], RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some(name)) {
        Some(f) => match &f.value {
            RakuAstFieldValue::List(items) => Ok(items),
            _ => Err(unsupported(node)),
        },
        None => Err(unsupported(node)),
    }
}
