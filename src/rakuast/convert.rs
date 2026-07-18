//! Internal AST (`Stmt`/`Expr`) → RakuAST node tree (read direction, ADR-0011).
//!
//! Covered so far: the literal + say-call cluster (Phase 1); and variables,
//! plain `my` declarations, infix/prefix/postfix operators, `=` assignment, and
//! method calls (Phase 2). Constructs outside that set produce an explicit
//! `RuntimeError` (the documented coverage boundary) rather than a
//! silently-wrong node.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{AssignOp, Expr, ParamDef, Stmt};
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
        // A bare `{ ... }` block at statement level -> Statement::Expression(Block).
        Stmt::Block(body) => Ok(Some(statement_expression(block_node(body)?))),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var,
        } => {
            // mutsu desugars `unless X` to `if !X`, so an `unless` renders as a
            // `Statement::If` whose condition is `ApplyPrefix(!)` — a documented
            // divergence (raku keeps `Statement::Unless` with the bare condition).
            if binding_var.is_some() {
                return Err(unsupported("`if EXPR -> $var` topic binding"));
            }
            if is_elsif_chain(else_branch) {
                return Err(unsupported("`elsif` chain"));
            }
            let mut fields = vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("then"), block_node(then_branch)?),
            ];
            if else_branch.iter().any(|s| !matches!(s, Stmt::SetLine(_))) {
                fields.push(node_field(Some("else"), block_node(else_branch)?));
            }
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementIf,
                fields,
            }))
        }
        Stmt::While { cond, body, label } => {
            // mutsu desugars `until X` to `while !X` (same as unless/if above).
            if label.is_some() {
                return Err(unsupported("loop label"));
            }
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementLoopWhile,
                fields: vec![
                    node_field(Some("condition"), convert_expr(cond)?),
                    node_field(Some("body"), block_node(body)?),
                ],
            }))
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat,
            label,
        } => {
            // Only the bare `loop { }` form; the C-style `loop (init; cond; step)`,
            // `repeat` loops, and labelled loops carry extra RakuAST shape.
            if init.is_some() || cond.is_some() || step.is_some() || *repeat || label.is_some() {
                return Err(unsupported("C-style / repeat / labelled loop"));
            }
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementLoop,
                fields: vec![node_field(Some("body"), block_node(body)?)],
            }))
        }
        Stmt::Assign { name, expr, op } => {
            // Phase 2 slice 2 covers only plain `=`; `:=` (Bind) and match-assign
            // map to distinct RakuAST nodes.
            if !matches!(op, AssignOp::Assign) {
                return Err(unsupported("non-`=` assignment"));
            }
            Ok(Some(statement_expression(assignment_infix(name, expr)?)))
        }
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// `$x = EXPR` -> `ApplyInfix(left => Var::Lexical, infix => Assignment, right)`.
/// The `Assignment` node carries `:item` for scalar (`$`) targets; the list form
/// (`@`/`%`) has no adverb.
fn assignment_infix(name: &str, rhs: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    let assignment = RakuAstNode {
        class: RakuAstClass::Assignment,
        fields: if sigil == "$" {
            vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Adverb("item"),
            }]
        } else {
            vec![]
        },
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), var_lexical(sigil, desigil)),
            node_field(Some("infix"), assignment),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
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
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => {
            // Phase 2 slice 2 covers only plain `.method(...)`; modifier forms
            // (`.?`/`.+`/`.*`) and quoted names map to distinct RakuAST nodes.
            if modifier.is_some() || *quoted {
                return Err(unsupported("method call modifier / quoted name"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), call_method(name.as_str(), args)?),
                ],
            })
        }
        // A bare `{ ... }` block in expression position.
        Expr::Block(body) => block_node(body),
        Expr::AnonSub {
            body,
            is_rw,
            is_block,
        } => {
            // Only the bare-block form maps to `RakuAST::Block`; `sub { }`
            // (is_block == false) is `RakuAST::Sub`, deferred to the sub slice.
            if *is_rw || !*is_block {
                return Err(unsupported("`sub {}` / `is rw` block"));
            }
            block_node(body)
        }
        Expr::Lambda {
            param,
            body,
            is_whatever_code,
        } => {
            if *is_whatever_code {
                return Err(unsupported("Whatever-code closure"));
            }
            pointy_block_from_lambda(param, body)
        }
        Expr::AnonSubParams {
            param_defs,
            body,
            is_rw,
            is_whatever_code,
            return_type,
            ..
        } => {
            if *is_rw || *is_whatever_code || return_type.is_some() {
                return Err(unsupported("`is rw` / Whatever / typed pointy block"));
            }
            pointy_block(param_defs, body)
        }
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// True when an `if`'s else-branch is a single nested `if` — i.e. an `elsif`
/// chain (mutsu nests `elsif` into `else_branch`). raku models these as a flat
/// `elsifs` list, deferred to a later slice, so we treat it as the boundary
/// rather than mis-render it as a plain `else => Block(If)`.
fn is_elsif_chain(else_branch: &[Stmt]) -> bool {
    let mut real = else_branch
        .iter()
        .filter(|s| !matches!(s, Stmt::SetLine(_)));
    matches!(real.next(), Some(Stmt::If { .. })) && real.next().is_none()
}

/// A `{ ... }` block body wraps its `StatementList` in a `Blockoid`.
fn blockoid(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Blockoid,
        fields: vec![node_field(None, statement_list(body)?)],
    })
}

/// A bare `{ ... }` block -> `Block(body => Blockoid)`.
fn block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![node_field(Some("body"), blockoid(body)?)],
    })
}

/// A multi/zero-parameter pointy block (`-> $a, $b { }`, `-> { }`). An empty
/// parameter list omits the `signature` field entirely (matching raku).
fn pointy_block(param_defs: &[ParamDef], body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    if !param_defs.is_empty() {
        fields.push(node_field(Some("signature"), signature(param_defs)?));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode {
        class: RakuAstClass::PointyBlock,
        fields,
    })
}

/// A single-parameter pointy block (`-> $x { }`). mutsu's `Lambda` node strips
/// the sigil from its single param and does NOT preserve `@`/`%` for a single
/// non-scalar param (`-> @a` becomes `param: "a"`), so we assume `$` — a
/// documented divergence from raku, which shows the real sigil.
fn pointy_block_from_lambda(param: &str, body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let sig = RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(simple_parameter(
                "$", param, None,
            )?))]),
        }],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::PointyBlock,
        fields: vec![
            node_field(Some("signature"), sig),
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// `Signature(parameters => (Parameter, ...))`.
fn signature(param_defs: &[ParamDef]) -> Result<RakuAstNode, RuntimeError> {
    let mut params = Vec::with_capacity(param_defs.len());
    for pd in param_defs {
        params.push(Value::rakuast(Box::new(parameter(pd)?)));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(params),
        }],
    })
}

/// One `Parameter`. Only plain positional params (name + optional default) are
/// modelled; anything richer (typed, named, slurpy, `where`, sub-signature,
/// traits, optional-marker, invocant, shaped) is the coverage boundary.
fn parameter(pd: &ParamDef) -> Result<RakuAstNode, RuntimeError> {
    if pd.named
        || pd.slurpy
        || pd.double_slurpy
        || pd.onearg
        || pd.type_constraint.is_some()
        || pd.literal_value.is_some()
        || pd.sub_signature.is_some()
        || pd.where_constraint.is_some()
        || !pd.traits.is_empty()
        || pd.optional_marker
        || pd.is_invocant
        || pd.shape_constraints.is_some()
        || pd.code_signature.is_some()
        || pd.outer_sub_signature.is_some()
    {
        return Err(unsupported("non-trivial signature parameter"));
    }
    let (sigil, desigil) = split_sigil(&pd.name);
    simple_parameter(sigil, desigil, pd.default.as_ref())
}

/// Build a `Parameter(target => ParameterTarget::Var, ...)`. A param with a
/// default renders `default => <expr>`; a required one renders `optional => False`.
fn simple_parameter(
    sigil: &str,
    desigil: &str,
    default: Option<&Expr>,
) -> Result<RakuAstNode, RuntimeError> {
    let target = RakuAstNode {
        class: RakuAstClass::ParameterTargetVar,
        fields: vec![leaf_field(
            Some("name"),
            Value::str(format!("{sigil}{desigil}")),
        )],
    };
    let mut fields = vec![node_field(Some("target"), target)];
    match default {
        Some(d) => fields.push(node_field(Some("default"), convert_expr(d)?)),
        None => fields.push(RakuAstField {
            name: Some("optional"),
            value: RakuAstFieldValue::Node(Value::truth(false)),
        }),
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    })
}

/// `.method` / `.method(args)` -> `Call::Method(name => Name, [args => ArgList])`.
fn call_method(name: &str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::CallMethod,
        fields,
    })
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
