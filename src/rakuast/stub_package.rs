//! A stub package (`class C {...}`, `role R {...}`).
//!
//! rakudo models it as an empty body plus `is-stub => True`; the parser keeps
//! the `...` as the body's only statement, a call of the stub marker.

use super::view::leaf_field;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt, stub};
use crate::value::{Value, ValueView};

/// Whether `body` is nothing but the `...` stub marker.
// Cost: O(n), n = statements in the body.
fn is_stub_body(body: &[Stmt]) -> bool {
    let mut real = body.iter().filter(|s| !matches!(s, Stmt::SetLine(..)));
    matches!(
        (real.next(), real.next()),
        (Some(Stmt::Expr(Expr::Call { name, args, .. })), None)
            if args.is_empty() && name.as_str() == stub::FAIL
    )
}

/// `value` with its node rebuilt by `f`; a leaf stays as it is.
fn rebuilt(value: &Value, f: &dyn Fn(&RakuAstNode) -> RakuAstNode) -> Value {
    match value.view() {
        ValueView::RakuAst(node) => Value::rakuast(Box::new(f(node))),
        _ => value.clone(),
    }
}

/// Drop the `...` statement from the package node `node` was built for, and
/// mark it `is-stub`.
// Cost: O(d), d = size of the package's body node.
pub(super) fn apply(stmt: &Stmt, node: RakuAstNode) -> RakuAstNode {
    let body = match stmt {
        Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } | Stmt::Package { body, .. } => {
            body
        }
        _ => return node,
    };
    if !is_stub_body(body) {
        return node;
    }
    if node.class != RakuAstClass::StatementExpression {
        return stub_package(&node);
    }
    let fields = node
        .fields
        .iter()
        .map(|field| RakuAstField {
            name: field.name,
            value: match &field.value {
                RakuAstFieldValue::Node(v) => RakuAstFieldValue::Node(rebuilt(v, &stub_package)),
                other => other.clone(),
            },
        })
        .collect();
    RakuAstNode {
        class: node.class,
        fields,
        origin: node.origin.clone(),
    }
}

fn stub_package(package: &RakuAstNode) -> RakuAstNode {
    if !matches!(
        package.class,
        RakuAstClass::Class
            | RakuAstClass::Role
            | RakuAstClass::Grammar
            | RakuAstClass::Module
            | RakuAstClass::Package
    ) {
        return package.clone();
    }
    let mut fields: Vec<RakuAstField> = package
        .fields
        .iter()
        .map(|field| RakuAstField {
            name: field.name,
            value: match &field.value {
                RakuAstFieldValue::Node(v) if field.name == Some("body") => {
                    RakuAstFieldValue::Node(rebuilt(v, &strip_stub))
                }
                other => other.clone(),
            },
        })
        .collect();
    fields.push(leaf_field(Some("is-stub"), Value::TRUE));
    RakuAstNode {
        class: package.class,
        fields,
        origin: package.origin.clone(),
    }
}

/// `node` without any `...` statement beneath it.
fn strip_stub(node: &RakuAstNode) -> RakuAstNode {
    let fields = node
        .fields
        .iter()
        .filter(|field| !matches!(&field.value, RakuAstFieldValue::Node(v) if is_stub_statement(v)))
        .map(|field| RakuAstField {
            name: field.name,
            value: match &field.value {
                RakuAstFieldValue::Node(v) => RakuAstFieldValue::Node(rebuilt(v, &strip_stub)),
                RakuAstFieldValue::List(items) => RakuAstFieldValue::List(
                    items
                        .iter()
                        .filter(|item| !is_stub_statement(item))
                        .map(|item| rebuilt(item, &strip_stub))
                        .collect(),
                ),
                other => other.clone(),
            },
        })
        .collect();
    RakuAstNode {
        class: node.class,
        fields,
        origin: node.origin.clone(),
    }
}

fn is_stub_statement(value: &Value) -> bool {
    let ValueView::RakuAst(node) = value.view() else {
        return false;
    };
    node.class == RakuAstClass::StatementExpression
        && node.fields.iter().any(|f| {
            matches!(&f.value, RakuAstFieldValue::Node(v)
                if matches!(v.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::StubFail))
        })
}

/// The inverse of [`apply`] for lowering: a package node marked `is-stub` gets
/// its `...` statement back, so it lowers to the stub the parser builds.
// Cost: O(d), d = size of the package node.
pub(super) fn restore(node: &RakuAstNode) -> Option<RakuAstNode> {
    if node.class != RakuAstClass::StatementExpression {
        return restore_package(node);
    }
    let mut changed = false;
    let fields = node
        .fields
        .iter()
        .map(|field| RakuAstField {
            name: field.name,
            value: match &field.value {
                RakuAstFieldValue::Node(v) => match v.view() {
                    ValueView::RakuAst(inner) => match restore_package(inner) {
                        Some(restored) => {
                            changed = true;
                            RakuAstFieldValue::Node(Value::rakuast(Box::new(restored)))
                        }
                        None => field.value.clone(),
                    },
                    _ => field.value.clone(),
                },
                other => other.clone(),
            },
        })
        .collect();
    changed.then(|| RakuAstNode {
        class: node.class,
        fields,
        origin: node.origin.clone(),
    })
}

fn restore_package(package: &RakuAstNode) -> Option<RakuAstNode> {
    let stub = package.fields.iter().any(|f| {
        f.name == Some("is-stub") && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
    });
    if !stub {
        return None;
    }
    let fields = package
        .fields
        .iter()
        .filter(|f| f.name != Some("is-stub"))
        .map(|field| RakuAstField {
            name: field.name,
            value: match &field.value {
                RakuAstFieldValue::Node(v) if field.name == Some("body") => {
                    RakuAstFieldValue::Node(rebuilt(v, &add_stub))
                }
                other => other.clone(),
            },
        })
        .collect();
    Some(RakuAstNode {
        class: package.class,
        fields,
        origin: package.origin.clone(),
    })
}

/// `node` with the `...` statement appended to its statement list.
fn add_stub(node: &RakuAstNode) -> RakuAstNode {
    let mut fields: Vec<RakuAstField> = node
        .fields
        .iter()
        .map(|field| RakuAstField {
            name: field.name,
            value: match &field.value {
                RakuAstFieldValue::Node(v) => RakuAstFieldValue::Node(rebuilt(v, &add_stub)),
                other => other.clone(),
            },
        })
        .collect();
    if node.class == RakuAstClass::StatementList {
        let fail = RakuAstNode {
            class: RakuAstClass::StubFail,
            fields: Vec::new(),
            origin: Default::default(),
        };
        let statement = RakuAstNode {
            class: RakuAstClass::StatementExpression,
            fields: vec![RakuAstField {
                name: Some("expression"),
                value: RakuAstFieldValue::Node(Value::rakuast(Box::new(fail))),
            }],
            origin: Default::default(),
        };
        // A role body keeps its closing `nqp::list(...)` statement last.
        let at = match fields.last() {
            Some(last) if super::role_implicit::is_closing(last) => fields.len() - 1,
            _ => fields.len(),
        };
        fields.insert(
            at,
            RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(Value::rakuast(Box::new(statement))),
            },
        );
    }
    RakuAstNode {
        class: node.class,
        fields,
        origin: node.origin.clone(),
    }
}
