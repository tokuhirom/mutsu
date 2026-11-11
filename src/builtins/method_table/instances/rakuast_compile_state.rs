//! The compile-time state a CHECK-phase rewrite reaches through `$*CU`:
//! `resolver`, `context`, and the `IMPL-BEGIN` / `IMPL-CHECK` phase hooks a
//! rewrite runs on a node it just built (Qwiratry's `attach-compile-time`).
//!
//! mutsu lowers the unit's finished tree once, after its CHECK phase, and
//! resolves every lookup then, so a node built or edited during CHECK has
//! nothing to settle early: the two hooks accept the resolver and context
//! and leave the node as it is. The handles are opaque objects of rakudo's
//! class names.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

/// `$*CU.context.^name`.
const CONTEXT_CLASS: &str = "RakuAST::IMPL::QASTContext";

// Cost: O(1).
fn resolver(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::runtime::attach_target::compile_resolver_handle())
}

// Cost: O(1).
fn context(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::make_instance(
        crate::symbol::Symbol::intern(CONTEXT_CLASS),
        std::collections::HashMap::new(),
    ))
}

// Cost: O(1).
fn phase_hook(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::NIL)
}

// Cost: O(f), f = fields of the node.
fn resolved_constant_value(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let value = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => node
            .fields
            .iter()
            .find(|f| f.name == Some("compile-time-value"))
            .and_then(|f| match &f.value {
                crate::rakuast::RakuAstFieldValue::Node(v) => Some(v.clone()),
                _ => None,
            }),
        _ => None,
    });
    Ok(value.unwrap_or(Value::NIL))
}

// Cost: O(f), f = fields of the node.
fn named_field(target: &Value, field: &str) -> Option<Value> {
    native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => {
            node.fields
                .iter()
                .find(|f| f.name == Some(field))
                .and_then(|f| match &f.value {
                    crate::rakuast::RakuAstFieldValue::Node(v) => Some(v.clone()),
                    _ => None,
                })
        }
        _ => None,
    })
}

/// A field of a `Declaration::Import` built by `.new` (a RakuAST node), `None`
/// for the instance form the resolver hands out.
// Cost: O(f).
pub(super) fn built_import_field(target: &Value, field: &str) -> Option<Value> {
    match target.view() {
        ValueView::RakuAst(node)
            if node.class == crate::rakuast::RakuAstClass::DeclarationImport =>
        {
            named_field(target, field)
        }
        _ => None,
    }
}

// Cost: O(f).
fn import_lexical_name(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(built_import_field(target, "lexical-name").unwrap_or(Value::NIL))
}

/// `$*CU.merge-generated-lexical-declaration($import, :resolver, :force)`:
/// a module installing a declaration into the unit being compiled at BEGIN
/// time (CompUnit::Util's `set-unit`, #15371).
// Cost: O(f) plus the install (`merge_generated_lexical`).
fn merge_generated_lexical(
    interp: &mut crate::runtime::Interpreter,
    _target: &Value,
    args: &[Value],
    _named: crate::builtins::method_table::Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let decl = args.first()?;
    let name = built_import_field(decl, "lexical-name")?.to_string_value();
    let value = built_import_field(decl, "compile-time-value").unwrap_or(Value::NIL);
    Some(interp.merge_generated_lexical(&name, value))
}

// Cost: O(f).
fn implicit_constant_name(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(named_field(target, "name").unwrap_or(Value::NIL))
}

// Cost: O(f).
fn implicit_constant_value(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(named_field(target, "value").unwrap_or(Value::NIL))
}

// Cost: O(f).
fn implicit_constant_scope(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(named_field(target, "scope").unwrap_or_else(|| Value::str_from("my")))
}

const fn implicit_constant_row(
    name: &'static str,
    f: fn(&Value, &[Value]) -> Result<Value, RuntimeError>,
) -> MethodRow {
    MethodRow {
        owner: "RakuAST::VarDeclaration::Implicit::Constant",
        name,
        arity: 0,
        handler: Handler::Pure(f),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    }
}

const fn unit_row(
    name: &'static str,
    arity: u8,
    f: fn(&Value, &[Value]) -> Result<Value, RuntimeError>,
) -> MethodRow {
    MethodRow {
        owner: "RakuAST::CompUnit",
        name,
        arity,
        handler: Handler::Pure(f),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    }
}

const fn node_row(
    name: &'static str,
    f: fn(&Value, &[Value]) -> Result<Value, RuntimeError>,
) -> MethodRow {
    MethodRow {
        owner: "RakuAST::Node",
        name,
        arity: 2,
        handler: Handler::Pure(f),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    unit_row("resolver", 0, resolver),
    unit_row("context", 0, context),
    MethodRow {
        owner: "RakuAST::Declaration::ResolvedConstant",
        name: "compile-time-value",
        arity: 0,
        handler: Handler::Pure(resolved_constant_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::CompUnit",
        name: "merge-generated-lexical-declaration",
        arity: 1,
        handler: Handler::Interp(merge_generated_lexical),
        flags: RowFlags::OWNER_ONLY,
        named: &["resolver", "force"],
    },
    MethodRow {
        owner: "RakuAST::Declaration::Import",
        name: "lexical-name",
        arity: 0,
        handler: Handler::Pure(import_lexical_name),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    implicit_constant_row("name", implicit_constant_name),
    implicit_constant_row("scope", implicit_constant_scope),
    implicit_constant_row("value", implicit_constant_value),
    node_row("IMPL-BEGIN", phase_hook),
    node_row("IMPL-CHECK", phase_hook),
];
