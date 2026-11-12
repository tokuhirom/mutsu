//! `RakuAST::Block`'s `has-placeholder-parameters`, the `ScopePhaser` queries and `replace-body`,
//! answered by [`crate::rakuast::block_queries`]; this file only registers the
//! rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(n), n = size of the block's own body.
fn has_placeholder_parameters(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(
            crate::rakuast::block_queries::has_placeholder_parameters(node),
        ),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(s), s = top-level statements of the block's body.
fn has_any_phasers(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::block_queries::has_any_phasers(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(s), s = top-level statements of the block's body.
fn has_loop_phasers(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::block_queries::has_loop_phasers(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(s), s = top-level statements of the block's body.
fn has_phaser(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let name = args.first().map(Value::to_string_value).unwrap_or_default();
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::block_queries::has_phaser(node, &name)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(f), f = fields of the block.
fn bare_block(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::block_queries::bare_block_flag(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

/// `RakuAST::Code.signature` as a plain `RakuAST::Block` answers it: a block
/// without a declared signature has none (`Nil`). `RakuAST::PointyBlock` and
/// the routines carry their own `signature` accessor.
// Cost: O(1).
fn block_signature(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::NIL)
}

// Cost: O(f), f = fields of the node.
fn replace_body(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_replace_body(args[0].clone())
}

// Cost: O(f), f = fields of the node.
fn replace_signature(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| {
        crate::rakuast::block_queries::replace_signature(node, &args[0]).map(|()| Value::NIL)
    })
}

// Cost: O(f), f = fields of the node.
fn replace_scope(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| {
        crate::rakuast::block_queries::replace_scope(node, &args[0]).map(|()| Value::NIL)
    })
}

// Cost: O(f), f = fields of the node.
fn replace_name(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| {
        crate::rakuast::block_queries::replace_name(node, &args[0]).map(|()| Value::NIL)
    })
}

// Cost: O(f), f = fields of the node.
fn set_implicit_topic(
    _interp: &mut crate::runtime::Interpreter,
    target: &Value,
    args: &[Value],
    named: super::super::Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let on = args.first().is_none_or(Value::truthy);
    let required = named.get("required").is_some_and(|v| v.truthy());
    let exception = named.get("exception").is_some_and(|v| v.truthy());
    target.rakuast_edit(|node| {
        crate::rakuast::block_queries::set_implicit_topic(node, on, required, exception)
            .map(|()| Value::NIL)
    })
}

// Cost: O(f), f = fields of the node.
fn otherwise(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::for_otherwise::otherwise(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(f), f = fields of the node.
fn replace_otherwise(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| crate::rakuast::for_otherwise::replace_otherwise(node, &args[0]))
}

pub(super) static ROWS: &[MethodRow] = &[
    // `Routine.replace-signature`; `Submethod` inherits `Method`'s.
    MethodRow {
        owner: "RakuAST::Routine",
        name: "replace-signature",
        arity: 1,
        handler: Handler::Narrow(replace_signature),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Declaration",
        name: "replace-scope",
        arity: 1,
        handler: Handler::Narrow(replace_scope),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Routine",
        name: "replace-name",
        arity: 1,
        handler: Handler::Narrow(replace_name),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    // `PointyBlock` takes a new signature too (rakudo: `Block`'s subclass).
    MethodRow {
        owner: "RakuAST::PointyBlock",
        name: "replace-signature",
        arity: 1,
        handler: Handler::Narrow(replace_signature),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Block",
        name: "set-implicit-topic",
        arity: 1,
        handler: Handler::Interp(set_implicit_topic),
        flags: RowFlags::OWNER_ONLY,
        named: &["required", "exception"],
    },
    MethodRow {
        owner: "RakuAST::PlaceholderParameterOwner",
        name: "has-placeholder-parameters",
        arity: 0,
        handler: Handler::Pure(has_placeholder_parameters),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ScopePhaser",
        name: "has-any-phasers",
        arity: 0,
        handler: Handler::Pure(has_any_phasers),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ScopePhaser",
        name: "has-loop-phasers",
        arity: 0,
        handler: Handler::Pure(has_loop_phasers),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ScopePhaser",
        name: "has-phaser",
        arity: 1,
        handler: Handler::Pure(has_phaser),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Block",
        name: "signature",
        arity: 0,
        handler: Handler::Pure(block_signature),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Block",
        name: "bare-block",
        arity: 0,
        handler: Handler::Pure(bare_block),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Block",
        name: "replace-body",
        arity: 1,
        handler: Handler::Narrow(replace_body),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    // rakudo declares `replace-body` on `Sub` and `Method` too (`Submethod` inherits `Method`'s); the `Block`
    // row above answers them all (its handler takes every class with a body),
    // these rows only let `.^can` / `.^methods` report the method.
    MethodRow {
        owner: "RakuAST::Sub",
        name: "replace-body",
        arity: 1,
        handler: Handler::Narrow(replace_body),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Method",
        name: "replace-body",
        arity: 1,
        handler: Handler::Narrow(replace_body),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Statement::For",
        name: "otherwise",
        arity: 0,
        handler: Handler::Pure(otherwise),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Statement::For",
        name: "replace-otherwise",
        arity: 1,
        handler: Handler::Narrow(replace_otherwise),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];
