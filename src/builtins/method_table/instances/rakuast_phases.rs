//! `RakuAST::Node.to-parse-time` and `to-begin-time`: rakudo runs a node's parse
//! and begin phases against the resolver while it parses. mutsu resolves names
//! when it lowers the tree, so a node built mid-parse (a slang's
//! `$*BLOCK.to-parse-time($*R, $*CU.context)`) has nothing to do at that point
//! and the call answers the node, as rakudo's `to-parse-time` does.
//!
//! `IMPL-BEGIN` / `IMPL-CHECK` on a node built at CHECK time (Red's
//! `red-compile`, #17501) resolve its lookups against the scopes the resolver
//! argument pushed, so its `Var::Lexical`s answer `is-resolved` / `resolution`
//! as rakudo's do; the lowering still resolves the names it compiles.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

// Cost: O(1).
fn phase_noop(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(target.clone())
}

// Cost: O(n * d) plus the declarations of the pushed scopes, n = nodes of the
// invocant, d = scope depth.
fn impl_phase(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let scopes = args
        .first()
        .map(crate::runtime::attach_target::resolver_scopes)
        .unwrap_or_default();
    let imported = |routine: &str| interp.unit_routine_imported(routine);
    let imported_type = |name: &str| interp.has_class(name) || interp.slang_minted_type(name);
    crate::rakuast::resolve_node_in(target, &scopes, &imported, &imported_type);
    Some(Ok(Value::NIL))
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Node",
        name: "IMPL-BEGIN",
        arity: 2,
        handler: Handler::Interp(impl_phase),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "IMPL-CHECK",
        arity: 2,
        handler: Handler::Interp(impl_phase),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "to-parse-time",
        arity: 2,
        handler: Handler::Pure(phase_noop),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "to-begin-time",
        arity: 2,
        handler: Handler::Pure(phase_noop),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
