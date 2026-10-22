//! Breadth-first and depth-first visitors for `RakuAST::Node`.

use std::collections::VecDeque;

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_node::children;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Node",
        name: "visit",
        arity: 1,
        handler: Handler::Interp(visit_breadth_first),
        flags: RowFlags::OWNER_ONLY,
        named: &["strict"],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "visit-dfs",
        arity: 1,
        handler: Handler::Interp(visit_depth_first),
        flags: RowFlags::OWNER_ONLY,
        named: &["strict"],
    },
];

/// `visit(&callback, :strict)` walks a node and its descendants breadth first.
// Cost: O(n + c), n = visited nodes, c = callback work over those nodes.
fn visit_breadth_first(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    visit_tree(interp, target, args, named, false)
}

/// `visit-dfs(&callback, :strict)` visits descendants in depth-first order.
// Cost: O(n + c), n = visited nodes, c = callback work over those nodes.
fn visit_depth_first(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    visit_tree(interp, target, args, named, true)
}

const MAX_VISIT_NODES: usize = 1_000_000;

fn visit_tree(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
    depth_first: bool,
) -> Option<Result<Value, RuntimeError>> {
    if !matches!(target.view(), ValueView::RakuAst(_)) {
        return None;
    }
    let callable = args.first()?;
    let strict = named.get("strict").is_some_and(Value::truthy);
    let mut pending = VecDeque::from([target.clone()]);
    let mut visited = 0;
    while let Some(value) = if depth_first {
        pending.pop_back()
    } else {
        pending.pop_front()
    } {
        visited += 1;
        if visited > MAX_VISIT_NODES {
            return Some(Err(RuntimeError::new(
                "RakuAST: visit exceeds one million nodes",
            )));
        }
        let descend = if strict && visited == 1 {
            true
        } else {
            match interp.call_sub_value(callable.clone(), vec![value.clone()], false) {
                Ok(result) => result.truthy(),
                Err(error) => return Some(Err(error)),
            }
        };
        if !descend {
            continue;
        }
        let ValueView::RakuAst(node) = value.view() else {
            return None;
        };
        let direct = children(node);
        if visited + pending.len() + direct.len() > MAX_VISIT_NODES {
            return Some(Err(RuntimeError::new(
                "RakuAST: visit exceeds one million nodes",
            )));
        }
        if depth_first {
            pending.extend(direct.into_iter().rev());
        } else {
            pending.extend(direct);
        }
    }
    Some(Ok(Value::package(Symbol::intern("Any"))))
}
