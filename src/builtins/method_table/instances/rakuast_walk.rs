//! The tree helpers rakudo 2026.09 gives every `RakuAST::Node`: `map`, `grep`
//! and `first` (`src/core.c/RakuAST/Fixups.rakumod`) and `find-nodes`
//! (`src/Raku/ast/base.rakumod`).
//!
//! `map` / `grep` / `first` walk the descendants of a node through
//! `visit-children`, each node before its children (pre-order) or, with
//! `:depth-first`, after them (post-order); the node itself is not visited.
//! `find-nodes` walks breadth first, a queue of nodes whose children it
//! visits in turn, and can stop the descent below a node (`:stopper`).

use std::collections::VecDeque;

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_node::children;
use super::rakuast_visit::native_node;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Node",
        name: "map",
        arity: 1,
        handler: Handler::Interp(map),
        flags: RowFlags::OWNER_ONLY,
        named: &["depth-first"],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "grep",
        arity: 1,
        handler: Handler::Interp(grep),
        flags: RowFlags::OWNER_ONLY,
        named: &["depth-first", "k", "kv", "p"],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "first",
        arity: 1,
        handler: Handler::Interp(first),
        flags: RowFlags::OWNER_ONLY,
        named: &["depth-first", "end"],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "find-nodes",
        arity: 1,
        handler: Handler::Interp(find_nodes),
        flags: RowFlags::OWNER_ONLY,
        named: &["condition", "stopper"],
    },
];

/// The most nodes one walk visits before it gives up, as `visit` does.
const MAX_WALK_NODES: usize = 1_000_000;

/// One step of a walk: a node to visit, and whether its children have been
/// put on the stack already (the second visit of a post-order walk).
enum Step {
    Enter(Value),
    Leave(Value),
}

/// The children of a node value, a subclass or mixin answering through the
/// native node it carries.
// Cost: O(c), c = children of the node.
fn node_children(value: &Value) -> Option<Vec<Value>> {
    let base = native_node(value)?;
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    Some(children(node))
}

/// Walk the descendants of `root` in pre-order, or in post-order when
/// `post_order`, calling `visit` on each until it answers `true` (stop).
// Cost: O(n + v), n = descendants of `root`, v = the work of `visit`.
fn walk(
    interp: &mut Interpreter,
    root: &Value,
    post_order: bool,
    mut visit: impl FnMut(&mut Interpreter, &Value) -> Result<bool, RuntimeError>,
) -> Option<Result<(), RuntimeError>> {
    let mut stack: Vec<Step> = node_children(root)?
        .into_iter()
        .rev()
        .map(Step::Enter)
        .collect();
    let mut visited = 0usize;
    while let Some(step) = stack.pop() {
        let value = match step {
            Step::Leave(value) => value,
            Step::Enter(value) => {
                visited += 1;
                if visited > MAX_WALK_NODES {
                    return Some(Err(too_many()));
                }
                let below = node_children(&value).unwrap_or_default();
                if post_order {
                    stack.push(Step::Leave(value));
                    stack.extend(below.into_iter().rev().map(Step::Enter));
                    continue;
                }
                stack.extend(below.into_iter().rev().map(Step::Enter));
                value
            }
        };
        match visit(interp, &value) {
            Ok(true) => break,
            Ok(false) => {}
            Err(error) => return Some(Err(error)),
        }
    }
    Some(Ok(()))
}

/// Whether `value` is a concrete object rather than a type object (`Mu`,
/// `RakuAST::Block`) or `Nil`.
// Cost: O(1).
fn concrete(value: &Value) -> bool {
    !value.is_nil() && !matches!(value.view(), ValueView::Package(_))
}

fn too_many() -> RuntimeError {
    RuntimeError::new("RakuAST: a tree walk exceeds one million nodes")
}

fn flag(named: &Named<'_>, name: &str) -> bool {
    named.get(name).is_some_and(Value::truthy)
}

/// `map(&mapper, :depth-first)`: the mapper's result for each descendant, a
/// `Slip` flattened into the result and `Empty` dropped.
// Cost: O(n + m), n = descendants, m = the work of the mapper over them.
fn map(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mapper = args.first()?.clone();
    let mut out = Vec::new();
    let walked = walk(
        interp,
        target,
        flag(&named, "depth-first"),
        |interp, node| {
            let result = interp.call_sub_value(mapper.clone(), vec![node.clone()])?;
            match result.view() {
                ValueView::Slip(items) => out.extend(items.iter().cloned()),
                _ => out.push(result),
            }
            Ok(false)
        },
    )?;
    Some(walked.map(|()| Value::seq(out)))
}

/// `grep($test, :depth-first, :k, :kv, :p)`: the descendants `$test`
/// smartmatches, or their indexes among the matches (`:k`), both (`:kv`) or
/// index => node pairs (`:p`).
// Cost: O(n + t), n = descendants, t = the work of the test over them.
fn grep(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let test = args.first()?.clone();
    let (k, kv, p) = (flag(&named, "k"), flag(&named, "kv"), flag(&named, "p"));
    let mut out = Vec::new();
    let mut index = 0i64;
    let walked = walk(
        interp,
        target,
        flag(&named, "depth-first"),
        |interp, node| {
            if interp.smart_match(node, &test) {
                if k {
                    out.push(Value::int(index));
                } else if kv {
                    out.push(Value::int(index));
                    out.push(node.clone());
                } else if p {
                    out.push(Value::value_pair(Value::int(index), node.clone()));
                } else {
                    out.push(node.clone());
                }
                index += 1;
            }
            Ok(false)
        },
    )?;
    Some(walked.map(|()| Value::seq(out)))
}

/// `first($test, :end, :depth-first)`: the first descendant `$test`
/// smartmatches, or the last one with `:end`; `Nil` when none does.
// Cost: O(n + t), n = descendants walked, t = the work of the test over them.
fn first(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let test = args.first()?.clone();
    let end = flag(&named, "end");
    let mut found = Value::NIL;
    let walked = walk(
        interp,
        target,
        flag(&named, "depth-first"),
        |interp, node| {
            if interp.smart_match(node, &test) {
                found = node.clone();
                return Ok(!end);
            }
            Ok(false)
        },
    )?;
    Some(walked.map(|()| found))
}

/// `find-nodes($type, :condition, :stopper)`: the descendants of type
/// `$type` (that satisfy `:condition`), breadth first. A code `:stopper`
/// that answers true skips a node and everything below it; a type
/// `:stopper` keeps a matching node but does not look below it.
// Cost: O(n + c), n = descendants walked, c = the work of the condition and
// stopper over them.
fn find_nodes(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let wanted = args.first()?.clone();
    let condition = named.get("condition").filter(|c| concrete(c)).cloned();
    let stopper = named.get("stopper").cloned();
    let code_stopper = stopper.as_ref().is_some_and(concrete);
    let mut queue = VecDeque::from([target.clone()]);
    let mut out = Vec::new();
    let mut visited = 0usize;
    while let Some(parent) = queue.pop_front() {
        for node in node_children(&parent).unwrap_or_default() {
            visited += 1;
            if visited > MAX_WALK_NODES {
                return Some(Err(too_many()));
            }
            if code_stopper && let Some(stopper) = &stopper {
                match interp.call_sub_value(stopper.clone(), vec![node.clone()]) {
                    Ok(stop) if stop.truthy() => continue,
                    Ok(_) => {}
                    Err(error) => return Some(Err(error)),
                }
            }
            if interp.smart_match(&node, &wanted) {
                let keep = match &condition {
                    Some(condition) => {
                        match interp.call_sub_value(condition.clone(), vec![node.clone()]) {
                            Ok(keep) => keep.truthy(),
                            Err(error) => return Some(Err(error)),
                        }
                    }
                    None => true,
                };
                if keep {
                    out.push(node.clone());
                }
            }
            let stops_here = !code_stopper
                && stopper
                    .as_ref()
                    .is_some_and(|stopper| interp.smart_match(&node, stopper));
            if !stops_here {
                queue.push_back(node);
            }
        }
    }
    Some(Ok(Value::array(out)))
}
