//! The call a statement-level `Stmt::Call` stands for (`is(2, 2, "x")`,
//! `foo 1, :a`).
//!
//! The positional arguments are converted where they sit in the statement,
//! not as copies: a term's range is found by the slot it sits in, while a
//! copy finds it only when no other term of the statement has its shape
//! (`is(2, 2, "x")` has two).

use super::view::{call_args_as_exprs, call_name, is_injected_named_arg, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::CallArg;
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// The call to `name` with `args`; `listop` when it was written without
/// parentheses.
// Cost: O(size of the arguments).
pub(super) fn call(
    names: &NameScope,
    name: &str,
    args: &[CallArg],
    listop: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let mut list = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ArgList,
        fields: Vec::with_capacity(args.len()),
    };
    for arg in args {
        let node = match arg {
            CallArg::Positional(expr) if !is_injected_named_arg(expr) => view_expr(names, expr)?,
            CallArg::Positional(_) => continue,
            other => {
                let [expr] = call_args_as_exprs(std::slice::from_ref(other))?
                    .try_into()
                    .map_err(|_| super::view::unsupported("statement call argument"))?;
                if is_injected_named_arg(&expr) {
                    continue;
                }
                view_expr(names, &expr)?
            }
        };
        list.fields.push(super::view::node_field(None, node));
    }
    // `call_name` over no arguments has none, as Rakudo's for a call written
    // with none.
    let mut call = call_name(names, name, &[], listop)?;
    if !list.fields.is_empty() {
        call.fields
            .push(super::view::node_field(Some("args"), list));
    }
    Ok(call)
}
