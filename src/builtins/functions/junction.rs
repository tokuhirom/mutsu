#![allow(clippy::result_large_err)]
use crate::value::Value;

/// Build a `Junction` for the `any`/`all`/`one`/`none` constructors.
///
/// Pure value assembly: applies the one-arg flattening rule (a single
/// list/seq/slip/range argument is spread into the junction's elements; with
/// any other arity each argument becomes one element) then wraps the elements.
/// Reads no interpreter state. Shared by the VM-native dispatch
/// (`native_function`) and the interpreter's `call_function` so there is a
/// single implementation (ledger §2: builtins lowered out of the tree-walk
/// `call_function` terminal).
pub(crate) fn build_junction(name: &str, args: Vec<Value>) -> Value {
    use crate::value::JunctionKind;
    let kind = match name {
        "any" => JunctionKind::Any,
        "all" => JunctionKind::All,
        "one" => JunctionKind::One,
        _ => JunctionKind::None,
    };
    let elems = if args.len() == 1 {
        let arg = args.into_iter().next().unwrap();
        match arg {
            Value::Array(items, ..) => items.to_vec(),
            Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            range @ (Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. }) => crate::runtime::utils::value_to_list(&range),
            other => vec![other],
        }
    } else {
        args
    };
    Value::junction(kind, elems)
}
