//! Pure zero-argument calls on receivers resolved through an owner chain.

use super::{RowFlags, RowId, admits, invoke, owner_row, row};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// Invoke the nearest registered zero-argument row through the common pure
/// entry. Selecting the row precedes invocation: an interpreter-only method
/// must decline rather than accidentally invoke a pure ancestor's method.
// Cost: O(o * k), o = owners, k = rows named method, plus the handler's own cost.
pub(crate) fn invoke_owner_pure(
    owners: &[&str],
    target: &Value,
    method: Symbol,
) -> Option<Result<Value, RuntimeError>> {
    invoke_selected(resolve_owner_row(owners, method)?, target)
}

/// Select the nearest row before invoking it; an interpreter row remains a
/// barrier even for a pure caller. The builtin table and owners are immutable.
// Cost: O(o * k), o = owners, k = rows named method.
pub(crate) fn resolve_owner_row(owners: &[&str], method: Symbol) -> Option<RowId> {
    owners
        .iter()
        .find_map(|owner| owner_row(Symbol::intern(owner), method, 0))
}

/// Invoke a selected pure row with the same admission as an uncached lookup.
// Cost: O(1) plus the handler's own cost.
pub(crate) fn invoke_selected(id: RowId, target: &Value) -> Option<Result<Value, RuntimeError>> {
    let selected = row(id);
    if !selected.flags.contains(RowFlags::TYPE_OBJECT_OK)
        && let ValueView::Package(name) = target.view()
    {
        return selected.type_object_refusal(name.as_str()).map(Err);
    }
    if !admits(id, &[]) {
        return None;
    }
    invoke(id, target, &[])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_interpreter_row_does_not_fall_through_to_a_pure_ancestor() {
        let regex = Value::regex("a".to_owned());
        let owners = ["Regex", "Method", "Routine", "Block", "Code", "Any", "Mu"];
        assert!(invoke_owner_pure(&owners, &regex, Symbol::intern("Str")).is_none());
    }

    #[test]
    fn code_rows_answer_the_pure_entry() {
        let regex = Value::regex("a".to_owned());
        assert!(matches!(
            super::super::code::pure_answer(&regex, Symbol::intern("clone")),
            Some(Ok(value)) if value == regex
        ));
        assert!(matches!(
            super::super::code::pure_answer(&regex, Symbol::intern("Capture")),
            Some(Err(_))
        ));
        assert!(super::super::code::pure_answer(&regex, Symbol::intern("name")).is_none());
        assert!(super::super::code::pure_answer(&Value::int(3), Symbol::intern("clone")).is_none());
    }

    #[test]
    fn a_definite_invocant_row_refuses_a_type_object() {
        let target = Value::package(Symbol::intern("Int"));
        assert!(matches!(
            invoke_owner_pure(&["Int", "Cool"], &target, Symbol::intern("lsb")),
            Some(Err(_))
        ));
    }
}
