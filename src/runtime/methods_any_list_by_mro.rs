//! `Any`'s iteration methods answered by their rows, by the receiver's MRO
//! (ADR-11276 §9.134).
//!
//! `map`, `grep`, `first`, `reduce`, ... are `Handler::Interp` rows of `Any`
//! (`method_table::collections::any_interp`) whose handlers call the one
//! `Interpreter::dispatch_*_method` each. The by-name cascades used to keep an
//! arm per name calling that same routine, for the calls the table entry in
//! `call_method_with_values` does not reach (an instance, a type object, a
//! call whose arguments a row does not bind). Those calls now walk the
//! receiver's MRO for the first row that binds, as `.^can` would find it.

use super::*;

/// The methods whose `Any` rows answer by the receiver's MRO.
const ANY_LIST_BY_MRO: [&str; 15] = [
    "map",
    "grep",
    "first",
    "reduce",
    "produce",
    "iterator",
    "eager",
    "squish",
    "rotor",
    "skip",
    "collate",
    "classify",
    "categorize",
    "classify-list",
    "categorize-list",
];

impl Interpreter {
    /// Answer one of `Any`'s iteration methods from the first row of the
    /// receiver's MRO that binds the call, or `None` for any other method and
    /// for a `Junction`, whose methods autothread.
    // Cost: O(n) to test the name, n = 15, plus one memoized MRO read and
    // `invoke_mro`'s own cost.
    pub(super) fn any_list_method_by_mro(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !ANY_LIST_BY_MRO.contains(&method)
            || matches!(target.view(), ValueView::Junction { .. })
        {
            return None;
        }
        let mro = self.classhow_mro_unhidden_names(target);
        let owners: Vec<&str> = mro.iter().map(String::as_str).collect();
        crate::builtins::method_table::invoke_mro(self, &owners, method, args, target)
    }
}
