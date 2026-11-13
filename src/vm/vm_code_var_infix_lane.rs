//! `cmp($a, $b)` through a frame's own `&cmp` slot bound to a built-in
//! operator (`:&cmp = &[cmp]`): the binary-search shape of Array::Sorted::Util.

use super::*;

impl Interpreter {
    /// Answer a two-argument call of the `&name` slot `slot` when it holds a
    /// plain built-in infix operator's routine value (`&infix:<cmp>`), with the
    /// operator body itself -- what `vm_call_on_value_at` reaches for that
    /// target through `try_plain_infix_routine_value`, without first
    /// collecting the arguments into a fresh vector and re-deriving the call
    /// site's argument shape. `None` (stack untouched) for anything else: a
    /// user routine, a routine a CALL-ME role was composed into, a pair
    /// argument, or an operator a user candidate or `proto` overrides.
    // Cost: O(1) probes plus the operator body.
    pub(super) fn code_var_local_plain_infix(
        &mut self,
        slot: u32,
    ) -> Option<Result<(), RuntimeError>> {
        let target = self.locals.get(slot as usize)?.clone().into_deref();
        let ValueView::Routine {
            package,
            name,
            is_regex: false,
            ..
        } = target.view()
        else {
            return None;
        };
        let n = self.stack.len();
        if n < 2
            || self.stack[n - 2..]
                .iter()
                .any(|v| matches!(v.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            || self.routine_call_me_view(&target).is_some()
        {
            return None;
        }
        let args = [self.stack[n - 2].clone(), self.stack[n - 1].clone()];
        let result = self.try_plain_infix_routine_value(package, name, &args)?;
        self.stack.truncate(n - 2);
        self.set_pending_callsite_line(None);
        Some(result.map(|value| self.stack.push(value)))
    }
}
