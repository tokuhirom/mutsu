//! A `Proxy` used as a subscript of an element store or increment.

use super::*;

impl Interpreter {
    /// FETCH a `Proxy` subscript sitting on top of the stack (`%h{$p} = v`,
    /// `@a[$q]++`), all the way down to its value: the element store and
    /// increment paths read the key as a value, as the subscript read does.
    // Cost: O(1) unless the top is a `Proxy` (a tag probe); then its FETCH
    // chain, bounded by `auto_fetch_proxy`.
    pub(super) fn fetch_proxy_subscript_on_top(&mut self) -> Result<(), RuntimeError> {
        if let Some(top) = self.stack.last()
            && top.is_proxy_value()
        {
            let key = self.stack.pop().unwrap_or(Value::NIL);
            let fetched = self.auto_fetch_proxy(&key)?;
            self.stack.push(fetched);
        }
        Ok(())
    }
}
