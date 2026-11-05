//! Value binding at native named-argument consumers. Raw capture relays keep
//! their original arguments; ordinary named parameters read their contents.

use super::{Value, ValueView};
use std::borrow::Cow;

// Cost: O(a + k), a = arguments, k = copied key bytes. Rakudo: O(a) -- see #9252.
pub(crate) fn read_named_args(args: &[Value]) -> Cow<'_, [Value]> {
    if !args
        .iter()
        .any(|arg| matches!(arg.view(), ValueView::Pair(_, value) if value.is_container_ref()))
    {
        return Cow::Borrowed(args);
    }
    Cow::Owned(
        args.iter()
            .map(|arg| match arg.view() {
                ValueView::Pair(key, value) if value.is_container_ref() => {
                    Value::pair(key.clone(), value.deref_container())
                }
                _ => arg.clone(),
            })
            .collect(),
    )
}
