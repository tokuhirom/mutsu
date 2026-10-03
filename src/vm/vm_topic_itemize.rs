//! Itemization of a scalar store reached by name, including the topic `$_`.

use super::*;

impl Interpreter {
    /// [`Self::itemize_scalar_store`] for a store reached by name, where the
    /// topic `$_` is settled per holder rather than exempted outright (#11229).
    ///
    /// The topic is bound to whatever its construct aliased. When that is a
    /// `Scalar` -- an array element (`for @a`), a `$` variable (`given $t`) or
    /// the topic's own default container -- an aggregate assigned to it is
    /// itemized like any `$` store: `for @a { $_ = [1,2] }` leaves `@a[0]` as
    /// `$[1, 2]`. When it is an `@`/`%` container itself (`given @a { .=reverse }`),
    /// the write is that container's `STORE`, so the value stays un-itemized.
    /// An un-itemized Array/Hash currently in the topic marks the latter case.
    // Cost: O(1).
    pub(crate) fn itemize_named_scalar_store(&self, name: &str, val: Value) -> Value {
        if name != "_" {
            return Self::itemize_scalar_store(name, val);
        }
        let aliases_aggregate = self
            .get_env_with_main_alias(name)
            .is_some_and(|current| Self::is_bare_aggregate(&current.deref_container()));
        if aliases_aggregate {
            val
        } else {
            Self::itemize_scalar_store_value(val)
        }
    }

    /// True for an Array or Hash that is not held in a Scalar container.
    // Cost: O(1).
    fn is_bare_aggregate(value: &Value) -> bool {
        match value.view() {
            ValueView::Array(_, kind) => !kind.is_itemized(),
            ValueView::Hash(_) => !value.hash_is_itemized(),
            _ => false,
        }
    }
}
