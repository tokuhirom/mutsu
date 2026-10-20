use crate::ast::ReadonlyKind;
use crate::runtime::Interpreter;

impl Interpreter {
    /// Whether a scalar name is bound directly to a value rather than a Scalar.
    /// An immutable binding reports the value from `.VAR`, while a readonly
    /// alias parameter still reports its Scalar container. The binding cell's
    /// decision is authoritative over a same-named registry entry.
    ///
    /// Aggregate sigils return false: their containers are handled separately
    /// by callers deciding whether a name denotes a Scalar.
    // Cost: O(n + c), n = name length, c = the bounded binding-cell chain.
    pub(crate) fn scalar_name_has_no_container(&self, name: &str) -> bool {
        if name.starts_with(['@', '%', '&']) {
            return false;
        }
        let bare = name.trim_start_matches('$');
        let own_kind = if crate::value::readonly_binding_cells_possible()
            || crate::value::readonly_cells_possible()
        {
            self.env()
                .get(bare)
                .and_then(Self::binding_chain_readonly_kind)
        } else {
            None
        };
        let kind = own_kind.unwrap_or_else(|| self.readonly_kind(bare));
        matches!(
            kind,
            Some(
                ReadonlyKind::Immutable
                    | ReadonlyKind::ImmutableValue
                    | ReadonlyKind::ImmutableDeep
                    | ReadonlyKind::TypeObject
            )
        )
    }
}
