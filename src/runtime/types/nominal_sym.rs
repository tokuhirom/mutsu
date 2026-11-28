//! The nominal type object of a typed variable's constraint, as a symbol.
use super::*;

impl Interpreter {
    /// [`Self::nominal_type_object_name_for_constraint`] as the type object's
    /// symbol, the form every caller that seeds a typed scalar wants.
    ///
    /// A plain type-name spelling (`Str`, `Tag`, `YAMLish::Tag`: identifier
    /// characters and `::` separators only) is what the general function
    /// returns unchanged unless an env binding renames it, the package chain
    /// re-anchors an unregistered short name, or a user `subset` stands for its
    /// base -- and those are the three things asked here. Answering the rest
    /// without the general function saves its owned copy of the spelling and
    /// the re-interning of its result, two allocations and a symbol-table
    /// probe per Nil stored into a typed scalar (#12917).
    // Cost: O(|constraint|) for a plain spelling (one env probe, at most two
    // registry probes); otherwise as `nominal_type_object_name_for_constraint`.
    pub(crate) fn nominal_type_object_sym_for_constraint(&self, constraint: &str) -> Symbol {
        if Self::is_plain_type_name_spelling(constraint) {
            let sym = Symbol::intern(constraint);
            let rebound = crate::symbol::maybe_env_key(sym)
                && matches!(
                    self.env.get_sym(sym).map(Value::view),
                    Some(ValueView::Package(bound)) if bound != sym
                );
            let reanchored = !crate::qualified::is_qualified(sym)
                && !self.has_type_direct(constraint)
                && !Self::is_builtin_type(constraint);
            if !rebound && !reanchored && !self.is_subset_type_name(constraint) {
                return sym;
            }
        }
        Symbol::intern(&self.nominal_type_object_name_for_constraint(constraint))
    }

    /// Identifier characters and `::` separators only, not starting with
    /// `::` (a type capture) or `CORE::` (a pin the general function strips).
    // Cost: O(|name|).
    fn is_plain_type_name_spelling(name: &str) -> bool {
        let bytes = name.as_bytes();
        if bytes.is_empty() || name.starts_with("CORE::") {
            return false;
        }
        let mut i = 0;
        while i < bytes.len() {
            let b = bytes[i];
            if b == b':' {
                if i == 0 || bytes.get(i + 1) != Some(&b':') || i + 2 >= bytes.len() {
                    return false;
                }
                i += 2;
                continue;
            }
            if !(b.is_ascii_alphanumeric() || b == b'_' || b == b'-' || b == b'\'' || b >= 0x80)
            {
                return false;
            }
            i += 1;
        }
        true
    }
}
