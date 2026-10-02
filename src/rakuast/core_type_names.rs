//! The type names the CORE setting declares, as RakuAST resolves a bareword.
//!
//! Rakudo resolves a bareword against the setting at parse time. A name that
//! is a type object there, such as `X::AdHoc` or `IO::Path`, renders as
//! `Type::Simple` (measured on 2026.09). mutsu's parser leaves it as
//! `Expr::BareWord`, so the converter needs the setting's type names to choose
//! the same node. `core_type_names.txt` is generated from rakudo by
//! `scripts/gen-core-type-names.raku`; regenerate it rather than editing it.

use std::collections::HashSet;
use std::sync::LazyLock;

static NAMES: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    include_str!("core_type_names.txt")
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .collect()
});

/// Whether `name` is a type the CORE setting declares.
// Cost: O(k), k = length of `name` (one hash lookup; the set is built once).
pub(super) fn contains(name: &str) -> bool {
    NAMES.contains(name)
}

#[cfg(test)]
mod tests {
    use super::contains;

    #[test]
    fn knows_setting_types_and_not_terms() {
        assert!(contains("Int"));
        assert!(contains("X::AdHoc"));
        assert!(contains("IO::Socket::Async"));
        // A defined constant is a `Term::Name`, not a type.
        assert!(!contains("IterationEnd"));
        assert!(!contains("NoSuchType"));
    }
}
