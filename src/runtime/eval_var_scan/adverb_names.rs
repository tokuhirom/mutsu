//! Extended names whose colonpair value is evaluated at BEGIN time
//! (`my $y:a(1)`, `my $y:a[1,2]`, `my $a:foo«$c»`) in the `EVAL` scan
//! (#18612).
//!
//! The parser leaves such a value marked with
//! [`crate::adverb_name::INTERP_MARK`] for the compiler to canonicalize
//! (`y:a\u{1}(1)\u{1}` becomes `y:a<1>`), and the scan runs before the
//! compiler does. A marked value that is a list of literals (`(1)`,
//! `[1, 'x']`) is canonicalized here as the compiler would (`<1>`, `<1 x>`);
//! a name with any other marked value is recorded by the part before it, and
//! a use whose canonical name (`y:a<1>`) starts with that part followed by a
//! value counts as declared: what the value evaluates to is the compiler's to
//! decide.

use crate::adverb_name::INTERP_MARK;

/// The key a marked declaration is recorded under: its canonical name when
/// every marked value is a list of literals (`y:a<1>`), else the part before
/// the first marked value (`y:a\u{1}`). `None` for a name without one.
// Cost: O(n), n = length of `name`.
pub(super) fn marked_key(name: &str) -> Option<String> {
    let at = name.find(INTERP_MARK)?;
    Some(crate::adverb_name::literal_canonical(name).unwrap_or_else(|| format!("{}{INTERP_MARK}", &name[..at])))
}

/// The keys a use of the canonical name `name` (`y:a<1>`) could have been
/// declared under: the name before each colonpair value's `<`, each followed
/// by the mark.
// Cost: O(n), n = length of `name`.
pub(super) fn use_keys(name: &str) -> impl Iterator<Item = String> + '_ {
    name.match_indices('<')
        .map(move |(at, _)| format!("{}{INTERP_MARK}", &name[..at]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn marked_declaration_matches_canonical_use() {
        assert_eq!(marked_key("y:a\u{1}(1)\u{1}").as_deref(), Some("y:a<1>"));
        assert_eq!(
            marked_key("y:a\u{1}(1, 'x')\u{1}:b<c>").as_deref(),
            Some("y:a<1 x>:b<c>")
        );
        let key = marked_key("y:a\u{1}(1+1)\u{1}").unwrap();
        assert!(use_keys("y:a<2>").any(|k| k == key));
        assert!(use_keys("y:b<2>").all(|k| k != key));
        assert!(marked_key("y:a<1>").is_none());
    }
}
