//! Membership in a Unicode property class written as the body of `\p{...}`
//! (`Alphabetic`, `Script=Latin`, `Word_Break=Extend`), without a regex
//! engine.
//!
//! `regex-syntax` resolves the class to its sorted codepoint ranges once per
//! name; membership is then a binary search. This replaced compiling
//! `^\p{...}$` with the `regex` crate and matching a one-character string
//! (#10439). Both read the same `regex-syntax` Unicode tables, so the answers
//! are identical.

use regex_syntax::hir::{Class, HirKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type Ranges = Rc<[(char, char)]>;

thread_local! {
    /// Resolved classes by property text; `None` records a name
    /// `regex-syntax` does not know, so it is not re-parsed per character.
    ///
    /// Thread-local, like the compiled-regex caches it replaces: these are
    /// per-character hot paths (`.uniprop`, UAX #29 segmentation) a lock would
    /// sit in.
    static CLASSES: RefCell<HashMap<String, Option<Ranges>>> = RefCell::new(HashMap::new());
}

fn resolve(prop: &str) -> Option<Ranges> {
    let hir = regex_syntax::Parser::new()
        .parse(&format!(r"\p{{{prop}}}"))
        .ok()?;
    match hir.kind() {
        HirKind::Class(Class::Unicode(class)) => Some(
            class
                .ranges()
                .iter()
                .map(|r| (r.start(), r.end()))
                .collect(),
        ),
        // A one-codepoint class (`Zl` is just U+2028, GCB=LF just U+000A)
        // comes back folded into a literal.
        HirKind::Literal(lit) => {
            let mut chars = std::str::from_utf8(&lit.0).ok()?.chars();
            let c = chars.next()?;
            chars.next().is_none().then(|| [(c, c)].into())
        }
        _ => None,
    }
}

/// Does `c` belong to the class `\p{prop}`? `None` when `prop` names no
/// property `regex-syntax` knows.
// Cost: O(log R) after the first call per `prop`, R = ranges in the class;
// the first call resolves the class, O(R).
pub(crate) fn in_property_class(prop: &str, c: char) -> Option<bool> {
    CLASSES.with(|cache| {
        let mut cache = cache.borrow_mut();
        let ranges = match cache.get(prop) {
            Some(entry) => entry.clone(),
            None => {
                let entry = resolve(prop);
                cache.insert(prop.to_string(), entry.clone());
                entry
            }
        }?;
        Some(
            ranges
                .binary_search_by(|&(lo, hi)| {
                    if hi < c {
                        std::cmp::Ordering::Less
                    } else if lo > c {
                        std::cmp::Ordering::Greater
                    } else {
                        std::cmp::Ordering::Equal
                    }
                })
                .is_ok(),
        )
    })
}

#[cfg(test)]
mod tests {
    use super::in_property_class;

    #[test]
    fn agrees_with_the_regex_probe_it_replaced() {
        for prop in [
            "Alphabetic",
            "White_Space",
            "Emoji",
            "Emoji_Presentation",
            "Extended_Pictographic",
            "Word_Break=Extend",
            "Grapheme_Cluster_Break=SpacingMark",
            "Script=Latin",
            "Script=Han",
            "General_Category=Nd",
            "Nd",
            "Letter",
            "Punctuation",
            "Uppercase",
            "Ideographic",
            "Zl",
            "Zp",
            "Grapheme_Cluster_Break=LF",
            "Grapheme_Cluster_Break=CR",
        ] {
            let re = regex::Regex::new(&format!(r"^\p{{{prop}}}$")).unwrap();
            let mut buf = [0u8; 4];
            // Dense over the BMP's first block of scripts (controls, the line
            // and paragraph separators), sampled beyond it.
            let probes = (0..0x3000u32).chain((0x3000..=0x10FFFF).step_by(7));
            for c in probes.filter_map(char::from_u32) {
                assert_eq!(
                    in_property_class(prop, c),
                    Some(re.is_match(c.encode_utf8(&mut buf))),
                    "{prop} U+{:04X}",
                    c as u32
                );
            }
        }
    }

    #[test]
    fn an_unknown_property_is_none() {
        assert_eq!(in_property_class("No_Such_Property", 'a'), None);
        assert_eq!(in_property_class("Line_Break=ID", 'a'), None);
        assert!(regex::Regex::new(r"^\p{No_Such_Property}$").is_err());
    }
}
