//! The first-step guard of an [`LtmNfa`](super::regex_ltm_nfa::LtmNfa) node
//! (#10710).
//!
//! A ranking runs every root of a proto or a `|` at the same position, and
//! almost every root fails at its first leaf: `value`'s seven candidates each
//! start with a different literal or class, and only one of them can start at
//! the character in front of the run. The leaf's matcher is a call into the
//! single-atom matcher (`match_consuming_atom`, about 160 instructions), made
//! to be told "no".
//!
//! A node whose every path begins with a leaf that consumes a character (a
//! literal, a class, `\n`), reached through splits alone, has a *guard*: the
//! set of characters that leaf could match at a start position, which is the
//! prefilter's `FirstSet` (ADR-0099), built the same way: from the engine's own
//! predicates over the ASCII range, never from a table of its own. A thread
//! that comes to such a node at a character the guard rejects dies there, and
//! the run drops it without asking the matcher: the leaves it would have
//! asked all fail, a failing leaf records nothing (no end, no fate, no `||`,
//! no `_LL` literal), so what the run reports is exactly what it reported
//! before.
//!
//! What has a guard is deliberately narrow, because a guard that rejects a
//! thread which would have gone on is a wrong measurement, while one that
//! admits a thread which dies is only a wasted call:
//!
//! - a `Consume` leaf of a literal, a grapheme literal, a class or `\n`, with no
//!   `:i` (the first-set constructors cover `:i`, but nothing here needs it
//!   yet, and the exact case-fold closure is the part with a history of
//!   surprises). `<+a -b>` composite classes may run grammar tokens that record
//!   fates, `.` and `\N` admit everything, and a property test may be a regex:
//!   none is guarded;
//! - a split whose every target is guarded (the union), so a root or an
//!   alternation of leaves is dropped at once;
//! - nothing else: a call, a return, a fate, an anchor, a `<.ws>` or a
//!   path that can reach an accept without consuming is a node that does
//!   something besides test a character.

use super::super::*;
use super::regex_ltm_nfa::{LeafKind, NfaNode};
use super::regex_prefilter_firstset::{
    FirstSet, class_first_set, literal_first_set, newline_first_set,
};

/// Splits nested deeper than this have no guard. Each target of a split is a
/// distinct node, so this bounds the work and never decides a measurement.
const MAX_DEPTH: usize = 32;

#[derive(Clone)]
enum Slot {
    Todo,
    /// On the path being resolved: a split that loops back to itself without
    /// consuming has no guard.
    Active,
    Done(Option<FirstSet>),
}

/// The guard of every node of `nodes`, `None` for a node that has none.
// Cost: O(s + t), s = nodes, t = split targets (each node is resolved once).
pub(super) fn node_guards(nodes: &[NfaNode]) -> Vec<Option<FirstSet>> {
    let mut slots = vec![Slot::Todo; nodes.len()];
    (0..nodes.len())
        .map(|node| resolve(nodes, node, &mut slots, 0))
        .collect()
}

fn resolve(nodes: &[NfaNode], node: usize, slots: &mut [Slot], depth: usize) -> Option<FirstSet> {
    match &slots[node] {
        Slot::Done(guard) => return guard.clone(),
        Slot::Active => return None,
        Slot::Todo => {}
    }
    slots[node] = Slot::Active;
    let guard = match &nodes[node] {
        NfaNode::Leaf {
            atom,
            ic: false,
            kind: LeafKind::Consume,
            ..
        } => atom_guard(atom),
        NfaNode::Split(targets) if !targets.is_empty() && depth < MAX_DEPTH => {
            let mut union = FirstSet::empty();
            let mut guarded = true;
            for &target in targets {
                match resolve(nodes, target as usize, slots, depth + 1) {
                    Some(guard) => union.union(&guard),
                    None => {
                        guarded = false;
                        break;
                    }
                }
            }
            guarded.then_some(union)
        }
        _ => None,
    }
    // A guard that admits everything tests nothing.
    .filter(|guard| !guard.is_universal());
    slots[node] = Slot::Done(guard.clone());
    guard
}

/// What the matcher of a `Consume` leaf's `atom` can accept at a start
/// position, for the atoms the module comment names.
fn atom_guard(atom: &RegexAtom) -> Option<FirstSet> {
    match atom {
        RegexAtom::Literal(ch) => Some(literal_first_set(*ch, false)),
        // The whole grapheme has to match, so its first character has to.
        RegexAtom::LiteralGrapheme(grapheme) => {
            Some(literal_first_set(grapheme.chars().next()?, false))
        }
        RegexAtom::CharClass(class) => Some(class_first_set(class, false)),
        RegexAtom::Newline => Some(newline_first_set()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::regex_parse::RegexParseMode;

    /// A guard is a superset of what its leaf matches: for every atom with a
    /// guard and every character at the start of a subject (alone, or followed
    /// by what can change how it groups), the matcher accepting the atom
    /// implies the guard admitting the character.
    #[test]
    fn a_guard_admits_every_character_its_leaf_matches() {
        let mut interp = Interpreter::new();
        let pkg = Symbol::intern("");
        let follows: [&[char]; 5] = [
            &[],
            &['\n'],
            &['\u{301}'],
            &['\u{94D}', '\u{937}'],
            &['b', 'c'],
        ];
        let mut guarded = 0;
        for pattern in [
            "a",
            "'abc'",
            "\\d",
            "\\w",
            "\\s",
            "\\n",
            "<[a..z]>",
            "<-[a..z]>",
            "<[a..z] - [aeiou]>",
            "<[\u{915}\u{94D}\u{937}]>",
            "<[\\n]>",
            "<-[\\n]>",
            "\\x[301]",
        ] {
            let parsed = interp
                .parse_regex_with_mode(pattern, RegexParseMode::Match)
                .expect("the pattern parses");
            let atom = &parsed.tokens[0].atom;
            let Some(guard) = atom_guard(atom) else {
                continue;
            };
            guarded += 1;
            for cp in (0u32..0x400).chain([0x661, 0x2028, 0x3000, 0x1F600]) {
                let Some(c) = char::from_u32(cp) else {
                    continue;
                };
                for follow in follows {
                    let mut chars = vec![c];
                    chars.extend_from_slice(follow);
                    if interp
                        .match_consuming_atom(atom, &chars, 0, pkg, false)
                        .is_some()
                    {
                        assert!(
                            guard.contains(c),
                            "{pattern}: the matcher accepts {c:?} followed by {follow:?}, the guard rejects it"
                        );
                    }
                }
            }
        }
        assert!(guarded >= 10, "most of the atoms have a guard ({guarded})");
    }
}
