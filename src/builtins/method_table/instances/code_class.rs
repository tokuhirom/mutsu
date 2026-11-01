//! The owners a code value's methods are found under (ADR-14477).
//!
//! In Rakudo a code object is an instance of an ordinary class and its
//! methods come from that class's MRO. [`owners_of`] decodes the receiver to
//! its built-in code class the way `type_name` does, so the answer cannot drift
//! from `.^name`, and names the class's owner chain, nearest first. Every row
//! of the `Code`, `Block`, `Routine`, `Method` and `Regex` owners is reached
//! through this one lookup: `Block.multi` finds no `Routine` row, and
//! `Regex.multi` does.

use crate::value::{Value, ValueView};

const OWNER_CHAINS: [&[&str]; 9] = [
    &[],
    &["Sub", "Routine", "Block", "Code"],
    &["Method", "Routine", "Block", "Code"],
    &["Submethod", "Routine", "Block", "Code"],
    &["Routine", "Block", "Code"],
    &["Regex", "Method", "Routine", "Block", "Code"],
    &["Block", "Code"],
    &["WhateverCode", "Code"],
    &["Code"],
];

/// The index is derived only from the immutable class spelling. Zero also
/// memoizes a negative answer for ordinary, non-code instance classes.
// Cost: O(1), eight comparisons against bounded core class names.
fn class_index(class: &str) -> u16 {
    OWNER_CHAINS
        .iter()
        .position(|owners| owners.first().is_some_and(|&owner| owner == class))
        .and_then(|index| u16::try_from(index).ok())
        .unwrap_or(0)
}

// Cost: O(n), n = class-name bytes (unmemoized classification).
fn instance_class_index(class: &str) -> u16 {
    class_index(class.split_once('+').map_or(class, |(base, _)| base))
}

// Cost: O(1).
fn chain_at(index: u16) -> Option<&'static [&'static str]> {
    let chain = OWNER_CHAINS[usize::from(index)];
    (!chain.is_empty()).then_some(chain)
}

/// The owner chain of the built-in code class `class`, nearest first.
// Cost: O(1).
fn chain_of(class: &str) -> Option<&'static [&'static str]> {
    chain_at(class_index(class))
}

/// The owners the rows reachable from `target` are found under, or `None` when
/// `target` is not a code object: a `Sub` value of any kind, a `&name`
/// handle, a regex, or an instance of a code class (a method object).
// Cost: O(1) on an instance-class memo hit; O(n) otherwise, n = class-name bytes.
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Sub(_)
        | ValueView::WeakSub(_)
        | ValueView::Routine { .. }
        | ValueView::Regex(_)
        | ValueView::RegexWithAdverbs(..) => {
            chain_of(crate::value::type_name::value_type_name(target))
        }
        // A mixin (`Method+{R}`) reaches the rows of the class it derives from
        // after its own and its roles' methods (ADR-14477 §2.4).
        ValueView::Instance { class_name, .. } => {
            static CLASSES: crate::symbol::SymbolMemo = crate::symbol::SymbolMemo::new();
            chain_at(CLASSES.get(class_name, instance_class_index))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;

    #[test]
    fn instance_classification_keeps_code_mixin_owners_and_negative_answers() {
        let method =
            Value::make_instance(Symbol::intern("Method+{Tag}"), crate::value::AttrMap::new());
        let ordinary =
            Value::make_instance(Symbol::intern("Plain+{Tag}"), crate::value::AttrMap::new());
        for _ in 0..2 {
            assert_eq!(owners_of(&method), Some(OWNER_CHAINS[2]));
            assert!(owners_of(&ordinary).is_none());
        }
    }
}
