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

/// The owner chain of the built-in code class `class`, nearest first.
// Cost: O(1).
fn chain_of(class: &str) -> Option<&'static [&'static str]> {
    Some(match class {
        "Sub" => &["Sub", "Routine", "Block", "Code"],
        "Method" => &["Method", "Routine", "Block", "Code"],
        "Submethod" => &["Submethod", "Routine", "Block", "Code"],
        "Routine" => &["Routine", "Block", "Code"],
        "Regex" => &["Regex", "Method", "Routine", "Block", "Code"],
        "Block" => &["Block", "Code"],
        "WhateverCode" => &["WhateverCode", "Code"],
        "Code" => &["Code"],
        _ => return None,
    })
}

/// The owners the rows reachable from `target` are found under, or `None` when
/// `target` is not a code object: a `Sub` value of any kind, a `&name`
/// handle, a regex, or an instance of a code class (a method object).
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Sub(_)
        | ValueView::WeakSub(_)
        | ValueView::Routine { .. }
        | ValueView::Regex(_)
        | ValueView::RegexWithAdverbs(..) => chain_of(crate::value::type_name::value_type_name(target)),
        // A mixin (`Method+{R}`) reaches the rows of the class it derives from
        // after its own and its roles' methods (ADR-14477 §2.4).
        ValueView::Instance { class_name, .. } => {
            let class = class_name.resolve();
            chain_of(class.split_once('+').map_or(class.as_str(), |(base, _)| base))
        }
        _ => None,
    }
}
