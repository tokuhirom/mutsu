//! A `constant` bound to a declared package is a qualifier of its own.
//!
//! `class Outer::Inner { enum Status <Started> }; constant E = Outer::Inner`
//! makes `E` the type and `E::Status::Started` the enum value: rakudo resolves
//! the constant at compile time, so it renders `E` as a `Type::Simple` and every
//! name under it the way it renders the same name under `Outer::Inner`
//! (measured on rakudo 2026.09).

use std::collections::HashMap;

use super::DeclaredKind;
use crate::qualified::qualified;
use crate::symbol::Symbol;

/// A constant the unit declares over a bare package name, in every spelling
/// that reaches it (`E`, `M::E` inside `module M`).
pub(super) struct PackageAlias {
    pub(super) spellings: Vec<Symbol>,
    pub(super) target: String,
}

/// Give each alias whose target is a type the unit declares the target's
/// kind and a copy of every name declared under the target.
// Cost: O(a * n), a = number of aliases, n = number of declared names.
pub(super) fn apply(aliases: &[PackageAlias], out: &mut HashMap<String, DeclaredKind>) {
    for alias in aliases {
        if out.get(&alias.target) != Some(&DeclaredKind::Type) {
            continue;
        }
        let nested: Vec<(Symbol, DeclaredKind)> = out
            .iter()
            .filter_map(|(name, kind)| {
                let rest = name.strip_prefix(alias.target.as_str())?.strip_prefix("::")?;
                Some((Symbol::intern(rest), *kind))
            })
            .collect();
        for spelling in &alias.spellings {
            out.insert(spelling.resolve(), DeclaredKind::Type);
            for (rest, kind) in &nested {
                out.entry(qualified(*spelling, *rest).resolve())
                    .or_insert(*kind);
            }
        }
    }
}
