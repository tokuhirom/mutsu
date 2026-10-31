//! A `constant` bound to a declared package is a qualifier of its own.
//!
//! `class Outer::Inner { enum Status <Started> }; constant E = Outer::Inner`
//! makes `E` the type and `E::Status::Started` the enum value: rakudo resolves
//! the constant at compile time, so it renders `E` as a `Type::Simple` and every
//! name under it the way it renders the same name under `Outer::Inner`
//! (measured on rakudo 2026.09).

use std::collections::HashMap;

use crate::qualified::qualified;
use crate::scope::DeclaredKind;
use crate::symbol::Symbol;

/// A constant the unit declares over a bare package name, in every spelling
/// that reaches it (`E`, `M::E` inside `module M`).
pub(super) struct PackageAlias {
    pub(super) spellings: Vec<Symbol>,
    pub(super) target: String,
}

/// Give each alias whose target is a type the unit declares the target's
/// kind and a copy of every name declared under the target.
// Cost: O(a * n * d), a = number of aliases, n = number of declared names,
// d = segments of a name.
pub(super) fn apply(aliases: &[PackageAlias], out: &mut HashMap<String, DeclaredKind>) {
    for alias in aliases {
        if out.get(&alias.target) != Some(&DeclaredKind::Type) {
            continue;
        }
        let target = Symbol::intern(&alias.target);
        let nested: Vec<(Symbol, DeclaredKind)> = out
            .iter()
            .filter(|(name, _)| name.as_str() != alias.target)
            .map(|(name, kind)| (Symbol::intern(name), *kind))
            .collect();
        for spelling in &alias.spellings {
            out.insert(spelling.resolve(), DeclaredKind::Type);
            for (name, kind) in &nested {
                if let Some(rebased) = rebase(*name, target, *spelling) {
                    out.entry(rebased.resolve()).or_insert(*kind);
                }
            }
        }
    }
}

/// `name` with its package prefix `target` replaced by `alias`, or `None`
/// when `name` is not declared under `target` (`Outer::Inner::Status`, with
/// `Outer::Inner` → `E`, is `E::Status`).
// Cost: O(d), d = number of `::` segments of `name`.
fn rebase(name: Symbol, target: Symbol, alias: Symbol) -> Option<Symbol> {
    if name == target {
        return Some(alias);
    }
    let parent = crate::qualified::package_parent(name)?;
    let rebased = rebase(parent, target, alias)?;
    Some(qualified(rebased, crate::qualified::unqualified_part(name)))
}
