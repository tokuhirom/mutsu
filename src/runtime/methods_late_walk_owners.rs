//! The owners a late-row walk reads (ADR-11276 §9.143): the receiver's MRO with
//! the built-in roles each class composes, as `.^can` sees them.
//!
//! mutsu's MRO for a built-in type is its class chain only (`Buf[uint8]`,
//! `Any`, `Mu`), while Rakudo answers a method from a role the class composes
//! too (`Blob[uint8]`'s `unpack`, `Setty`'s `default`). The catalog records
//! those roles, so this list puts each one right after the class that composes
//! it, ahead of that class's parent, where Rakudo's method resolution flattens
//! a role's methods. An enum (its type object or a value) has `Enumeration`
//! after its own type.

use super::*;

impl Interpreter {
    /// The owners of a late-row walk for `target`, most specific first.
    // Cost: O(m * r), m = MRO length, r = roles a catalog class composes (one
    // catalog hash probe per class).
    pub(crate) fn late_walk_owners(&mut self, target: &Value) -> Vec<String> {
        let mro = self.classhow_mro_unhidden_names(target);
        let strs: Vec<&str> = mro.iter().map(String::as_str).collect();
        let mut owners = Vec::with_capacity(mro.len() + 4);
        for (m, class) in mro.iter().enumerate() {
            owners.push(class.clone());
            if m == 0 && self.is_enum_receiver(target) {
                owners.push("Enumeration".to_string());
            }
            let Some(info) = crate::builtin_types::catalog::builtin_type_info(class) else {
                continue;
            };
            for role in info.roles {
                if crate::builtin_types::catalog::composes_role_at(&strs, m, role) {
                    let bare = role.split('[').next().unwrap_or(role);
                    if !owners.iter().any(|owner| owner == bare) {
                        owners.push(bare.to_string());
                    }
                }
            }
        }
        owners
    }

    /// Whether `target` is an enum's type object or one of its values.
    // Cost: O(1) (one registry probe).
    fn is_enum_receiver(&self, target: &Value) -> bool {
        match target.view() {
            // `Bool` is the core's own enum.
            ValueView::Enum { .. } | ValueView::Bool(_) => true,
            ValueView::Package(name) => {
                name == "Bool" || self.registry().enum_types.contains_key(&*name.resolve())
            }
            _ => false,
        }
    }
}
