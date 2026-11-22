//! Which copy of a private attribute an `is rw` method's bare `$!x` tail
//! names when a parent and a child both declare it.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// Whether the scalar attribute `bare` of an instance is stored under
    /// owner-qualified keys (`Owner\0x` / `Owner\0$x`), which happens when a
    /// parent and a child both declare a private `$!x` (ADR-0121). A store by
    /// the bare name then reaches neither copy; only the access's owner names
    /// the right one.
    // Cost: O(a), a = the instance's attributes.
    pub(crate) fn attr_stored_per_owner(map: &crate::value::AttrMap, bare: &str) -> bool {
        map.keys().any(|key| {
            key.with_str(|key| {
                key.split_once('\0')
                    .is_some_and(|(_, name)| name.strip_prefix('$').unwrap_or(name) == bare)
            })
        })
    }

    /// The attribute key an `is rw` method `method` of `class_name`, whose body
    /// is a bare `$!bare`, stores into on `target` (#18558):
    ///
    /// - `None`: the attribute is not stored per owner; the bare name is right.
    /// - `Some(Some((key, owner)))`: the copy of `owner`, the class that
    ///   declares the method, whose declaration also types the store.
    /// - `Some(None)`: stored per owner, but the method's owner has no copy of
    ///   its own; the caller writes through the container the method returns.
    // Cost: O(a) for the per-owner probe, plus one method resolution.
    pub(crate) fn rw_method_owner_attr_key(
        &mut self,
        target: &Value,
        class_name: &str,
        method: &str,
        bare: &str,
    ) -> Result<Option<Option<(String, String)>>, RuntimeError> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return Ok(None);
        };
        if !Self::attr_stored_per_owner(&attributes.as_map(), bare) {
            return Ok(None);
        }
        let owner = self
            .resolve_method_with_owner(class_name, method, &[])?
            .map(|(owner, _)| owner);
        let map = attributes.as_map();
        let key = Self::attr_key_in_map(owner, crate::symbol::Symbol::intern(bare), true, '$', &map)
            .filter(|key| key.with_str(|k| k.contains('\0')))
            .zip(owner)
            .map(|(key, owner)| (key.resolve(), owner.resolve()));
        Ok(Some(key))
    }
}
