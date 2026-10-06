//! Compare-and-swap on an attribute reached through an rw accessor:
//! `cas($obj.attr, $expected, $new)`.

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Args: [invocant, attribute_name_str, expected, new_val].
    /// The swap happens under the instance's attribute-cell write lock, which
    /// is the atomic primitive every alias and thread of the object shares.
    // Cost: O(a), a = attributes of the invocant (key lookup).
    pub(super) fn builtin_cas_attr(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let [target, name, expected, new_val] = args.as_slice() else {
            return Err(RuntimeError::new(
                "__mutsu_cas_attr requires 4 arguments (invocant, attribute, expected, new)",
            ));
        };
        let ValueView::Instance { attributes, .. } = target.view() else {
            return Err(RuntimeError::new(format!(
                "Cannot cas the attribute '{}' of a non-instance",
                name.to_string_value()
            )));
        };
        let bare = name.to_string_value();
        let key = {
            let map = attributes.as_map();
            if map.contains_key(&bare) {
                bare
            } else {
                let suffix = format!("\0{bare}");
                map.keys()
                    .map(|k| k.resolve().to_string())
                    .find(|k| k.ends_with(&suffix))
                    .unwrap_or(bare)
            }
        };
        let (current, _) = attributes.compare_and_swap(
            &key,
            |cur| Self::cas_retry_matches(cur, expected),
            new_val.clone(),
        );
        Ok(current)
    }
}
