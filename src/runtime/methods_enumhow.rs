//! `Perl6::Metamodel::EnumHOW` introspection helpers.
//!
//! `.^enum_values`, `.^elems`, `.^enum_from_value` and `.^enum_value_list` are
//! EnumHOW-only: they answer from the enum's declared `(key, value)` variant
//! list, in declaration order. On any other HOW they do not exist at all, so
//! the dispatch must report a missing method rather than fall through to the
//! generic value-level handler of the same name.

use super::*;
use crate::value::EnumValue;

impl Interpreter {
    /// The declared variants of the enum this MOP call's invocant type object
    /// stands for, or `None` when the invocant is not an enum type.
    pub(crate) fn enum_how_variants(
        &mut self,
        type_value: &Value,
    ) -> Option<Vec<(String, EnumValue)>> {
        let name = match type_value.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Str(name) => name.to_string(),
            ValueView::Enum { enum_type, .. } => enum_type.resolve(),
            _ => self.mop_receiver_owner(type_value),
        };
        self.registry().enum_types.get(&name).cloned()
    }

    /// The error a non-enum HOW answers for an EnumHOW-only method. Raku
    /// reports these as an unresolvable caller on the HOW itself (e.g.
    /// `elems(Perl6::Metamodel::ClassHOW:D: C:U)`); mutsu reports the
    /// equivalent missing-method error, naming the HOW that lacks it.
    pub(crate) fn enum_how_method_missing(method: &str, type_value: &Value) -> RuntimeError {
        let owner = match type_value.view() {
            ValueView::Package(name) => name.resolve(),
            _ => crate::runtime::utils::value_type_name(type_value).to_string(),
        };
        RuntimeError::new(format!(
            "Cannot resolve caller {method}({owner}.HOW: {owner}); \
             {method} is only defined on Metamodel::EnumHOW"
        ))
    }
}
