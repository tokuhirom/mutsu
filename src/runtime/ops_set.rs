use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// `left (==) right`: both operands, promoted to the higher of their
    /// levels, hold the same elements with the same weights. Reads the
    /// operands with the same coercions as every other set operator
    /// (`runtime::utils::set_operand`), so `(a => 2,) ≡ bag(<a a>)`.
    pub(crate) fn apply_set_equality(left: &Value, right: &Value) -> Result<bool, RuntimeError> {
        let (l, r) = (set_operand(left), set_operand(right));
        if is_lazy_set_operand(&l) || is_lazy_set_operand(&r) {
            return Err(RuntimeError::cannot_lazy_with_action("coerce", "Set"));
        }
        let mut scratch = ValueMap::default();
        Ok(match set_level(&l).max(set_level(&r)) {
            SetLevel::Mix => {
                operand_mix_weights(&l, &mut scratch) == operand_mix_weights(&r, &mut scratch)
            }
            SetLevel::Bag => {
                operand_bag_counts(&l, &mut scratch) == operand_bag_counts(&r, &mut scratch)
            }
            SetLevel::Set => coerce_to_set(&l, &mut scratch) == coerce_to_set(&r, &mut scratch),
        })
    }
}
