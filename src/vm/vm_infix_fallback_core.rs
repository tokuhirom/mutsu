//! The core bodies of the operators whose generic infix-call route must
//! still coerce and autothread like their opcodes do (#18478).
use super::*;

/// A core binary operator body over two owned operands.
type CoreBinary = fn(&mut Interpreter, Value, Value) -> Result<Value, RuntimeError>;

impl Interpreter {
    /// The core answer of `name` over `call_args` when `name` is a string or
    /// numeric comparison or `~`; `None` for every other operator. Reached
    /// from [`Self::call_infix_fallback`] once user candidates declined.
    // Cost: the operator's own cost (O(p) for a string comparison, p = common
    // prefix; O(n) for `~` over n result bytes); O(1) to decline.
    pub(super) fn core_comparison_or_concat(
        &mut self,
        name: &str,
        call_args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        // `eq ne lt gt le ge` reach here when the compiler kept the generic
        // infix-call route for a user candidate that does not apply at run
        // time (a module's operator that is out of scope here, #18478). The
        // core operator coerces with `.Stringy` and autothreads a Junction
        // operand, which the fallback's reduction table does not.
        if call_args.len() == 2
            && let Some(kind) = super::vm_operator_values::StrCmp::from_op(name)
        {
            return self
                .str_cmp_values_core(
                    kind,
                    Self::unwrap_operand_value(call_args[0].clone()),
                    Self::unwrap_operand_value(call_args[1].clone()),
                )
                .map(Some);
        }
        // `~` likewise, folded through its one implementation (`infix_concat`
        // threads a Junction operand and coerces with `.Stringy`).
        if name == "~" && call_args.len() >= 2 {
            let mut acc =
                self.fetch_proxy_for_store(Self::unwrap_operand_value(call_args[0].clone()))?;
            for rhs in &call_args[1..] {
                let rhs = self.fetch_proxy_for_store(Self::unwrap_operand_value(rhs.clone()))?;
                acc = self.infix_concat(acc, rhs)?;
            }
            return Ok(Some(acc));
        }
        // The numeric comparisons likewise, through the one implementation
        // their opcodes and `&infix:<==>` share.
        if call_args.len() == 2 {
            let compare: Option<CoreBinary> = match name {
                "==" => Some(Self::num_eq_values),
                "!=" | "≠" => Some(Self::num_ne_values),
                "<" => Some(Self::num_lt_values),
                "<=" | "≤" => Some(Self::num_le_values),
                ">" => Some(Self::num_gt_values),
                ">=" | "≥" => Some(Self::num_ge_values),
                _ => None,
            };
            if let Some(compare) = compare {
                let left =
                    self.fetch_proxy_for_store(Self::unwrap_operand_value(call_args[0].clone()))?;
                let right =
                    self.fetch_proxy_for_store(Self::unwrap_operand_value(call_args[1].clone()))?;
                return compare(self, left, right).map(Some);
            }
        }
        Ok(None)
    }
}
