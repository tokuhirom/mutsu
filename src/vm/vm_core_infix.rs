//! Core infix bodies that are not pure reduction-table operations.
use super::*;

impl Interpreter {
    /// Execute the existing opcode body after user candidates have declined.
    /// Never look the operator up by name again: doing so would redispatch to
    /// the same candidate family, including a rejected `where` predicate.
    // Cost: the selected operator's cost; O(1) dispatch and temporary stack space.
    pub(super) fn core_infix_values(
        &mut self,
        name: &str,
        left: &Value,
        right: &Value,
        code: &CompiledCode,
    ) -> Result<Option<Value>, RuntimeError> {
        if !matches!(
            name,
            ".." | "..^"
                | "^.."
                | "^..^"
                | "but"
                | "unicmp"
                | "coll"
                | "?|"
                | "?&"
                | "?^"
                | "<=>"
                | "=~="
                | "o"
                | "..."
                | "...^"
        ) {
            return Ok(None);
        }
        let left = self.fetch_proxy_for_store(Self::unwrap_var_ref_value(left.clone()))?;
        let right = self.fetch_proxy_for_store(Self::unwrap_var_ref_value(right.clone()))?;
        let direct = match name {
            "<=>" => Some(self.spaceship_values(left.clone(), right.clone())),
            "=~=" => Some(self.approx_eq_values(left.clone(), right.clone())),
            "o" => Some(Ok(self.compose_callables(left.clone(), right.clone()))),
            "..." | "...^" => Some(loan_env!(
                self,
                eval_sequence_values(left.clone(), right.clone(), name == "...^")
            )),
            _ => None,
        };
        if let Some(result) = direct {
            return result.map(Some);
        }
        let mark = self.stack.len();
        self.stack.push(left);
        self.stack.push(right);
        let result = match name {
            ".." => self.exec_make_range_op(),
            "..^" => self.exec_make_range_excl_op(),
            "^.." => self.exec_make_range_excl_start_op(),
            "^..^" => self.exec_make_range_excl_both_op(),
            "but" => self.exec_but_mixin_op(code),
            "unicmp" => self.exec_unicmp_op(),
            "coll" => self.exec_coll_op(),
            "?|" => {
                self.exec_bool_bit_or_op();
                Ok(())
            }
            "?&" => {
                self.exec_bool_bit_and_op();
                Ok(())
            }
            "?^" => {
                self.exec_bool_bit_xor_op();
                Ok(())
            }
            _ => unreachable!("the direct operators returned above"),
        };
        let value = result.map(|()| self.stack.pop().expect("an infix body returns one value"));
        self.stack.truncate(mark);
        value.map(Some)
    }
}
