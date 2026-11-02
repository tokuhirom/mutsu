//! A string interpolation in the scope of a user `infix:<~>` (#15932).
//!
//! `"A: $a"` is `"A: " ~ $a` as far as operator overloading goes: when a user
//! `multi infix:<~>` is in scope, its candidates are asked with the pieces as
//! they are (an object whose `.Str` answers itself is not stringified first),
//! and only a pair no candidate takes is stringified the ordinary way.

use super::*;

impl Interpreter {
    /// Build the string of the `n` pieces on top of the stack.
    // Cost: O(1) over the plain op when no user `infix:<~>` is in scope;
    // otherwise O(n) candidate rankings plus the winners' calls.
    pub(crate) fn exec_string_concat_op(
        &mut self,
        n: u32,
        part_names: &[Option<crate::symbol::Symbol>],
    ) -> Result<(), RuntimeError> {
        if n < 2
            || self.dispatch.string_concat_user_fold
            || !self.user_infix_override("infix:<~>")
        {
            return self.exec_string_concat_plain(n, part_names);
        }
        let start = self.stack.len() - n as usize;
        let mut pieces = self.stack.drain(start..);
        let Some(mut acc) = pieces.next() else {
            return Ok(());
        };
        let rest: Vec<Value> = pieces.collect();
        self.dispatch.string_concat_user_fold = true;
        let folded = self.fold_pieces_through_user_concat(&mut acc, rest);
        self.dispatch.string_concat_user_fold = false;
        folded?;
        self.stack.push(acc);
        Ok(())
    }

    fn fold_pieces_through_user_concat(
        &mut self,
        acc: &mut Value,
        rest: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        for next in rest {
            match self.try_user_concat(acc, &next)? {
                Some(v) => *acc = v,
                None => {
                    let left = std::mem::replace(acc, Value::NIL);
                    self.stack.push(left);
                    self.stack.push(next);
                    self.exec_string_concat_plain(2, &[None, None])?;
                    *acc = self.stack.pop().unwrap_or(Value::NIL);
                }
            }
        }
        Ok(())
    }
}
