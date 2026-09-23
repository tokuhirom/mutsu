//! `%h{$k} ~= rhs` / `@a[$i] ~= rhs`: the element half of the in-place string
//! append (#9141). `OpCode::ConcatAssignLocal` is the scalar half (#8695).
//!
//! The unfused sequence -- read the element, evaluate the RHS, `Concat`, store
//! -- has the element's string held twice when `Concat` runs: once by the
//! container and once by the stack. Concatenation then has to build a fresh
//! buffer, O(len) per append, so building an n-character string in an element
//! is O(n²) (100k appends of one kana: ~5 s, rakudo ~0.05 s).
//!
//! The fused store (`IndexAssignExprNamed { concat_append: true }`) arrives
//! with `[index, element, rhs]` on the stack and releases the container's
//! reference before appending: it first stores an empty string into the
//! element *through the ordinary fast store lane*, which leaves the stack's
//! copy of the element the buffer's only holder, grows that buffer in place,
//! and stores the result through the same lane. Going through the lane twice,
//! rather than writing into the container directly, is what keeps every store
//! rule it enforces (bound / `Proxy` / typed / shared-across-threads elements,
//! local-slot coherence, `our` and unit-lexical roots) the lane's business:
//! a store the lane would decline is never attempted here.

use super::*;

impl Interpreter {
    /// Run the fused element append. Answers `true` when the store is done
    /// (its result pushed, exactly as the ordinary store pushes it); `false`
    /// when the stack has been turned into the ordinary store's
    /// `[index, element ~ rhs]` and the caller should run that store.
    pub(super) fn exec_index_concat_append_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Result<bool, RuntimeError> {
        let rhs = self.stack.pop().unwrap_or(Value::NIL);
        let element = self.stack.pop().unwrap_or(Value::NIL);
        match self.try_index_concat_append_in_place(
            code,
            name_idx,
            is_positional,
            target_slot,
            element,
            &rhs,
        )? {
            Ok(()) => Ok(true),
            Err(element) => {
                // Declined without touching anything: this is the unfused
                // `Concat`, run on the values the unfused sequence would have
                // had on the stack.
                self.stack.push(element);
                self.stack.push(rhs);
                self.exec_concat_op()?;
                Ok(false)
            }
        }
    }

    /// The in-place path. `Ok(Err(element))` hands the element back untouched
    /// when any gate declines.
    // Cost: amortized O(m), m = chars of the RHS, when the element's string is
    // held by nothing else; O(n + m) when it is shared (`my $b = %h<k>`), n =
    // chars already in the element -- the copy Raku's value semantics require.
    fn try_index_concat_append_in_place(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
        element: Value,
        rhs: &Value,
    ) -> Result<Result<(), Value>, RuntimeError> {
        // Both sides plain `Str`: an allomorph or mixin views as `Mixin`, a user
        // object needs `.Stringy`, a junction threads -- all `Concat`'s job.
        let ValueView::Str(suffix) = rhs.view() else {
            return Ok(Err(element));
        };
        if !matches!(element.view(), ValueView::Str(_)) {
            return Ok(Err(element));
        }
        // A plain lexical `%name` / `@name` only: twigil forms (`@!attr`,
        // `%*ENV`, `@.x`) carry extra store-side bookkeeping this op does not
        // repeat.
        let var_name = Self::const_str(code, name_idx);
        let sigil_ok = if is_positional {
            var_name.starts_with('@')
        } else {
            var_name.starts_with('%')
        };
        if !sigil_ok
            || !var_name[1..]
                .chars()
                .next()
                .is_some_and(|c| c.is_alphabetic() || c == '_')
        {
            return Ok(Err(element));
        }
        let Some(index) = self.stack.last().cloned() else {
            return Ok(Err(element));
        };
        // The element read onto the stack must still BE the container's
        // element -- the same allocation, not merely an equal string. That is
        // what makes appending to the stack's copy equivalent to appending to
        // the element: the read and the lane's write address one slot, and the
        // RHS did not replace it in between. (The lanes read and write env's
        // container and refuse a diverged local slot, so env is the place to
        // compare against.)
        let var_sym = code.const_sym(name_idx);
        let is_current = match self.env().get_sym(var_sym).map(Value::view) {
            Some(ValueView::Hash(hash)) if !is_positional => hash
                .get(&index.to_string_value())
                .is_some_and(|v| v.same_binding(&element)),
            Some(ValueView::Array(items, _)) if is_positional => match index.view() {
                ValueView::Int(i) if i >= 0 => items
                    .items()
                    .get(i as usize)
                    .is_some_and(|v| v.same_binding(&element)),
                _ => false,
            },
            _ => false,
        };
        if !is_current {
            return Ok(Err(element));
        }
        // Release the container's reference by storing an empty string through
        // the fast lane. A decline there touches nothing, so the unfused path
        // still sees exactly what it would have.
        self.stack.push(Value::str(String::new()));
        self.stack.push(index.clone());
        match self.fast_element_store_lane(code, name_idx, is_positional, target_slot) {
            None => {
                self.stack.pop();
                self.stack.pop();
                return Ok(Err(element));
            }
            Some(result) => {
                result?;
                // The lane pushed the placeholder store's value.
                self.stack.pop();
            }
        }
        // -- committed: the element holds "" until the store below --
        let plan = crate::value::StrAppendPlan::for_suffix(suffix.as_str());
        let appended = element.str_appended_nfc(&plan);
        let index = self.stack.pop().unwrap_or(index);
        self.stack.push(appended);
        self.stack.push(index);
        // The same lane with the same gates normally commits again; should it
        // not, the ordinary store writes the value instead.
        match self.fast_element_store_lane(code, name_idx, is_positional, target_slot) {
            Some(result) => result?,
            None => {
                let pre = self.attr_elem_env_snapshot(code, name_idx);
                self.exec_index_assign_expr_named_op(code, name_idx, is_positional, target_slot)?;
                self.mirror_attr_elem_env_to_cell(code, name_idx, pre);
            }
        }
        Ok(Ok(()))
    }

    fn fast_element_store_lane(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if is_positional {
            self.try_fast_array_element_assign_early(code, name_idx, is_positional, target_slot)
        } else {
            self.try_fast_hash_element_assign_early(code, name_idx, is_positional, target_slot)
        }
    }
}
