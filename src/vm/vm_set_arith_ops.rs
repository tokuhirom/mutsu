//! Set comparison ops (subset/superset) and junction ops -- split from
//! `vm_set_ops` (§7-8). The binary set operators live in
//! `runtime::utils::set_algebra`.
use super::*;

impl Interpreter {
    pub(super) fn exec_set_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::truth(Self::quant_hash_subset(&left, &right)));
    }

    pub(super) fn exec_set_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::truth(Self::quant_hash_subset(&right, &left)));
    }

    pub(super) fn exec_set_strict_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::truth(Self::quant_hash_strict_subset(&left, &right)));
    }

    pub(super) fn exec_set_strict_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::truth(Self::quant_hash_strict_subset(&right, &left)));
    }

    pub(super) fn exec_junction_any_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::Any, left, right));
    }

    pub(super) fn exec_junction_all_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::All, left, right));
    }

    pub(super) fn exec_junction_one_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::One, left, right));
    }

    /// Execute a multi-operand junction opcode. Pops `count` values from
    /// the stack. If a user-defined infix operator exists in scope, calls
    /// it once with all operands (list-associative). Otherwise builds the
    /// junction from all values.
    pub(super) fn exec_junction_n_op(
        &mut self,
        count: u32,
        kind: JunctionKind,
        infix_name: &str,
    ) -> Result<(), RuntimeError> {
        let n = count as usize;
        let mut values: Vec<Value> = Vec::with_capacity(n);
        for _ in 0..n {
            values.push(self.stack.pop().unwrap_or(Value::NIL));
        }
        values.reverse();

        // Check for user-defined override
        if let Some(def) = loan_env!(self, resolve_function_with_types(infix_name, &values)) {
            let empty_fns = CompiledFns::default();
            let result = self.compile_and_call_function_def(&def, values.clone(), &empty_fns)?;
            self.stack.push(result);
            return Ok(());
        }

        // No user override: build junction from all values
        let result = Value::junction(kind, values);
        self.stack.push(result);
        Ok(())
    }
}
