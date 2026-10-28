use super::*;

impl Compiler {
    /// The call that builds the VM's bind-value marker from the bound value
    /// and the names of its source variables.
    // Cost: O(1).
    pub(super) fn index_bind_marker_call(value: Expr, sources: Expr) -> Expr {
        Expr::Call {
            name: Symbol::intern("__mutsu_bind_index_value"),
            args: vec![value, sources],
            listop: false,
        }
    }

    /// Semicolon subscripts bind the RHS value, unlike chained subscripts.
    // Cost: O(n + d), n = RHS expression size, d = dimensions.
    pub(super) fn compile_multidim_value_bind(
        &mut self,
        name: String,
        dimensions: &[Expr],
        value: &Expr,
        is_positional: bool,
    ) {
        let Expr::IndexBindValue(rhs) = value else {
            unreachable!("indexed bind has a bind value");
        };
        // No source metadata and no lvalue promotion: BIND-POS receives the
        // value, even when the expression names a writable scalar container.
        self.compile_expr(&Self::index_bind_marker_call(
            Expr::Call {
                name: Symbol::intern("nqp::decont"),
                args: vec![rhs.as_ref().clone()],
                listop: false,
            },
            Expr::Literal(Value::NIL),
        ));
        for dimension in dimensions.iter().rev() {
            self.compile_expr(dimension);
        }
        let target_slot = self.local_map.get(&name).copied();
        let name_idx = self.code.add_constant(Value::str(name));
        let flags = vec![Value::truth(is_positional); dimensions.len()];
        let positional_flags_idx = self.code.add_constant(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(flags)),
            crate::value::ArrayKind::Array,
        ));
        self.code.emit(OpCode::IndexAssignDeepNested {
            name_idx,
            depth: dimensions.len() as u32,
            positional_flags_idx,
            target_slot,
        });
    }
}
