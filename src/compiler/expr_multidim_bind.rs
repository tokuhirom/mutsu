use super::*;

impl Compiler {
    /// Push the source-variable names an indexed `:=` hands
    /// [`OpCode::IndexBindValue`]. They are `Str`/`Nil` literals fixed at
    /// compile time, so they load as one constant list rather than being
    /// rebuilt by a `MakeArray` on every bind (a dispatch-table build binds
    /// thousands of elements, #17429). Every consumer only reads the list.
    // Cost: O(s) at compile time, s = sources; O(1) per execution.
    pub(super) fn emit_index_bind_sources(&mut self, rhs: &Expr) {
        let sources = crate::parser::bind_source_metadata_expr(rhs);
        let literals = match &sources {
            Expr::ArrayLiteral(items) => items
                .iter()
                .map(|item| match item {
                    Expr::Literal(value) => Some(value.clone()),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>(),
            _ => None,
        };
        let Some(literals) = literals else {
            self.compile_call_arg(&sources);
            return;
        };
        let idx = self.code.add_constant(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(literals)),
            crate::value::ArrayKind::List,
        ));
        self.code.emit(OpCode::LoadConst(idx));
    }

    /// A semicolon-subscript bind (`@s[0;0] := $x`): like a chained one, it
    /// binds the source's container when the RHS names a variable (rakudo
    /// 2026.11; #18541), and the value otherwise.
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
        self.compile_call_arg(rhs);
        self.emit_index_bind_sources(rhs);
        self.code.emit(OpCode::IndexBindValue);
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
