//! A direct `dd` call: each argument with its `.raku`.

use super::*;

impl Compiler {
    /// A direct `dd ARGS` call compiles to `__mutsu_dd(v1, v1.raku, v2,
    /// v2.raku, ...)`. rakudo's `dd` is `note prefix ~ $var.raku`, so the
    /// value part is whatever `.raku` answers: a typed Array, a Block's
    /// signature or a user `raku` method (#19047). The `.raku` is an ordinary
    /// method call the VM dispatches, not a runtime slow path.
    ///
    /// An element of a named container (`dd @a[0]`) is handed over as its
    /// `.VAR` descriptor, which carries the owning container's name and `of`
    /// (`element of @a = 1`, #19046); its `.raku` is the element's.
    ///
    /// `false` (nothing emitted) for a call this form does not cover: no
    /// arguments, a named or `|slip` argument, or a `dd` this scope declares
    /// or binds, which the call must reach unchanged.
    pub(super) fn try_compile_dd_call(&mut self, name: &Symbol, args: &[Expr]) -> bool {
        if args.is_empty()
            || args
                .iter()
                .any(|arg| Self::is_named_arg_expr(arg) || Self::is_slip_interpolation_arg(arg))
            || !matches!(self.amp_binding_state(name.as_str()), AmpBinding::None)
            || self
                .scope_routine_decls
                .iter()
                .any(|&(_, decl)| decl == *name)
        {
            return false;
        }
        let raku_idx = self.code.add_constant(Value::str("raku".to_string()));
        for arg in args {
            if self.var_on_index_takes_element_path(arg) {
                self.compile_expr_method_var_on_index(arg);
            } else {
                self.compile_expr(arg);
            }
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::CallMethod {
                name_idx: raku_idx,
                arity: 0,
                modifier_idx: None,
                quoted: false,
                arg_sources_idx: None,
            });
        }
        // Argument `k`'s source sits at index `2k`, its `.raku` at `2k + 1`.
        let interleaved: Vec<Expr> = args
            .iter()
            .flat_map(|arg| [arg.clone(), Expr::Literal(Value::NIL)])
            .collect();
        let arg_sources_idx = self.add_arg_sources_constant(&interleaved);
        let name_idx = self.code.add_constant(Value::str("__mutsu_dd".to_string()));
        self.code.emit(OpCode::CallFunc {
            name_idx,
            arity: (args.len() * 2) as u32,
            arg_sources_idx,
            literal_native_args: 0,
            static_arg_types: false,
        });
        true
    }
}
