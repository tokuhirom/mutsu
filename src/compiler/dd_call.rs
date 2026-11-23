//! `dd` of a container element (`dd @a[0]`, `dd %h<k>`).

use super::*;

impl Compiler {
    /// `dd @a[0]` prints the element's container (`element of @a = 1`), as
    /// rakudo's `dd` does from `$var.VAR` (#19046). Each element argument is
    /// handed over as its `.VAR` descriptor, which carries the owning
    /// container's name and `of`, instead of the bare element value; the
    /// other arguments compile unchanged. `false` (nothing emitted) when no
    /// argument is such an element.
    pub(super) fn try_compile_dd_element_call(&mut self, name: &Symbol, args: &[Expr]) -> bool {
        if !args
            .iter()
            .any(|arg| self.var_on_index_takes_element_path(arg))
        {
            return false;
        }
        for arg in args {
            if self.var_on_index_takes_element_path(arg) {
                self.compile_expr_method_var_on_index(arg);
            } else {
                self.compile_expr(arg);
            }
        }
        let arg_sources_idx = self.add_arg_sources_constant(args);
        let name_idx = self.code.add_constant(Value::str(name.resolve()));
        self.code.emit(OpCode::CallFunc {
            name_idx,
            arity: args.len() as u32,
            arg_sources_idx,
            literal_native_args: 0,
            static_arg_types: false,
        });
        true
    }
}
