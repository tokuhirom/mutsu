//! The right-hand side of `does`/`but`: a role initializer `R(v)`.

use super::*;

impl Compiler {
    /// Compile the right operand of `does`/`but`.
    ///
    /// Rakudo rewrites a top-level call with exactly one argument (`$x does
    /// R(v)`, or a named `R(:x(v))`) into `infix:<does>($x, R, :value(v))`
    /// when `R` names a type, so the role is applied with `v` as its
    /// initializer instead of being called (a `CALL-ME` is not consulted, and
    /// `R(1, 2)` or `(R(v))` is an ordinary call that fails to coerce). Whether `R` is a role is only
    /// known at run time here, so both arms are emitted:
    ///
    /// ```text
    ///   JumpIfNotRole(R, call)
    ///   <v>  MakeRoleInit  Jump(end)       (R was pushed by JumpIfNotRole)
    /// call:
    ///   <R(v) as an ordinary call>
    /// end:
    /// ```
    ///
    /// Any other operand compiles as an ordinary expression.
    pub(super) fn compile_mixin_rhs(&mut self, rhs: &Expr) {
        let Expr::Call { name, args } = rhs else {
            self.compile_expr(rhs);
            return;
        };
        let [arg] = args.as_slice() else {
            self.compile_expr(rhs);
            return;
        };
        // A single named argument is the initializer too, its name ignored
        // (raku: `1 but R(:y(5))` sets R's one attribute to 5). A slip
        // (`R(|@a)`) is an ordinary call.
        let value = match arg {
            Expr::Binary {
                op: TokenKind::FatArrow,
                right,
                ..
            } => right.as_ref(),
            _ if Self::is_named_arg_expr(arg) => {
                self.compile_expr(rhs);
                return;
            }
            _ => arg,
        };
        let name_idx = self
            .code
            .add_constant(Value::str(name.resolve().to_string()));
        let not_role = self.code.emit(OpCode::JumpIfNotRole(name_idx, 0));
        self.compile_expr(value);
        self.code.emit(OpCode::MakeRoleInit);
        let end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(not_role);
        self.compile_expr(rhs);
        self.code.patch_jump(end);
    }
}
