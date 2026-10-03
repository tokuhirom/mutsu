//! Binding through a conditional: `my $x := $c ?? @a[$i] !! @a[$j]`.
//!
//! A ternary yields the *container* of the branch it selects, so binding to it
//! aliases that branch's element, exactly as `my $x := @a[$i]` would
//! (Crypt::RC4's `my $sy := $!y < 0 ?? @!state[*+$!y] !! @!state[$!y]`). The
//! condition is compiled as an ordinary expression; each branch goes through
//! the bind-target argument path, so only the selected one is evaluated and
//! bound.

use super::*;

impl Compiler {
    /// Compile a ternary that is the direct source of a scalar bind. Returns
    /// false (emitting nothing) when `arg` is no ternary.
    ///
    /// `suppress_multidim_bind_ref` is the caller's already-consumed one-shot
    /// flag, handed on to each branch.
    // Cost: O(1) per selector level (compile time).
    pub(super) fn compile_bind_through_ternary(
        &mut self,
        arg: &Expr,
        escaping: bool,
        suppress_multidim_bind_ref: bool,
    ) -> bool {
        let Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } = arg
        else {
            return false;
        };
        // The bind flags describe the bound source, not the condition: an
        // index in the condition must stay an ordinary read.
        let saved = (
            self.scalar_bind_autovivify,
            self.bind_terminal,
            self.raw_list_elem_terminal,
        );
        self.scalar_bind_autovivify = false;
        self.bind_terminal = false;
        self.raw_list_elem_terminal = false;
        self.compile_expr(cond);
        (
            self.scalar_bind_autovivify,
            self.bind_terminal,
            self.raw_list_elem_terminal,
        ) = saved;
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        self.compile_bind_branch(then_expr, escaping, suppress_multidim_bind_ref);
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_else);
        self.compile_bind_branch(else_expr, escaping, suppress_multidim_bind_ref);
        self.code.patch_jump(jump_end);
        true
    }

    fn compile_bind_branch(&mut self, branch: &Expr, escaping: bool, suppress: bool) {
        self.bind_target_direct = true;
        self.suppress_multidim_bind_ref_arg = suppress;
        self.compile_call_arg_with_escape(branch, escaping);
    }
}
