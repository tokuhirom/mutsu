//! The definedness-testing `nqp::` control forms (#11500): `with`,
//! `without` and `defor`.
//!
//! Like `nqp::if`, their branches are thunks: QAST evaluates only the branch
//! the test selects, so they compile to jumps rather than to an eager-argument
//! call. The test is Raku's `.defined` (a `Failure` takes the undefined arm),
//! the same `JumpIfNotNil` peek that `//` and `nqp::ifnull` compile to.
//!
//! A branch value is the operand's own value: QAST does not call a block
//! operand (`nqp::with(42, -> $x { ... })` yields the block), and a missing
//! `else` yields the tested value itself (`nqp::with(Any, 1)` is `Any`,
//! `nqp::without(42, 1)` is `42`).

use super::*;

impl Compiler {
    /// Compile `nqp::with` / `nqp::without` / `nqp::defor`; `false` (with
    /// nothing emitted) when the arity does not fit, which leaves the call to
    /// the runtime's loud unsupported-op error.
    pub(super) fn try_compile_nqp_cond_form(
        &mut self,
        name: &str,
        args: &[Expr],
        sunk: bool,
    ) -> bool {
        match (name, args) {
            // nqp::with(c, then) / nqp::with(c, then, else): `then` when `c`
            // is defined, else `else` (or `c` itself without one).
            // Cost: O(1) plus `c`'s `.defined` (compiles to jumps; no runtime op).
            ("nqp::with", [cond, then, rest @ ..]) if rest.len() <= 1 => {
                self.compile_nqp_definedness_branch(cond, Some(then), rest.first(), sunk);
                true
            }
            // nqp::without(c, then) / nqp::without(c, then, else): `then` when
            // `c` is undefined, else `else` (or `c` itself without one).
            // Cost: O(1) plus `c`'s `.defined` (compiles to jumps; no runtime op).
            ("nqp::without", [cond, then, rest @ ..]) if rest.len() <= 1 => {
                self.compile_nqp_definedness_branch(cond, rest.first(), Some(then), sunk);
                true
            }
            // nqp::defor(a, b): `a // b` -- `b` is evaluated only when `a` is
            // undefined.
            // Cost: O(1) plus `a`'s `.defined` (compiles to jumps; no runtime op).
            ("nqp::defor", [value, fallback]) => {
                self.compile_nqp_definedness_branch(value, None, Some(fallback), sunk);
                true
            }
            _ => false,
        }
    }

    /// Evaluate `cond` once and yield `defined` when it is defined, `undefined`
    /// when it is not; a missing arm yields `cond` itself.
    fn compile_nqp_definedness_branch(
        &mut self,
        cond: &Expr,
        defined: Option<&Expr>,
        undefined: Option<&Expr>,
        sunk: bool,
    ) {
        self.compile_expr(cond);
        // `JumpIfNotNil` peeks: `cond` stays on the stack on both paths, so an
        // absent arm leaves it there as the result.
        let jump_defined = self.code.emit(OpCode::JumpIfNotNil(0));
        if let Some(arm) = undefined {
            self.code.emit(OpCode::Pop);
            self.compile_nqp_operand(arm, sunk);
        }
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_defined);
        if let Some(arm) = defined {
            self.code.emit(OpCode::Pop);
            self.compile_nqp_operand(arm, sunk);
        }
        self.code.patch_jump(jump_end);
    }
}
