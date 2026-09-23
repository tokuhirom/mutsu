//! `return` inside a TRIR body.
//!
//! Raku's `return` is lexotic: it leaves the routine it is written in, wherever
//! in that routine's body it appears. The statement form (`return $x;`) was
//! always lowered to the chunk's own return op. The expression form, which is
//! how JSON::Fast leaves its scanning loops (`nqp::while(1, nqp::stmts(...,
//! (return @result)))`), was compiled as a generic CALL to a routine named
//! `return`. That call raises the untyped path's return control, which is only
//! caught by an untyped routine frame. A TRIR frame is not one, so the control
//! went straight through every statically linked `CallTr` frame and was taken
//! by the outermost untyped call site as the return of ITS callee. The callee
//! then answered with the innermost routine's value, and the caller's `is rw`
//! position was never written back. `from-json('[[1]]')` died with
//! "additional content". It went unseen only because a statically linked call
//! in a module never ran (#9072). Both forms now lower to the same op.

use super::TrirCompiler;
use crate::ast::Expr;
use crate::trir::{TrKind, TrOp};

impl TrirCompiler<'_> {
    /// Compile `return` / `return EXPR`, answering a nominal kind for the
    /// (never produced) value of the expression form.
    ///
    /// The op ends the chunk, so nothing after it on this path runs. The
    /// caller may still emit ops that consume the kind answered here (a sink
    /// `PopObj`, a branch-arm box); they are unreachable, and the frame's
    /// operand stacks are truncated when it is popped, so a `return` from the
    /// middle of an expression leaves nothing behind.
    pub(super) fn compile_return(&mut self, args: &[Expr]) -> Option<TrKind> {
        match args {
            [] => self.ops.push(TrOp::ReturnNil),
            [e] => {
                let kind = self.compile_expr(e)?;
                if self.returns_nil {
                    // `--> Nil` discards the value, exactly as the fall-off
                    // return does.
                    self.drop_top(kind);
                    self.ops.push(TrOp::ReturnNil);
                } else {
                    self.ops.push(match kind {
                        TrKind::Int => TrOp::ReturnI,
                        TrKind::Num => TrOp::ReturnN,
                        TrKind::Obj => TrOp::ReturnObj,
                    });
                }
            }
            _ => {
                // `return 1, 2` returns a List, which is a construction this
                // compiler does not perform.
                self.note_decline(|| "a `return` of several values".to_string());
                return None;
            }
        }
        Some(TrKind::Obj)
    }
}
