//! A positional store into a literal (`5[0] = 3`, `"a"[0] = 3`), #18960.
//!
//! A literal is a value, not a container. rakudo's `Any.ASSIGN-POS` stores
//! through `AT-POS`, which for index 0 is the literal itself, so the store
//! dies with X::Assignment::RO naming it ("Cannot modify an immutable Int
//! (5)"); any other index dies as the read would ("Index out of range"). The
//! right-hand side is evaluated first, as rakudo evaluates the arguments of
//! `ASSIGN-POS` before calling it.
//!
//! This is decided at compile time because only the compiler knows the target
//! is a literal: at run time an `is rw` routine's result and a literal are the
//! same value, and the routine's must stay assignable.

use super::Compiler;
use crate::ast::Expr;
use crate::opcode::OpCode;
use crate::value::{Value, ValueView};

/// Whether `target` is a scalar literal: an Int, Str, Num, Rat or Bool.
// Cost: O(1).
fn is_scalar_literal(target: &Expr) -> bool {
    let value = match target {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => v,
        _ => return false,
    };
    matches!(
        value.view(),
        ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Str(_)
            | ValueView::Num(_)
            | ValueView::Rat(..)
            | ValueView::Bool(_)
    )
}

impl Compiler {
    /// Compile `target[index] = value` for a scalar-literal `target` (looked
    /// at through its parentheses): evaluate the operands in source order,
    /// read the element as `AT-POS` would, and die with X::Assignment::RO
    /// naming it. `false`, emitting nothing, for any other target.
    // Cost: O(1) emitted opcodes.
    pub(super) fn compile_literal_index_assign(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
        is_positional: bool,
    ) -> bool {
        if !is_positional || matches!(value, Expr::IndexBindValue(_)) {
            return false;
        }
        if !is_scalar_literal(target) {
            return false;
        }
        self.compile_expr(target);
        self.compile_expr(index);
        self.compile_expr(value);
        self.code.emit(OpCode::Pop);
        self.code.emit(OpCode::Index {
            is_positional: true,
        });
        let name_idx = self
            .code
            .add_constant(Value::str_from("__mutsu_assignment_ro"));
        self.code.emit(OpCode::CallFunc {
            name_idx,
            arity: 1,
            arg_sources_idx: None,
            literal_native_args: 0,
            static_arg_types: false,
        });
        true
    }
}
