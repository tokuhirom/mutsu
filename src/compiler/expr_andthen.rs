//! The right-hand side of `andthen` / `orelse`.
//!
//! Raku runs the RHS with the LHS as its topic, and *calls* it with the LHS
//! only when the source wrote a block there: a bare block, a placeholder block
//! or a pointy block, parenthesized or not (`5 andthen -> $x { $x * 3 }` is
//! 15). Any other RHS is a value, even when that value is Code: a `sub`
//! literal, a `&var`, or an expression that yields a Callable
//! (`%cb andthen .<code>` is the callback itself). The choice is made here,
//! from the syntax, not at run time from the RHS value.

use super::*;

impl Compiler {
    /// Whether `rhs` is a block literal the operator calls with the LHS.
    /// A Whatever-curried closure (`5 andthen * + 1`) is called too, as it
    /// always has been here.
    // TODO: rakudo curries the whole `andthen` over a `*` RHS
    // (`5 andthen * + 1` is a WhateverCode, not 6); that needs the currying
    // pass to treat `andthen`/`orelse` as curryable operators.
    // Cost: O(g), g = nesting depth of the parentheses around the RHS.
    pub(super) fn andthen_rhs_is_block_literal(rhs: &Expr) -> bool {
        let mut rhs = rhs;
        while let Expr::Grouped(inner) = rhs {
            rhs = inner;
        }
        match rhs {
            Expr::AnonSub { is_block, .. } => *is_block,
            Expr::Lambda { .. } => true,
            Expr::AnonSubParams {
                declarator,
                is_whatever_code,
                ..
            } => {
                *is_whatever_code
                    || matches!(
                        declarator,
                        crate::ast::RoutineDeclarator::Block
                            | crate::ast::RoutineDeclarator::PointyBlock
                    )
            }
            _ => false,
        }
    }

    /// Compile the RHS of `andthen`/`orelse` with the LHS (on the stack) as
    /// its topic, leaving the operator's result. A block literal is called
    /// with the LHS (`__mutsu_andthen_finalize`); any other RHS is the result
    /// as it evaluates.
    pub(super) fn compile_andthen_rhs(&mut self, rhs: &Expr, calls_rhs: bool) {
        if !calls_rhs {
            // The LHS is only the topic: `SetTopic` consumes it.
            self.code.emit(OpCode::SetTopic);
            self.compile_expr(rhs);
            return;
        }
        self.code.emit(OpCode::Dup);
        self.code.emit(OpCode::SetTopic);
        self.compile_expr(rhs);
        let finalize_name_idx = self
            .code
            .add_constant(Value::str_from("__mutsu_andthen_finalize"));
        self.code.emit(OpCode::CallFunc {
            name_idx: finalize_name_idx,
            arity: 2,
            arg_sources_idx: None,
            literal_native_args: 0,
            static_arg_types: false,
        });
    }
}
