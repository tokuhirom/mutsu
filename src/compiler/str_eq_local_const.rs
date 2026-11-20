//! Emit `OpCode::StrEqLocalConst` for `$local eq 'literal'` (ADR-18357 D1).

use super::*;

impl Compiler {
    /// Compile `left OP right` as a fused `StrEqLocalConst` when `OP` is `eq`
    /// or `ne`, `left` is a plain lexical that compiles to a bare `GetLocal`
    /// and `right` is a `Str` literal, outside every lexical-import scope
    /// (which would put a `LexImportBuiltinOp` prefix before the operator).
    /// Returns `false`, having emitted nothing, when the shape does not
    /// qualify; `true` once it has compiled the whole expression, fused or
    /// not.
    // Cost: O(1) beyond compiling the operands.
    pub(super) fn try_emit_str_eq_local_const(
        &mut self,
        opcode: &OpCode,
        left: &Expr,
        right: &Expr,
    ) -> bool {
        let negate = match opcode {
            OpCode::StrEq => false,
            OpCode::StrNe => true,
            _ => return false,
        };
        let (Expr::Var(name), Expr::Literal(literal)) = (left, right) else {
            return false;
        };
        if literal.as_str().is_none()
            || !self.lexical_import_chain.is_empty()
            || !Self::is_plain_lexical_name(name)
            || !self.local_map.contains_key(name)
        {
            return false;
        }
        let start = self.code.ops.len();
        self.compile_expr(left);
        // Only a read that compiled to exactly one `GetLocal` is fused: any
        // other read (a sigilless alias, an outer write target, a captured
        // upvalue) keeps its own ops. Nothing can have recorded a jump target
        // at the `GetLocal` (it was emitted just now, with no statement
        // boundary after it), and a jump to its index lands on the fused op,
        // which does what falling through `GetLocal` did.
        if self.code.ops.len() == start + 1
            && let OpCode::GetLocal(slot) = self.code.ops[start]
        {
            let const_idx = self.code.add_constant(literal.clone());
            self.code.ops[start] = OpCode::StrEqLocalConst {
                slot,
                const_idx,
                negate,
            };
            return true;
        }
        self.compile_expr(right);
        self.note_numeric_operands(opcode, Some(left), right);
        self.code.emit(opcode.clone());
        true
    }
}
