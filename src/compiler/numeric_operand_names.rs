//! Source names of the variable operands of numeric infix ops, recorded so the
//! "Use of uninitialized value $x of type Any in numeric context" warning can
//! name the variable the way rakudo's does (#9359). The runtime value is
//! already decontainerized by the time the op sees it, so the name has to come
//! from the source; see `CompiledCode::numeric_operand_names`.
use super::*;

impl Compiler {
    /// The rakudo spelling (`$x`, `$*x`, `$!x`) of a plain scalar variable
    /// operand, or `None` for anything else (an element, a call, a sigilless
    /// name, a placeholder parameter, a compiler temporary).
    fn numeric_operand_name(&self, expr: &Expr) -> Option<Symbol> {
        let Expr::Var(name) = expr else {
            return None;
        };
        if name.is_empty()
            || name.starts_with('^')
            || name.contains("__")
            || self.sigilless_locals.contains(name)
        {
            return None;
        }
        Some(Symbol::intern(&format!("${name}")))
    }

    /// Record the operand names of the numeric infix `opcode` about to be
    /// emitted. `left` is `None` when the left operand is the result of the
    /// previous op in a flattened chain (`$a + $b + $c`).
    pub(super) fn note_numeric_operands(
        &mut self,
        opcode: &OpCode,
        left: Option<&Expr>,
        right: &Expr,
    ) {
        if !matches!(
            opcode,
            OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Mod
                | OpCode::Pow
                | OpCode::NumEq
                | OpCode::NumNe
                | OpCode::NumLt
                | OpCode::NumLe
                | OpCode::NumGt
                | OpCode::NumGe
        ) {
            return;
        }
        let names = [
            left.and_then(|e| self.numeric_operand_name(e)),
            self.numeric_operand_name(right),
        ];
        self.code.note_numeric_operand_names(names);
    }
}
