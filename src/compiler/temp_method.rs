//! `temp` / `let` over a method-call lvalue (`Stmt::TempMethodAssign`).

use super::Compiler;
use crate::ast::{Expr, Stmt};
use crate::opcode::OpCode;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

impl Compiler {
    /// `temp $obj.m = v`, `let Type.^meta = v`, a bare `temp $obj.m;`: save
    /// what the rw call yields now, then (when a value is given) assign through
    /// it. The save is restored at scope exit for `temp`, and only on a failed
    /// scope for `let`.
    ///
    /// `self` and the `$.attr` spelling save the frame's invocant, the one
    /// object every holder shares. Any other invocant -- a variable, an
    /// element, a type object -- saves the ATTRIBUTE through
    /// `__mutsu_temp_method_save`: its old value goes back through the same rw
    /// accessor, so `$obj` stays the one shared object (#16144) and a type
    /// object's class-level state (`temp Bla.^table`, Red) is what is restored
    /// (#18392).
    // Cost: O(n), n = nodes in the method arguments and the assigned value.
    pub(super) fn compile_temp_method_assign(&mut self, stmt: &Stmt) {
        let Stmt::TempMethodAssign {
            var_name,
            method_name,
            method_args,
            value,
            is_let,
            is_attribute,
            type_invocant,
            invocant_expr,
        } = stmt
        else {
            return;
        };
        let (is_let, is_attribute, type_invocant) = (*is_let, *is_attribute, *type_invocant);
        let invocant_expr = invocant_expr.as_deref();
        let invocant = if let Some(e) = invocant_expr {
            e.clone()
        } else if var_name == "self" || type_invocant {
            Expr::BareWord(var_name.clone())
        } else {
            Expr::Var(var_name.clone())
        };
        let frame_invocant = invocant_expr.is_none() && (var_name == "self" || is_attribute);
        if frame_invocant {
            let slot = self.local_map.get(var_name).copied();
            let name_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LetSave {
                name_idx,
                is_temp: !is_let,
                slot,
            });
        } else {
            let save = Expr::Call {
                name: Symbol::intern("__mutsu_temp_method_save"),
                args: vec![
                    invocant.clone(),
                    Expr::Literal(Value::str(method_name.clone())),
                    Expr::ArrayLiteral(method_args.clone()),
                    Expr::Literal(if is_let { Value::FALSE } else { Value::TRUE }),
                ],
                listop: false,
            };
            self.compile_expr(&save);
            self.code.emit(OpCode::Pop);
        }
        let Some(value) = value else {
            return;
        };
        let value = match value {
            Expr::Unary {
                op: TokenKind::Pipe,
                ..
            } => Expr::Grouped(Box::new(value.clone())),
            _ => value.clone(),
        };
        // The variable the call's result is written back to: none for an
        // element, and a sigilless spelling for a type object, which is no
        // variable to rebind.
        let target_var = if invocant_expr.is_some() {
            None
        } else if type_invocant {
            Some(format!("\\{var_name}"))
        } else {
            Some(var_name.clone())
        };
        let assign_expr = crate::lvalue_call::assign_method(
            invocant,
            Expr::Literal(Value::str(method_name.clone())),
            method_args.clone(),
            value,
            crate::lvalue_call::AssignMethodHints {
                target_var,
                ..Default::default()
            },
        );
        self.compile_expr(&assign_expr);
        self.code.emit(OpCode::Pop);
    }
}
