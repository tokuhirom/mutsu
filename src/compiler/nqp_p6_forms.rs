//! The Rakudo `p6*` `nqp::` ops that compile to ordinary Raku code (#11505).
//!
//! Rakudo registers these for the `Raku` HLL in `src/vm/moar/Perl6/Ops.nqp`
//! as QAST rewrites rather than VM instructions: each is the bytecode of a
//! plain Raku construct with the operand left in place. They are lowered the
//! same way here, because a value op would see its operand decontainerized —
//! `p6store` needs the variable, `p6sink` needs to know whether the operand
//! is a container, and `p6return` / `p6invokeflat` are control flow and a call.
//! The `p6*` ops that are plain value ops are in `runtime::nqp_ops_p6`.

use super::*;

impl Compiler {
    /// Compile one of the `p6*` forms; `false` (with nothing emitted) when the
    /// arity does not fit, which leaves the call to the runtime's loud
    /// unsupported-op error.
    pub(super) fn try_compile_nqp_p6_form(&mut self, name: &str, args: &[Expr]) -> bool {
        match (name, args) {
            // nqp::p6store($cont, $value): `$cont = $value` — a container is
            // assigned (with its type check), anything else gets `.STORE`.
            // Yields the container's new value.
            // Cost: O(1) plus the assignment's own cost.
            ("nqp::p6store", [target, value]) => {
                let assign = Self::p6store_expr(target, value);
                self.compile_expr(&assign);
                true
            }
            // nqp::p6sink($v): sink the value and yield it. Like a statement,
            // a container (a variable) is not sunk; a fresh value runs its
            // `sink` method, and an unhandled Failure throws.
            // Cost: O(1) plus the operand's `sink` method.
            ("nqp::p6sink", [operand]) => {
                self.compile_expr(operand);
                self.code.emit(OpCode::Dup);
                self.code.emit(OpCode::SinkPop(
                    Self::stmt_value_may_user_sink(operand),
                    !Self::stmt_value_is_bare_container_read(operand),
                ));
                true
            }
            // nqp::p6return($value): return `$value` from the enclosing
            // routine, which is `return $value`.
            // Cost: O(1) (the `return` it compiles to).
            ("nqp::p6return", [value]) => {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("return"),
                    args: vec![value.clone()],
                    listop: false,
                });
                true
            }
            // nqp::p6invokeflat($code, $list): call `$code` with the list's
            // elements as its positional arguments, `$code(|$list)`.
            // Cost: O(e), e = elements of `$list`, plus the call.
            ("nqp::p6invokeflat", [code, list]) => {
                self.compile_expr(&Expr::CallOn {
                    target: Box::new(code.clone()),
                    args: vec![Expr::Unary {
                        op: crate::token_kind::TokenKind::Pipe,
                        expr: Box::new(list.clone()),
                    }],
                });
                true
            }
            _ => false,
        }
    }

    /// The assignment `nqp::p6store(target, value)` performs.
    fn p6store_expr(target: &Expr, value: &Expr) -> Expr {
        let value = Box::new(value.clone());
        let assign = |name: String| Expr::AssignExpr {
            name,
            expr: value.clone(),
            is_bind: false,
        };
        match target {
            Expr::Var(name) | Expr::BareWord(name) => assign(name.clone()),
            Expr::ArrayVar(name) => assign(format!("@{name}")),
            Expr::HashVar(name) => assign(format!("%{name}")),
            Expr::Index {
                target,
                index,
                is_positional,
                ..
            } => Expr::IndexAssign {
                target: target.clone(),
                index: index.clone(),
                value,
                is_positional: *is_positional,
                spelling: Default::default(),
            },
            other => Expr::MethodCall {
                target: Box::new(other.clone()),
                name: Symbol::intern("STORE"),
                args: vec![*value],
                modifier: None,
                quoted: false,
                sugar: false,
            },
        }
    }
}
