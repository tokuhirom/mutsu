//! `nqp::getattr` / `nqp::bindattr` sites whose attribute name is a literal
//! (ADR-0121 D3): the name is resolved here, once, instead of by the generic
//! `NqpOpGen` dispatch on every execution.

use super::TrirCompiler;
use crate::ast::Expr;
use crate::runtime::nqp_attr::{NqpAttrConv, NqpAttrName};
use crate::trir::class_operand::ClassOperandSite;
use crate::trir::{TrKind, TrOp};
use crate::value::ValueView;

impl TrirCompiler<'_> {
    /// Lower an attribute op with a literal name. `None`: not such a site,
    /// and the caller takes the generic path. `Some(None)`: an operand
    /// declined.
    pub(super) fn try_attr_op(&mut self, op: &str, args: &[Expr]) -> Option<Option<TrKind>> {
        let arity = match op {
            "getattr" | "getattr_i" | "getattr_n" | "getattr_s" => 3,
            "bindattr" | "bindattr_i" | "bindattr_n" | "bindattr_s" => 4,
            _ => return None,
        };
        if args.len() != arity {
            return None;
        }
        let Expr::Literal(lit) = &args[2] else {
            return None;
        };
        let ValueView::Str(name) = lit.view() else {
            return None;
        };
        let site = Box::new((NqpAttrName::new(&name), NqpAttrConv::of_op(op)));
        // The operands in source order, minus the literal: evaluating a
        // string literal has no effect to preserve.
        for (i, a) in args.iter().enumerate() {
            if i == 2 {
                continue;
            }
            if i == 1
                && let Expr::BareWord(class) = a
                && self.plain_bareword(class)
            {
                self.ops
                    .push(TrOp::ClassOperand(Box::new(ClassOperandSite::new(
                        crate::symbol::Symbol::intern(class),
                    ))));
                continue;
            }
            let Some(got) = self.compile_nqp_operand(a) else {
                return Some(None);
            };
            if self.coerce(got, TrKind::Obj).is_none() {
                return Some(None);
            }
        }
        self.ops.push(if arity == 3 {
            TrOp::GetAttrC(site)
        } else {
            TrOp::BindAttrC(site)
        });
        self.nqp_sourced = true;
        self.nqp_int_result = super::nqp::nqp_op_returns_int(op);
        Some(Some(TrKind::Obj))
    }

    /// Whether `name` in operand position compiles to a plain `LoadBareWord`
    /// (see the `BareWord` arm of `compile_expr`): not a no-paren `nqp::`
    /// term, a sigilless parameter or an inlined inner sub.
    fn plain_bareword(&self, name: &str) -> bool {
        !name.starts_with("nqp::")
            && self
                .binding_of(&super::params::sigilless_key(name))
                .is_none()
            && !self.inline_subs.contains_key(name)
    }
}
