//! Method calls inside a TRIR body (ADR-0112 Step 2).
//!
//! `parse-numeric` ends in `nqp::substr(...).Numeric`, and every scanner's
//! error message renders a value with `.raku` or `.base(16)`. None of them is
//! hot, and refusing the whole routine for one would leave its hot loop on
//! the untyped path, so a method call goes out through the ordinary method
//! dispatch with its receiver and arguments boxed: the method-call twin of
//! `CallGen`.
//!
//! What is admitted is bounded by one question: could the method need the
//! receiver's CONTAINER rather than its value? The untyped compiler emits
//! `CallMethodMut` on a variable receiver, which writes an autovivified or
//! rebound value back into the variable (`my $a; $a.push(1)` makes `$a` an
//! `Array`). A TRIR slot holds a value, and nothing is written back, so only
//! methods that read their receiver's value and never rebind it are
//! admitted — for those the two opcodes are the same call.

use super::{Binding, TrirCompiler};
use crate::ast::Expr;
use crate::trir::{TrKind, TrMethodCall, TrOp};

/// Methods that read their receiver's value and nothing else.
const VALUE_METHODS: &[&str] = &[
    "Numeric", "Bool", "Str", "Int", "Num", "raku", "gist", "base", "chars", "substr", "fmt",
];

impl TrirCompiler<'_> {
    pub(super) fn compile_method_call(
        &mut self,
        target: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Option<TrKind> {
        if !VALUE_METHODS.contains(&name) {
            self.note_decline(|| format!("method .{name}"));
            return None;
        }
        if args.len() > u8::MAX as usize || args.iter().any(Self::is_named_or_spread) {
            self.note_decline(|| format!("method .{name} with a named or spread argument"));
            return None;
        }
        // `$v.raku` renders the container's itemization (`$(1, 2)`), which
        // the untyped binder adds to a `$` parameter and a TRIR slot does
        // not carry. A native or a computed receiver has none to show.
        if matches!(name, "raku" | "gist")
            && let Some(Binding {
                kind: TrKind::Obj, ..
            }) = match target {
                Expr::Var(n) => self.binding_of(n),
                Expr::BareWord(n) => self.binding_of(&super::params::sigilless_key(n)),
                _ => None,
            }
        {
            self.note_decline(|| format!("method .{name} on a boxed variable"));
            return None;
        }
        let tk = self.compile_expr(target)?;
        self.coerce(tk, TrKind::Obj)?;
        for a in args {
            let k = self.compile_expr(a)?;
            self.coerce(k, TrKind::Obj)?;
        }
        let idx = self.methods.len() as u32;
        self.methods.push(TrMethodCall {
            name: crate::symbol::Symbol::intern(name),
            arity: args.len() as u8,
        });
        self.ops.push(TrOp::MethodGen(idx));
        Some(TrKind::Obj)
    }

    fn is_named_or_spread(e: &Expr) -> bool {
        match e {
            Expr::Binary { op, .. } => *op == crate::token_kind::TokenKind::FatArrow,
            Expr::Unary { op, .. } => *op == crate::token_kind::TokenKind::Pipe,
            Expr::Literal(lit) => matches!(lit.view(), crate::value::ValueView::Pair(..)),
            _ => false,
        }
    }
}
