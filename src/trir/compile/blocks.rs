//! Block statements for [`super::TrirCompiler`]: `if`/`else`, and the
//! topicalizing `given X { ... }` / `with X { ... }`.

use super::TrirCompiler;
use crate::ast::{Expr, Stmt};
use crate::trir::{TrKind, TrOp};

impl TrirCompiler<'_> {
    /// `if COND { ... } else { ... }` as a statement. Both branches are
    /// compiled in sink position: an `if` statement's own value is not one
    /// TRIR tracks, and every JSON::Fast use of it is for effect.
    pub(super) fn compile_if(
        &mut self,
        cond: &crate::ast::Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
    ) -> Option<()> {
        let ck = self.compile_expr(cond)?;
        self.truthy(ck)?;
        let branch_at = self.ops.len();
        self.ops.push(TrOp::JumpIfFalseI(0));
        self.compile_body(then_branch, true)?;
        let jump_end_at = self.ops.len();
        self.ops.push(TrOp::Jump(0));
        let else_at = self.ops.len() as u32;
        self.compile_body(else_branch, true)?;
        let end = self.ops.len() as u32;
        match &mut self.ops[branch_at] {
            TrOp::JumpIfFalseI(x) => *x = else_at,
            _ => return None,
        }
        match &mut self.ops[jump_end_at] {
            TrOp::Jump(x) => *x = end,
            _ => return None,
        }
        Some(())
    }

    /// `given TOPIC { BODY }`, and `STMT given TOPIC`, in sink position.
    ///
    /// The topic's VALUE goes into a fresh slot that `$_` names for the body.
    /// Raku aliases `$_` to the topic's container instead, so only a topic
    /// with no container of its own is admitted, where the copy IS the
    /// topic: a computed value (`STMT given .Str`), or the fresh temporary
    /// `with X { ... }` reaches here as (`if (my $__with_tmp = X).defined {
    /// given $__with_tmp { ... } }`). A variable topic declines. A direct
    /// write to the slot declines too ([`Self::check_not_bound`]); handed to
    /// a callee, it travels like any other local. The block form opens a
    /// scope; the statement modifier does not, but its body is the one
    /// statement, so restoring the bindings afterwards changes nothing it
    /// could see.
    pub(super) fn compile_given(
        &mut self,
        topic: &Expr,
        body: &[Stmt],
        _is_statement_modifier: bool,
    ) -> Option<()> {
        let containerless = match topic {
            Expr::Var(n) => crate::with_desugar::is_tmp_name(n),
            Expr::BareWord(_)
            | Expr::ArrayVar(_)
            | Expr::HashVar(_)
            | Expr::Index { .. }
            | Expr::DoStmt(_) => false,
            _ => true,
        };
        if !containerless {
            self.note_decline(|| "a `given` over a variable, which `$_` aliases".to_string());
            return None;
        }
        let k = self.compile_expr(topic)?;
        self.coerce(k, TrKind::Obj)?;
        let saved = self.locals.clone();
        let slot = self.alloc("_", TrKind::Obj);
        self.store(slot, TrKind::Obj);
        self.topic_slots.insert(slot);
        let compiled = self.compile_body(body, true);
        self.locals = saved;
        compiled.map(|_| ())
    }
}
