use super::*;

impl Compiler {
    /// Compile a bare block in value position (`do { ... }`, a block used as a
    /// term). A thin wrapper over the shared lowering in `control_block.rs`;
    /// the only difference from the statement form is
    /// [`BlockPosition::Value`](crate::compiler::control_block::BlockPosition).
    ///
    /// `origin` says whether the node is a Raku block at all: `Expr::DoBlock` is
    /// also the generic "run these statements, yield a value" vehicle of some
    /// forty desugars, and only a real one owns a `let`/`temp` save frame.
    pub(super) fn compile_do_block_expr(
        &mut self,
        body: &[Stmt],
        label: &Option<String>,
        origin: crate::ast::DoBlockOrigin,
    ) {
        self.compile_block_construct(
            body,
            label,
            crate::compiler::control_block::BlockPosition::Value {
                isolate: false,
                origin,
            },
        );
    }

    /// [`Compiler::compile_do_block_expr`] with `OpCode::DoBlockExpr`'s
    /// `scope_isolate` on: the block's own scalar/array `my`/`state`
    /// declarations revert on exit while mutations of outer variables persist.
    pub(super) fn compile_do_block_expr_scoped(
        &mut self,
        body: &[Stmt],
        label: &Option<String>,
        origin: crate::ast::DoBlockOrigin,
    ) {
        self.compile_block_construct(
            body,
            label,
            crate::compiler::control_block::BlockPosition::Value {
                isolate: true,
                origin,
            },
        );
    }

    /// Compile an `if`/`elsif` chain in value (expression) position.
    ///
    /// A thin wrapper over the shared lowering in `control_if.rs`; the only
    /// difference from the statement form is
    /// [`IfPosition::Value`](crate::compiler::control_if::IfPosition::Value).
    pub(super) fn compile_do_if_expr_bound(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
        binding_var: &Option<String>,
        is_statement_modifier: bool,
    ) {
        self.compile_if_construct(
            cond,
            then_branch,
            else_branch,
            binding_var,
            is_statement_modifier,
            crate::compiler::control_if::IfPosition::Value,
        );
    }

    /// Compile a `for` in expression position.
    ///
    /// Identical to the statement form apart from `collect`: the shared
    /// lowering in `control_for.rs` gathers each iteration's value and leaves
    /// the resulting list on the stack.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_do_for_expr(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        params: &[String],
        params_def: &[crate::ast::ParamDef],
        rw_block: bool,
        explicit_zero_params: bool,
        body: &[Stmt],
        label: &Option<String>,
        is_statement_modifier: bool,
        mode: crate::ast::ForMode,
        uses_block_magic: bool,
    ) {
        // Parser currently lowers labeled `do { ... }` / labeled bare blocks into
        // a dummy single-iteration `for Nil` with a label. Preserve block semantics
        // here so control flow like `LABEL.leave(...)` returns the block value.
        // A real `for Nil -> { ... }` is NOT one of those: its explicit empty
        // signature makes it an ordinary (and immediately failing) loop.
        if param.is_none()
            && params.is_empty()
            && !explicit_zero_params
            && matches!(
                iterable,
                Expr::ArrayLiteral(items)
                    if items.len() == 1
                        && matches!(&items[0], Expr::Literal(lit) if lit.is_nil())
            )
        {
            // Behaviour-preserving: this dummy `for Nil` shape has no producer
            // left in the parser (a labelled block lowers straight to a
            // labelled `Expr::DoBlock`), so keep it off the block path rather
            // than granting it `let` resolution this cannot exercise.
            self.compile_do_block_expr(body, label, crate::ast::DoBlockOrigin::Desugar);
            return;
        }
        self.compile_for_construct(crate::compiler::control_for::ForParts {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body,
            label,
            mode,
            rw_block,
            explicit_zero_params,
            is_statement_modifier,
            uses_block_magic,
            collect: true,
        });
    }

    /// Compile `lazy for` expression: lower to `gather { for @items -> $param { take do { body } } }`.
    /// This defers execution of the body until the resulting Seq is consumed.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_lazy_for_expr(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        params: &[String],
        params_def: &[crate::ast::ParamDef],
        rw_block: bool,
        explicit_zero_params: bool,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        // Build `take <last_expr>` body: replace the last expression with `take <expr>`
        // and wrap the body in a for loop inside a gather block.
        let take_body = Self::wrap_loop_body_last_in_take(body);
        // Build inner for loop with the take body
        let inner_for = AStmt::For {
            iterable: iterable.clone(),
            param: param.clone(),
            param_def: Box::new(param_def.clone()),
            params: params.to_vec(),
            params_def: params_def.to_vec(),
            body: take_body,
            label: label.clone(),
            mode: crate::ast::ForMode::Normal,
            rw_block,
            explicit_zero_params,
            // Placeholders were already resolved on the source `lazy for` node;
            // this synthesized loop wraps an ordinary block body.
            is_statement_modifier: false,
            uses_block_magic: false,
        };
        // Build gather block wrapping the for loop, then mark it `.lazy` so the
        // body does not run until the resulting Seq is consumed (`lazy for`
        // semantics — S04 for.t "Lazy for loop does not execute until asked").
        let gather_body = vec![inner_for];
        let gather_expr = AExpr::Gather(gather_body);
        let lazy_expr = AExpr::MethodCall {
            target: Box::new(gather_expr),
            name: crate::symbol::Symbol::intern("lazy"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        };
        self.compile_expr(&lazy_expr);
    }

    /// Rewrite a loop body so its per-iteration value is `take`n: the last
    /// expression statement becomes `take <expr>`; a value-bearing `if`/`given`
    /// last statement is taken through `do` (so a false `if`-modifier yields
    /// `Empty`, which `take` slips away); any other shape appends `take Nil`.
    /// The loop's KEEP/UNDO result capture sees through the trailing `Take`
    /// (see `expand_loop_phasers`). Shared by the `lazy for` lowering and the
    /// `while`/`loop` expression forms.
    fn wrap_loop_body_last_in_take(body: &[Stmt]) -> Vec<Stmt> {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let mut stmts = body.to_vec();
        let last_idx = stmts.iter().rposition(|s| !matches!(s, AStmt::SetLine(_)));
        if let Some(idx) = last_idx {
            match stmts[idx].clone() {
                AStmt::Expr(expr) => stmts[idx] = AStmt::Take(expr, false),
                s @ (AStmt::If { .. } | AStmt::Given { .. }) => {
                    stmts[idx] = AStmt::Take(AExpr::DoStmt(Box::new(s)), false);
                }
                _ => {
                    stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::NIL), false));
                }
            }
        } else {
            stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::NIL), false));
        }
        stmts
    }

    /// Compile `do while` / `do until` (and parenthesized `(while ...)`)
    /// expression: lower to `gather { while COND { take do { body } } }` so
    /// the result is a lazy Seq pulled on demand, matching raku —
    /// `(while $++ < 2 { 42.say; 43 }).map: *.say` interleaves 42/43, and
    /// the whole loop only runs to completion when the Seq is reified.
    pub(super) fn compile_do_while_expr(
        &mut self,
        cond: &Expr,
        body: &[Stmt],
        label: &Option<String>,
        is_until: bool,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let inner = AStmt::While {
            cond: cond.clone(),
            body: Self::wrap_loop_body_last_in_take(body),
            label: label.clone(),
            is_statement_modifier: false,
            is_until,
        };
        let gather_expr = AExpr::Gather(vec![inner]);
        self.compile_expr(&gather_expr);
    }

    /// Compile `do loop (...) { ... }` / `(loop { ... })` expression: lower to
    /// `gather { loop (...) { take do { body } } }` — a lazy Seq, so an
    /// infinite `(loop { 42.say })[2]` pulls exactly three iterations.
    /// The C-style init runs inside the gather, deferred until first pull.
    pub(super) fn compile_do_loop_expr(
        &mut self,
        init: &Option<Box<Stmt>>,
        cond: &Option<Expr>,
        step: &Option<Expr>,
        body: &[Stmt],
        label: &Option<String>,
        is_until: bool,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let inner = AStmt::Loop {
            init: init.clone(),
            cond: cond.clone(),
            step: step.clone(),
            body: Self::wrap_loop_body_last_in_take(body),
            repeat: false,
            label: label.clone(),
            is_until,
        };
        let gather_expr = AExpr::Gather(vec![inner]);
        self.compile_expr(&gather_expr);
    }

    pub(super) fn do_if_branch_supported(stmts: &[Stmt]) -> bool {
        if Self::has_phasers(stmts) {
            return false;
        }
        let last = stmts.len().saturating_sub(1);
        for (i, stmt) in stmts.iter().enumerate() {
            match stmt {
                // A `given` in final position is handled by compile_block_inline
                // (it leaves the block value on the stack); only a non-final
                // `given` is unsupported here.
                Stmt::Given { .. } if i != last => return false,
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } if !Self::do_if_branch_supported(then_branch)
                    || !Self::do_if_branch_supported(else_branch) =>
                {
                    return false;
                }
                _ => {}
            }
        }
        true
    }
}
