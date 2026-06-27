use super::*;

impl Interpreter {
    pub(super) fn rewrite_proto_dispatch_stmt(stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::Expr(Expr::Whatever) => Stmt::Expr(Expr::Call {
                name: Symbol::intern("__PROTO_DISPATCH__"),
                args: Vec::new(),
            }),
            Stmt::Expr(expr) => Stmt::Expr(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Return(expr) => Stmt::Return(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Take(expr, rw) => Stmt::Take(Self::rewrite_proto_dispatch_expr(expr), *rw),
            Stmt::Die(expr) => Stmt::Die(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Fail(expr) => Stmt::Fail(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint,
            } => Stmt::VarDecl {
                name: name.clone(),
                expr: Self::rewrite_proto_dispatch_expr(expr),
                type_constraint: type_constraint.clone(),
                is_state: *is_state,
                is_our: *is_our,
                is_dynamic: *is_dynamic,
                is_export: *is_export,
                export_tags: export_tags.clone(),
                custom_traits: custom_traits.clone(),
                where_constraint: where_constraint.clone(),
            },
            Stmt::Assign { name, expr, op } => Stmt::Assign {
                name: name.clone(),
                expr: Self::rewrite_proto_dispatch_expr(expr),
                op: *op,
            },
            Stmt::TempMethodAssign {
                var_name,
                method_name,
                method_args,
                value,
            } => Stmt::TempMethodAssign {
                var_name: var_name.clone(),
                method_name: method_name.clone(),
                method_args: method_args
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
                value: Self::rewrite_proto_dispatch_expr(value),
            },
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            } => Stmt::If {
                cond: Self::rewrite_proto_dispatch_expr(cond),
                then_branch: Self::rewrite_proto_dispatch_stmts(then_branch),
                else_branch: Self::rewrite_proto_dispatch_stmts(else_branch),
                binding_var: binding_var.clone(),
            },
            Stmt::While { cond, body, label } => Stmt::While {
                cond: Self::rewrite_proto_dispatch_expr(cond),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
            },
            Stmt::For {
                iterable,
                body,
                label,
                param,
                param_def,
                params,
                params_def,
                mode,
                rw_block,
                explicit_zero_params,
            } => Stmt::For {
                iterable: Self::rewrite_proto_dispatch_expr(iterable),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
                param: param.clone(),
                param_def: Box::new((**param_def).clone()),
                params: params.clone(),
                params_def: params_def.clone(),
                mode: *mode,
                rw_block: *rw_block,
                explicit_zero_params: *explicit_zero_params,
            },
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                label,
                repeat,
            } => Stmt::Loop {
                init: init
                    .as_ref()
                    .map(|s| Box::new(Self::rewrite_proto_dispatch_stmt(s))),
                cond: cond.as_ref().map(Self::rewrite_proto_dispatch_expr),
                step: step.as_ref().map(Self::rewrite_proto_dispatch_expr),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
                repeat: *repeat,
            },
            Stmt::Block(stmts) => Stmt::Block(Self::rewrite_proto_dispatch_stmts(stmts)),
            Stmt::SyntheticBlock(stmts) => {
                Stmt::SyntheticBlock(Self::rewrite_proto_dispatch_stmts(stmts))
            }
            other => other.clone(),
        }
    }

    fn rewrite_proto_dispatch_expr(expr: &Expr) -> Expr {
        match expr {
            Expr::AnonSub { body, .. }
                if {
                    let non_setline: Vec<_> = body
                        .iter()
                        .filter(|s| !matches!(s, Stmt::SetLine(_)))
                        .collect();
                    non_setline.len() == 1 && matches!(non_setline[0], Stmt::Expr(Expr::Whatever))
                } =>
            {
                Expr::Call {
                    name: Symbol::intern("__PROTO_DISPATCH__"),
                    args: Vec::new(),
                }
            }
            Expr::Unary { op, expr } => Expr::Unary {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::PostfixOp { op, expr } => Expr::PostfixOp {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                op: op.clone(),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
            },
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => Expr::Ternary {
                cond: Box::new(Self::rewrite_proto_dispatch_expr(cond)),
                then_expr: Box::new(Self::rewrite_proto_dispatch_expr(then_expr)),
                else_expr: Box::new(Self::rewrite_proto_dispatch_expr(else_expr)),
            },
            Expr::Call { name, args } => Expr::Call {
                name: *name,
                // Keep call arguments intact so closure literals like `{*}`
                // used as callbacks are not treated as proto dispatch.
                args: args.to_vec(),
            },
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } => Expr::MethodCall {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                name: *name,
                // Same rule as Expr::Call: don't rewrite callback arguments.
                args: args.to_vec(),
                modifier: *modifier,
                quoted: *quoted,
            },
            Expr::ArrayLiteral(items) => Expr::ArrayLiteral(
                items
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
            ),
            Expr::Index {
                target,
                index,
                is_positional,
                ..
            } => Expr::Index {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                index: Box::new(Self::rewrite_proto_dispatch_expr(index)),
                is_positional: *is_positional,
            },
            Expr::IndexAssign {
                target,
                index,
                value,
                is_positional,
            } => Expr::IndexAssign {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                index: Box::new(Self::rewrite_proto_dispatch_expr(index)),
                value: Box::new(Self::rewrite_proto_dispatch_expr(value)),
                is_positional: *is_positional,
            },
            Expr::AssignExpr {
                name,
                expr,
                is_bind,
            } => Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
                is_bind: *is_bind,
            },
            Expr::Block(stmts) => Expr::Block(Self::rewrite_proto_dispatch_stmts(stmts)),
            Expr::DoBlock { body, label } => Expr::DoBlock {
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
            },
            Expr::Try { body, catch } => Expr::Try {
                body: Self::rewrite_proto_dispatch_stmts(body),
                catch: catch
                    .as_ref()
                    .map(|b| Self::rewrite_proto_dispatch_stmts(b)),
            },
            Expr::Gather(body) => Expr::Gather(Self::rewrite_proto_dispatch_stmts(body)),
            Expr::HyperOp {
                op,
                left,
                right,
                dwim_left,
                dwim_right,
            } => Expr::HyperOp {
                op: op.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
                dwim_left: *dwim_left,
                dwim_right: *dwim_right,
            },
            Expr::MetaOp {
                meta,
                op,
                left,
                right,
            } => Expr::MetaOp {
                meta: meta.clone(),
                op: op.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
            },
            Expr::Reduction { op, expr } => Expr::Reduction {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::InfixFunc {
                name,
                left,
                right,
                modifier,
            } => Expr::InfixFunc {
                name: name.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: right
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
                modifier: modifier.clone(),
            },
            Expr::CallOn { target, args } => Expr::CallOn {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                args: args.to_vec(),
            },
            Expr::Lambda {
                param,
                body,
                is_whatever_code,
            } => Expr::Lambda {
                param: param.clone(),
                body: Self::rewrite_proto_dispatch_stmts(body),
                is_whatever_code: *is_whatever_code,
            },
            Expr::AnonSub {
                body,
                is_rw,
                is_block,
            } => Expr::AnonSub {
                body: Self::rewrite_proto_dispatch_stmts(body),
                is_rw: *is_rw,
                is_block: *is_block,
            },
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw,
                is_whatever_code,
            } => Expr::AnonSubParams {
                params: params.clone(),
                param_defs: param_defs.clone(),
                return_type: return_type.clone(),
                body: Self::rewrite_proto_dispatch_stmts(body),
                is_rw: *is_rw,
                is_whatever_code: *is_whatever_code,
            },
            other => other.clone(),
        }
    }

    pub(super) fn restore_env_preserving_existing(&mut self, saved_env: &Env, params: &[String]) {
        let current = self.env.clone();
        let mut restored = saved_env.clone();
        for key in saved_env.keys() {
            if params.iter().any(|p| *key == p.as_str()) || key == "_" || key == "@_" || key == "%_"
            {
                continue;
            }
            if let Some(v) = current.get_sym(*key) {
                restored.insert_sym(*key, v.clone());
            }
        }
        self.env = restored;
    }
}
