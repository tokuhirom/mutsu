use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::symbol::Symbol;
use crate::value::Value;

static SUPPLY_EMITTER_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

pub(crate) fn supply_method_call(body: Vec<Stmt>) -> Expr {
    // Each `supply { ... }` block gets a UNIQUE emitter variable name. The
    // emitter is bound as the on-demand lambda's parameter and `emit` is
    // rewritten to `$emitter.emit(...)`. A shared name would be clobbered when
    // supply blocks nest at runtime: a chained transform
    // `supply { whenever (supply { ... }) { emit ... } }` runs the inner block
    // inside the outer's frame, and with one shared name the outer's `emit`
    // would resolve to the inner emitter (an infinite emit loop). A per-parse
    // unique name keeps each block's `emit` bound to its own emitter.
    let id = SUPPLY_EMITTER_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let emitter_name = format!("__mutsu_supply_emitter_{id}");
    let lowered_body = rewrite_supply_body(body, &emitter_name);
    Expr::MethodCall {
        target: Box::new(Expr::BareWord("Supply".to_string())),
        name: Symbol::intern("on-demand"),
        args: vec![Expr::Lambda {
            param: emitter_name,
            body: lowered_body,
            is_whatever_code: false,
        }],
        modifier: None,
        quoted: false,
    }
}

fn rewrite_supply_body(stmts: Vec<Stmt>, emitter_name: &str) -> Vec<Stmt> {
    // Phasers are set up at block entry, not when control textually reaches them.
    // Hoist top-level CLOSE phaser registrations to the front of the body so a
    // CLOSE that appears after a (potentially non-terminating) loop is still
    // registered before the loop runs — e.g.
    //   supply { until my $done { emit(...) } CLOSE { $done = True } }
    // relies on the CLOSE phaser being able to break the loop.
    let mut closes = Vec::new();
    let mut rest = Vec::new();
    for stmt in stmts {
        let lowered = rewrite_supply_stmt(stmt, emitter_name);
        if is_close_registration(&lowered) {
            closes.push(lowered);
        } else {
            rest.push(lowered);
        }
    }
    closes.extend(rest);
    closes
}

/// True if `stmt` is the registration call a CLOSE phaser is lowered to.
fn is_close_registration(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Expr(Expr::MethodCall { name, .. })
            if name.resolve().as_str() == "__mutsu_register_close_phaser"
    )
}

fn rewrite_supply_stmt(stmt: Stmt, emitter_name: &str) -> Stmt {
    match stmt {
        Stmt::Expr(expr) => {
            if let Expr::Call { name, args } = &expr
                && name.resolve().as_str() == "emit"
            {
                return Stmt::Expr(Expr::MethodCall {
                    target: Box::new(Expr::Var(emitter_name.to_string())),
                    name: Symbol::intern("emit"),
                    args: args.clone(),
                    modifier: None,
                    quoted: false,
                });
            }
            // `.emit` (topic method call) inside a supply block is
            // `$emitter.emit($_)` — emit the current topic value.
            if let Expr::MethodCall {
                target, name, args, ..
            } = &expr
                && matches!(target.as_ref(), Expr::Var(n) if n == "_")
                && name.resolve().as_str() == "emit"
                && args.is_empty()
            {
                return Stmt::Expr(Expr::MethodCall {
                    target: Box::new(Expr::Var(emitter_name.to_string())),
                    name: Symbol::intern("emit"),
                    args: vec![Expr::Var("_".to_string())],
                    modifier: None,
                    quoted: false,
                });
            }
            Stmt::Expr(expr)
        }
        Stmt::Call { name, args } if name.resolve().as_str() == "emit" => {
            // Statement-form `emit ARGS;` becomes `$emitter.emit(ARGS)`.
            let positional_args: Vec<Expr> = args
                .into_iter()
                .filter_map(|arg| match arg {
                    crate::ast::CallArg::Positional(expr) => Some(expr),
                    _ => None,
                })
                .collect();
            Stmt::Expr(Expr::MethodCall {
                target: Box::new(Expr::Var(emitter_name.to_string())),
                name: Symbol::intern("emit"),
                args: positional_args,
                modifier: None,
                quoted: false,
            })
        }
        Stmt::ReactDone => Stmt::SyntheticBlock(vec![
            Stmt::Expr(Expr::MethodCall {
                target: Box::new(Expr::Var(emitter_name.to_string())),
                name: Symbol::intern("done"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            }),
            Stmt::Return(Expr::Literal(Value::Nil)),
        ]),
        Stmt::Block(stmts) => Stmt::Block(rewrite_supply_body(stmts, emitter_name)),
        Stmt::SyntheticBlock(stmts) => {
            Stmt::SyntheticBlock(rewrite_supply_body(stmts, emitter_name))
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var,
        } => Stmt::If {
            cond,
            then_branch: rewrite_supply_body(then_branch, emitter_name),
            else_branch: rewrite_supply_body(else_branch, emitter_name),
            binding_var,
        },
        Stmt::While { cond, body, label } => Stmt::While {
            cond,
            body: rewrite_supply_body(body, emitter_name),
            label,
        },
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat,
            label,
        } => Stmt::Loop {
            init: init.map(|boxed| Box::new(rewrite_supply_stmt(*boxed, emitter_name))),
            cond,
            step,
            body: rewrite_supply_body(body, emitter_name),
            repeat,
            label,
        },
        Stmt::For {
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
        } => Stmt::For {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body: rewrite_supply_body(body, emitter_name),
            label,
            mode,
            rw_block,
            explicit_zero_params,
        },
        Stmt::Given { topic, body } => Stmt::Given {
            topic,
            body: rewrite_supply_body(body, emitter_name),
        },
        Stmt::When { cond, body } => Stmt::When {
            cond,
            body: rewrite_supply_body(body, emitter_name),
        },
        Stmt::Default(body) => Stmt::Default(rewrite_supply_body(body, emitter_name)),
        Stmt::Catch(body) => Stmt::Catch(rewrite_supply_body(body, emitter_name)),
        Stmt::Control(body) => Stmt::Control(rewrite_supply_body(body, emitter_name)),
        Stmt::React { body } => Stmt::React {
            body: rewrite_supply_body(body, emitter_name),
        },
        Stmt::Whenever {
            supply,
            param,
            body,
        } => Stmt::Whenever {
            supply,
            param,
            body: rewrite_supply_body(body, emitter_name),
        },
        Stmt::Subtest { name, body } => Stmt::Subtest {
            name,
            body: rewrite_supply_body(body, emitter_name),
        },
        // A CLOSE phaser in a `supply { ... }` block registers its body as a
        // close callback on the emitter, to run when the tap is closed or the
        // supply terminates. Rewrite it to a registration call so it survives
        // as a value (a bare phaser compiles to a no-op).
        Stmt::Phaser {
            kind: crate::ast::PhaserKind::Close,
            body,
        } => Stmt::Expr(Expr::MethodCall {
            target: Box::new(Expr::Var(emitter_name.to_string())),
            name: Symbol::intern("__mutsu_register_close_phaser"),
            args: vec![make_anon_sub(rewrite_supply_body(body, emitter_name))],
            modifier: None,
            quoted: false,
        }),
        // Phaser bodies (LAST/QUIT/FIRST/NEXT/...) inside a supply/whenever
        // can also `emit`/`done`; rewrite them to the emitter just like the
        // main body so e.g. `LAST { emit "done" }` forwards to the supply.
        Stmt::Phaser { kind, body } => Stmt::Phaser {
            kind,
            body: rewrite_supply_body(body, emitter_name),
        },
        Stmt::Label { name, stmt } => Stmt::Label {
            name,
            stmt: Box::new(rewrite_supply_stmt(*stmt, emitter_name)),
        },
        other => other,
    }
}
