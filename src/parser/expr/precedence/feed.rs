use super::*;

/// Build a deferred `Expr::Feed` node, resolving the operator's direction so the
/// node always means "`source` flows into `sink`". Deferring the fold (rather than
/// folding into the sink call immediately) lets an assignment/declaration on the
/// textually-left side split it at Sequencer precedence (looser than `=`); see
/// `Expr::Feed`, `lower_feed_node`, and `feed_leftmost_operand_mut`.
pub(in crate::parser::expr) fn make_feed_node(op: FeedOp, left: Expr, right: Expr) -> Expr {
    let (source, sink, append, left_is_source) = match op {
        FeedOp::ToRight => (left, right, false, true),
        FeedOp::ToLeft => (right, left, false, false),
        FeedOp::AppendRight => (left, right, true, true),
        FeedOp::AppendLeft => (right, left, true, false),
    };
    Expr::Feed {
        source: Box::new(source),
        sink: Box::new(sink),
        append,
        left_is_source,
    }
}

/// Lower a (possibly chained) `Expr::Feed` node into its executable form. Nested
/// feeds on the source/sink sides are lowered first; non-feed operands are left
/// for the compiler to handle normally. Called by the compiler's `Expr::Feed` arm.
pub(crate) fn lower_feed_node(source: Expr, sink: Expr, append: bool) -> Expr {
    let source = match source {
        Expr::Feed {
            source: s,
            sink: k,
            append: a,
            ..
        } => lower_feed_node(*s, *k, a),
        other => other,
    };
    let sink = match sink {
        Expr::Feed {
            source: s,
            sink: k,
            append: a,
            ..
        } => lower_feed_node(*s, *k, a),
        other => other,
    };
    if append {
        build_append_feed_expr(source, sink)
    } else {
        build_pipe_feed_expr(source, sink)
    }
}

/// Descend a (possibly chained) `Expr::Feed` to the mutable slot holding its
/// textually-leftmost operand — the one adjacent to a preceding `=`. For `==>`
/// chains that is the deepest `source`; for `<==` chains the deepest `sink`.
/// Used to split `my @a = … ==> …` so the declaration wraps that operand.
pub(crate) fn feed_leftmost_operand_mut(feed: &mut Expr) -> &mut Expr {
    match feed {
        Expr::Feed {
            source,
            sink,
            left_is_source,
            ..
        } => {
            if *left_is_source {
                feed_leftmost_operand_mut(source)
            } else {
                feed_leftmost_operand_mut(sink)
            }
        }
        other => other,
    }
}

pub(crate) fn build_pipe_feed_expr(source: Expr, sink: Expr) -> Expr {
    match sink {
        // Feeding into a scalar collects the feed into an Array, like an array
        // sink: `42 ==> $x` and `(1,2,3) ==> $x` both leave `$x` as `[…]`
        // (an Array), matching Raku (the scalar holds the materialized feed).
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_array_assign"),
                args: vec![source],
            }),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_array_assign"),
                args: vec![source],
            }),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(source),
            is_bind: false,
        },
        Expr::CodeVar(name) => Expr::AssignExpr {
            name: format!("&{}", name),
            expr: Box::new(source),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
        } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(source),
            is_positional,
        },
        Expr::MultiDimIndex { target, dimensions } => Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value: Box::new(source),
        },
        Expr::Call { name, mut args } => {
            args.push(source);
            Expr::Call { name, args }
        }
        Expr::CallOn { target, mut args } => {
            args.push(source);
            Expr::CallOn { target, args }
        }
        Expr::BareWord(name) => Expr::Call {
            name: Symbol::intern(&name),
            args: vec![source],
        },
        Expr::Whatever => Expr::Call {
            name: Symbol::intern("__mutsu_feed_whatever"),
            args: vec![source],
        },
        // Feed into an inline declaration: `... ==> my @o`. The sink parses as a
        // `DoStmt` wrapping a `VarDecl` with a default (empty) initializer;
        // rebuild it with the fed source as the initializer so the new variable
        // (declared in the enclosing scope by `compile_expr_do_stmt`) receives
        // the feed. Mirrors the bare-variable arms above (an array sink flattens
        // the source via `__mutsu_feed_array_assign`).
        Expr::DoStmt(stmt) if matches!(stmt.as_ref(), Stmt::VarDecl { .. }) => {
            let Stmt::VarDecl {
                name,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint,
                ..
            } = *stmt
            else {
                unreachable!("guarded by matches! above")
            };
            // Array and scalar sinks materialize the feed into an Array (see the
            // bare-variable arms above); a hash (`%`) or code (`&`) sink takes
            // the source directly. Scalar `VarDecl` names are stored without a
            // sigil (`my $x` -> "x"), arrays/hashes with one ("@o"/"%h").
            let is_hash_or_code = name.starts_with('%') || name.starts_with('&');
            let init = if is_hash_or_code {
                source
            } else {
                Expr::Call {
                    name: Symbol::intern("__mutsu_feed_array_assign"),
                    args: vec![source],
                }
            };
            Expr::DoStmt(Box::new(Stmt::VarDecl {
                name,
                expr: init,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint,
            }))
        }
        other => Expr::CallOn {
            target: Box::new(other),
            args: vec![source],
        },
    }
}

pub(crate) fn build_append_feed_expr(source: Expr, sink: Expr) -> Expr {
    match sink {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::Var(name), source],
            }),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::ArrayVar(name), source],
            }),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::HashVar(name), source],
            }),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => {
            let current = Expr::Index {
                target: target.clone(),
                index: index.clone(),
                is_positional,
            };
            Expr::IndexAssign {
                target,
                index,
                value: Box::new(Expr::Call {
                    name: Symbol::intern("__mutsu_feed_append"),
                    args: vec![current, source],
                }),
                is_positional: true,
            }
        }
        Expr::Whatever => Expr::Call {
            name: Symbol::intern("__mutsu_feed_append_whatever"),
            args: vec![source],
        },
        other => Expr::Call {
            name: Symbol::intern("__mutsu_feed_append"),
            args: vec![other, source],
        },
    }
}
