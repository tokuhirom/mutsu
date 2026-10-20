use super::*;

impl Compiler {
    pub(super) fn hoist_feed_body(&mut self, body: &[Stmt]) -> Option<Vec<Stmt>> {
        match self.hoist_feed_sinks(&Stmt::SyntheticBlock(body.to_vec()))? {
            Stmt::SyntheticBlock(body) => Some(body),
            _ => unreachable!("a feed-sink rewrite preserves its statement wrapper"),
        }
    }

    pub(super) fn is_feed_modifier(stmt: &Stmt) -> bool {
        matches!(
            stmt,
            Stmt::If {
                is_statement_modifier: true,
                ..
            } | Stmt::For {
                is_statement_modifier: true,
                ..
            } | Stmt::While {
                is_statement_modifier: true,
                ..
            } | Stmt::Given {
                is_statement_modifier: true,
                ..
            } | Stmt::When {
                is_statement_modifier: true,
                ..
            }
        )
    }

    /// Declaration feed sinks belong to the surrounding lexical scope even
    /// when a modifier never runs; their append runs once per taken iteration.
    // Cost: O(n), n = nodes of the modified statement.
    pub(super) fn hoist_feed_sinks(&mut self, stmt: &Stmt) -> Option<Stmt> {
        use crate::ast_visit::VisitMut;
        struct Hoist {
            declarations: Vec<Stmt>,
        }
        impl VisitMut for Hoist {
            fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
                match stmt {
                    Stmt::Expr(_) | Stmt::SyntheticBlock(_) => {
                        crate::ast_visit::walk_stmt_mut(self, stmt)
                    }
                    Stmt::If {
                        is_statement_modifier: true,
                        then_branch,
                        ..
                    } => self.visit_stmts_mut(then_branch),
                    Stmt::For {
                        is_statement_modifier: true,
                        body,
                        ..
                    }
                    | Stmt::While {
                        is_statement_modifier: true,
                        body,
                        ..
                    }
                    | Stmt::Given {
                        is_statement_modifier: true,
                        body,
                        ..
                    }
                    | Stmt::When {
                        is_statement_modifier: true,
                        body,
                        ..
                    } => self.visit_stmts_mut(body),
                    _ => {}
                }
            }
            fn visit_expr_mut(&mut self, expr: &mut Expr) {
                match expr {
                    Expr::Feed {
                        sink,
                        append: false,
                        ..
                    } => {
                        if let Expr::DoStmt(stmt) = sink.as_mut()
                            && let Stmt::VarDecl {
                                name,
                                is_state: false,
                                custom_traits,
                                ..
                            } = stmt.as_ref()
                            && !custom_traits
                                .iter()
                                .any(|(name, _)| name == "__has_initializer")
                        {
                            let target = feed_variable(name);
                            self.declarations.push((**stmt).clone());
                            **sink = target;
                        }
                        crate::ast_visit::walk_expr_mut(self, expr);
                    }
                    Expr::Grouped(_) => crate::ast_visit::walk_expr_mut(self, expr),
                    Expr::DoStmt(stmt) if Compiler::is_feed_modifier(stmt) => {
                        crate::ast_visit::walk_expr_mut(self, expr)
                    }
                    _ => {}
                }
            }
        }
        let mut rewritten = stmt.clone();
        let mut hoist = Hoist {
            declarations: Vec::new(),
        };
        hoist.visit_stmt_mut(&mut rewritten);
        if hoist.declarations.is_empty() {
            return None;
        }
        for declaration in hoist.declarations {
            self.compile_stmt(&declaration);
        }
        Some(rewritten)
    }

    /// A variable feed appends into its sink, then returns its original source.
    /// The raw call temporary preserves List/Array identity and itemization;
    /// an ordinary scalar store would change the fed value's context.
    // Cost: O(n), n = source and sink AST nodes compiled.
    pub(super) fn compile_feed_tap(&mut self, source: &Expr, sink: &Expr) -> bool {
        let sink = sink.peel_parens();
        let target = match sink {
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_) => sink.clone(),
            Expr::DoStmt(stmt) => {
                let Stmt::VarDecl { name, .. } = stmt.as_ref() else {
                    return false;
                };
                let target = feed_variable(name);
                self.compile_expr(sink);
                self.code.emit(OpCode::Pop);
                target
            }
            _ => return false,
        };
        let temp = format!("__feed#{}", super::compile_session::mint());
        let temp_idx = self.code.add_constant(Value::str(temp.clone()));
        self.compile_expr(source);
        self.code.emit(OpCode::Dup);
        self.code.emit(OpCode::SetCallTemp(temp_idx));
        self.compile_expr(&Expr::MethodCall {
            target: Box::new(target),
            name: Symbol::intern("append"),
            // Feed sinks iterate scalar-held aggregates too. A plain scalar
            // argument would be re-itemized by ordinary call compilation.
            args: vec![Expr::ZenSlice(Box::new(Expr::Var(temp)))],
            modifier: None,
            quoted: false,
            sugar: false,
        });
        self.code.emit(OpCode::Pop);
        true
    }
}

// Cost: O(n), n = name length.
fn feed_variable(name: &str) -> Expr {
    match name.split_at_checked(1) {
        Some(("@", name)) => Expr::ArrayVar(name.into()),
        Some(("%", name)) => Expr::HashVar(name.into()),
        Some(("&", name)) => Expr::CodeVar(name.into()),
        _ => Expr::Var(name.into()),
    }
}

/// Expand a callable or legacy append feed. Operands remain expressions so
/// nested variable taps take the compiler's common append path.
pub(crate) fn lower_feed_node(source: Expr, sink: Expr, append: bool) -> Expr {
    if append {
        build_append_feed_expr(source, sink)
    } else {
        build_pipe_feed_expr(source, sink)
    }
}

pub(crate) fn build_pipe_feed_expr(source: Expr, sink: Expr) -> Expr {
    match sink {
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
            ..
        } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(source),
            is_positional,
            spelling,
        },
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value: Box::new(source),
            is_positional,
        },
        Expr::Call { name, mut args, .. } => {
            args.push(source);
            Expr::Call {
                name,
                args,
                listop: false,
            }
        }
        Expr::CallOn { target, mut args } => {
            args.push(source);
            Expr::CallOn { target, args }
        }
        Expr::BareWord(name) => Expr::Call {
            name: Symbol::intern(&name),
            args: vec![source],
            listop: false,
        },
        Expr::Whatever => Expr::Call {
            name: Symbol::intern("__mutsu_feed_whatever"),
            args: vec![source],
            listop: false,
        },
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
                listop: false,
            }),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::ArrayVar(name), source],
                listop: false,
            }),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::HashVar(name), source],
                listop: false,
            }),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
            ..
        } => {
            let current = Expr::Index {
                target: target.clone(),
                index: index.clone(),
                is_positional,
                spelling: Default::default(),
            };
            Expr::IndexAssign {
                target,
                index,
                value: Box::new(Expr::Call {
                    name: Symbol::intern("__mutsu_feed_append"),
                    args: vec![current, source],
                    listop: false,
                }),
                is_positional: true,
                spelling,
            }
        }
        Expr::Whatever => Expr::Call {
            name: Symbol::intern("__mutsu_feed_append_whatever"),
            args: vec![source],
            listop: false,
        },
        other => Expr::Call {
            name: Symbol::intern("__mutsu_feed_append"),
            args: vec![other, source],
            listop: false,
        },
    }
}
